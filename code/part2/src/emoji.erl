-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
  analytics/5, get_analytics/2, remove_analytics/3,
  stop/1]).

%% Time Complexity is O(n) rather than O(n^2)
-spec checkDuplicate(List :: list()) -> boolean().
checkDuplicate(List) ->
  MyMap = maps:from_list(List),
  maps:size(MyMap) == length(List).


%% For every shortcode, we save it in a tuple {Emoji, [alias], [analysis], root}.
%% where the second position is for alias list and the third one for analysis function.
%% root is used in alias shortcode, which records shortcode that is first registered.
start(Initial) ->
  case checkDuplicate(Initial) of
    false -> {error, "Duplicated input shortcodes!"};
    true ->
      EmojiList = lists:map(
        fun(Emoji) ->
          case Emoji of
            {ShortCode, Emo} -> {ShortCode, {Emo, [], [], ShortCode}}
          end
        end, Initial),
      EmojiDict = maps:from_list(EmojiList),
      try
        E = spawn(fun() -> loop(EmojiDict) end),
        {ok, E}
      catch
        _:Reason -> {error, Reason}
      end
  end.

new_shortcode(E, Short, Emo) ->
  E ! {self(), new_shortcode, Short, Emo},
  receive
    {E, ok} -> ok;
    {E, {error, Reason}} -> {error, Reason}
  end.

alias(E, ShortCode1, ShortCode2) ->
  E ! {self(), alias, ShortCode1, ShortCode2},
  receive
    {E, ok} -> ok;
    {E, {error, Reason}} -> {error, Reason}
  end.

delete(E, Short) ->
  E ! {self(), delete, Short},
  ok.

lookup(E, Short) ->
  E ! {self(), lookup, Short},
  receive
    {E, {ok, Emo}} -> {ok, Emo};
    {E, no_emoji} -> no_emoji
  end.

analytics(E, Short, Fun, Label, Init) ->
  E ! {self(), analytics, Short, Fun, Label, Init},
  receive
    {E, ok} -> ok;
    {E, {error, Reason}} -> {error, Reason}
  end.

get_analytics(E, Short) ->
  E ! {self(), get_analytics, Short},
  receive
    {E, {ok, Stat}} -> {ok, Stat};
    {E, {error, Reason}} -> {error, Reason}
  end.

remove_analytics(E, Short, Label) ->
  E ! {self(), remove_analytics, Short, Label}.

stop(E) ->
  E ! {self(), stop},
  receive
    {E, ok} -> ok;
    {E, {error, Reason}} -> {error, Reason}
  end.

inEmojiDict(Short, EmojiDict) ->
  maps:is_key(Short, EmojiDict).


updateAnalysisState(FuncList, ShortCode) ->
  case FuncList of
    [] -> [];
    [{Fun, Label, State }| Rest] ->
      try Fun(ShortCode, State) of
        NewState -> [{Fun, Label, NewState}| updateAnalysisState(Rest, ShortCode)]
      catch
        _ -> [{Fun, Label, State}| updateAnalysisState(Rest, ShortCode)]
      end
  end.


sameLabelInFuncList(Label, FuncList) ->
  case FuncList of
    [] -> false;
    [{_, CurLabel, _}| Rest] ->
      case CurLabel =:= Label of
        true -> true;
        false -> sameLabelInFuncList(Label, Rest)
      end
  end.

getRealData(ShortCode, EmojiDict) ->
  {_, _, _, Root} = maps:get(ShortCode, EmojiDict),
  maps:get(Root, EmojiDict).


getLabelAndState(FuncList) ->
  lists:map(
    fun(X) ->
      case X of
        {_, Label, State} -> {Label, State}
      end
    end,
    FuncList).


removeElement(FuncList, L) ->
  case FuncList of
    [] -> [];
    [{Fun, Label, State} | Rest] ->
      case Label =:= L of
        true -> Rest;
        false -> [{Fun, Label, State} | removeElement(Rest, L)]
      end
  end.

deleteShort(EmojiDict, List) ->
  case List of
    [] -> EmojiDict;
    [First | ListRest] ->
      deleteShort(maps:remove(First, EmojiDict), ListRest)
  end.

loop(EmojiDict) ->
  receive
    {From, new_shortcode, Short, Emo} ->
      case inEmojiDict(Short, EmojiDict) of
        false ->
          From ! {self(), ok},
          NewEmojiDict = maps:put(Short, {Emo, [], [], Short}, EmojiDict),
          loop(NewEmojiDict);
        true ->
          From ! {self(), {error, "Same shortcode, should not register it again!"}},
          loop(EmojiDict)
      end;

    {From, alias, Short1, Short2} ->
      case inEmojiDict(Short1, EmojiDict) of
        false ->
          From ! {self(), {error, "Short1 is not registered!"}},
          loop(EmojiDict);
        true ->
          case inEmojiDict(Short2, EmojiDict) of
            true ->
              From ! {self(), {error, "Short2 is already registered!"}},
              loop(EmojiDict);
            false ->
              From ! {self(), ok},
              case getRealData(Short1, EmojiDict) of
                {Emo, AliasList, AnalysisList, Root} ->
                  NewEmojiDict = maps:put(Root, {Emo, [Short2 | AliasList], AnalysisList, Root}, EmojiDict),
                  NewEmojiDict2 = maps:put(Short2, {Emo, [], [], Root}, NewEmojiDict),
                  loop(NewEmojiDict2)
              end
          end
      end;

    {_, delete, Short} ->
      case inEmojiDict(Short, EmojiDict) of
        false -> loop(EmojiDict);
        true ->
          {_, AliasList, _, Root} = getRealData(Short, EmojiDict),
          NewEmojiDict = deleteShort(EmojiDict, [Root|AliasList]),
          loop(NewEmojiDict)
      end;

    {From, lookup, Short} ->
      case inEmojiDict(Short, EmojiDict) of
        false ->
          From ! {self(), no_emoji},
          loop(EmojiDict);
        true ->
          {Emoji, AliasList, FuncList, Root} = getRealData(Short, EmojiDict),
          From ! {self(), {ok, Emoji}},
          NewFuncList = updateAnalysisState(FuncList, Short),
          NewEmojiDict = maps:put(Root, {Emoji, AliasList, NewFuncList, Root}, EmojiDict),
          loop(NewEmojiDict)
      end;

    {From, stop} -> From ! {self(), ok};

    {From, analytics, Short, Fun, Label, Init} ->
      case inEmojiDict(Short, EmojiDict) of
        false ->
          From ! {self(), {error, "Short is not registered!"}},
          loop(EmojiDict);
        true ->
          {Emo, AliasList, FuncList, Root} = getRealData(Short, EmojiDict),
          case sameLabelInFuncList(Label, FuncList) of
            true ->
              From ! {self(), {error, "The Label is already registered!"}},
              loop(EmojiDict);
            false ->
              NewEmojiDict = maps:put(Root, {Emo, AliasList, [{Fun, Label, Init} |FuncList], Root} , EmojiDict),
              From ! {self(), ok},
              loop(NewEmojiDict)
          end
      end;

    {From, get_analytics, Short} ->
      case inEmojiDict(Short, EmojiDict) of
        true ->
          {_, _ , FuncList, _} = getRealData(Short, EmojiDict),
          From ! {self(), {ok, getLabelAndState(FuncList)}},
          loop(EmojiDict);
        false ->
          From ! {self(), {error, "Short is not registered!"}},
          loop(EmojiDict)
      end;

    {_, remove_analytics, Short, Label} ->
      case inEmojiDict(Short, EmojiDict) of
        false -> loop(EmojiDict);
        true ->
          {Emo, AliasList, FuncList, Root} = getRealData(Short, EmojiDict),
          NewFuncList = removeElement(FuncList, Label),
          NewEmojiDict = maps:put(Root, {Emo, AliasList, NewFuncList, Root}, EmojiDict),
          loop(NewEmojiDict)
      end;

    {From, _, _} ->
      From ! {self(), {error, "Unknown Command!"}},
      loop(EmojiDict)

  end.