-module(test_emoji).
-export([test_all/0]).
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(tests(), [verbose]).

tests() ->
  [ {"Basic behaviour", spawn,
    [ check_start(),
      check_new_shortcode(),
      check_lookup(),
      check_alias(),
      delete(),
      check_stop(),
      check_analytics(),
      check_get_analytics(),
      check_remove_analytics(),
      check_analytics_lookup()] } ].

check_start() ->
  [{"start1",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([]))
    end },
    {"start2",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"start3",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"poop", <<240, 159, 152, 131>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"start4",
      fun ()->
        Initial = [{"smiley", <<240,159,148,145>>}, {"smiley", <<240, 159, 152, 131>>}],
        ?assertMatch({error, "Duplicated input shortcodes!"}, emoji:start(Initial))
      end}].

check_new_shortcode() ->
  [{"new_shortcode1",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(ok, emoji:new_shortcode(S, "smiley", <<240,159,148,145>>))
    end },
    {"new_shortcode2",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"poop", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, "Same shortcode, should not register it again!"}, emoji:new_shortcode(S, "smiley", <<240,240,240,240>>))
      end }].

check_alias() ->
  [{"alias1",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, "Short1 is not registered!"}, emoji:alias(S, "smiley", "other"))
    end },
    {"alias2",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:alias(S, "smiley", "other"))
      end },
    {"alias3",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"other", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, "Short2 is already registered!"}, emoji:alias(S, "smiley", "other"))
      end }].

check_lookup() ->
  [{"lookup1",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"lookup2",
      fun () ->
        Initial = [{"smiley", <<240,159,144,142>>},{"poop", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(no_emoji, emoji:lookup(S, "other"))
      end },
    {"lookup3",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>},{"other", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "other"))
      end },
    {"lookup4",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        ?assertEqual({ok, <<240,159,148,145>>}, emoji:lookup(S, "other"))
      end }].

delete() ->
  [{"delete1",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"delete2",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual(no_emoji, emoji:lookup(S, "other"))
      end },
    {"delete3",
      fun () ->
        Initial = [{"smiley", <<42, 42, 42, 42>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        emoji:delete(S, "other"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")), 
        ?assertEqual(no_emoji, emoji:lookup(S, "other"))
      end },
    {"delete4",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"poop", <<42, 42, 42, 42>>},
          {"i", <<99, 99, 99, 99>>}, {"intel", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "intel")),
        ?assertEqual({ok, <<42, 42, 42, 42>>}, emoji:lookup(S, "poop")),
        ?assertEqual({ok, <<99, 99, 99, 99>>}, emoji:lookup(S, "i"))
      end }].

check_stop() ->
  [{"stop_server1",
    fun () ->
      Initial = [{"smiley", <<240,159,148,145>>}, {"poop", <<240,159,140,138>>}],
      {ok, E} = emoji:start(Initial),
      ?assertEqual(ok, emoji:stop(E))
    end }].

check_analytics() ->
  [{"analytics1",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, "Short is not registered!"}, emoji:analytics(S, "smiley", fun(_, N) -> N*10 end, "Add", 0))
    end },
    {"analytics2",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N*10 end, "Add", 0))
      end },
    {"analytics3",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>},{"poop", <<240,159,144,142>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "poop", fun(_, N) -> N*1 end, "Multi", 1))
      end },
    {"analytics4",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 1))
      end },
    {"analytics5",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        ?assertEqual(ok, emoji:analytics(S, "other", fun(_, N) -> N-1 end, "Minus", 1))
      end }].


check_get_analytics() ->
  [{"get_analytics0",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:get_analytics(S, "Pikachu"))
    end },
    {"get_analytics1",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:get_analytics(S, "Pikachu"))
      end },
    {"get_analytics2",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "Multi", 1),
        ?assertMatch({ok, [{"Multi",1}, {"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "smiley"))
      end },
    {"get_analytics3",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley","smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smiley")) ,
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smileyAlias"))
      end },
    {"get_analytics4",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Add",0}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Minus",0}]}, emoji:get_analytics(S, "happy"))
      end }].

check_remove_analytics() ->
  [{"remove_analytics0",
    fun () ->
      Initial = [{"smiley", <<240,159,148,145>>}],
      {ok, S} = emoji:start(Initial),
      emoji:remove_analytics(S, "smiley", "Add"),
      ?assertMatch({error, _}, emoji:get_analytics(S, "happy"))
    end },
    {"remove_analytics1",
      fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "smiley", "Add"),
        ?assertMatch({error, _}, emoji:get_analytics(S, "happy"))
      end },
    {"remove_analytics2",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "smiley", "Add"),
        ?assertEqual({ok, []}, emoji:get_analytics(S, "smiley"))
      end },
    {"remove_analytics3",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "smileyAlias", "Add"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))
      end },
    {"remove_analytics4",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "Multi", 1),
        emoji:remove_analytics(S, "smiley", "Add"),
        emoji:remove_analytics(S, "smiley", "Minus"),
        emoji:remove_analytics(S, "smiley", "Multi"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))
      end },
    {"remove_analytics5",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"happy", <<42, 42, 42, 42>>},
          {"intelligence", <<99, 99, 99, 99>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "smileyAlias", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "intelligence", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "smileyAlias", "Add"),
        ?assertEqual({ok,[{"Minus", 0}]}, emoji:get_analytics(S, "smileyAlias")),
        ?assertEqual({ok,[{"Minus", 0}]}, emoji:get_analytics(S, "smiley")),
        ?assertEqual({ok,[{"Add", 0}]}, emoji:get_analytics(S, "happy")),
        ?assertEqual({ok,[{"Add", 0}]}, emoji:get_analytics(S, "intelligence"))
      end }].

check_analytics_lookup() ->
  [
    {"analytics_lookup1",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))
      end },
    {"analytics_lookup2",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:alias(S, "smiley", "smileyAlias"),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smileyAlias")),
        emoji:lookup(S, "smileyAlias"),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "smileyAlias"))
      end },
    {"analytics_lookup3",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "happy")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "happy"))
      end },
    {"analytics_lookup4",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "smiley", fun(T, _) -> throw(T) end, "Throw", 1),
        emoji:alias(S, "smiley", "smileyAlias"),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 0}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smileyAlias"),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 1}]}, emoji:get_analytics(S, "smileyAlias")),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 1}]}, emoji:get_analytics(S, "smiley"))
      end }].
