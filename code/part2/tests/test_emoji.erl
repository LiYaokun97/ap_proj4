-module(test_emoji).
-export([test_all/0]).
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(tests(), [verbose]).

tests() ->
  [ {"Basic behaviour", spawn,
    [ test_start_server(),
      test_new_shortcode(),
      test_lookup(),
      test_alias(),
      test_delete(),
      test_stop_server(),
      test_analytics(),
      test_get_analytics(),
      test_remove_analytics(),
      test_analytics_lookup()] } ].

test_start_server() ->
  [{"Test: Call start/1 without crash.",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([]))
    end },
    {"Test:Call start/1 with unique one value Initial.",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"Test with two different values Initial.",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"poop", <<240, 159, 152, 131>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"Test error with two same values Initial.",
      fun ()->
        Initial = [{"smiley", <<240,159,148,145>>}, {"smiley", <<240, 159, 152, 131>>}],
        ?assertMatch({error, "Duplicated input shortcodes!"}, emoji:start(Initial))
      end}].

test_new_shortcode() ->
  [{"Test:Register new shortcode",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(ok, emoji:new_shortcode(S, "smiley", <<240,159,148,145>>))
    end },
    {"Test Error: Assign an existing shortcode",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"poop", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, "Same shortcode, should not register it again!"}, emoji:new_shortcode(S, "smiley", <<240,240,240,240>>))
      end }].

test_alias() ->
  [{"Test Error: Non-exist Alias of shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, "Short1 is not registered!"}, emoji:alias(S, "smiley", "other"))
    end },
    {"Test:Alias of shortcode",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:alias(S, "smiley", "other"))
      end },
    {"Test:Alias of shortcode 2 which exists",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"other", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, "Short2 is already registered!"}, emoji:alias(S, "smiley", "other"))
      end }].

test_lookup() ->
  [{"Test:Lookup a non-existing shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Test:Lookup of a non-exist shortcode 2",
      fun () ->
        Initial = [{"smiley", <<240,159,144,142>>},{"poop", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(no_emoji, emoji:lookup(S, "other"))
      end },
    {"Test:Lookup an existing shortcode",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>},{"other", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "other"))
      end },
    {"Test:Lookup an existing shortcode with its alias",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        ?assertEqual({ok, <<240,159,148,145>>}, emoji:lookup(S, "other"))
      end }].

test_delete() ->
  [{"Test: Delete shortcode with empty Initial",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Test: Delete shortcode which has an alias",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual(no_emoji, emoji:lookup(S, "other"))
      end },
    {"Test: Delete shortcode with its alias",
      fun () ->
        Initial = [{"smiley", <<42, 42, 42, 42>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        emoji:delete(S, "other"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")), 
        ?assertEqual(no_emoji, emoji:lookup(S, "other"))
      end },
    {"Test: Delete specific shortcode without changing others",
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

test_stop_server() ->
  [{"Test stopping server",
    fun () ->
      Initial = [{"smiley", <<240,159,148,145>>}, {"poop", <<240,159,140,138>>}],
      {ok, E} = emoji:start(Initial),
      ?assertEqual(ok, emoji:stop(E))
    end }].

test_analytics() ->
  [{"Test: Analyzing a server with empty Initial",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, "Short is not registered!"}, emoji:analytics(S, "smiley", fun(_, N) -> N*10 end, "Add", 0))
    end },
    {"Test: Analyzing a server with shortcode in Initial",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N*10 end, "Add", 0))
      end },
    {"Test: Adding different label analytics to shortcodes",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>},{"poop", <<240,159,144,142>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "poop", fun(_, N) -> N*1 end, "Multi", 1))
      end },
    {"Test: Adding two different analytics to one shortcode",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 1))
      end },
    {"Test: Adding label analytics to shortcode with alias",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "other"),
        ?assertEqual(ok, emoji:analytics(S, "other", fun(_, N) -> N-1 end, "Minus", 1))
      end }].


test_get_analytics() ->
  [{"Test: Get the analytics of a shortcode that does not exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:get_analytics(S, "Pikachu"))
    end },
    {"Test: Get another analytics of the not existing shortcode ",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:get_analytics(S, "Pikachu"))
      end },
    {"Test: Get the three analytics of the existing shortcode",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "Multi", 1),
        ?assertMatch({ok, [{"Multi",1}, {"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "smiley"))
      end },
    {"Test: Get the analytics from an existing shortcode with alias",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley","smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smiley")) ,
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "smileyAlias"))
      end },
    {"Test: Get the analytics from an existing shortcode which alias one analytics, and another to different shortcode",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Add",0}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Minus",0}]}, emoji:get_analytics(S, "happy"))
      end }].

test_remove_analytics() ->
  [{"Test: Removing analytics which shortcode doesnt exist",
    fun () ->
      Initial = [{"smiley", <<240,159,148,145>>}],
      {ok, S} = emoji:start(Initial),
      emoji:remove_analytics(S, "smiley", "Add"),
      ?assertMatch({error, _}, emoji:get_analytics(S, "happy"))
    end },
    {"Test: Removing analytics which shortcode doesnt exist, continue example",
      fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "smiley", "Add"),
        ?assertMatch({error, _}, emoji:get_analytics(S, "happy"))
      end },
    {"Test: Removing analytics which label does not exist",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "smiley", "Add"),
        ?assertEqual({ok, []}, emoji:get_analytics(S, "smiley"))
      end },
    {"Test: Remove analytics which shortocode alias and label exist, check by alias",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smileyAlias"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "smileyAlias", "Add"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))
      end },
    {"Test: Remove all analytics which shortocode and label exist",
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
    {"Test: Remove analytics which shortocode alias and other shortcodes stays same",
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

test_analytics_lookup() ->
  [
    {"Test: Lookup of emtpy analytics",
      fun () ->
        Initial = [{"smiley", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))
      end },
    {"Test: Lookup of an analytics using  alias and origin shortcode",
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
    {"Test: Lookup of one analytics for one shortcode, other remain the previous state",
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
    {"Test: Register Hit and Throw (fun(S, _) -> throw(S) end) for alias",
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
