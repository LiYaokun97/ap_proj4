-module(test_emoji).
-export([test_all/0]).
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
  [ {"Basic behaviour", spawn,
    [ test_start_server(),
      test_stop_server(),
      test_new_shortcode(),
      test_lookup(),
      test_alias(),
      test_delete(),
      test_analytics(),
      test_get_analytics(),
      test_remove_analytics(),
      test_analytics_lookup()] } ].

test_start_server() ->
  [{"First test: call start/1 without crash",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([]))
    end },
    {"Test:Call start/1 with unique one value Initial",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"Test with unique two different values",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"happy", <<222, 222, 222, 222>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"Test with two alias values Initial and it returns {ok,E}",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"super_smiley", <<240, 159, 152, 131>>}],
        ?assertMatch({ok, _}, emoji:start(Initial))
      end },
    {"Test error with two same values Initial.",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"Key", <<240,159,148,145>>}],
        ?assertMatch({error, _}, emoji:start(Initial))
      end }].

test_stop_server() ->
  [{"Test Starting and stopping",
    fun () ->
      Initial = [{"Key", <<240,159,148,145>>}, {"water wave", <<240,159,140,138>>}],
      {ok, E} = emoji:start(Initial),
      ?assertEqual(ok, emoji:stop(E))
    end },
    {"Test:Starting the servers and stopping them",
      fun () ->
        Initial1 = [{"Key", <<240,159,148,145>>}],
        Initial2 = [{"water wave", <<240,159,140,138>>}],
        {ok, E1} = emoji:start(Initial1),
        {ok, E2} = emoji:start(Initial2),
        ?assertEqual(ok, emoji:stop(E1)),
        ?assertEqual(ok, emoji:stop(E2))
      end }].

test_new_shortcode() ->
  [{"Test:Register new shortcode",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(ok, emoji:new_shortcode(S, "Key", <<240,159,148,145>>))
    end },
    {"Test:Register a new shortcode with unique value Initial",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "happy", <<222, 222, 222, 222>>))
      end },
    {"Test:Test: regist using 2 same shortcode to one value Initial ",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"water wave", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "Key", <<240,240,240,240>>))
      end },
    {"Test:RegisterError test: Assign an existing shortcode to two unique value Initial",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"water wave", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "Key", <<240,240,240,240>>))
      end },
    {"Test:Register two unique shortcode with same emoji sequence",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"water wave", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "lucky", <<240,159,140,138>>))
      end }].

test_lookup() ->
  [{"Test:Lookup a non-exist shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "key"))
    end },

    {"Test:Lookup of an existing shortcode with alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>},{"other_key", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "other_key"))
      end },
    {"Test:Lookup an existing shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual({ok, <<240,159,148,145>>}, emoji:lookup(S, "Key"))
      end },
    {"Test:Lookup of shortcode in shortcodes which exists",
      fun () ->
        Initial = [{"horse", <<240,159,144,142>>},{"water wave", <<240,159,140,138>>},{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual({ok, <<240,159,140,138>>}, emoji:lookup(S, "water wave"))
      end },
    {"Test:Lookup of a non-exist shortcode in two shortcodes",
      fun () ->
        Initial = [{"horse", <<240,159,144,142>>},{"water wave", <<240,159,140,138>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(no_emoji, emoji:lookup(S, "Key"))
      end }].

test_alias() ->
  [{"Test: Non-exist Alias of shortcode",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Test:Alias of shortcode which exists",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley"))
      end },
    {"Test:Alias of shortcode 2 which exists",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"other_smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:alias(S, "smiley", "other_smiley"))
      end },
    {"Test:Alias of alias of shortcode which exists",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"other_smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:alias(S, "other_smiley", "cute_smiley"))
      end },
    {"Test:Alias of an existing alias",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley")),
        ?assertEqual(ok, emoji:alias(S, "other_smiley", "alias_other_smiley"))
      end },
    {"Test:Alias of alias of shortcode which exists",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley")),
        ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley_alias")),
        ?assertEqual(ok, emoji:alias(S, "smiley", "more_alias"))
      end }].

test_delete() ->
  [{"Test: Delete shortcode with empty Initial",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Test: Delete shortcode with one value Initial",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
      end },
    {"Test: Delete shortcode with one unique value Initial with alias",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "nice_smiley"),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual(no_emoji, emoji:lookup(S, "nice_smiley"))
      end },
    {"Test: Delete specific shortcode without changing others",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<42, 42, 42, 42>>},
          {"intelligence", <<99, 99, 99, 99>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer")),
        ?assertEqual({ok, <<42, 42, 42, 42>>}, emoji:lookup(S, "happy")),
        ?assertEqual({ok, <<99, 99, 99, 99>>}, emoji:lookup(S, "intelligence"))
      end },
    {"Test: Delete alias and every related shortcode",
      fun () ->
        Initial = [{"happy", <<42, 42, 42, 42>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "happy", "funny"),
        %emoji:alias(S, "happy", "good"),
        emoji:delete(S, "funny"),
        ?assertEqual(no_emoji, emoji:lookup(S, "happy")), %% this line
        %?assertEqual(no_emoji, emoji:lookup(S, "good")),
        ?assertEqual(no_emoji, emoji:lookup(S, "funny")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer"))
      end },
    {"Test: Delete specific shortcode other shortcode no change",
      fun () ->
        Initial = [{"happy", <<42, 42, 42, 42>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "happy", "funny"),
        emoji:alias(S, "happy", "good"),
        emoji:alias(S, "software_engineer", "programmer"),
        emoji:alias(S, "software_engineer", "problem_solver"),
        emoji:delete(S, "funny"),
        ?assertEqual(no_emoji, emoji:lookup(S, "happy")),
        ?assertEqual(no_emoji, emoji:lookup(S, "funny")),
        ?assertEqual(no_emoji, emoji:lookup(S, "good")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "programmer")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "problem_solver"))
      end }].

test_analytics() ->
  [{"Test: Analyzing a server with empty Initial",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:analytics(S, "key", fun(_, N) -> N*10 end, "Add", 0))
    end },
    {"Test: Analyzing a server with shortcode in Initial",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "Key", fun(_, N) -> N*10 end, "Add", 0))
      end },
    {"Test: Analyzing with shortcode adding alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "other_key"),
        ?assertEqual(ok, emoji:analytics(S, "other_key", fun(_, N) -> N-2 end, "Minus", 0)) ,
        ?assertMatch({error, _}, emoji:analytics(S, "other_key", fun(_, N) -> N-2 end, "Minus", 0))
      end },
    {"Test: Adding different label analytics to shortcodes",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>},{"horse", <<240,159,144,142>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "horse", fun(_, N) -> N*1 end, "Multi", 1))
      end },
    {"Test: Adding two different analytics to one shortcode which is reasonable",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "Key", fun(_, N) -> N-1 end, "Minus", 1))
      end },
    {"Test: Adding two same label analytics to shortcode and alias of that shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "other_key"),
        ?assertEqual(ok, emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0)),
        ?assertEqual(ok, emoji:analytics(S, "other_key", fun(_, N) -> N-1 end, "Minus", 1))
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
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:get_analytics(S, "Pikachu"))
      end },
    {"Test: Get the analytics of the existing shortcode with empty analytics",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Get the analytics of the existing shortcode with real analytic",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Get all analytics from an existing shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Get the three analytics of the existing shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N*1 end, "Multi", 1),
        ?assertMatch({ok, [{"Multi",1}, {"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Get the analytics from an existing shortcode with alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key","KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key")) ,
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "KeyAlias"))
      end },
    {"Test: Get the two analytics from an existing shortcode alias two analytics",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "KeyAlias", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "KeyAlias")),
        ?assertMatch({ok, [{"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Get the three analytics from an existing shortcode alias three analytics",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:alias(S, "KeyAlias", "KeyAlias2"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "KeyAlias", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "KeyAlias2", fun(_, N) -> N*1 end, "Multi", 1),
        ?assertMatch({ok, [{"Multi",1}, {"Minus",0}, {"Add",0}]}, emoji:get_analytics(S, "KeyAlias2"))
      end },
    {"Test: Get the analytics from an existing shortcode with analytic and not from other shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key")),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "happy"))
      end },
    {"Test: Get the analytics from an existing shortcode which alias one analytics, and another to different shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Add",0}]}, emoji:get_analytics(S, "Key")),
        ?assertMatch({ok, [{"Minus",0}]}, emoji:get_analytics(S, "happy"))
      end }].

test_remove_analytics() ->
  [{"Test: Removing analytics which shortcode doesnt exist",
    fun () ->
      Initial = [{"Key", <<240,159,148,145>>}],
      {ok, S} = emoji:start(Initial),
      emoji:remove_analytics(S, "Key", "Add"),
      ?assertMatch({error, _}, emoji:get_analytics(S, "happy"))
    end },
    {"Test: Removing analytics which shortcode doesnt exist, continue example",
      fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "Key", "Add"),
        ?assertMatch({error, _}, emoji:get_analytics(S, "happy"))
      end },
    {"Test: Removing analytics which label does not exist",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "Key", "Add"),
        ?assertEqual({ok, []}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Removing analytics which shortocode has alias and label exists, check by shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "Key", "Add"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Remove analytics which shortocode alias and label exist, check by alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "KeyAlias", "Add"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Remove analytics which shortocode and label exist check shortcode alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "Key", "Add"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "KeyAlias"))
      end },
    {"Test: Remove analytics which shortocode and label exist",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:remove_analytics(S, "Key", "Minus"),
        ?assertEqual({ok,[{"Add", 0}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Remove all analytics which shortocode and label exist",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N*1 end, "Multi", 1),
        emoji:remove_analytics(S, "Key", "Add"),
        emoji:remove_analytics(S, "Key", "Minus"),
        emoji:remove_analytics(S, "Key", "Multi"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Remove all analytics and check by shortocode as well as alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "KeyAlias", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "Key", fun(_, N) -> N*1 end, "Multi", 1),
        emoji:remove_analytics(S, "KeyAlias", "Multi"),
        emoji:remove_analytics(S, "Key", "Add"),
        emoji:remove_analytics(S, "Key", "Minus"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "Key")),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "KeyAlias"))
      end },
    {"Test: Remove analytics which shortocode alias and other shortcodes stays same",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"happy", <<42, 42, 42, 42>>},
          {"intelligence", <<99, 99, 99, 99>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "KeyAlias", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "intelligence", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "KeyAlias", "Add"),
        ?assertEqual({ok,[{"Minus", 0}]}, emoji:get_analytics(S, "KeyAlias")),
        ?assertEqual({ok,[{"Minus", 0}]}, emoji:get_analytics(S, "Key")),
        ?assertEqual({ok,[{"Add", 0}]}, emoji:get_analytics(S, "happy")),
        ?assertEqual({ok,[{"Add", 0}]}, emoji:get_analytics(S, "intelligence"))
      end },
    {"Test: Remove all analytics which alias other shortcodes stays same",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"happy", <<42, 42, 42, 42>>},
          {"intelligence", <<99, 99, 99, 99>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "Key", "KeyAlias"),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "KeyAlias", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "intelligence", fun(_, N) -> N+1 end, "Add", 0),
        emoji:remove_analytics(S, "KeyAlias", "Add"),
        emoji:remove_analytics(S, "KeyAlias", "Minus"),
        emoji:remove_analytics(S, "happy", "Add"),
        emoji:remove_analytics(S, "intelligence", "Add"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "Key")),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "KeyAlias")),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "happy")),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "intelligence"))
      end }].

test_analytics_lookup() ->
  [
    {"Test: Lookup of emtpy analytics",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:lookup(S, "Key"),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Lookup of one analytics from one shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key")),
        emoji:lookup(S, "Key"),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Lookup of an analytics using  alias and origin shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:alias(S, "Key", "KeyAlias"),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key")),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "KeyAlias")),
        emoji:lookup(S, "KeyAlias"),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "Key")),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "KeyAlias"))
      end },
    {"Test: Lookup of two analytics from one shortcode",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key")),
        emoji:lookup(S, "Key"),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "Key")),
        emoji:analytics(S, "Key", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus", 0},{"Add", 1}]}, emoji:get_analytics(S, "Key")),
        emoji:lookup(S, "Key"),
        ?assertMatch({ok, [{"Minus", -1},{"Add", 2}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Lookup of one analytics for one shortcode, other remain the previous state",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Add", 0),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "Key")),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "happy")),
        emoji:lookup(S, "Key"),
        ?assertMatch({ok, [{"Add", 1}]}, emoji:get_analytics(S, "Key")),
        ?assertMatch({ok, [{"Add", 0}]}, emoji:get_analytics(S, "happy"))
      end },
    {"Test: Register Hit and Throw (fun(S, _) -> throw(S) end)",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "Key", fun(T, _) -> throw(T) end, "Throw", 1),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 0}]}, emoji:get_analytics(S, "Key")),
        emoji:lookup(S, "Key"),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 1}]}, emoji:get_analytics(S, "Key"))
      end },
    {"Test: Register Hit and Throw (fun(S, _) -> throw(S) end) for alias",
      fun () ->
        Initial = [{"Key", <<240,159,148,145>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "Key", fun(_, N) -> N+1 end, "Add", 0),
        emoji:analytics(S, "Key", fun(T, _) -> throw(T) end, "Throw", 1),
        emoji:alias(S, "Key", "KeyAlias"),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 0}]}, emoji:get_analytics(S, "Key")),
        emoji:lookup(S, "KeyAlias"),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 1}]}, emoji:get_analytics(S, "KeyAlias")),
        ?assertMatch({ok, [{"Throw", 1}, {"Add", 1}]}, emoji:get_analytics(S, "Key"))
      end }].
