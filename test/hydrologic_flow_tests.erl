-module(hydrologic_flow_tests).
-include_lib("eunit/include/eunit.hrl").

hydrologic_flow_tests_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        hydrologic:new(
          testf,
          [
           {fun(X) ->
                {reduce, X rem 2 == 0}
            end, a},
           {b, fanin},
           return,
           {a, fun(X) ->
                   X * 2
               end},
           b
          ]
         ),
        ?assertEqual({ok, [2, 2, 6, 4]},
                     hydrologic:run(testf, [1, 2, 3, 4])),
        hydrologic:new(
          test,
          [
           {fun(X) ->
                {reduce, X rem 2 == 0}
            end, aa},
           {bb, return},
           {aa, fun(X) ->
                   X * 2
               end},
           bb
          ]
         ),
        ?assertEqual(
           {ok, [Y || {ok, Y} <- [hydrologic:run(test, X) || X <- [1, 2, 3, 4]]]},
           hydrologic:run(testf, [1, 2, 3, 4])),
        hydrologic:stop(testf),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {{match, ["Hello"]}, a},
           fun(X) ->
               string:concat(X, " World!")
           end,
           {i, fanin},
           return,
           {a, fun string:to_upper/1},
           i
          ]
         ),
        ?assertEqual(
           {ok, ["BONJOUR", "Hello World!", "HOLA"]},
           hydrologic:run(test, ["Bonjour", "Hello", "Hola"])),
        hydrologic:stop(test)
    end
   ]}.
