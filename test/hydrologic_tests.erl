-module(hydrologic_tests).
-include_lib("eunit/include/eunit.hrl").

hydrologic_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        Pipeline = hydrologic:new(
                     [
                      {a, {fun(X) ->
                               {reduce, X rem 2 == 0}
                           end, b}},
                      fun(X) ->
                          {map, X * 2}
                      end,
                      {b, fun(X) ->
                              {map, X + 2}
                          end},
                      return
                     ]
                    ),
        ?assertEqual(22, hydrologic:run(Pipeline, 10)),
        ?assertEqual(9, hydrologic:run(Pipeline, 7)),
        hydrologic:stop(Pipeline)
    end,
    fun() ->
        Pipeline = hydrologic:new(
                     [
                      {fun(X) ->
                           {reduce, X rem 2 == 0}
                       end, a},
                      fun(X) ->
                          {map, X * 2}
                      end,
                      {b, fun(X) ->
                              {map, X + 2}
                          end},
                      return,
                      {a, b}
                     ]
                    ),
        ?assertEqual(22, hydrologic:run(Pipeline, 10)),
        ?assertEqual(9, hydrologic:run(Pipeline, 7)),
        hydrologic:stop(Pipeline)
    end
   ]}.
