-module(bot_paths_test).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {
        setup,
        fun() ->
            mnesia:start(),
            grid_mnesia:init(),
            grid_mnesia:initialize_grid(10)
        end,
        fun(_) ->
            mnesia:stop(),
            mnesia:delete_schema([node()])
        end,
        [
            {
                "TestCase 1: Spawn bots and verify positions",
                fun() ->
                    bot_paths:spawn_bots([{1, 1}, {2, 2}, {3, 3}]),
                    {atomic, Bots} = mnesia:transaction(fun() -> mnesia:match_object({bot, '_', '_'}) end),
                    ExpectedBots = [{bot, 1, {1, 1}}, {bot, 2, {2, 2}}, {bot, 3, {3, 3}}],
                    ?assertEqual(lists:sort(ExpectedBots), lists:sort(Bots))
                end
            },
            {
                "TestCase 2: Request paths for multiple bots",
                fun() ->
                    Starts1 = [{0,0}, {9,9}, {0,9}, {9,0}, {5,5}],
                    Goals1 = [{4,4}, {5,9}, {9,4}, {4,0}, {6,6}],
                    bot_paths:spawn_bots(Starts1),
                    {ok, Paths1} = bot_paths:request_paths(Goals1),
                    ?assert(validate_paths(Starts1, Goals1, Paths1)),

                    Starts2 = [{1,1}, {8,8}, {1,8}, {8,1}, {4,4}],
                    Goals2 = [{7,7}, {2,2}, {7,2}, {2,7}, {5,5}],
                    bot_paths:spawn_bots(Starts2),
                    {ok, Paths2} = bot_paths:request_paths(Goals2),
                    ?assert(validate_paths(Starts2, Goals2, Paths2)),

                    Starts3 = [{0,0}, {9,9}, {4,4}, {0,9}, {9,0}],
                    Goals3 = [{9,0}, {0,9}, {2,2}, {7,7}, {3,3}],
                    bot_paths:spawn_bots(Starts3),
                    ?assertMatch({error, []}, bot_paths:request_paths(Goals3))
                end
            }
        ]
    }.

validate_paths(Starts, Goals, Paths) ->
    SortedPaths = lists:sort(fun({Id1, _}, {Id2, _}) -> Id1 < Id2 end, Paths),
    lists:foreach(fun({BotId, Path}) -> io:format("Bot ~p -> Path: ~p~n", [BotId, Path]) end, SortedPaths),
    ValidPaths = lists:all(fun({BotId, Path}) ->
        case Path of
            [Start | _] = FullPath ->
                End = lists:last(FullPath),
                {_, ExpectedStart} = lists:keyfind(BotId, 1, lists:zip(lists:seq(1, length(Starts)), Starts)),
                {_, ExpectedGoal} = lists:keyfind(BotId, 1, lists:zip(lists:seq(1, length(Goals)), Goals)),
                io:format("Validating bot ~p: Expected Start: ~p, Actual Start: ~p, Expected End: ~p, Actual End: ~p~n",
                          [BotId, ExpectedStart, Start, ExpectedGoal, End]),
                (Start =:= ExpectedStart) and (End =:= ExpectedGoal)
        end
    end, SortedPaths),
    AllCoordinates = lists:flatten([Path || {_, Path} <- SortedPaths]),
    UniqueCoordinates = lists:usort(AllCoordinates),
    PathsDoNotCross = (length(AllCoordinates) =:= length(UniqueCoordinates)),
    ValidPaths and PathsDoNotCross.
