-module(bot_paths).
-export([spawn_bots/1, request_paths/1]).

-type position() :: {integer(), integer()}.
-type path() :: [position()].

-spec spawn_bots([{position()}]) -> ok.
spawn_bots(Sources) ->
    grid_mnesia:init(),
    grid_mnesia:initialize_grid(10),
    lists:foreach(fun({Id, Pos}) ->
        io:format("Spawning Bot ~p at ~p~n", [Id, Pos]),
        Result = grid_mnesia:add_bot(Id, Pos),
        grid_mnesia:update_grid(Pos, occupied),
        io:format("Bot ~p Insert Result: ~p~n", [Id, Result])
    end, lists:zip(lists:seq(1, length(Sources)), Sources)).

-spec request_paths([{position()}]) -> ok.
request_paths(Goals) ->
    {atomic, Bots} = mnesia:transaction(fun() -> mnesia:match_object({bot, '_', '_'}) end),
    BotPositions = [{Id, Pos} || {bot, Id, Pos} <- Bots],
    SortedBotPositions = lists:sort(fun({Id1, _}, {Id2, _}) -> Id1 < Id2 end, BotPositions),
    ReservedInitial = [Start || {_, Start} <- SortedBotPositions, Start =/= undefined],
    FullReservation = ReservedInitial ++ Goals,

    io:format("Starting pathfinding for all bots...~n"),
    BotGoalPairs = lists:zip(SortedBotPositions, Goals),
    ShuffledBotGoalPairs = shuffle_list(BotGoalPairs),
    ShuffledBots = [{Id, Pos} || {{Id, Pos}, _Goal} <- ShuffledBotGoalPairs],
    ShuffledGoals = [Goal || {_Bot, Goal} <- ShuffledBotGoalPairs],

    case find_paths_for_all(ShuffledBots, ShuffledGoals, FullReservation, []) of
        {ok, Paths} ->
            io:format("All bots found paths successfully. Reserving paths now.~n"),
            lists:foreach(fun({BotId, Path}) ->
                grid_mnesia:reserve_path(Path, BotId)
            end, Paths),
            print_paths(lists:map(fun({BotId, Path}) -> {BotId, lists:reverse(Path)} end, Paths));
        {error, _BlockedBots} ->
            retry_path_assignment(SortedBotPositions, Goals, FullReservation, 50)
    end.

-spec find_paths_for_all([{integer(), position()}], [position()], [position()], [{integer(), path()}]) -> {ok, [{integer(), path()}]} | {error, [integer()]}.
find_paths_for_all(Bots, Goals, Reserved, Acc) ->
    BotGoalPairs = lists:zip(Bots, Goals),
    find_paths_for_all_pairs(BotGoalPairs, Reserved, Acc).

-spec find_paths_for_all_pairs([{{integer(), position()}, position()}], [position()], [{integer(), path()}]) -> {ok, [{integer(), path()}]} | {error, [integer()]}.
find_paths_for_all_pairs([], _, Acc) -> {ok, Acc};
find_paths_for_all_pairs([{{BotId, Start}, Goal} | Rest], Reserved, Acc) ->
    UpdatedReserved = Reserved -- [Start],
    FinalReservation = UpdatedReserved -- [Goal],
    case astar:find_path(Start, Goal, 10, FinalReservation) of
        {error, _} -> {error, [BotId | [B || {{B, _}, _} <- Rest]]};
        {ok, Path} -> find_paths_for_all_pairs(Rest, Reserved ++ Path, [{BotId, Path} | Acc])
    end.

-spec retry_path_assignment([{integer(), position()}], [position()], [position()], integer()) -> ok.
retry_path_assignment(_, _, _, 0) ->
    io:format("Failed to assign paths after multiple attempts.~n");
retry_path_assignment(Bots, Goals, Reserved, Attempts) ->
    SortedBots = lists:sort(fun({Id1, _}, {Id2, _}) -> Id1 < Id2 end, Bots),
    BotGoalPairs = lists:zip(SortedBots, Goals),
    ShuffledBotGoalPairs = shuffle_list(BotGoalPairs),
    ShuffledBots = [{Id, Pos} || {{Id, Pos}, _Goal} <- ShuffledBotGoalPairs],
    ShuffledGoals = [Goal || {_Bot, Goal} <- ShuffledBotGoalPairs],
    case find_paths_for_all(ShuffledBots, ShuffledGoals, Reserved, []) of
        {ok, Paths} ->
            lists:foreach(fun({BotId, Path}) ->
                grid_mnesia:reserve_path(Path, BotId)
            end, Paths),
            print_paths(lists:map(fun({BotId, Path}) -> {BotId, lists:reverse(Path)} end, Paths));
        {error, _BlockedBots} ->
            retry_path_assignment(Bots, Goals, Reserved, Attempts - 1)
    end.

-spec shuffle_list([T]) -> [T].
shuffle_list(List) ->
    Randomized = [{rand:uniform(), X} || X <- List],
    Sorted = lists:sort(Randomized),
    [X || {_, X} <- Sorted].

-spec print_paths([{integer(), path()}]) -> ok.
print_paths(Paths) ->
    io:format("Final bot paths:~n"),
    lists:foreach(fun({BotId, Path}) ->
        io:format("Bot ~p -> Path: ~p~n", [BotId, Path])
    end, Paths).
