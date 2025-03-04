-module(astar).
-export([find_path/4]).

-type position() :: {integer(), integer()}.
-type grid_size() :: integer().
-type path() :: [position()].
-type reserved_positions() :: [position()].
-type search_result() :: {ok, path()} | {error, no_path}.

-spec manhattan(position(), position()) -> integer().
manhattan({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2).

-spec find_path(position(), position(), grid_size(), reserved_positions()) -> search_result().
find_path(Start, Goal, GridSize, Reserved) ->
    search(Start, fun neighbour_fn/3, fun score_fn/3, Goal, GridSize, Reserved).

-spec search(position(), fun(), fun(), position(), grid_size(), reserved_positions()) -> search_result().
search(Start, NeighbourFn, ScoreFn, Goal, GridSize, Reserved) ->
    {OpenQueue, ClosedSet, ParentMap} = data_sets(),
    push(OpenQueue, [], Start, ScoreFn([], Start, Goal)),
    ets:insert(ParentMap, {Start, none}),
    continue(OpenQueue, ClosedSet, ParentMap, NeighbourFn, ScoreFn, Goal, GridSize, Reserved).

-spec continue(reference(), reference(), reference(), fun(), fun(), position(), grid_size(), reserved_positions()) -> search_result().
continue(OpenQueue, ClosedSet, ParentMap, NeighbourFn, ScoreFn, Goal, GridSize, Reserved) ->
    case pop_best(OpenQueue, ClosedSet) of
        none -> {error, no_path};
        {_, _Path, Goal} -> reconstruct_path(ParentMap, Goal);
        {_BestScore, Path, State} ->
            lists:foreach(
                fun(Nstate) ->
                    case {is_closed(ClosedSet, Nstate), is_open(OpenQueue, Nstate), ets:lookup(ParentMap, Nstate)} of
                        {true, _, _} -> skip;
                        {_, true, _} -> skip;
                        {_, _, [{Nstate, _}]} -> skip;
                        _ ->
                            Nfullpath = [State | Path],
                            push(OpenQueue, Nfullpath, Nstate, ScoreFn(Nfullpath, Nstate, Goal)),
                            ets:insert(ParentMap, {Nstate, State})
                    end
                end,
                NeighbourFn(State, GridSize, Reserved)
            ),
            continue(OpenQueue, ClosedSet, ParentMap, NeighbourFn, ScoreFn, Goal, GridSize, Reserved)
    end.

-spec reconstruct_path(reference(), position()) -> {ok, path()}.
reconstruct_path(ParentMap, Goal) ->
    reconstruct_path(ParentMap, Goal, []).

reconstruct_path(ParentMap, State, Acc) ->
    case ets:lookup(ParentMap, State) of
        [{State, none}] -> {ok, lists:reverse([State | Acc])};
        [{State, Parent} | _] -> reconstruct_path(ParentMap, Parent, [State | Acc]);
        _ -> {error, no_path}
    end.

-spec neighbour_fn(position(), grid_size(), reserved_positions()) -> [position()].
neighbour_fn({X, Y}, GridSize, Reserved) ->
    Neighbors = [
        {X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}
    ],
    ValidNeighbors = [{NX, NY} || {NX, NY} <- Neighbors, 
                                   NX >= 0, NX < GridSize, 
                                   NY >= 0, NY < GridSize, 
                                   not lists:member({NX, NY}, Reserved)],
    lists:sort(fun(_, _) -> rand:uniform() > 0.5 end, ValidNeighbors).

-spec score_fn(path(), position(), position()) -> integer().
score_fn(Path, State, Goal) ->
    G = length(Path),
    H = manhattan(State, Goal),
    G + H.

-spec data_sets() -> {reference(), reference(), reference()}.
data_sets() ->
    OpenQueue = ets:new(openqueue, [public, ordered_set]),
    ClosedSet = ets:new(closedset, [public, set]),
    ParentMap = ets:new(parentmap, [public, set, {keypos, 1}]),
    {OpenQueue, ClosedSet, ParentMap}.

-spec push(reference(), path(), position(), integer()) -> true.
push(OpenQueue, Path, State, Score) ->
    ets:insert(OpenQueue, {Score, Path, State}).

-spec pop_best(reference(), reference()) -> none | {integer(), path(), position()}.
pop_best(OpenQueue, ClosedSet) ->
    case ets:first(OpenQueue) of
        '$end_of_table' -> none;
        BestScore ->
            case ets:lookup(OpenQueue, BestScore) of
                [{BestScore, Path, State}] ->  
                    ets:delete(OpenQueue, BestScore),
                    ets:insert(ClosedSet, {State}),
                    {BestScore, Path, State};
                _ -> none
            end
    end.

-spec is_open(reference(), position()) -> boolean().
is_open(OpenQueue, State) -> ets:member(OpenQueue, State).

-spec is_closed(reference(), position()) -> boolean().
is_closed(ClosedSet, State) -> ets:member(ClosedSet, State).
