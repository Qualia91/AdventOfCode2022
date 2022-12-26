-module(solution).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    part_1/0,
    part_2/0,
    get_index/2
]).

get_row(Y) -> [{{'$1','$2'},[{'==','$2',{const,Y}}],['$1']}].
get_col(X) -> [{{'$1','$2'},[{'==','$1',{const,X}}],['$2']}].

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    [Line] = readlines("input.txt"),
    GasInput = binary_to_list(Line),
    Rocks = create_rocks(),
    case simulate(GasInput, Rocks, 2022) of
        {FirstStep, LastStep, OldMaxHeight, MaxHeight, Diff} ->
            io:format("Diff: ~p~n", [Diff]),
            io:format("FirstStep: ~p~n", [2022 - FirstStep]),
            io:format("LastStep: ~p~n", [2022 - LastStep]),
            io:format("DiffSteps: ~p~n", [FirstStep - LastStep]),
            io:format("OldMaxHeight: ~p~n", [OldMaxHeight]),
            io:format("MaxHeight: ~p~n", [MaxHeight]),
            io:format("DiffHeight: ~p~n", [MaxHeight - OldMaxHeight]),
            io:format("Height: ~p~n", [FirstStep div (FirstStep - LastStep) * (MaxHeight - OldMaxHeight) + OldMaxHeight - 1]);
        MaxHeight ->
            MaxHeight + 1
    end.

part_2() ->
    [Line] = readlines("testInput.txt"),
    GasInput = binary_to_list(Line),
    Rocks = create_rocks(),
    case simulate(GasInput, Rocks, 1000000000000) of
        {FirstStep, LastStep, OldMaxHeight, MaxHeight, Diff, Tab} ->
            case ets:lookup(Tab, height) of
                Val ->
                    io:format("~p~n", [Val])
            end,
            io:format("Diff: ~p~n", [Diff]),
            io:format("FirstStep: ~p~n", [1000000000000 - FirstStep]),
            io:format("LastStep: ~p~n", [1000000000000 - LastStep]),
            io:format("DiffSteps: ~p~n", [FirstStep - LastStep]),
            io:format("OldMaxHeight: ~p~n", [OldMaxHeight]),
            io:format("MaxHeight: ~p~n", [MaxHeight]),
            io:format("DiffHeight: ~p~n", [MaxHeight - OldMaxHeight]),
            io:format("FirstStep: ~p~n", [FirstStep]),
            io:format("Rem: ~p~n", [(FirstStep - 1) rem (FirstStep - LastStep)]),
            io:format("Height: ~p~n", [FirstStep div (FirstStep - LastStep) * (MaxHeight - OldMaxHeight) + OldMaxHeight - 1]);
        MaxHeight ->
            MaxHeight + 1
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

simulate(GasInput, Rocks, StepsRemaining) ->
    simulate(GasInput, Rocks, StepsRemaining, 1, 1, ets:new(tab, [private, bag]), -1).

simulate(_, _, 0, _, _, _, MaxHeight) ->
    % io:format("~p~n", [ets:tab2list(TakenPoints)]),
    MaxHeight + 1;
simulate(GasInput, Rocks, StepsRemaining, RockIndex, GasIndex, TakenPointsTab, MaxHeight) ->
    % io:format("~p~n", [StepsRemaining]),
    RockType = get_item(RockIndex, Rocks),
    StartRock = translate_rock(RockType, {2, MaxHeight + 4}),
    SolHash = crypto:hash(sha256, term_to_binary({RockType, get_index(length(GasInput), GasIndex), get_floor(TakenPointsTab)})),
    % io:format("~p~n", [SolHash]),
    % io:format("~p~n", [ets:lookup(TakenPointsTab, SolHash)]),
    case ets:lookup(TakenPointsTab, SolHash) of
        [{SolHash, hash, OldStepsRemaining, OldMaxHeight}] ->
            {_, RockMaxHeight} = simulate_rock_fall(StartRock, GasInput, GasIndex, TakenPointsTab, MaxHeight),
            {OldStepsRemaining, StepsRemaining + 1, OldMaxHeight, MaxHeight, MaxHeight - RockMaxHeight, TakenPointsTab};
        _ ->
            % io:format("StartRock: ~p~n", [StartRock]),
            {NewGasIndex, RockMaxHeight} = simulate_rock_fall(StartRock, GasInput, GasIndex, TakenPointsTab, MaxHeight),
            ets:insert(TakenPointsTab, {SolHash, hash, StepsRemaining + 1, MaxHeight}),
            ets:insert(TakenPointsTab, {height, 1000000000000 - StepsRemaining, max(MaxHeight, RockMaxHeight)}),
            simulate(GasInput, Rocks, StepsRemaining - 1, RockIndex + 1, NewGasIndex, TakenPointsTab, max(MaxHeight, RockMaxHeight))
    end.

simulate_rock_fall(Rock, GasList, GasIndex, TakenPointsTab, MaxHeight) ->
    % Get gas
    GasImpulse = get_gas_impulse(get_item(GasIndex, GasList)),

    % check if it can move by gas input checking points with edge, then take points
    % If yes, move it. If not, dont.
    % io:format("GasImpulse: ~p~n", [GasImpulse]),
    
    RockAfterImpulse = case try_move(Rock, GasImpulse, TakenPointsTab) of
        false -> Rock;
        Value -> Value
    end,
    % io:format("RockAfterImpulse: ~p~n", [RockAfterImpulse]),
    
    % Check if it can move down with lower bound then taken points.
    % If yes, carry on simulating rock. If no, add rock points to taken points and start a new rock sim
    case try_move(RockAfterImpulse, {0, -1}, TakenPointsTab) of
        false ->
            % io:format("EndRock ~p~n~n", [RockAfterImpulse]),
            insert_points(RockAfterImpulse, TakenPointsTab),
            {GasIndex + 1, find_max_height(RockAfterImpulse)};
        Final ->
            simulate_rock_fall(Final, GasList, GasIndex + 1, TakenPointsTab, MaxHeight) 
    end.

get_floor(TakenPointsTab) ->
    Floor = lists:foldl(
        fun(X, Acc) ->
            case ets:select(TakenPointsTab, get_col(X)) of
                [] -> [0 | Acc];
                Xs -> [lists:max(Xs) | Acc]
            end
        end,
        [],
        lists:seq(0, 6)),
    Min = lists:min(Floor),
    lists:map(
        fun(A) -> A - Min end,
        Floor
    ).

insert_points(RockAfterImpulse, TakenPointsTab) ->
    ets:insert(TakenPointsTab, RockAfterImpulse).

try_move(Rock, Impulse, TakenPointsTab) ->
    NewRock = translate_rock(Rock, Impulse),
    case is_in_bounds(NewRock) of
        true ->
            case does_rock_overlap(NewRock, TakenPointsTab) of
                false -> NewRock;
                true -> false
            end;
        false ->
            false
    end.

does_rock_overlap(Rock, TakenPointsTab) ->
    lists:any(
        fun(Point) ->
            ets:match(TakenPointsTab, Point) == [[]] end,
        Rock
    ).

is_in_bounds(Rock) ->
    lists:all(
        fun({X, Y}) ->
            X >= 0 andalso X =< 6 andalso Y >= 0
        end,
        Rock 
    ).

get_item(Index, List) ->
    lists:nth(get_index(length(List), Index), List).

translate_rock(Rock, {X, Y}) ->
    lists:foldl(
        fun({RX, RY}, Acc) -> [{RX + X, RY + Y} | Acc] end,
        [],
        Rock).

find_max_height(Points) ->
    lists:foldl(
        fun({_, Y}, Max) -> max(Y, Max) end,
        -1,
        Points
    ).

create_rocks() ->
    [
        [{0, 0}, {1, 0}, {2, 0}, {3, 0}],
        [{1, 2}, {0, 1}, {1, 1}, {2, 1}, {1, 0}],
        [{2, 2}, {2, 1}, {0, 0}, {1, 0}, {2, 0}],
        [{0, 0}, {0, 1}, {0, 2}, {0, 3}],
        [{0, 0}, {1, 0}, {0, 1}, {1, 1}]
    ].

get_index(Len, Index) ->
    case Index rem Len of
        0 ->
            Len;
        Other ->
            Other
    end.

get_gas_impulse($<) ->
    {-1, 0};
get_gas_impulse($>) ->
    {1, 0}.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).