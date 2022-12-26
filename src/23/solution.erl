-module(solution).

-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([part_1/0, part_2/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    Matrix = parse_matrix(readlines("testInput.txt")),
    simulate(Matrix, 10).

part_2() ->
    readlines("testInput.txt").

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

simulate(_, 0) ->
    ok;
simulate(Matrix, Rounds) ->
    propose_positions(Matrix),
    move_elves(Matrix),
    simulate(Matrix, Rounds - 1).

propose_positions(Matrix) ->
    ets:foldl(
        fun({X, Y}, _) ->
            check_neighbours
    ).

parse_matrix(Lines) ->
    Ets = ets:new(tab, [private, bag]),
    fori(fun(Y, LineBin) ->
            Line = binary_to_list(LineBin),
            fori(
                fun(X, Val) ->
                    case Val of
                        $# -> ets:insert(Ets, {{X, Y}, {none, none}});
                        _ -> ok
                    end
                end,
                Line)
         end,
         Lines),
    Ets.

fori(F, List) when is_function(F, 2), is_list(List) ->
    case List of
        [Hd | Tail] ->
            F(0, Hd),
            fori(F, Tail, 1);
        [] ->
            []
    end.

fori(F, [Hd | Tail], Index) ->
    F(Index, Hd),
    fori(F, Tail, Index + 1);
fori(_F, [], _Index) ->
    ok.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).
