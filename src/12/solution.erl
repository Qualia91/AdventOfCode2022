-module(solution).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    part_1/0,
    part_2/0
]).

-define(START, $S).
-define(END, $E).

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    {StartPos, EndPos, Graph, _} = create_graph(readlines("input.txt")),
    PathLength = calc_shorted_path(StartPos, EndPos, Graph),
    digraph:delete(Graph),
    PathLength.

part_2() ->
    {StartPos, EndPos, Graph, LowestElevationPositions} = create_graph(readlines("input.txt")),
    StartingPathLength = calc_shorted_path(StartPos, EndPos, Graph),
    FinalLowestPathLength = lists:foldl(
        fun(CurrentStartPos, LowestPathLength) ->
            PathLength = calc_shorted_path(CurrentStartPos, EndPos, Graph),
            lists:min([LowestPathLength, PathLength])
        end,
        StartingPathLength,
        LowestElevationPositions
    ),
    digraph:delete(Graph),
    FinalLowestPathLength.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

create_graph(Lines) ->
    Graph = digraph:new(),
    LinesIter = lists:zip(lists:seq(0, length(Lines) - 1), Lines),
    Matrix = lists:foldl(
        fun({Y, LineBin}, AccMap) ->
            Line = binary_to_list(LineBin),
            LineIter = lists:zip(lists:seq(0, length(Line) - 1), Line),
            maps:merge(
                AccMap,
                lists:foldl(
                    fun({X, Val}, AccMapInner) ->
                        AccMapInner#{{X, Y} => Val}
                    end,
                    AccMap,
                    LineIter)
            )
        end,
        #{},
        LinesIter
    ),
    {StartPos, EndPos, LowestElevationPositions} = maps:fold(
        fun(Key, Value, {StartPosIn, EndPosIn, LowestElevAcc}) ->
            {UValue, UStartVal, UEndVal, ULowestElevAcc} = case Value of
                ?START -> {97, Key, EndPosIn, LowestElevAcc};
                ?END -> {122, StartPosIn, Key, LowestElevAcc};
                97 -> {Value, StartPosIn, EndPosIn, [Key | LowestElevAcc]};
                _ -> {Value, StartPosIn, EndPosIn, LowestElevAcc}
            end,
            digraph:add_vertex(Graph, Key),
            add_neighbours(Graph, Key, UValue, Matrix),
            {UStartVal, UEndVal, ULowestElevAcc}
        end,
        {ok, ok, []},
        Matrix
    ),
    {StartPos, EndPos, Graph, LowestElevationPositions}.

calc_shorted_path(StartPos, EndPos, Graph) ->
    case digraph:get_short_path(Graph, StartPos, EndPos) of
        false -> 9999999999999;
        Path -> length(Path) + 1
    end.

add_neighbours(Graph, {X, Y}, InValue, Matrix) ->
    add_neighbour(Graph, {X, Y}, {X + 1, Y}, InValue, Matrix),
    add_neighbour(Graph, {X, Y}, {X - 1, Y}, InValue, Matrix),
    add_neighbour(Graph, {X, Y}, {X, Y + 1}, InValue, Matrix),
    add_neighbour(Graph, {X, Y}, {X, Y - 1}, InValue, Matrix).

add_neighbour(Graph, InPos, NePos, InValue, Matrix) ->
    case maps:find(NePos, Matrix) of
        {ok, NeValue} ->
            case can_reach_next_value(InValue, NeValue) of
                true -> digraph:add_edge(Graph, InPos, NePos);
                false -> ok
            end,
            case can_reach_next_value(NeValue, InValue) of
                true -> digraph:add_edge(Graph, NePos, InPos);
                false -> ok
            end;
        error ->
            ok
    end.

can_reach_next_value(V1, V2) ->
    ((V2 - V1) < 2).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).