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

-define(PATTERN, "(?<A>\\d+),(?<B>\\d+),(?<C>\\d+)").

find_point(X, Y, Z) -> [{{'$1','$2','$3'},[{'==','$1',{const,X}},{'==','$2',{const,Y}},{'==','$3',{const,Z}}],[found]}].

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    Ets = ets:new(tab, [private, bag]),
    parse_points(Ets, readlines("input.txt")),
    find_faces(Ets).

part_2() ->
    Ets = ets:new(tab, [private, bag]),
    parse_points(Ets, readlines("input.txt")),
    Limits = find_min_and_max(Ets),
    find_faces(flood_fill(Ets, Limits)) - num_outside_faces(Limits).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

num_outside_faces({MinX, MaxX, MinY, MaxY, MinZ, MaxZ}) ->
    DX = MaxX - MinX + 1,
    DY = MaxY - MinY + 1,
    DZ = MaxZ - MinZ + 1,
    2 * ((DX * DY) + (DY * DZ) + (DX * DZ)).

flood_fill(Ets, Limits = {MinX, _, MinY, _, MinZ, _}) ->
    StartPoint = {MinX, MinY, MinZ},
    NewPoints = ets:new(tab, [private, bag]),
    flood_fill(StartPoint, Ets, NewPoints, Limits),
    NewPoints.

flood_fill(Point = {X, Y, Z}, Ets, NewPoints, Limits) ->
    ets:insert(NewPoints, Point),
    AdjPoints = filter_in_bounds(Ets, NewPoints, adjacent_points(X, Y, Z), Limits),
    lists:foreach(
        fun(NextPoint) ->
            flood_fill(NextPoint, Ets, NewPoints, Limits)
        end,
        AdjPoints
    ).

filter_in_bounds(OldPoints, NewPoints, PotentialPoints, {MinX, MaxX, MinY, MaxY, MinZ, MaxZ}) ->
    lists:filter(
        fun(PotPoint = {X, Y, Z}) ->
            X >= MinX andalso X =< MaxX andalso
            Y >= MinY andalso Y =< MaxY andalso
            Z >= MinZ andalso Z =< MaxZ andalso
            doesnt_contains_point(PotPoint, OldPoints) andalso
            doesnt_contains_point(PotPoint, NewPoints)
        end,
        PotentialPoints
    ).

doesnt_contains_point(Point, Points) ->
    ets:match(Points, Point) == [].

find_min_and_max(Ets) ->
    {Xs, Ys, Zs} = lists:unzip3(ets:tab2list(Ets)),
    {lists:min(Xs) - 1, lists:max(Xs) + 1, lists:min(Ys) - 1, lists:max(Ys) + 1, lists:min(Zs) - 1, lists:max(Zs) + 1}.

adjacent_points(X, Y, Z) ->
    [
        {X - 1, Y, Z},
        {X + 1, Y, Z},
        {X, Y - 1, Z},
        {X, Y + 1, Z},
        {X, Y, Z - 1},
        {X, Y, Z + 1}
    ].

adjacent_side_patterns(X, Y, Z) ->
    [
        find_point(X - 1, Y, Z),
        find_point(X + 1, Y, Z),
        find_point(X, Y - 1, Z),
        find_point(X, Y + 1, Z),
        find_point(X, Y, Z - 1),
        find_point(X, Y, Z + 1)
    ].

is_open_side(Ets, SelectPattern) ->
    case ets:select(Ets, SelectPattern) of
        [found] -> 0;
        [] -> 1
    end.

find_faces(Ets) ->
    ets:foldl(
        fun({X, Y, Z}, FaceSum) ->
            lists:foldl(
                fun(SearchPattern, Sum) ->
                    is_open_side(Ets, SearchPattern) + Sum
                end,
                FaceSum,
                adjacent_side_patterns(X, Y, Z)
            )
        end,
        0,
        Ets
    ).

parse_points(Ets, Lines) ->
    lists:foreach(
        fun(Line) ->
            {match, [X, Y, Z]} = re:run(Line, ?PATTERN, [{capture, all_names, binary}]),
            ets:insert(Ets, {binary_to_integer(X), binary_to_integer(Y), binary_to_integer(Z)})
        end,
        Lines
    ).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).