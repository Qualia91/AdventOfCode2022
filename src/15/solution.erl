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

-define(REGEX_EXTRACT_NUMBERS, <<"Sensor at x=([\\d-]*), y=([\\d-]*): closest beacon is at x=([\\d-]*), y=([\\d-]*)">>).

-define(Y, 2000000).
-define(MAX, 4000000).

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    Ets = ets:new(matrix, [private]),
    Diamonds = parse_lines(readlines("input.txt"), Ets),
    Filtered = [{{X, _}, _} | _] = lists:filter(fun diamond_filter/1, Diamonds),
    {MinX, MaxX} = lists:foldl(
        fun({{InX, _}, Dist}, {InMinX, InMaxX}) ->
            {min(InX - Dist, InMinX), max(InX + Dist, InMaxX)}
        end,
        {X, X},
        Filtered),
    Taken = ets:tab2list(Ets),
    lists:foldl(
        fun(CurrentX, Sum) ->
            CurrentPoint = {CurrentX, ?Y},
            % Check if taken already
            case lists:member(CurrentPoint, Taken) of
                true ->
                    Sum;
                false ->
                    % Check if diamond contains it
                    case does_diamond_overlap(Filtered, CurrentPoint) of
                        true -> Sum + 1;
                        false -> Sum
                    end
            end
        end,
        0,
        lists:seq(MinX, MaxX)
    ).

part_2() ->
    Start = os:system_time(1000),
    runner_manager:start_link(),
    Ets = ets:new(matrix, [private]),
    Diamonds = parse_lines(readlines("input.txt"), Ets),
    Self = self(),
    lists:foreach(
        fun(NextDiamond) ->
            spawn(fun() ->
                Self ! find_points_around_diamond(NextDiamond, remove_diamond(Diamonds, NextDiamond))
            end)
        end,
        Diamonds),
    Result = collect(length(Diamonds)),
    End = os:system_time(1000),
    Diff = (End - Start) / 1000.0,
    io:format("Result: ~p - Calculated in ~p~n", [Result, Diff]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

collect(0) ->
    not_found;
collect(Remaining) ->
    receive
        not_found -> collect(Remaining - 1);
        {X, Y} -> X * 4000000 + Y     
    end.

find_points_around_diamond({{CX, CY}, Dist}, RestOfDiamonds) ->

    U = {CX, CY - Dist - 1},
    R = {CX + Dist + 1, CY},
    D = {CX, CY + Dist + 1},
    L = {CX - Dist - 1, CY},
    
    PointPairs = [{U, R}, {R, D}, {D, L}, {L, U}],

    foldx(
        fun({A, B}, not_found) ->
            Diag = create_diagonals(A, B),
            case check_overlap(Diag, RestOfDiamonds) of
                [] ->
                    not_found;
                [Value] ->
                    {exit, Value}
            end
        end,
        not_found,
        PointPairs).

remove_diamond(Diamonds, Di) ->
    lists:filter(fun(InDi) -> InDi =/= Di end, Diamonds).

foldx(F, Accu, List) when is_function(F, 2) ->
    case List of
        [Hd | Tail] -> foldx_1(F, F(Hd, Accu), Tail);
        [] -> Accu
    end.

foldx_1(_, {exit, Value}, _) ->
    Value;
foldx_1(F, Accu, [Hd | Tail]) ->
    foldx_1(F, F(Hd, Accu), Tail);
foldx_1(_F, Accu, []) ->
    Accu.
    
check_overlap(Diag, RestOfDiamonds) ->
    lists:filter(
        fun(Point) ->
            (not does_diamond_overlap(RestOfDiamonds, Point)) andalso is_in_bounds(Point)
        end,
        Diag).

is_in_bounds({X, Y}) ->
    X >= 0 andalso X =< ?MAX andalso Y >= 0 andalso Y =< ?MAX.

does_diamond_overlap(Diamonds, CurrentPoint) ->
    lists:any(
        fun({Center, Dist}) ->
            dist(CurrentPoint, Center) =< Dist
        end,
        Diamonds
    ).

parse_lines(InputLines, Ets) ->
    lists:foldl(
        fun(InputLine, Acc) ->
            [parse_line(InputLine, Ets) | Acc]
        end,
        [],
        InputLines).

diamond_filter({{_, Y}, Dist}) ->
    % Check if diamond can reach Y
    (Y - Dist) =< ?Y andalso (Y + Dist) >= ?Y.

parse_line(Line, Ets) ->
    {match, [[_, X1, Y1, X2, Y2]]} = re:run(Line, ?REGEX_EXTRACT_NUMBERS, [{capture, all, binary}, global]),
    StartPoint = {binary_to_integer(X1), binary_to_integer(Y1)},
    EndPoint = {binary_to_integer(X2), binary_to_integer(Y2)},
    ets:insert(Ets, StartPoint),
    ets:insert(Ets, EndPoint),
    create_diamond(StartPoint, EndPoint).

create_diamond(StartPoint, EndPoint) ->
    Dist = dist(StartPoint, EndPoint),
    {StartPoint, Dist}.

dist({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

create_diagonals(A, B) ->
    Dir = sign(sub(B, A)),
    seq(A, B, Dir, []).

seq(A, B, _, Seq) when A == B ->
    [B | Seq];
seq(A, B, Dir, Seq) ->
    seq(add(A, Dir), B, Dir, [A | Seq]).

add({X, Y}, {Dx, Dy}) ->
    {X + Dx, Y + Dy};
add(Scale, {X, Y}) ->
    {X + Scale, Y + Scale}.

sign({X, Y}) ->
    {sign(X), sign(Y)};
sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

sub({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).