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

-define(SAND_START_POS, {500, 0}).

get_all_x() -> [{{{'$1','_'},'_'},[],['$1']}].
get_all_y() -> [{{{'_','$1'},'_'},[],['$1']}].
get_sand() -> [{{{'_','_'},'$1'},[{'==','$1',{const,2}}],['$1']}].

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    Matrix = parse_lines(readlines("input.txt")),
    Limits = find_matrix_limits(Matrix),
    simulate(Matrix, Limits, start),
    length(ets:select(Matrix, get_sand())).

part_2() ->
    Matrix = parse_lines(readlines("input.txt")),
    {_, _, _, MY} = find_matrix_limits(Matrix),
    simulate(Matrix, {floor, MY + 1}, start),
    length(ets:select(Matrix, get_sand())) + 1.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

simulate(Matrix, Limits, start) ->
    % Create sand point at start point
    SandPos = ?SAND_START_POS,
    simulate(Matrix, Limits, SandPos);
simulate(Matrix, Limits, SP) ->
    % Find where it can move to
    find_move_spot(SP, Limits, Matrix).

find_move_spot(SP = {SX, SY}, Limits, Matrix) ->
    % First check down, then bottom left, then bottom right
    CheckPos = [{SX, SY + 1}, {SX - 1, SY + 1}, {SX + 1, SY + 1}],
    case find_next_point(SP, CheckPos, Matrix) of
        none -> insert_and_continue(SP, Limits, Matrix);
        NextPos -> check_limits_and_continue(NextPos, Limits, Matrix)
    end.

check_limits_and_continue(SP, Limits = {floor, FloorY}, Matrix) ->
    case SP of
        {_, FloorY} -> insert_and_continue(SP, Limits, Matrix);
        ?SAND_START_POS -> ok;
        _ -> simulate(Matrix, Limits, SP)
    end;
check_limits_and_continue(SP = {SX, SY}, Limits = {LX, MX, LY, MY}, Matrix) ->
    case SX < LX orelse SX > MX orelse SY < LY orelse SY > MY of
        true -> ok;
        false -> simulate(Matrix, Limits, SP)
    end.

insert_and_continue(SP, Limits, Matrix) ->
    case SP of
        ?SAND_START_POS ->
            ok;
        _ ->
            ets:insert(Matrix, {SP, 2}),
            simulate(Matrix, Limits, start)
    end.

find_next_point(_, [], _) ->
    none;
find_next_point(_, [NextPoint | RestOfNextPoints], Matrix) ->
    case ets:lookup(Matrix, NextPoint) of
        [] -> NextPoint;
        _ -> find_next_point(NextPoint, RestOfNextPoints, Matrix)
    end.

find_matrix_limits(Matrix) ->
    Xs = ets:select(Matrix, get_all_x()),
    Ys = ets:select(Matrix, get_all_y()),
    {lists:min(Xs), lists:max(Xs), 0, lists:max(Ys)}.

parse_lines(InputLines) ->
    Ets = ets:new(matrix, [private]),
    lists:foreach(
        fun(InputLine) ->
            parse_line(string:split(InputLine, " -> ", all), Ets)
        end,
        InputLines),
    Ets.

parse_line([FirstPoint | Rest], Ets) ->
    lists:foldl(
        fun(PointBin, LastPoint) ->
            CurrentPoint = convert_to_point(PointBin),
            create_line(LastPoint, CurrentPoint, Ets),
            CurrentPoint
        end,
        convert_to_point(FirstPoint),
        Rest
    ).

convert_to_point(PointBin) ->
    [XBin, YBin] = string:split(PointBin, ",", all),
    {binary_to_integer(XBin), binary_to_integer(YBin)}.

create_line(LastPoint, CurrentPoint, Ets) ->
    Sub = sub(LastPoint, CurrentPoint),
    Dir = dir(Sub),
    Steps = steps(Sub),
    lists:foreach(
        fun(Step) ->
            ets:insert(Ets, {take_step(CurrentPoint, Dir, Step), 1})
        end,
        lists:seq(0, Steps)
    ).

sub({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

dir({X, 0}) when X > 0 ->
    {1, 0};
dir({_, 0}) ->
    {-1, 0};
dir({0, Y}) when Y > 0 ->
    {0, 1};
dir({0, _})->
    {0, -1}.

steps({X, Y}) ->
    abs(X + Y).

take_step({X, Y}, {DX, DY}, Step) ->
    {X + (DX * Step), Y + (DY * Step)}.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).