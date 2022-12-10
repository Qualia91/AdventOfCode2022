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

-define(WIDTH, 40).

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    calculate_signal_sum(parse_input(readlines("input.txt")), [20, 60, 100, 140, 180, 220]).

part_2() ->
    Input = parse_input(readlines("input.txt")),
    render_lines(lists:zip(lists:seq(1, length(Input)), Input)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

render_lines(Inputs) ->
    io:format("~s", [lists:foldl(
        fun({Index, SpritePos}, AccBin) ->
            Pixel = create_pixel(Index, SpritePos),
            draw(Pixel, AccBin, Index)
        end,
        "",
        Inputs
    )]).

draw(Pixel, AccBin, Index) ->
    case Index rem ?WIDTH of
        0 -> AccBin ++ Pixel ++ "\n";
        _ -> AccBin ++ Pixel
    end.

create_pixel(Index, SpritePos) ->
    Column = Index rem ?WIDTH,
    case is_sprite_overlapping(Column, SpritePos) of
        true -> "#";
        false -> "."
    end.

is_sprite_overlapping(Column, SpritePos) ->
    SpritePos =< Column andalso Column =< SpritePos + 2.

parse_input(Lines) ->
    lists:reverse(lists:foldl(
        fun line_fold_fun/2,
        [1],
        Lines)).

calculate_signal_sum(InputList, TargetSignals) ->
    lists:foldl(
        fun(TargetSignal, Sum) ->
            (lists:nth(TargetSignal, InputList) * TargetSignal) + Sum
        end,
        0,
        TargetSignals).

line_fold_fun(<<"addx", _:1/binary, ValueBin/binary>>, [LastValue | _] = Acc) ->
    [binary_to_integer(ValueBin) + LastValue, LastValue| Acc];
line_fold_fun(<<"noop">>, [LastValue | _] = Acc) ->
    [LastValue | Acc].

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).