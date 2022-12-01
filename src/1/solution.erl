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

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    lists:max(lists:foldl(
        fun fold_fun/2,
        [0],
        readlines("input.txt")
    )).

part_2() ->
    [A, B, C | _] = lists:reverse(lists:sort(lists:foldl(
        fun fold_fun/2,
        [0],
        readlines("input.txt")
    ))),
    A + B + C.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

fold_fun(<<>>, AccSumList) ->
    [0 | AccSumList];
fold_fun(<<Number/binary>>, [Sum | Rest]) ->
    [Sum + binary_to_integer(Number) | Rest].

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).