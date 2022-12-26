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
    calc(readlines("testInput.txt"), 1, 1).

part_2() ->
    calc(readlines("testInput.txt"), 811589153, 10).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

calc([Hd | Lines] = Inputs, Mult, MixAmount) ->
    Len = length(Inputs),
    {ok, StartNode} = mix_node:start_link('0', binary_to_integer(Hd), null, Len),
    Nodes = [LastNode | _] = lists:foldl(
        fun({Index, Number}, AccList = [Prev | _]) ->
            {ok, Pid} = mix_node:start_link(binary_to_atom(integer_to_binary(Index)), binary_to_integer(Number) * Mult, Prev, Len),
            mix_node:notify_left_of_right(Prev, Pid),
            [Pid | AccList]
        end,
        [StartNode],
        lists:zip(lists:seq(1, length(Lines)), Lines)
    ),
    mix_node:notify_right_of_left(StartNode, LastNode),
    mix_node:notify_left_of_right(LastNode, StartNode),
    lists:foreach(
        fun(Node) ->
            mix_node:move(Node),
            io:format("~p~n", [Node])
        end,
        lists:reverse(Nodes)
    ),
    OrderedList = mix_node:get_order(StartNode, StartNode),
    StartAtZero = rearrange(OrderedList),

    timer:sleep(1000),
    lists:foreach(
        fun(_) ->
            lists:foreach(
                fun(NodePid) -> mix_node:kill(NodePid) end,
                Nodes
            )
        end,
        lists:seq(1, MixAmount)
    ),
    (lists:nth(1000 rem length(StartAtZero) + 1, StartAtZero)) +
        (lists:nth(2000 rem length(StartAtZero) + 1, StartAtZero)) +
        (lists:nth(3000 rem length(StartAtZero) + 1, StartAtZero)).

rearrange(OrderedList) ->
    {End, Start} = lists:splitwith(fun(E) -> 0 =/= E end, OrderedList),
    Start ++ End.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).