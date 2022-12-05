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

-define(ACTION_PATTERN, "move (?<A>\\d+) from (?<B>\\d+) to (?<C>\\d+)").

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    get_top_list(apply_actions(parse_input(readlines("input.txt")), fun apply_action/2)).

part_2() ->
    get_top_list(apply_actions(parse_input(readlines("input.txt")), fun apply_action_pt2/2)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

parse_input(Lines) ->
    lists:foldl(
        fun parse_fun/2,
        #{stack => #{}},
        Lines).

parse_fun(<<_:1/binary, First:1/binary, _/binary>>, AccMap) when First == <<"1">> ->
    AccMap#{actions => []};
parse_fun(Line, #{stack := _Stack, actions := Actions} = AccMap) ->
    case re:run(Line, ?ACTION_PATTERN, [{capture, all_names, binary}]) of
        {match, [Amount, Source, Sink]} ->
            AccMap#{actions => Actions ++ [{binary_to_integer(Amount), binary_to_integer(Source), binary_to_integer(Sink)}]};
        _ ->
            AccMap
    end;
parse_fun(StackInput, #{stack := StackMap} = AccMap) when StackInput =/= <<"\n">>->
    AccMap#{stack => parse_stack_input(StackInput, StackMap)};
parse_fun(_, AccMap) ->
    AccMap.

parse_stack_input(StackInputBin, StackMap) ->
    InputList = binary_to_list(StackInputBin),
    InputListZip = lists:zip(InputList, lists:seq(0, length(InputList) - 1)),
    lists:foldl(
        fun append_to_stack/2,
        StackMap,
        InputListZip).

append_to_stack({Elem, Index}, StackMap) ->
    case (Index - 1) rem 4 of
        0 ->
            insert_to_stack(Elem, ((Index - 1) div 4) + 1, StackMap);
        _ ->
            StackMap
    end.

insert_to_stack(32, _, StackMap) ->
    StackMap;
insert_to_stack(Elem, Index, StackMap) ->
    case maps:find(Index, StackMap) of
        {ok, Value} -> StackMap#{Index => Value ++ [Elem]};
        error -> StackMap#{Index => [Elem]}
    end.

apply_actions(#{stack := Stack, actions := Actions}, ApplyFun) ->
    lists:foldl(
        ApplyFun,
        Stack,
        Actions
    ).

apply_action({0, _, _}, Stack) ->
    Stack;
apply_action({Amount, Source, Sink}, Stack) ->
    case hd_n_tl(maps:get(Source, Stack)) of
        [] ->
            Stack;
        {ToMove, NewSourceList} ->
            SinkList = maps:get(Sink, Stack),
            NewSinkList = lists:flatten([ToMove | SinkList]),
            apply_action({Amount - 1, Source, Sink}, Stack#{Source => NewSourceList, Sink => NewSinkList})
    end.

hd_n_tl([]) ->
    [];
hd_n_tl([Hd | Tl]) ->
    {Hd, Tl}.

apply_action_pt2({Amount, Source, Sink}, Stack) ->
    {ToMove, NewSourceList} = lists:split(Amount, maps:get(Source, Stack)),
    SinkList = maps:get(Sink, Stack),
    NewSinkList = lists:flatten([ToMove | SinkList]),
    Stack#{Source => NewSourceList, Sink => NewSinkList}.

get_top_list(Stack) ->
    lists:reverse(maps:fold(
        fun
            (_, [V | _], AccStr) -> [V | AccStr];
            (_, [], AccStr) -> AccStr
        end,
        "",
        Stack
    )).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).