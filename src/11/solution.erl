-module(solution).
-author("nickolaswood").

-include_lib("monkey_lib.hrl").

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
    calculate_monkey_business("input.txt", 20, true).

part_2() ->
    calculate_monkey_business("input.txt", 10000, false).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

calculate_monkey_business(FileInput, Iterations, HaveRelief) ->
    Monkeys = parse_input(readlines(FileInput)),
    CommonDivisor = find_common_divisor(Monkeys),
    UpdatedMonkeys = add_common_divisor(Monkeys, CommonDivisor),
    create_monkeys(UpdatedMonkeys, HaveRelief),
    lists:foreach(
        fun(_) ->
            monkey_serv:start_iter(0),
            receive
                end_of_iter -> ok
            end
        end,
        lists:seq(1, Iterations)),
    [F, S | _] = lists:reverse(lists:sort(lists:foldl(
        fun(MonkeyIndex, Acc) ->
            MB = monkey_serv:get_monkey_business(MonkeyIndex),
            [MB | Acc]
        end,
        [],
        lists:seq(0, length(Monkeys) - 1)))),

    kill_monkeys(Monkeys),

    F * S.

% Sorry monkeys :(
kill_monkeys(Monkeys) ->
    lists:foreach(
        fun(#monkey{number = Number}) -> monkey_serv:kill(Number)
    end,
    Monkeys).

find_common_divisor(Monkeys) ->
    lists:foldl(
        fun(#monkey{divisible_number = DN}, Multiplic) -> DN * Multiplic end,
        1,
        Monkeys).

add_common_divisor(Monkeys, CommonDivisor) ->
    lists:foldl(
        fun(Monkey, Acc) ->
            [Monkey#monkey{common_divisor = CommonDivisor} | Acc]
        end,
        [],
        Monkeys
    ).

create_monkeys(Monkeys, HaveRelief) ->
    lists:foreach(fun(Monkey) -> monkey_serv:start_link(Monkey, self(), length(Monkeys), HaveRelief) end, Monkeys).

parse_input(Lines) ->
    lists:foldl(
        fun parse_fold_fun/2,
        [],
        Lines).

parse_fold_fun(<<"Monkey ", Number:1/binary, _/binary>>, Monkeys) ->
    [#monkey{number = binary_to_integer(Number)} | Monkeys];
parse_fold_fun(<<"  Starting items: ", StartingItemsBin/binary>>, [CurrentMonkey | Rest]) ->
    StartingItems = parse_starting_items(StartingItemsBin),
    [CurrentMonkey#monkey{items = StartingItems} | Rest];
parse_fold_fun(<<"  Operation: new = old ", OperatorBin:1/binary, _:1/binary, ValueBin/binary>>, [CurrentMonkey | Rest]) ->
    Operation = get_operator(OperatorBin, ValueBin),
    [CurrentMonkey#monkey{operation = Operation} | Rest];
parse_fold_fun(<<"  Test: divisible by ", ValueBin/binary>>, [CurrentMonkey | Rest]) ->
    [CurrentMonkey#monkey{divisible_number = binary_to_integer(ValueBin)} | Rest];
parse_fold_fun(<<"    If true: throw to monkey ", MonkeyBin/binary>>, [CurrentMonkey | Rest]) ->
    [CurrentMonkey#monkey{if_true = binary_to_integer(MonkeyBin)} | Rest];
parse_fold_fun(<<"    If false: throw to monkey ", MonkeyBin/binary>>, [CurrentMonkey | Rest]) ->
    [CurrentMonkey#monkey{if_false = binary_to_integer(MonkeyBin)} | Rest];
parse_fold_fun(<<"">>, Monkeys) ->
    Monkeys.

parse_starting_items(StartingItemsBin) ->
    case re:run(StartingItemsBin, "\\d\\d", [{capture, all, list}, global]) of
        {match, Items} ->
            lists:map(fun ([Match]) -> list_to_integer(Match) end, Items);
        _ ->
            []
    end.

get_operator(<<"+">>, To) ->
    fun(Old) -> Old + get_to_value(To, Old) end;
get_operator(<<"*">>, To) ->
    fun(Old) -> Old * get_to_value(To, Old) end.

get_to_value(<<"old">>, Old) ->
    Old;
get_to_value(To, _) ->
    binary_to_integer(To).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).