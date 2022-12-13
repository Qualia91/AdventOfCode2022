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
    Parsed = parse_input(readlines("input.txt")),
    CompList = compare_pairs(Parsed, []),
    lists:foldl(
        fun({Idx, Val}, Sum) ->
            case Val of
                true -> Sum + Idx;
                continue -> Sum + Idx;
                false -> Sum
            end
        end,
        0,
        lists:zip(lists:seq(1, length(CompList)), CompList)).

part_2() ->
    Parsed = parse_input(readlines("input.txt")) ++ [[[2]]] ++ [[[6]]],
    Sorted = sort(Parsed),
    lists:foldl(
        fun fold_fun/2,
        1,
        lists:zip(lists:seq(1, length(Sorted)), Sorted)
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

sort(Parsed) ->
    lists:sort(
        fun(A, B) -> compare(A, B) end,
        Parsed
    ).

fold_fun({Index, Value}, Sum) ->
    case Value == [[2]] orelse Value == [[6]] of
        true -> Sum * Index;
        false -> Sum
    end.

parse_input(Lines) ->
    lists:reverse(lists:foldl(
        fun(<<>>, Acc) ->
                Acc;
           (Line, Acc) ->
                {ok, Str, _} = erl_scan:string(binary_to_list(Line) ++ "."),
                {ok, Output} = erl_parse:parse_term(Str),
                [Output | Acc]
        end,
        [],
        Lines)).

compare_pairs([], Acc) ->
    Acc;
compare_pairs([InA, InB | Rest], Acc) ->
    [compare(InA, InB) | compare_pairs(Rest, Acc)].

compare(Left, Right) when is_integer(Left), is_integer(Right), Left =:= Right ->
    continue;
compare(Left, Right) when is_integer(Left), is_integer(Right) ->
    Left < Right;
compare([L | Ls], [R | Rs]) ->
    case compare(L, R) of
        true -> true;
        false -> false;
        continue -> compare(Ls, Rs)
    end;
compare(Left, Right) when is_list(Left), is_list(Right) ->
    case {is_empty(Left), is_empty(Right)} of
        {true, false} -> true;
        {false, true} -> false;
        {true, true} -> continue
    end;
compare(Left, Right) ->
    compare(box(Left), box(Right)).

is_empty([]) -> true;
is_empty(_) -> false.

box(Val) when is_integer(Val) ->
    [Val];
box(Val) ->
    Val.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).