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
    lists:foldl(
        fun find_target_priority_num_and_sum/2,
        0,
        readlines("input.txt")).

part_2() ->
    fold_groups_and_find_badge_sum(readlines("input.txt")).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

find_target_priority_num_and_sum(LineBin, CurrentSum) ->
    LineList = binary_to_list(LineBin),
    LineListLengthHalf = trunc(length(LineList) / 2),
    {PartA, PartB} = lists:split(LineListLengthHalf, LineList),
    {value, DuplicateLetter} = lists:search(
        fun(ItemA) -> lists:member(ItemA, PartB) end,
        PartA),
    DupNum = convert_to_number(DuplicateLetter),
    CurrentSum + DupNum.

%% Great job erlang....
convert_to_number(LetterNumber) when LetterNumber < 97 ->
    LetterNumber - 38;
convert_to_number(LetterNumber) ->
    LetterNumber - 96.

fold_groups_and_find_badge_sum(Lines) ->
    fold_groups_and_find_badge_sum(Lines, 0).

fold_groups_and_find_badge_sum([], CurrentSum) ->
    CurrentSum;
fold_groups_and_find_badge_sum(Lines, CurrentSum) ->
    {NextGroup, Rest} = lists:split(3, Lines),
    fold_groups_and_find_badge_sum(Rest, convert_to_number(find_badge(NextGroup)) + CurrentSum).

find_badge([ElfA, ElfB, ElfC]) ->
    find_repeating(binary_to_list(ElfA), [binary_to_list(ElfB), binary_to_list(ElfC)]).

find_repeating([Repeating | _], []) ->
    Repeating;
find_repeating(ListA, [NextList | Rest]) ->
    find_repeating(
        lists:filter(
            fun(Elem) -> lists:member(Elem, NextList) end,
            ListA),
        Rest).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).