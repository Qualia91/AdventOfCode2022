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
    find_start_packet_marker(readline("input.txt"), 4).

part_2() ->
    find_start_packet_marker(readline("input.txt"), 14).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

find_start_packet_marker(List, Split) ->
    find_start_packet_marker(List, Split, Split).

find_start_packet_marker([A | Rest], Split, Count) when length(Rest) > Split ->
    case lists:split(Split, [A | Rest]) of
        {Group, _} ->
            check_same_and_continue(Group, Rest, Split, Count);
        _ ->
            find_start_packet_marker(Rest, Split, Count + 1)
    end;
find_start_packet_marker(_, _, _) ->
    error.

check_same_and_continue(Group, Rest, Split, Count) ->
    case (length(lists:uniq(Group)) == length(Group)) of
        true -> Count;
        false -> find_start_packet_marker(Rest, Split, Count + 1)
    end.

readline(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary_to_list(Data).