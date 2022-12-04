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

-define(PATTERN, "^(?<A>\\d+)\\-(?<B>\\d+),(?<C>\\d+)\\-(?<D>\\d+)").

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    find_how_many_ranges_overlap(readlines("input.txt"), fun intersects_fully/2).

part_2() ->
    find_how_many_ranges_overlap(readlines("input.txt"), fun intersects/2).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

find_how_many_ranges_overlap(Lines, IntersectMethod) ->
    lists:foldl(
        fun(Line, Sum) ->
            {R1, L1, R2, L2} = extract_numbers(Line),
            Range1 = {binary_to_integer(R1), binary_to_integer(L1)},
            Range2 = {binary_to_integer(R2), binary_to_integer(L2)},
            case IntersectMethod(Range1, Range2) orelse IntersectMethod(Range2, Range1) of
                true -> Sum + 1;
                false -> Sum
            end
        end,
        0,
        Lines).

intersects_fully({R1, L1}, {R2, L2}) ->
    R1 =< R2 andalso L1 >= L2.

intersects({R1, L1}, {R2, L2}) ->
    (R1 =< R2 andalso L1 >= R2) orelse (R1 =< L2 andalso R1 >= R2).
        

extract_numbers(Line) ->
    {match, [R1, L1, R2, L2]} = re:run(Line, ?PATTERN, [{capture, all_names, binary}]),
    {R1, L1, R2, L2}.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).