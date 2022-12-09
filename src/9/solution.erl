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

-record(pos, {
    x = 0,
    y = 0
}).

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() -> simulate(1).

part_2() -> simulate(9).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

simulate(RopeLength) ->
    Moves = parse_input(readlines("input.txt")),
    InitHeadPositions = find_positions_from_moves(Moves, [#pos{}]),
    {TailPoss, _} = lists:foldl(
        fun(_, {HeadPositions, TailPositions}) ->
            NewTailPositionsReverse = find_tail_positions(HeadPositions, TailPositions),
            {NewTailPositionsReverse, [#pos{}]}
        end,
        {InitHeadPositions, [#pos{}]},
        lists:seq(1, RopeLength)),
    length(lists:uniq(TailPoss)).

parse_input(Lines) ->
    lists:reverse(lists:foldl(
        fun(<<Dir:1/binary, _:1/binary, LenBin/binary>>, Acc) ->
            case binary_to_integer(LenBin) of
                1 -> [{Dir, 1} | Acc];
                Other -> seq_action(Dir, Other, Acc)
            end
        end,
        [],
        Lines)).

seq_action(_, 0, Acc) ->
    Acc;
seq_action(Dir, Len, Acc) ->
    seq_action(Dir, Len - 1, [{Dir, 1} | Acc]).

find_positions_from_moves([], Acc) ->
    lists:reverse(Acc);
find_positions_from_moves([NextInput | Rest], [Pos | _] = Acc) ->
    NextPos = find_next_head_pos(NextInput, Pos),
    find_positions_from_moves(Rest, [NextPos | Acc]).

find_tail_positions([_, LastHeadPosition], TailPositions = [TailPos | _]) ->
    NextTailPos = find_next_tail_pos(LastHeadPosition, TailPos),
    lists:reverse([NextTailPos | TailPositions]);
find_tail_positions([_ | RestHead = [NextHeadPos | _]], TailPositions = [TailPos | _]) ->
    NextTailPos = find_next_tail_pos(NextHeadPos, TailPos),
    find_tail_positions(RestHead, [NextTailPos | TailPositions]).

find_next_head_pos({<<"U">>, Dist}, Pos = #pos{y = Y}) ->
    Pos#pos{y = Y + Dist};
find_next_head_pos({<<"D">>, Dist}, Pos = #pos{y = Y}) ->
    Pos#pos{y = Y - Dist};
find_next_head_pos({<<"R">>, Dist}, Pos = #pos{x = X}) ->
    Pos#pos{x = X + Dist};
find_next_head_pos({<<"L">>, Dist}, Pos = #pos{x = X}) ->
    Pos#pos{x = X - Dist}.

find_next_tail_pos(HeadPos, PrevTailPos) ->
    NewTailPos = case distance_sqr(HeadPos, PrevTailPos) of
        Dist when Dist >= 4 -> move(HeadPos, PrevTailPos);
        _ -> PrevTailPos
    end,
    NewTailPos.

move(HeadPos, TailPos) ->
    sum(sign(diff(HeadPos, TailPos)), TailPos).

sign(#pos{x = X, y = Y}) ->
    #pos{x = sign(X), y = sign(Y)};
sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

sum(#pos{x = HX, y = HY}, #pos{x = TX, y = TY}) ->
    #pos{x = HX + TX, y = HY + TY}.

diff(#pos{x = HX, y = HY}, #pos{x = TX, y = TY}) ->
    #pos{x = HX - TX, y = HY - TY}.

distance_sqr(#pos{x = HX, y = HY}, #pos{x = TX, y = TY}) ->
    ((HX - TX) * (HX - TX)) + ((HY - TY) * (HY - TY)).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).