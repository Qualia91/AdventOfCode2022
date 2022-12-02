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
        fun(<<Op:1/binary, _:1/binary, Ow:1/binary>>, Sum) ->
            outcome(Op, Ow) + selected(Ow) + Sum
        end,
        0,
        readlines("input.txt")).

part_2() ->
    lists:foldl(
        fun(<<Op:1/binary, _:1/binary, Ow:1/binary>>, Sum) ->
            outcome(Ow) + selected(Op, Ow) + Sum
        end,
        0,
        readlines("input.txt")).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

outcome(<<"A">>, Pl) ->
    case Pl of
        <<"X">> -> 3;
        <<"Y">> -> 6;
        <<"Z">> -> 0
    end;
outcome(<<"B">>, Pl) ->
    case Pl of
        <<"X">> -> 0;
        <<"Y">> -> 3;
        <<"Z">> -> 6
    end;
outcome(<<"C">>, Pl) ->
    case Pl of
        <<"X">> -> 6;
        <<"Y">> -> 0;
        <<"Z">> -> 3
    end.

selected(Bin) ->
    [Val] = binary_to_list(Bin),
    Val - 87.

outcome(Bin) ->
    [Val] = binary_to_list(Bin),
    (Val - 88) * 3.

selected(Op, <<"X">>) ->
    case Op of
        <<"A">> -> selected(<<"Z">>);
        <<"B">> -> selected(<<"X">>);
        <<"C">> -> selected(<<"Y">>)
    end;
selected(Op, <<"Y">>) ->
    case Op of
        <<"A">> -> selected(<<"X">>);
        <<"B">> -> selected(<<"Y">>);
        <<"C">> -> selected(<<"Z">>)
    end;
selected(Op, <<"Z">>) ->
    case Op of
        <<"A">> -> selected(<<"Y">>);
        <<"B">> -> selected(<<"Z">>);
        <<"C">> -> selected(<<"X">>)
    end.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).