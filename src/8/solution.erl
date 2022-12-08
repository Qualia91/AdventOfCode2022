-module(solution).
-author("nickolaswood").
-include_lib("stdlib/include/ms_transform.hrl").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    part_1/0,
    part_2/0
]).

-record(tree, {
    x,
    y,
    h
}).

look_left(INX, INY) -> [{{tree,'$1','$2','$3'},[{'<','$1',{const,INX}},{'==','$2',{const,INY}}],['$3']}].
look_right(INX, INY) -> [{{tree,'$1','$2','$3'},[{'>','$1',{const,INX}},{'==','$2',{const,INY}}],['$3']}].
look_up(INX, INY) -> [{{tree,'$1','$2','$3'},[{'<','$2',{const,INY}},{'==','$1',{const,INX}}],['$3']}].
look_down(INX, INY) -> [{{tree,'$1','$2','$3'},[{'>','$2',{const,INY}},{'==','$1',{const,INX}}],['$3']}].
current_val(INX, INY) -> [{{tree,'$1','$2','$3'},[{'==','$1',{const,INX}},{'==','$2',{const,INY}}],['$3']}].

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    {Tab, SizeX, SizeY} = parse_into_ets(readlines("input.txt")),
    find_trees(Tab, SizeX, SizeY).

part_2() ->
    {Tab, SizeX, SizeY} = parse_into_ets(readlines("input.txt")),
    find_view_distance_score(Tab, SizeX, SizeY).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

find_view_distance_score(Tab, SizeX, SizeY) ->
    lists:foldl(
        fun({X, Y}, Largest) ->
            case is_edge(X, Y, SizeX, SizeY) of
                true ->
                    Largest;
                false ->
                    [H] = ets:select(Tab, current_val(X, Y)),
                    Score = calc_view_score(Tab, X, Y, H),
                    case Score > Largest of
                        true ->
                            Score;
                        false -> Largest
                    end
            end
        end,
        0,
        [{X, Y} || X <- lists:seq(1, SizeX), Y <- lists:seq(1, SizeY)]).

calc_view_score(Tab, X, Y, H) ->
    calc_view_score(H, Tab, look_left(X, Y), look_right(X, Y), look_up(X, Y), look_down(X, Y)).

calc_view_score(CurrentVal, Tab, Left, Right, Up, Down) ->
    {UpList, RemUp} = lists:splitwith(
        fun(TreeHeight) ->            
            TreeHeight < CurrentVal
        end,
        lists:reverse(ets:select(Tab, Up))
    ),
    {DownList, RemDown} = lists:splitwith(
        fun(TreeHeight) ->
            TreeHeight < CurrentVal
        end,
        ets:select(Tab, Down)
    ),
    {LeftList, RemLeft} = lists:splitwith(
        fun(TreeHeight) ->
            TreeHeight < CurrentVal
        end,
        lists:reverse(ets:select(Tab, Left))
    ),
    {RightList, RemRight} = lists:splitwith(
        fun(TreeHeight) ->
            TreeHeight < CurrentVal
        end,
        ets:select(Tab, Right)
    ),

    view_dist(UpList, RemUp) * view_dist(DownList, RemDown) * view_dist(LeftList, RemLeft) * view_dist(RightList, RemRight).

view_dist(List, []) ->
    length(List);
view_dist(List, _) ->
    length(List) + 1.

find_trees(Tab, SizeX, SizeY) ->
    lists:foldl(
        fun({X, Y}, Sum) ->
            [H] = ets:select(Tab, current_val(X, Y)),
            case is_edge(X, Y, SizeX, SizeY) of
                true -> Sum + 1;
                false -> Sum + can_see(Tab, X, Y, H)
            end
        end,
        0,
        [{X, Y} || X <- lists:seq(1, SizeX), Y <- lists:seq(1, SizeY)]).

is_edge(X, Y, SizeX, SizeY) when X == 1; Y == 1; X == SizeX; Y == SizeY ->
    true;
is_edge(_, _, _, _) ->
    false.

can_see(Tab, CX, CY, H) ->
    check_ray_paths(H, Tab, [look_left(CX, CY), look_right(CX, CY), look_up(CX, CY), look_down(CX, CY)]).
    
check_ray_paths(_, _, []) ->
    0;
check_ray_paths(CurrentVal, Tab, [RayPathFun | Rest]) ->
    case CurrentVal > lists:max(ets:select(Tab, RayPathFun)) of
        true -> 1;
        false -> check_ray_paths(CurrentVal, Tab, Rest)
    end.

parse_into_ets([F | _ ] = Lines) ->
    Ets = ets:new(trees, [private, bag]),
    IndexedLines = index(Lines),
    SizeY = length(Lines),
    SizeX = length(binary_to_list(F)),
    lists:foreach(
        fun({Y, Line}) ->
            IndexedLine = index(binary_to_list(Line)),
            lists:foreach(
                fun({X, Val}) ->
                    ets:insert(Ets, #tree{x = X, y = Y, h = Val - 48})
                end,
                IndexedLine)
            end,
            IndexedLines
    ),
    {Ets, SizeX, SizeY}.

index(List) ->
    lists:zip(lists:seq(1, length(List)), List).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).