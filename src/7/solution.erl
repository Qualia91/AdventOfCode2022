-module(solution).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    part_0/0,
    part_1/1,
    part_2/1
]).

-define(PATTERN, "^(?<A>\\d+) (?<B>.+)").

%%%=============================================================================
%%% API
%%%=============================================================================

% L = solution:part_0().
% solution:part_1(L).
% solution:part_2(L).

part_0() ->
    parse_into_tree(readlines("input.txt")).

part_1(FolderList) ->
    lists:foldl(
        fun(Folder, Sum) ->
            DirSize = node_serv:get_folder_size(Folder),
            case DirSize =< 100000 of
                true -> Sum + DirSize;
                false -> Sum
            end
        end,
        0,
        FolderList).

part_2(FolderList) ->
    SpaceRemaining = 70000000 - node_serv:get_folder_size(<<"/">>),
    SpaceNeeded = 30000000 - SpaceRemaining,
    [HD | _] = lists:sort(lists:foldl(
        fun(<<"/">>, Acc) ->
            Acc;
           (Folder, Acc) ->
            Size = node_serv:get_folder_size(Folder),
            case Size >= SpaceNeeded of
                true -> [Size | Acc];
                false -> Acc
            end
        end,
        [],
        FolderList)),
    HD.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

parse_into_tree(Lines) ->
    {Folders, _} = lists:foldl(fun parse_line/2, {[<<"/">>], <<"/">>}, Lines),
    lists:uniq(Folders).

parse_line(<<"$ cd ..">>, {Folders, CurrentFolderName}) ->
    {Folders, node_serv:get_parent(CurrentFolderName)};
parse_line(<<"$ cd ", SubFolder/binary>>, {Folders, CurrentFolderName}) ->
    NewPath = case SubFolder of
        <<"/">> -> <<"/">>;
        _ -> <<CurrentFolderName/binary, SubFolder/binary>>
    end,
    node_serv:add_folder(CurrentFolderName, NewPath),
    {[NewPath | Folders], NewPath};
parse_line(<<"$ ls">>, {Folders, CurrentFolderName}) ->
    {Folders, CurrentFolderName};
parse_line(<<"dir ", _/binary>>, {Folders, CurrentFolderName}) ->
    {Folders, CurrentFolderName};
parse_line(Line, {Folders, CurrentFolderName}) ->
    {match, [FileSize, FileName]} = re:run(Line, ?PATTERN, [{capture, all_names, binary}]),
    node_serv:add_file(CurrentFolderName, FileSize, FileName),
    {Folders, CurrentFolderName}.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).