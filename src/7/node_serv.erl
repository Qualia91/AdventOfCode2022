%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen Server built from template.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(node_serv).
-author("nickolaswood").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/1,
    get_parent/1,
    add_file/3,
    add_folder/2,
    get_folder_size/1
]).

%% Callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
    sub_folders = [],
    files = [],
    parent = null
}).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(FolderName) ->
    gen_server:start_link({local, FolderName}, ?MODULE, [], []).

get_parent(FolderName) ->
    gen_server:call(binary_to_atom(FolderName), get_parent).

add_file(FolderName, FileName, FileSize) ->
    gen_server:cast(binary_to_atom(FolderName), {add_file, FileName, FileSize}).

add_folder(FolderName, SubFolderName) ->
    FolderProcName = binary_to_atom(FolderName),
    gen_server:cast(FolderProcName, {add_folder, SubFolderName}),
    SubFolderProcName = binary_to_atom(SubFolderName),

    case whereis(SubFolderProcName) of
        undefined -> start_link(SubFolderProcName);
        _ -> ok
    end,
    
    gen_server:cast(SubFolderProcName, {set_parent, FolderName}).

get_folder_size(FolderName) ->
    gen_server:call(binary_to_atom(FolderName), get_size, 10000).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([]) ->
    {ok, #loop_state{}}.

handle_call(get_size, _From, LoopState = #loop_state{files = Files, sub_folders = SubFolders}) ->
    FileSize = lists:foldl(fun({_FileName, Size}, Sum) -> Sum + Size end, 0, Files),
    SubSize = lists:foldl(
        fun(SubFolder, Sum) ->
            node_serv:get_folder_size(SubFolder) + Sum
        end,
        FileSize,
        SubFolders),
    {reply, SubSize, LoopState};
handle_call(get_parent, _From, LoopState = #loop_state{parent = Parent}) ->
    {reply, Parent, LoopState};
handle_call(print, _From, LoopState = #loop_state{files = Files, sub_folders = SubFolders}) ->
    lists:foreach(
        fun(SubFolder) ->
            io:format("SubFolder: ~p~n", [SubFolder])
        end,
        SubFolders),
    lists:foreach(
        fun(A) ->
            io:format("File: ~p~n", [A])
        end,
        Files),
    {reply, ok, LoopState}.

handle_cast({add_file, FileSize, FileName}, LoopState = #loop_state{files = Files}) ->
    {noreply, LoopState#loop_state{files = [{FileName, binary_to_integer(FileSize)} | Files]}};
handle_cast({add_folder, SubFolderName}, LoopState = #loop_state{sub_folders = SubFolders}) ->
    {noreply, LoopState#loop_state{sub_folders = lists:uniq([SubFolderName | SubFolders])}};
handle_cast({set_parent, Parent}, LoopState) ->
    {noreply, LoopState#loop_state{parent = Parent}};
handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    io:format("~p~n", [term]),
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
