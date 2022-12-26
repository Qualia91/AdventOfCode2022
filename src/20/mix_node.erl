%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen Server built from template.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(mix_node).
-author("nickolaswood").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/4,
    notify_left_of_right/2,
    notify_right_of_left/2,
    move/1,
    print/1,
    kill/1,
    get_order/2
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

%% Loop state
-record(loop_state, {
    value,
    left,
    right,
    moves_left = 0,
    num_of_vals
}).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Name, Value, Left, Len) ->
    gen_server:start_link({local, Name}, ?MODULE, [Value, Left, Len], []).

notify_left_of_right(LeftPid, Right) ->
    gen_server:cast(LeftPid, {right, Right}).

notify_right_of_left(RightPid, Left) ->
    gen_server:cast(RightPid, {left, Left}).

move(Pid) ->
    gen_server:call(Pid, move).

print(NodePid) ->
    gen_server:cast(NodePid, print).

swap_right(SwapWith, Left, Right) ->
    gen_server:call(SwapWith, {swap_right, Left, Right}).

swap_left(SwapWith, Right, Left) ->
    gen_server:call(SwapWith, {swap_left, Right, Left}).

get_order(Pid, StartPid) ->
    gen_server:call(Pid, {get_order, StartPid}).

kill(Pid) ->
    gen_server:cast(Pid, stop).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([Value, Left, Len]) ->
    LoopState = #loop_state{
        value = Value,
        left = Left,
        num_of_vals = Len
    }, 
    {ok, LoopState}.

handle_call({get_order, StartPid}, _From, LoopState = #loop_state{value = Value, right = Right}) ->
    Ret = case StartPid of
        Right -> [Value];
        _ -> [Value | get_order(Right, StartPid)]
    end,
    {reply, Ret, LoopState};
handle_call(move, _From, LoopState = #loop_state{value = Value, left = Left, right = Right, num_of_vals = Len}) ->
    Sign = case Value =< 0 of
        true -> -1;
        false -> 1
    end,
    {NewLeft, NewRight} = lists:foldl(
        fun(_, {UpLeft, UpRight}) ->
            case Sign of
                1 ->
                    notify_left_of_right(UpLeft, UpRight),
                    SwapsRight = swap_right(UpRight, UpLeft, self()),
                    {UpRight, SwapsRight};
                -1 ->
                    notify_right_of_left(UpRight, UpLeft),
                    SwapsLeft = swap_left(UpLeft, UpRight, self()),
                    {SwapsLeft, UpLeft}
            end
        end,
        {Left, Right},
        lists:seq(0, Len rem abs(Value))
    ),
    {reply, ok, LoopState#loop_state{left = NewLeft, right = NewRight}};
handle_call({swap_right, NewLeft, NewRight}, _From, LoopState = #loop_state{right = Right}) ->
    notify_right_of_left(Right, NewRight),
    {reply, Right, LoopState#loop_state{left = NewLeft, right = NewRight}};
handle_call({swap_left, NewRight, NewLeft}, _From, LoopState = #loop_state{left = Left}) ->
    notify_left_of_right(Left, NewLeft),
    {reply, Left, LoopState#loop_state{left = NewLeft, right = NewRight}}.

handle_cast(stop, LoopState) ->
    {stop, normal, LoopState};
handle_cast(print, LoopState) ->
    {noreply, LoopState};
handle_cast({right, Right}, LoopState) ->
    {noreply, LoopState#loop_state{right = Right}};
handle_cast({left, Left}, LoopState) ->
    {noreply, LoopState#loop_state{left = Left}}.

handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

convert(Index) ->
    binary_to_atom(integer_to_binary(Index)).