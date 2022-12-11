%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen Server built from template.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(monkey_serv).
-author("nickolaswood").
-behaviour(gen_server).

-include_lib("monkey_lib.hrl").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/4,
    start_iter/1,
    throw_to/2,
    next_monkey/3,
    get_monkey_business/1,
    get_items/1,
    kill/1
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
    monkey,
    home_loc,
    number_of_monkeys,
    inspected_items = 0,
    have_refeif
}).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Monkey, HomeLoc, NumberOfMonkeys, HaveRelief) ->
    gen_server:start_link({local, list_to_atom(integer_to_list(Monkey#monkey.number))}, ?MODULE, [Monkey, HomeLoc, NumberOfMonkeys, HaveRelief], []).

start_iter(CurrentMonkeyIndex) ->
    gen_server:cast(list_to_atom(integer_to_list(CurrentMonkeyIndex)), start_iter).

throw_to(ToMonkeyIndex, Item) ->
    gen_server:cast(list_to_atom(integer_to_list(ToMonkeyIndex)), {catch_item, Item}).

next_monkey(CurrentMonkeyIndex, NumberOfMonkeys, HomeLoc) ->
    NextMonkey = CurrentMonkeyIndex + 1,
    case NextMonkey < NumberOfMonkeys of
        true -> start_iter(NextMonkey);
        false -> HomeLoc ! end_of_iter
    end.

get_monkey_business(MonkeyIndex) ->
    gen_server:call(list_to_atom(integer_to_list(MonkeyIndex)), get_monkey_business).

get_items(MonkeyIndex) ->
    gen_server:call(list_to_atom(integer_to_list(MonkeyIndex)), get_items).

kill(MonkeyIndex) ->
    gen_server:cast(list_to_atom(integer_to_list(MonkeyIndex)), stop).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([Monkey, HomeLoc, NumberOfMonkeys, HR]) ->
    LoopState = #loop_state{
        monkey = Monkey,
        home_loc = HomeLoc,
        number_of_monkeys = NumberOfMonkeys,
        have_refeif = HR
    },
    {ok, LoopState}.

handle_call(get_monkey_business, _From, LoopState = #loop_state{inspected_items = InspectedItem}) ->
    {reply, InspectedItem, LoopState};
handle_call(get_items, _From, LoopState = #loop_state{monkey = Monkey}) ->
    {reply, Monkey#monkey.items, LoopState}.

handle_cast(stop, LoopState) ->
    {stop, normal, LoopState};
handle_cast({catch_item, Item}, LoopState = #loop_state{monkey = Monkey}) ->
    UpdatedMonkey = Monkey#monkey{items = [Item | Monkey#monkey.items]},
    {noreply, LoopState#loop_state{monkey = UpdatedMonkey}};
handle_cast(start_iter, LoopState = #loop_state{monkey = Monkey, home_loc = HomeLoc, number_of_monkeys = NumberOfMonkeys, inspected_items = InspectedItem, have_refeif = HR}) ->
    lists:foreach(
        fun(Item) ->
            Op = Monkey#monkey.operation,
            Value = case HR of
                true -> trunc(Op(Item) / 3);
                false -> mod(trunc(Op(Item)), Monkey#monkey.common_divisor)
            end,
            ToMonkeyNumber = case Value rem Monkey#monkey.divisible_number == 0 of
                true -> Monkey#monkey.if_true;
                false -> Monkey#monkey.if_false
            end,
            monkey_serv:throw_to(ToMonkeyNumber, Value)
        end,
        Monkey#monkey.items),
    UpdatedInspectedItem = InspectedItem + length(Monkey#monkey.items),
    UpdatedMonkey = Monkey#monkey{items = []},
    monkey_serv:next_monkey(Monkey#monkey.number, NumberOfMonkeys, HomeLoc),
    {noreply, LoopState#loop_state{monkey = UpdatedMonkey, inspected_items = UpdatedInspectedItem}}.

handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_Y) -> 0.