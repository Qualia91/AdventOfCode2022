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

-record(blueprint, {
    index :: integer(),
    robot_blueprints :: list(robot_blueprint())
}).
-record(robot_blueprint, {
    type :: ore | clay | obsidian | geode,
    costs :: list(cost())
}).
-type robot_blueprint() :: robot_blueprint.
-record(cost, {
    type :: ore | clay | obsidian | geode,
    amount :: integer()
}).
-type cost() :: cost.

-define(PATTERN, "Blueprint (?<A>\\d+):(?<B>.*)").
-define(ROBOT_PATTERN, "Each (?<A>[^\\s]+) robot costs (?<B>.*)").
-define(COST_PATTERN, "(?<A>\\d+) (?<B>[^\\s]*)").

%%%=============================================================================
%%% API
%%%=============================================================================

part_1() ->
    calculate_blueprints_cost(parse_lines(readlines("testInput.txt"))).

part_2() ->
    readlines("testInput.txt").

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

calculate_blueprints_cost(BluePrints) ->
    acc(
        fun(#blueprint{index = Index, robot_blueprints = RobotBlueprints}) ->
            calculate_blueprint_cost(RobotBlueprints, {0, 0, 0, 0}, {1, 0, 0, 0}) * Index
        end,
        BluePrints
    ).

calculate_blueprint_cost(RobotBlueprints, Inv, Robots) ->
    NewInv = collect(Robots, Inv),
    CanAfford = can_afford(RobotBlueprints, NewInv),
    acc(
        fun(Purchases) ->
            {ReducedInv, NewRobots} = buy(Purchases, NewInv, Robots),
            calculate_blueprint_cost(RobotBlueprints, ReducedInv, NewRobots)
        end,
        CanAfford
    ).

collect({OreR, ClaR, ObsR, GeoR}, {Ore, Cla, Obs, Geo}) ->
    {Ore + OreR, Cla + ClaR, Obs + ObsR, Geo + GeoR}.

can_afford(RobotBlueprints, Inv, Purchases) ->
    acc(
        fun(#robot_blueprint{costs = Costs, type = Type}) ->
            case maybe_buy(Inv, Costs, Type) of
                fail -> Purchases;
                {Type, NewInv} -> can_afford(RobotBlueprints, NewInv, [Type | Purchases])
            end
        end,
        RobotBlueprints
    ).

maybe_buy(Inv, Costs, Type) ->
    lists:
    case can_afford(Costs, Inv) of
        true -> {Type, spend_inv(Inv, Costs, Type)};
        false -> fail
    end.

parse_lines(Lines) ->
    acc(
        fun(Line) ->
            parse_line(Line)
        end,
        Lines
    ).

parse_line(Line) ->
    {match, [Index, Rest]} = re:run(Line, ?PATTERN, [{capture, all_names, binary}]),
    #blueprint{
        index = binary_to_integer(Index),
        robot_blueprints = parse_blueprint(string:trim(Rest))
    }.

parse_blueprint(Rest) ->
    acc(
        fun(RobotBluePrint) ->
            io:format("~p~n", [re:run(RobotBluePrint, ?ROBOT_PATTERN, [{capture, all_names, binary}])]),
            {match, [Type, Costs]} = re:run(RobotBluePrint, ?ROBOT_PATTERN, [{capture, all_names, binary}]),
            #robot_blueprint{
                type = type(Type),
                costs = parse_costs(Costs)
            }
        end,
        string:split(Rest, <<". ">>, all)).

parse_costs(Costs) ->
    acc(
        fun(Cost) ->
            {match, [Amount, Type]} = re:run(Cost, ?COST_PATTERN, [{capture, all_names, binary}]),
            #cost{amount = binary_to_integer(Amount), type = type(Type)}
        end,
        string:split(Costs, <<" and ">>, all)).

type(<<"ore">>) -> ore;
type(<<"ore.">>) -> ore;
type(<<"clay">>) -> clay;
type(<<"clay.">>) -> clay;
type(<<"obsidian">>) -> obsidian;
type(<<"obsidian.">>) -> obsidian;
type(<<"geode">>) -> geode;
type(<<"geode.">>) -> geode.

acc(F, List) when is_function(F, 1), is_list(List) ->
    case List of
        [Hd | Tail] -> acc_1(F, append(F(Hd), []), Tail);
        [] -> []
    end.

acc_1(F, Acc, [Hd | Tail]) when is_list(Acc) ->
    acc_1(F, append(F(Hd), Acc), Tail);
acc_1(_F, Acc, []) ->
    Acc.

append([], Acc) ->
    Acc;
append([Hd | Tl], Acc) ->
    append(Tl, [Hd | Acc]);
append(Val, Acc) ->
    [Val | Acc].

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).