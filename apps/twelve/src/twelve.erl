-module(twelve).

-export([part1/0, part2/0, ship_and_waypoint/2, ship/1]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, part2_solve(read_input(test))},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    Ship = spawn_link(?MODULE, ship, [{0, 0, east}]),
    send_all(Ship, Input ++ [{stop, self()}]),
    receive Answer -> Answer end.

part2_solve(Input) ->
    ShipAndWaypoint = spawn_link(?MODULE, ship_and_waypoint, [{0, 0}, {10, 1}]),
    send_all(ShipAndWaypoint, Input ++ [{stop, self()}]),
    receive Answer -> Answer end.

send_all(_, []) -> ok;
send_all(Pid, [I | Is]) ->
    Pid ! I,
    send_all(Pid, Is).

ship_and_waypoint(Ship, WP) ->
    receive
        {$N, Value} -> ship_and_waypoint(Ship, move(north, WP, Value));
        {$E, Value} -> ship_and_waypoint(Ship, move(east,  WP, Value));
        {$S, Value} -> ship_and_waypoint(Ship, move(south, WP, Value));
        {$W, Value} -> ship_and_waypoint(Ship, move(west,  WP, Value));
        {$R, Value} -> ship_and_waypoint(Ship, rotate(WP, Value));
        {$L, Value} -> ship_and_waypoint(Ship, rotate(WP, -Value));
        {$F, Value} -> ship_and_waypoint(move_to(Ship, WP, Value), WP);
        {stop, From} ->
            {X, Y} = Ship,
            From ! abs(X) + abs(Y),
            exit(normal)
    end.

rotate({X, Y},  90) -> {Y, -X};
rotate(WP,  180) -> rotate(rotate(WP, 90), 90);
rotate(WP,  270) -> rotate(rotate(WP, 180), 90);
rotate(WP, -90) -> rotate(WP, 270);
rotate(WP, -180) -> rotate(WP, 180);
rotate(WP, -270) -> rotate(WP, 90).

move_to(Ship, _, 0) -> Ship;
move_to({X, Y}, {DX, DY}=WP, Times) ->
    move_to({X+DX, Y+DY}, WP, Times-1).

ship(Ship) ->
    receive
        {$N, Value} -> ship(move(north, Ship, Value));
        {$E, Value} -> ship(move(east,  Ship, Value));
        {$S, Value} -> ship(move(south, Ship, Value));
        {$W, Value} -> ship(move(west,  Ship, Value));
        {$R, Value} -> ship(turn(Ship, Value));
        {$L, Value} -> ship(turn(Ship, -Value));
        {$F, Value} -> ship(forward(Ship, Value));
        {stop, From} ->
            {X, Y, _} = Ship,
            From ! abs(X) + abs(Y),
            exit(normal)
    end.

move(north, {X, Y},      Value) -> {X, Y + Value};
move(east,  {X, Y},      Value) -> {X + Value, Y};
move(south, {X, Y},      Value) -> {X, Y - Value};
move(west,  {X, Y},      Value) -> {X - Value, Y};
move(north, {X, Y, Dir}, Value) -> {X,         Y + Value, Dir};
move(east,  {X, Y, Dir}, Value) -> {X + Value, Y,         Dir};
move(south, {X, Y, Dir}, Value) -> {X,         Y - Value, Dir};
move(west,  {X, Y, Dir}, Value) -> {X - Value, Y,         Dir}.

turn({X, Y, Dir},  90)  -> {X, Y, right(Dir)};
turn({X, Y, Dir},  180) -> {X, Y, right(right(Dir))};
turn({X, Y, Dir},  270) -> {X, Y, right(right(right(Dir)))};
turn({X, Y, Dir}, -90)  -> {X, Y, left(Dir)};
turn({X, Y, Dir}, -180) -> {X, Y, left(left(Dir))};
turn({X, Y, Dir}, -270) -> {X, Y, left(left(left(Dir)))}.

forward({_, _, Dir} = Ship, Value) -> move(Dir, Ship, Value).

right(north) -> east;
right(east) -> south;
right(south) -> west;
right(west) -> north.

left(north) -> west;
left(west) -> south;
left(south) -> east;
left(east) -> north.

read_input(test) -> read_input("apps/twelve/src/test.txt");
read_input(main) -> read_input("apps/twelve/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    List = binary:split(Data, <<"\n">>, [global, trim]),
    lists:map(
      fun(<<Dir, Value/binary>>) -> {Dir, binary_to_integer(Value)} end,
      List).
