-module(seventeen).
-compile(export_all).
-export([]).

-define(ACTIVE, 1).
-define(INACTIVE, 0).

part1(InputType) ->
    Input = read_input(part1, InputType),
    End = do_cycles(6, Input),
    count_active(End).

part2(InputType) ->
    Input = read_input(part2, InputType),
    End = do_cycles(6, Input),
    count_active(End).

get_neighbor_coords({X, Y, Z}) ->
    [{X + DX, Y + DY, Z + DZ} ||
        DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        DZ <- [-1, 0, 1],
        {DX, DY, DZ} /= {0, 0, 0}];
get_neighbor_coords({X, Y, Z, W}) ->
    [{X + DX, Y + DY, Z + DZ, W + DW} ||
        DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        DZ <- [-1, 0, 1],
        DW <- [-1, 0, 1],
        {DX, DY, DZ, DW} /= {0, 0, 0, 0}].

lookup(Map, Coord) ->
    maps:get(Coord, Map, ?INACTIVE).

get_neighbors(Map, Coord) ->
    [lookup(Map, N) || N <- get_neighbor_coords(Coord)].

num_active_neighbors(Neighbors) ->
    lists:sum(
      lists:map(
        fun(?ACTIVE) -> 1; (?INACTIVE) -> 0 end,
        Neighbors)).

count_active(Map) ->
    num_active_neighbors(maps:values(Map)).

next_state_for_cube(Me, Neighbors) ->
    case {Me, num_active_neighbors(Neighbors)} of
        {?ACTIVE, 2} -> ?ACTIVE;
        {?ACTIVE, 3} -> ?ACTIVE;
        {?ACTIVE, _} -> ?INACTIVE;
        {?INACTIVE, 3} -> ?ACTIVE;
        {?INACTIVE, _} -> ?INACTIVE
    end.

expand_map(Start) ->
    lists:foldl(
      fun (ListOfCoords, M) ->
              NM = maps:from_list([{Coord, ?INACTIVE} || Coord <- ListOfCoords]),
              maps:merge(M, NM)
      end,
      #{},
      [get_neighbor_coords(Coord) || Coord <- maps:keys(Start)]).

next_state(Start) ->
    Expanded = expand_map(Start),
    lists:foldl(
      fun(Coord, Acc) ->
              Neighbors = [lookup(Start, N) || N <- get_neighbor_coords(Coord)],
              Me = lookup(Start, Coord),
              maps:put(Coord, next_state_for_cube(Me, Neighbors), Acc)
      end,
      Expanded,
      maps:keys(Expanded)).

do_cycles(0, Map) -> Map;
do_cycles(N, Map) ->
    do_cycles(N-1, next_state(Map)).

read_input(Part, test) -> read_input(Part, <<".#.\n..#\n###">>);
read_input(Part, main) -> read_input(Part, <<".#.#.#..\n..#....#\n#####..#\n#####..#\n#####..#\n###..#.#\n#..##.##\n#.#.####">>);
read_input(Part, Input) ->
    L = binary:split(Input, <<"\n">>, [global]),
    case Part of
        part1 -> to_map(3, L, -1, -1, #{});
        part2 -> to_map(4, L, -1, -1, #{})
    end.

to_map(_, [], _, _, M) -> M;
to_map(D, [<<>> | Next], Row, _Col, M) -> to_map(D, Next, Row + 1, -1, M);
to_map(3, [<<X, Rest/binary>> | Next], Row, Col, M) -> to_map(3, [Rest | Next], Row, Col + 1, maps:put({Row, Col, 0}, to_int(X), M));
to_map(4, [<<X, Rest/binary>> | Next], Row, Col, M) -> to_map(4, [Rest | Next], Row, Col + 1, maps:put({Row, Col, 0, 0}, to_int(X), M)).

to_int($#) -> ?ACTIVE;
to_int($.) -> ?INACTIVE.
