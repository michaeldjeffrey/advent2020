-module(seventeen).
-compile(export_all).
-export([]).

-define(ACTIVE, $#).
-define(INACTIVE, $.).

part1(InputType) ->
    Input = read_input(part1, InputType),
    End = do_cycles(6, Input),
    count_active(End).

part2(InputType) ->
    Input = read_input(part2, InputType),
    End = do_cycles_4d(6, Input),
    count_active(End).

get_neighbor_coords_3d({X, Y, Z}) ->
    [{X + DX, Y + DY, Z + DZ} ||
        DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        DZ <- [-1, 0, 1],
        {DX, DY, DZ} /= {0, 0, 0}].

get_neighbor_coords_4d({X, Y, Z, W}) ->
    [{X + DX, Y + DY, Z + DZ, W + DW} ||
        DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        DZ <- [-1, 0, 1],
        DW <- [-1, 0, 1],
        {DX, DY, DZ, DW} /= {0, 0, 0, 0}].

lookup(Map, Coord) ->
    maps:get(Coord, Map, ?INACTIVE).

get_neighbors_3d(Map, Coord) ->
    [lookup(Map, N) || N <- get_neighbor_coords_3d(Coord)].

get_neighbors_4d(Map, Coord) ->
    [lookup(Map, N) || N <- get_neighbor_coords_4d(Coord)].

num_active_neighbors(Neighbors) ->
    lists:sum(
      lists:map(
        fun(?ACTIVE) -> 1; (?INACTIVE) -> 0 end,
        Neighbors)).

count_active(Map) ->
    num_active_neighbors(maps:values(Map)).

next_state_for_cube_3d(Map, Coord) ->
    Neighbors = get_neighbors_3d(Map, Coord),
    case {lookup(Map, Coord), num_active_neighbors(Neighbors)} of
        {?ACTIVE, 2} -> ?ACTIVE;
        {?ACTIVE, 3} -> ?ACTIVE;
        {?ACTIVE, _} -> ?INACTIVE;
        {?INACTIVE, 3} -> ?ACTIVE;
        {?INACTIVE, _} -> ?INACTIVE
    end.

next_state_for_cube_4d(Map, Coord) ->
    Neighbors = get_neighbors_4d(Map, Coord),
    case {lookup(Map, Coord), num_active_neighbors(Neighbors)} of
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
      [get_neighbor_coords_3d(Coord) || Coord <- maps:keys(Start)]).

expand_map_4d(Start) ->
    lists:foldl(
      fun (ListOfCoords, M) ->
              NM = maps:from_list([{Coord, ?INACTIVE} || Coord <- ListOfCoords]),
              maps:merge(M, NM)
      end,
      #{},
      [get_neighbor_coords_4d(Coord) || Coord <- maps:keys(Start)]).

next_state(Start) ->
    Expanded = expand_map(Start),
    lists:foldl(
      fun(Coord, Acc) ->
              maps:put(Coord, next_state_for_cube_3d(Start, Coord), Acc)
      end,
      Expanded,
      maps:keys(Expanded)).

next_state_4d(Start) ->
    Expanded = expand_map_4d(Start),
    lists:foldl(
      fun(Coord, Acc) -> maps:put(Coord, next_state_for_cube_4d(Start, Coord), Acc) end,
      Expanded,
      maps:keys(Expanded)).


do_cycles(0, Map) -> Map;
do_cycles(N, Map) ->
    do_cycles(N-1, next_state(Map)).

do_cycles_4d(0, Map) -> Map;
do_cycles_4d(N, Map) ->
    do_cycles_4d(N-1, next_state_4d(Map)).


read_input(Part, test) -> read_input(Part, <<".#.\n..#\n###">>);
read_input(Part, main) -> read_input(Part, <<".#.#.#..\n..#....#\n#####..#\n#####..#\n#####..#\n###..#.#\n#..##.##\n#.#.####">>);
read_input(Part, Input) ->
    L = binary:split(Input, <<"\n">>, [global]),
    case Part of
        part1 -> to_map(L, -1, -1, #{});
        part2 -> to_map_2(L, -1, -1, #{})
    end.

to_map([], _, _, M) -> M;
to_map([<<>> | Next], Row, _Col, M) -> to_map(Next, Row + 1, -1, M);
to_map([<<X, Rest/binary>> | Next], Row, Col, M) -> to_map([Rest | Next], Row, Col + 1, maps:put({Row, Col, 0}, X, M)).

to_map_2([], _, _, M) -> M;
to_map_2([<<>> | Next], Row, _Col, M) -> to_map_2(Next, Row + 1, -1, M);
to_map_2([<<X, Rest/binary>> | Next], Row, Col, M) -> to_map_2([Rest | Next], Row, Col + 1, maps:put({Row, Col, 0, 0}, X, M)).
