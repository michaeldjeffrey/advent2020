-module(seventeen).
-compile(export_all).
-export([]).

-define(ACTIVE, $#).
-define(INACTIVE, $.).

part1(Input) ->
    count_active(do_cycles(6, read_input(Input))).

get_neighbor_coords({X, Y, Z}) ->
    [{X + DX, Y + DY, Z + DZ} ||
        DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        DZ <- [-1, 0, 1],
        {DX, DY, DZ} /= {0, 0, 0}].

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

next_state_for_cube(Map, Coord) ->
    Neighbors = get_neighbors(Map, Coord),
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
      [get_neighbor_coords(Coord) || Coord <- maps:keys(Start)]
     ).

next_state(Start) ->
    Expanded = expand_map(Start),
    lists:foldl(
      fun(Coord, Acc) ->
              maps:put(Coord, next_state_for_cube(Start, Coord), Acc)
      end,
      Expanded,
      maps:keys(Expanded)).


do_cycles(0, Map) -> Map;
do_cycles(N, Map) ->
    do_cycles(next_state(Map), N-1).


read_input(test) -> read_input(<<".#.\n..#\n###">>);
read_input(main) -> read_input(<<".#.#.#..\n..#....#\n#####..#\n#####..#\n#####..#\n###..#.#\n#..##.##\n#.#.####">>);
read_input(Input) ->
    L = binary:split(Input, <<"\n">>, [global]),
    to_map(L, -1, -1, #{}).

to_map([], _, _, M) -> M;
to_map([<<>> | Next], Row, _Col, M) -> to_map(Next, Row + 1, -1, M);
to_map([<<X, Rest/binary>> | Next], Row, Col, M) -> to_map([Rest | Next], Row, Col + 1, maps:put({Row, Col, 0}, X, M)).
