-module(seventeen).

-export([part1/1, part2/1]).

-define(ACTIVE, 1).
-define(INACTIVE, 0).

part1(InputType) ->
    Input = read_input(part1, InputType),
    End = do_cycles(6, {Input, #{}}),
    count_active(End).

part2(InputType) ->
    Input = read_input(part2, InputType),
    End = do_cycles(6, {Input, #{}}),
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

get_neighbors(Map, NCoords) ->
    [lookup(Map, N) || N <- NCoords].

count_active(Grid) when is_map(Grid) -> count_active(maps:values(Grid));
count_active(Neighbors) when is_list(Neighbors) -> lists:sum(Neighbors).

next_state_for_cube(Me, Neighbors) ->
    case {Me, count_active(Neighbors)} of
        {?ACTIVE, 2} -> ?ACTIVE;
        {?ACTIVE, 3} -> ?ACTIVE;
        {?ACTIVE, _} -> ?INACTIVE;
        {?INACTIVE, 3} -> ?ACTIVE;
        {?INACTIVE, _} -> ?INACTIVE
    end.

expand_map(Start, Cache) ->
    lists:foldl(
      fun(Coord, {Map, Cache2}) ->
              {NCoords, NCache3} = n_cache(Cache2, Coord),
              NM = maps:from_list([{C, ?INACTIVE} || C <- NCoords]),
              {maps:merge(Map, NM), NCache3}
      end,
      {#{}, Cache},
      maps:keys(Start)).

n_cache(Map, Coord) ->
    case maps:get(Coord, Map, undefined) of
        undefined ->
            N = get_neighbor_coords(Coord),
            {N, Map#{Coord => N}};
        N ->
            {N, Map}
    end.

next_state({Start, N_Cache}) ->
    {Expanded, N_Cache2} = expand_map(Start, N_Cache),
    lists:foldl(
      fun(Coord, {Acc, N_Cache3}) ->
              %% Get neighbors from cache and update
              {NCoords, N_Cache4} = n_cache(N_Cache3, Coord),

              NextState = next_state_for_cube(
                            lookup(Start, Coord),
                            get_neighbors(Start, NCoords)),

              {Acc#{Coord => NextState},
               N_Cache4}
      end,
      {Expanded, N_Cache2},
      maps:keys(Expanded)).

do_cycles(0, {Map, _}) -> Map;
do_cycles(N, Info) ->
    do_cycles(N-1, next_state(Info)).

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
