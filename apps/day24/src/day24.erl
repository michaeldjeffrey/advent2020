-module(day24).
-compile(export_all).
-export([]).

part1(IT) ->
    Input = input(IT),
    count_colors(follow_paths(Input, #{})).

part2(IT) ->
    Input = input(IT),
    Initial = follow_paths(Input, #{}),
    {Time, Res} = time(fun tick_days/2, [100, Initial]),
    io:format("All took ~pms~n", [Time]),
    Res.

time(F, Args) ->
    Start = erlang:monotonic_time(millisecond),
    Return = apply(F, Args),
    Finish = erlang:monotonic_time(millisecond),
    {Finish - Start, Return}.

tick_days(0, Map) ->
    count_colors(Map);
tick_days(N, Map) ->
    {Time, NextDay} = time(fun next_state/1, [Map]),
    io:format("Day ~p took ~p ms~n", [N, Time]),
    tick_days(N-1, NextDay).

next_state(Start) ->
    Expanded = expand_map(Start),
    lists:foldl(
      fun(Coord, Acc) ->
              %% Get neighbors from cache and update
              NCoords = get_neighbor_coords(Coord),

              NextState = next_state_for_tile(
                            lookup(Start, Coord),
                            get_neighbors(Start, NCoords)),

              Acc#{Coord => NextState}
      end,
      Expanded,
      maps:keys(Expanded)).

next_state_for_tile(Me, Neighbors) ->
    {black, Num, white, _} = count_colors(Neighbors),
    case {Me, Num} of
        {black, N} when N > 2 -> white;
        {black, 0} -> white;
        {white, 2} -> black;
        _ -> Me
    end.

expand_map(Start) ->
    lists:foldl(
      fun(Coord, Map) ->
              NCoords = get_neighbor_coords(Coord),
              NM = maps:from_list([{C, white} || C <- NCoords]),
              maps:merge(Map, NM)
      end,
      #{},
      maps:keys(Start)).

follow_paths([], Map)->
    Map;
follow_paths([Path | Paths], Map) ->
    follow_paths(Paths, flip(follow(Path), Map)).

get_neighbor_coords(Coord) ->
    NFuns = [fun east/1, fun southeast/1, fun southwest/1,
             fun west/1, fun northeast/1, fun northwest/1],
    [Neighbor(Coord) || Neighbor <- NFuns].

lookup(Map, Coord) ->
    maps:get(Coord, Map, white).

get_neighbors(Map, NCoords) ->
    [lookup(Map, N) || N <- NCoords].

flip(Coord, Map) ->
    case lookup(Map, Coord) of
        white -> Map#{Coord => black};
        black -> Map#{Coord => white}
    end.

count_colors(Map) when is_map(Map)->
    count_colors(maps:values(Map));
count_colors(Tiles) ->
    {Black, White} = lists:partition(fun(T) -> T == black end, Tiles),
    {black, length(Black), white, length(White)}.

%% e, se, sw, w, nw, and ne

%%
%%     NW       NE
%%
%%  W                 E
%%
%%     SW       SE
%%

east({X, Y, Z})      -> {X+1, Y-1, Z  }.
southeast({X, Y, Z}) -> {X,   Y-1, Z+1}.
southwest({X, Y, Z}) -> {X-1, Y,   Z+1}.
west({X, Y, Z})      -> {X-1, Y+1, Z  }.
northeast({X, Y, Z}) -> {X+1, Y,   Z-1}.
northwest({X, Y, Z}) -> {X,   Y+1, Z-1}.

follow(Path) -> follow(Path, {0, 0, 0}).

follow([], Coord) -> Coord;
follow("se" ++ Input, Coord) -> follow(Input, southeast(Coord));
follow("sw" ++ Input, Coord) -> follow(Input, southwest(Coord));
follow("ne" ++ Input, Coord) -> follow(Input, northeast(Coord));
follow("nw" ++ Input, Coord) -> follow(Input, northwest(Coord));
follow("e" ++ Input, Coord) -> follow(Input, east(Coord));
follow("w" ++ Input, Coord) -> follow(Input, west(Coord)).

input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    L = unicode:characters_to_list(Data),
    string:lexemes(L, "\n").
