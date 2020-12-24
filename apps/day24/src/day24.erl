-module(day24).
-compile(export_all).
-export([]).


%% e, se, sw, w, nw, and ne

%%
%%     NW       NE
%%
%%  W                 E
%%
%%     SW       SE
%%

east({X, Y})      -> {X+1, Y}.
southeast({X, Y}) -> {X, Y+1}.
southwest({X, Y}) -> {X-1, Y+1}.
west({X, Y})      -> {X-1, Y}.
northeast({X, Y}) -> {X, Y-1}.
northwest({X, Y}) -> {X-1, Y-1}.

follow(Path) ->
    io:format("Following: ~p~n", [Path]),
    follow(Path, {0, 0}).

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

part1(IT) ->
    Input = input(IT),
    go(Input, #{}).

go([], Map)->
    Tiles = maps:values(Map),
    {Black, White} = lists:partition(fun(T) -> T == black end, Tiles),
    {black, length(Black), white, length(White)};
go([Path | Paths], Map) ->
    go(Paths, flip(follow(Path), Map)).

flip(Coord, Map) ->
    case maps:get(Coord, Map, white) of
        white -> Map#{Coord => black};
        black -> Map#{Coord => white}
    end.



test() ->
    {4, 3} = east({3, 3}),
    {3, 4} = southeast({3, 3}),
    {2, 4} = southwest({3, 3}),
    {2, 3} = west({3, 3}),
    {3, 2} = northeast({3, 3}),
    {2, 2} = northwest({3, 3}).
