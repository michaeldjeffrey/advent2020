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
