-module(twenty).
-compile(export_all).
-export([]).

part1(InputType) ->
    Tiles = input(InputType),
    Matches = map_matches(Tiles),
    Corners = find_corners(Matches),
    answer(Corners).

num_matches({tile, _, Left}, {tile, _, Right}) ->
    One = sets:from_list(maps:values(Left)),
    Two = sets:from_list(maps:values(Right)),
    sets:to_list(sets:intersection(One, Two)).

matches({tile, LId, _}=Left, {tile, RId, _}=Right) ->
    case length(num_matches(Left, Right)) of
        2 ->
            {match, LId, RId};
        0 ->
            {nomatch, LId, RId}
    end.

map_matches(Tiles) ->
    Matches = lists:foldl(
                fun({nomatch, _, _}, Acc) -> Acc;
                   ({match, L, R}, Acc) ->
                        Acc1 = maps:update_with(L, fun(V) -> sets:add_element(R, V) end, Acc),
                        maps:update_with(R, fun(V) -> sets:add_element(L, V) end, Acc1)
                end,
                maps:from_list([{Id, sets:new()} || {tile, Id, _} <- Tiles]),
                [matches(L, R) || L <- Tiles, R <- Tiles, L /= R]),
    maps:map(fun(_K, V) -> sets:to_list(V) end, Matches).

find_corners(MatchMap) ->
    maps:filter(
      fun(_K, V) -> V == 2 end,
      MatchMap
     ).

answer(CornerMap) ->
    M = maps:map(fun(_K, V) -> length(V) end, CornerMap),
    lists:foldl(fun(V, A) -> V*A end, 1, maps:keys(M)).

input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Tiles = binary:split(Data, <<"\n\n">>, [global, trim]),
    [parse_tile(Tile) || Tile <- Tiles].

parse_tile(T) ->
    [ID | Rest] = binary:split(T, <<"\n">>, [global, trim]),
    <<"Tile ", Num:4/binary, ":">> = ID,
    {tile, binary_to_integer(Num), pull_edges(Rest)}.

pull_edges(Tile) ->
    First = hd(Tile),
    Last = lists:last(Tile),
    Left = list_to_binary(lists:map(fun(L) -> binary:at(L, 0) end, Tile)),
    Right = list_to_binary(lists:map(fun(L) -> binary:at(L, 9) end, Tile)),
    #{top => First, bottom => Last, left => Left, right => Right,
     top_r => rev(First), bottom_r => rev(Last), left_r => rev(Left), right_r => rev(Right)}.

rev(Bin) ->
    rev(Bin, <<>>).

rev(<<>>, Acc) -> Acc;
rev(<<H:1/binary, Rest/binary>>, Acc) ->
    rev(Rest, <<H/binary, Acc/binary>>).
