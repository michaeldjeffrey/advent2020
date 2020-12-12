-module(eleven).
-compile(export_all).
-export([]).

-define(OPEN, $.).
-define(EMPTY, $L).
-define(OCCUPIED, $#).

prep() ->
    go(read_input(test)).

main() ->
    continue2(go(read_input(main))).

go(Input) ->
    Height = length(Input),
    Width = byte_size(hd(Input)),
    io:format("Working with inpu ~p by ~p~n", [Width, Height]),
    Map = maps:from_list([{{X, Y}, new_position(Input, X, Y)} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)]),
    {Map, Width, Height}.

step({Map, Width, Height}) ->
    Map1 = mark_each_for_change({Map, Width, Height}),
    change_for_each({Map1, Width, Height}).

step2({Map, W, H}) ->
    Map1 = mark_each_for_change2({Map, W, H}),
    change_for_each({Map1, W, H}).

continue2({Map, _, _}=MapInfo) ->
    print(MapInfo),
    continue2(MapInfo, occupied_count(Map)).

continue2({_Map, W, H} = Input, PrevCount) ->
    Map1 = step2(Input),
    print({Map1, W, H}),
    NewCount = occupied_count(Map1),
    io:format("Count: ~p -> ~p~n", [PrevCount, NewCount]),
    if
        PrevCount == NewCount ->
            {ok, PrevCount};
        true ->
            continue2({Map1, W, H}, NewCount)
    end.

continue({Map, Width, Height}) ->
    print({Map, Width, Height}),
    continue({Map, Width, Height}, occupied_count(Map)).

continue({_Map, Width, Height} = Input, PrevCount) ->
    Map1 = step(Input),
    print({Map1, Width, Height}),
    NewCount = occupied_count(Map1),
    io:format("Count: ~p -> ~p~n", [PrevCount, NewCount]),
    if
        PrevCount == NewCount ->
            {ok, PrevCount};
        true ->
            continue({Map1, Width, Height}, NewCount)
    end.

mark_each_for_change({Map, Width, Height}) ->
    map_values(fun(Coord) ->
                       Me = maps:get(Coord, Map),
                       Neighbors = get_neighbors(Coord, Map),
                       mark_for_change(Me, Neighbors)
               end, {Map, Width, Height}).

mark_each_for_change2({Map, _W, _H} = MapInfo) ->
    map_values(fun(Coord) ->
                       Me = maps:get(Coord, Map),
                       Neighbors = get_visible_neighbors(Coord, MapInfo),
                       %%io:format("Neighbors: ~p~n", [Neighbors]),
                       mark_for_change2(Me, Neighbors)
               end, MapInfo).

change_for_each({Map, Width, Heigth}) ->
    map_values(fun (Coord) ->
                       change(maps:get(Coord, Map))
               end, {Map, Width, Heigth}).

new_position(Input, X, Y) ->
    {to_type(Input, X, Y), false}.

get_neighbors({X, Y} = Me, Map) ->
    [maps:get({NX, NY}, Map) ||
        NX <- [X-1, X, X+1],
        NY <- [Y-1, Y, Y+1],
        {NX, NY} /= Me, %% Not myself
        maps:get({NX, NY}, Map, false) /= false]. %% For corners

get_visible_neighbors(Me, MapInfo) ->
    [
     has_left_neighbor(left(Me), MapInfo),
     has_left_up_neighbor(left_up(Me), MapInfo),
     has_up_neighbor(up(Me), MapInfo),
     has_right_up_neighbor(right_up(Me), MapInfo),
     has_right_neighbor(right(Me), MapInfo),
     has_right_down_neighbor(right_down(Me), MapInfo),
     has_down_neighbor(down(Me), MapInfo),
     has_left_down_neighbor(left_down(Me), MapInfo)
    ].

left({X, Y}) -> {X-1, Y}.
left_up({X, Y}) -> {X-1, Y-1}.
up({X, Y}) -> {X, Y-1}.
right_up({X, Y}) -> {X+1, Y-1}.
right({X, Y}) -> {X+1, Y}.
right_down({X, Y}) -> {X+1, Y+1}.
down({X, Y}) -> {X, Y+1}.
left_down({X, Y}) -> {X-1, Y+1}.

has_left_neighbor({0, _}, _) -> false;
has_left_neighbor(Coord, {Map, W, H}) ->
    Test = element(1, maps:get(Coord, Map)),
    %%io:format("left search: ~p -> ~p~n", [Coord, Test]),
    case Test of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_left_neighbor(left(Coord), {Map, W, H})
    end.

has_left_up_neighbor({0, _}, _) -> false;
has_left_up_neighbor({_, 0}, _) -> false;
has_left_up_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_left_up_neighbor(left_up(Coord), {Map, W, H})
    end.

has_up_neighbor({_, 0}, _) -> false;
has_up_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_up_neighbor(up(Coord), {Map, W, H})
    end.

has_right_up_neighbor({X, _}, {_, W, _}) when X > W -> false;
has_right_up_neighbor({_, 0}, _) -> false;
has_right_up_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_right_up_neighbor(right_up(Coord), {Map, W, H})
    end.

has_right_neighbor({X, _}, {_, W, _}) when X > W -> false;
has_right_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_right_neighbor(right(Coord), {Map, W, H})
    end.

has_right_down_neighbor({X, _}, {_, W, _}) when X > W -> false;
has_right_down_neighbor({_, Y}, {_, _, H}) when Y > H -> false;
has_right_down_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_right_down_neighbor(right_down(Coord), {Map, W, H})
    end.


has_down_neighbor({_, Y}, {_, _, H}) when Y > H-> false;
has_down_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_down_neighbor(down(Coord), {Map, W, H})
    end.

has_left_down_neighbor({0, _}, {_, _, _}) -> false;
has_left_down_neighbor({_, Y}, {_, _, H}) when Y > H -> false;
has_left_down_neighbor(Coord, {Map, W, H}) ->
    case element(1, maps:get(Coord, Map)) of
        ?OCCUPIED -> true;
        ?EMPTY -> false;
        ?OPEN -> has_left_down_neighbor(left_down(Coord), {Map, W, H})
    end.


last_not_floor(List) ->
    first_not_floor(lists:reverse(List)).

first_not_floor([]) -> {?OPEN, false};
first_not_floor([{?OPEN, _} | Rest]) ->
    first_not_floor(Rest);
first_not_floor([H|_T]) ->
    H.

get_left_neighbors({X, Y}, {Map, _W, _H}) ->
    [maps:get({NX, Y}, Map) || NX <- lists:seq(1, X)].

get_right_neighbors({X, Y}, {Map, W,  _H}) ->
    [maps:get({NX, Y}, Map) || NX <- lists:seq(X+1, W)].

get_up_neighbors({X, Y}, {Map, _W, _H}) ->
    [maps:get({X, NY}, Map) || NY <- lists:seq(1, Y)].

get_down_neighbors({X, Y}, {Map, _W, H}) ->
    [maps:get({X, NY}, Map) || NY <- lists:seq(Y+1, H)].

get_up_left_neighbors(Coord, {Map, _W, _H}) ->
    get_up_left_neighbors(Coord, Map, []).

get_up_left_neighbors({0, _}, _, Acc) -> Acc;
get_up_left_neighbors({_, 0}, _, Acc) -> Acc;
get_up_left_neighbors({X, Y}, Map, Acc) ->
    get_up_left_neighbors({X-1, Y-1}, Map, [maps:get({X, Y}, Map) | Acc]).

get_down_left_neighbors(Coord, MapInfo) ->
    get_down_left_neighbors(Coord, MapInfo, []).

get_down_left_neighbors({0, _}, _, Acc)  -> Acc;
get_down_left_neighbors({_, Height1}, {_, _, Height}, Acc) when Height1 == Height + 1-> Acc;
get_down_left_neighbors({X, Y}, {Map, _, _} = MapInfo, Acc) ->
    get_down_left_neighbors({X-1, Y+1}, MapInfo, [maps:get({X, Y}, Map) | Acc]).

get_up_right_neighbors(Coord, MapInfo) ->
    get_up_right_neighbors(Coord, MapInfo, []).

get_up_right_neighbors({Width1, _}, {_, Width, _}, Acc) when Width1 == Width + 1-> Acc;
get_up_right_neighbors({_, 0}, _, Acc) -> Acc;
get_up_right_neighbors({X, Y}, {Map, _, _} = MapInfo, Acc) ->
    get_up_right_neighbors({X+1, Y-1}, MapInfo, [maps:get({X, Y}, Map) | Acc]).

get_down_right_neighbors(Coord, MapInfo) ->
    get_down_right_neighbors(Coord, MapInfo, []).

get_down_right_neighbors({Width1, _}, {_, Width, _}, Acc) when Width1 == Width+1-> Acc;
get_down_right_neighbors({_, Height1}, {_, _, Height}, Acc) when Height1 == Height+1 -> Acc;
get_down_right_neighbors({X, Y}, {Map, _, _} = MapInfo, Acc) ->
    get_down_right_neighbors({X+1, Y+1}, MapInfo, [maps:get({X, Y}, Map) | Acc]).

-spec to_type(
        Input :: [binary()],
        X :: non_neg_integer(),
        Y :: non_neg_integer()) -> floor | empty | occupied.
to_type(Input, X, Y) ->
    Row = lists:nth(Y, Input),
    binary:at(Row, X-1).

map_values(Fn, {_Map, Width, Height}) ->
    maps:from_list([{{X, Y}, Fn({X, Y})} ||
                       X <- lists:seq(1, Width),
                       Y <- lists:seq(1, Height)]).

occupied_count(Map) ->
    occupied_count(maps:values(Map), 0).

occupied_count([], Count) -> Count;
occupied_count([{?OCCUPIED, false} | Rest], Count) ->
    occupied_count(Rest, Count + 1);
occupied_count([_H | Rest], Count) ->
    occupied_count(Rest, Count).

print_row(Num, Input, Width) ->
    Row = [maps:get({X, Num}, Input) || X <- lists:seq(1, Width)],
    Chars = [Char || {Char, _} <- Row],
    io:format("~p~n", [lists:flatten(Chars)]).

print(_) -> ok.
%% print({Map, W, H}) ->
%%     [print_row(N, Map, W) || N <- lists:seq(1, H)].
%% print({Map, Width, Height}) ->
%%     %% ok.
%%     io:format("map:~n"),
%%     map_values(fun (Coord) ->
%%                        FString =
%%                        case Coord of
%%                            {_, Width} -> "~c~n";
%%                            _ -> "~c"
%%                        end,
%%                        {Char, _} = maps:get(Coord, Map),
%%                        io:format(FString, [Char])
%%                    %% ({Width, Y}) -> io:format("~p~n", [maps:get({Width, Y}, Map)]);
%%                    %% (Coord) -> io:format("~p", [maps:get(Coord, Map)])
%%                end, {Map, Width, Height}),
%%     io:format("~n~n", []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Seats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {type, should_change}.
-type position() :: {?OPEN | ?EMPTY | ?OCCUPIED, boolean}.

-spec change(Me :: position()) -> boolean().
change(Me = { _, false}) ->
    Me;
change({?EMPTY, true}) ->
    {?OCCUPIED, false};
change({?OCCUPIED, true}) ->
    {?EMPTY, false}.

-spec mark_for_change(Me :: position(), Neighbors :: [position()]) -> boolean().
mark_for_change(Me = {?OPEN, _}, _) ->
    Me;
mark_for_change({?EMPTY, false}, Neighbors) ->
    {?EMPTY,
     lists:all(fun is_true/1, [is_empty(N) || N <- Neighbors])};
mark_for_change({?OCCUPIED, false}, Neighbors) ->
    {?OCCUPIED,
     4 =< length([N || N <- Neighbors, not is_empty(N)])}.

-spec mark_for_change2(Me :: position(), Neighbors :: [position()]) -> boolean().
mark_for_change2(Me = {?OPEN, _}, _) ->
    Me;
mark_for_change2({?EMPTY, false}, Neighbors) ->
    AllEmpty = [not is_true(N) || N <- Neighbors],
    %%AllTrue = lists:all(fun is_true/1, Neighbors),
    %io:format("Empty: ~p~n", [AllEmpty]),
    {?EMPTY, lists:all(fun is_true/1, AllEmpty)};
mark_for_change2({?OCCUPIED, false}, Neighbors) ->
    %io:format("Occupied: ~p~n", [[is_true(N) || N <- Neighbors]]),
    {?OCCUPIED,
     5 =< length([N || N <- Neighbors, is_true(N)])}.


-spec is_empty(position()) -> boolean().
is_empty({?EMPTY, _}) -> true;
is_empty({?OPEN, _}) -> true;
is_empty(_) -> false.

is_true(true) -> true;
is_true(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reading input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_input(test) -> read_input("apps/eleven/src/test.txt");
read_input(main) -> read_input("apps/eleven/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    binary:split(Data, <<"\n">>, [global, trim]).
