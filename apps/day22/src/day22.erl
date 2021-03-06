-module(day22).

-export([part1/1, part2/1]).

part1(IT) ->
    {One, Two} = input(IT),
    game(One, Two, 1).

part2(IT) ->
    {One, Two} = input(IT),
    recursive_game(One, Two, 1, 1, sets:new()).

recursive_game([], Cards, _Round, _Game, _) ->
    %% io:format("Player 2 wins Game ~p after ~p rounds with score of ~p~n", [_Game, _Round, score(Cards)]),
    {player2, score(Cards)};
recursive_game(Cards, [], _Round, _Game, _) ->
    %% io:format("Player 1 wins Game ~p after ~p rounds with score of ~p~n", [_Game, _Round, score(Cards)]),
    {player1, score(Cards)};
recursive_game([A | T1]=P1, [B | T2]=P2, Round, Game, Seen) ->
    %% io:format("-- Round ~p (Game ~p) --~n", [Round, Game]),
    %% io:format("Player 1 deck: ~p~nPlayer 2 deck: ~p~n", [P1, P2]),
    %% io:format("Player 1 plays: ~p~nPlayer 2 plays: ~p~n", [A, B]),
    Seen1 = sets:add_element({P1, P2}, Seen),
    case the_rules(P1, P2, Seen) of
        game_seen -> recursive_game(P1, [], Round + 1, Game, Seen1);
        recurse ->
            %% io:format("Playing a sub-game to determint eh winner...~n~n"),
            SubDeck1 = lists:sublist(T1, A),
            SubDeck2 = lists:sublist(T2, B),
            case new_game(A, B, SubDeck1, SubDeck2, Game + 1) of
                {player1, Cards} -> recursive_game(T1 ++ Cards, T2, Round + 1, Game, Seen1);
                {player2, Cards} -> recursive_game(T1, T2 ++ Cards, Round + 1, Game, Seen1)
            end;
        {player1, Cards} ->
            %% io:format("Player 1 wins round ~p of game ~p!~n~n", [Round, Game]),
            recursive_game(T1 ++ Cards, T2, Round + 1, Game, Seen1);
        {player2, Cards} ->
            %% io:format("Player 2 wins round ~p of game ~p!~n~n", [Round, Game]),
            recursive_game(T1, T2 ++ Cards, Round + 1, Game, Seen1)
    end.

the_rules(One, Two, Seen) ->
    case sets:is_element({One, Two}, Seen) of
        true -> game_seen;
        false -> the_rules(One, Two, unseen)
    end;
the_rules(One, Two, unseen) ->
    case can_recurse(One) andalso can_recurse(Two) of
        true -> recurse;
        false -> compare(One, Two)
    end.

can_recurse([H | T]) -> H =< length(T).

new_game(A, B, Deck1, Deck2, Game) ->
    %% io:format("~n=== Game ~p ===~n~n", [Game]),
    case recursive_game(Deck1, Deck2, 0, Game, sets:new()) of
        {player1, _Score} -> {player1, [A, B]};
        {player2, _Score} -> {player2, [B, A]}
    end.

game([], Cards, _Round) ->
    %% io:format("Player 2 wins after ~p rounds~n", [_Round]),
    score(Cards);
game(Cards, [], _Round) ->
    %% io:format("Player 1 wins after ~p rounds~n", [_Round]),
    score(Cards);
game([A | T1]=_P1, [B | T2]=_P2, Round) ->
    %% io:format("-- Round ~p --~n", [Round]),
    %% io:format("Player 1 deck: ~p~nPlayer 2 deck: ~p~n", [_P1, _P2]),
    %% io:format("Player 1 plays: ~p~nPlayer 2 plays: ~p~n", [A, B]),
    case compare(A, B) of
        {player1, Cards} ->
            %% io:format("Player 1 wins the round!~n"),
            game(T1 ++ Cards, T2, Round + 1);
        {player2, Cards} ->
            %% io:format("Player 2 wins the round!~n"),
            game(T1, T2 ++ Cards, Round + 1)
    end.

score(Cards) ->
    NumCards = length(Cards),
    Indexed = lists:zip(lists:reverse(lists:seq(1, NumCards)), Cards),
    Combined = lists:map(fun ({X,Y}) -> X*Y end, Indexed),
    lists:sum(Combined).

compare([A | _], [B | _]) ->
    compare(A, B);
compare(One, Two) ->
    case One > Two of
        true -> {player1, [One, Two]};
        false -> {player2, [Two, One]}
    end.

input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    [Player1, Player2] = split(nlnl, Data),
    {parse_deck(Player1), parse_deck(Player2)}.

split(nl, Bin)     -> binary:split(Bin, <<"\n">>, [global, trim]);
split(nlnl, Bin) -> binary:split(Bin, <<"\n\n">>, [global, trim]);
split(space, Bin)  -> binary:split(Bin, <<" ">>, [global, trim]);
split(comma, Bin)  -> binary:split(Bin, <<", ">>, [global, trim]).

parse_deck(Lines) ->
    [_ | Cards] = split(nl, Lines),
    lists:map(fun binary_to_integer/1, Cards).
