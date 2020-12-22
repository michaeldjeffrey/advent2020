-module(day21).
-compile(export_all).
-export([]).

go() ->
    IAndA = input(test),
    AllIngredients = frequencies(gather_ingredients(IAndA, [])),
    Maps = lists:map(fun allergens_to_ingredients/1, IAndA),
    Map = merge(lists:map(fun maps:to_list/1, Maps)),

    PossibleIngredients = maps:map(fun(_K, V) -> uniq(V) end, Map),
    Fixed = fix(PossibleIngredients),

    Ingredients = subtract(maps:keys(AllIngredients), maps:values(Fixed)),
    io:format("Ingredients: ~p~nFrom: ~p~nTaken: ~p~n", [Ingredients, PossibleIngredients, Fixed]),
    lists:sum(lists:map(fun(I) -> maps:get(I, AllIngredients) end, Ingredients)).

gather_ingredients([], Acc) -> Acc;
gather_ingredients([{I, _} | Rest], Acc) ->
    gather_ingredients(Rest, Acc ++ I).

frequencies(List) ->
    frequencies(List, #{}).

frequencies([], F) -> F;
frequencies([H|T], F) ->
    Inc = fun(Count) ->
                  Count + 1 end,
    NewF = maps:update_with(H, Inc, _Default=1, F),
    frequencies(T, NewF).

fix(M) ->
    fix(maps:size(M), M, #{}).

fix(0, _, Fixed) ->
    Fixed;
fix(_MapSize, Is, Fixed) ->
    {_Size, {Allergen, [ToRemove | _Ingredients]}} = min(Is),
    Fixed2 = Fixed#{Allergen => ToRemove},
    Is2 = remove_from(Is, ToRemove),
    Is3 = maps:remove(Allergen, Is2),

    fix(maps:size(Is3), Is3, Fixed2).

remove_from(MapOfLists, ToRemove) ->
    maps:map(fun(_K, V) -> lists:delete(ToRemove, V) end, MapOfLists).


allergens_to_ingredients({I, A}) ->
    allergens_to_ingredients(A, I, #{}).

allergens_to_ingredients([], _, M) -> M;
allergens_to_ingredients([A | As], Is, M) ->
    allergens_to_ingredients(As, Is, M#{A => Is}).


input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Foods = split(nl, Data),
    [parse_food(Food) || Food <- Foods].

parse_food(Bin) ->
    [Ingredients, Allergens] = split(recipe, Bin),
    {uniq(split(space, Ingredients)),
     uniq(split(comma, Allergens))}.


split(nl, Bin)     -> binary:split(Bin, <<"\n">>, [global, trim]);
split(recipe, Bin) -> binary:split(Bin, [<<" (contains ">>,<<")">>], [global, trim]);
split(space, Bin)  -> binary:split(Bin, <<" ">>, [global, trim]);
split(comma, Bin)  -> binary:split(Bin, <<", ">>, [global, trim]).

merge(LoL) ->
    lists:foldl(
      fun({K, Vs}, Acc) ->
              case maps:get(K, Acc, undefined) of
                  undefined -> maps:put(K, Vs, Acc);
                  Value -> maps:put(K, uniq(Value ++ Vs), Acc)
              end
      end,
      #{},
      lists:flatten(LoL)
     ).

uniq(L) ->
    sets:to_list(sets:from_list(L)).

min(Map) ->
    [{StartKey, StartValue} | Values] = maps:to_list(Map),
    lists:foldl(
      fun({K, V}, {Min, MinL}) ->
              if
                  length(V) < Min -> {length(V), {K, V}};
                  true -> {Min, MinL}
              end
      end,
      {length(StartValue), {StartKey, StartValue}},
      Values
     ).

subtract(L1, L2) ->
    sets:to_list(
      sets:subtract(
        sets:from_list(L1),
        sets:from_list(L2)
       )
     ).
