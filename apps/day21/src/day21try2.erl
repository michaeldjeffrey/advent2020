-module(day21try2).
-compile(export_all).

one() ->
    % Guessed 2542, incorrect (count_ingredients base case is 1, not 0)
    Input = input(main),
    Counts = count_ingredients(Input),
    Safe = find_safe_ingredients(Input),
    lists:sum(maps:values(maps:with(ordsets:to_list(Safe), Counts))).

two() ->
    Input = input(main),
    Allergens = lists:sort(identify_allergens(Input)),
    string:join([binary_to_list(I) || {_, I} <- Allergens], ",").

identify_allergens(Foods) ->
    Possible = find_possible_allergens(Foods),
    eliminate_ingredients(Possible).

eliminate_ingredients(Possible) ->
    Result = maps:fold(
               fun(Key, [Ingredient], Acc) ->
                       maps:map(
                         fun (Key2, Possibilities) ->
                                 case Key == Key2 of
                                     true -> Possibilities;
                                     false -> ordsets:del_element(Ingredient, Possibilities)
                                 end
                         end,
                         Acc);
                  (_, _, Acc) -> Acc
               end,
               Possible,
               Possible),

    case Result == Possible of
        true -> [{Allergen, Value} || {Allergen, [Value]} <- maps:to_list(Possible)];
        false -> eliminate_ingredients(Result)
    end.

find_safe_ingredients(Foods) ->
    maps:fold(
      fun(_Allergen, Ingredients, Acc) -> ordsets:subtract(Acc, Ingredients) end,
      all_ingredients(Foods),
      find_possible_allergens(Foods)).

find_possible_allergens(Foods) ->
    find_possible_allergens(Foods, #{}).

find_possible_allergens([], Map) -> Map;
find_possible_allergens([{Ingredients, Allergens} | Foods], Map) ->
    ISet = ordsets:from_list(Ingredients),
    Result = lists:foldl(
               fun(Allergen, Acc) ->
                       maps:update_with(Allergen, intersect_with(ISet), ISet, Acc)
               end,
               Map,
               Allergens),
    find_possible_allergens(Foods, Result).

all_ingredients(Foods) ->
    all_ingredients(Foods, ordsets:new()).

all_ingredients([], Set) -> Set;
all_ingredients([{Ingredients, _A} | Foods], Set) ->
    all_ingredients(
     Foods,
     ordsets:union(
       Set,
       ordsets:from_list(Ingredients)
      )).

count_ingredients(Foods) ->
    count_ingredients(Foods, #{}).

count_ingredients([], Counts) -> Counts;
count_ingredients([{Ingredients, _Allergens} | Foods], Counts) ->
    Result = lists:foldl(
               fun(I, A) -> maps:update_with(I, fun add_one/1, 1, A) end,
               Counts,
               Ingredients),
    count_ingredients(Foods, Result).

input(test) -> input("src/test.txt");
input(main) -> input("src/input.txt");
input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Foods = split(nl, Data),
    [parse_food(Food) || Food <- Foods].

parse_food(Bin) ->
    [Ingredients, Allergens] = split(recipe, Bin),
    {split(space, Ingredients),
     split(comma, Allergens)}.


split(nl, Bin)     -> binary:split(Bin, <<"\n">>, [global, trim]);
split(recipe, Bin) -> binary:split(Bin, [<<" (contains ">>,<<")">>], [global, trim]);
split(space, Bin)  -> binary:split(Bin, <<" ">>, [global, trim]);
split(comma, Bin)  -> binary:split(Bin, <<", ">>, [global, trim]).

uniq(L) ->
    sets:to_list(sets:from_list(L)).

add_one(V) -> V + 1.

intersect_with(One) ->
    fun(Two) ->
            ordsets:intersection(One, Two)
    end.
