-module(day7).
-compile(export_all).
-export([part1/0, part2/0]).

part1() ->
    [{test, part1_solve(read_input(test))},
     {main, part1_solve(read_input(main))}].

part2() ->
    [{test, [{one, part2_solve(read_input(test))},
             {two, part2_solve(read_input(test2))}]},
     {main, part2_solve(read_input(main))}].

part1_solve(Input) ->
    Graph = prepare_graph_with_input(Input),
    MyBag = {bag, shiny, gold},
    Keys = lists:delete(MyBag, digraph:vertices(Graph)),
    LongestPath = sets:size(sets:from_list([digraph:get_path(Graph, Key, MyBag) || Key <- Keys])),
    LongestPath - 1. %% Remove shiny gold from path

part2_solve(Input) ->
    Graph = prepare_graph_with_input(Input),
    Start = {bag, shiny, gold},
    count_bags(Graph, Start).

prepare_graph_with_input(Input) ->
    Graph = digraph:new(),
    CleanedInput = lists:map(fun parse_line/1, Input),
    add_bags_to_graph(Graph, CleanedInput),
    Graph.

count_bags(Graph, Bag) ->
    OutEdges = digraph:out_edges(Graph, Bag),
    Edges = [digraph:edge(Graph, Edge) || Edge <- OutEdges],
    CleanedEdges = amounts_of_other_bags(Edges),
    Counts = [Amount + (Amount * count_bags(Graph, Next)) || {Amount, Next} <- CleanedEdges],
    lists:sum(Counts).

amounts_of_other_bags(Edges) ->
    [{Amount, Next} || {_Edge, _Bag, Next, {amount, Amount}} <- Edges].

parse_line(Line) ->
    [Holder, Bags0] = split(Line, <<" contain ">>),
    case Bags0 of
        <<"no other bags.">> ->
            {bins_to_bags(Holder), no_bags};
        _ ->
            Bags1 = split(Bags0, <<", ">>),
            Bags2 = lists:map(fun bins_to_bags/1, Bags1),
            {bins_to_bags(Holder), Bags2}
    end.

bins_to_bags(Bin) when is_binary(Bin) ->
    bins_to_bags(split(Bin, <<" ">>));
bins_to_bags([Adjective, Color, <<"bags">>]) ->
    {bag, binary_to_atom(Adjective), binary_to_atom(Color)};
bins_to_bags([Amount, Adjective, Color, _]) ->
    [{amount, binary_to_integer(Amount)},
     {bag, binary_to_atom(Adjective), binary_to_atom(Color)}].

add_bags_to_graph(_Graph, []) ->
    ok;
add_bags_to_graph(Graph, [{Holder, no_bags} | Rest]) ->
    digraph:add_vertex(Graph, Holder),
    add_bags_to_graph(Graph, Rest);
add_bags_to_graph(Graph, [{Holder, HeldBags} | Rest]) ->
    HolderVertex = digraph:add_vertex(Graph, Holder),
    ok = add_bags_held(Graph, HolderVertex, HeldBags),
    add_bags_to_graph(Graph, Rest).

add_bags_held(_Graph, _Parent, []) -> ok;
add_bags_held(Graph, Parent, [[Amount, Bag] | Rest]) ->
    Vertex = digraph:add_vertex(Graph, Bag),
    digraph:add_edge(Graph, Parent, Vertex, Amount),
    add_bags_held(Graph, Parent, Rest).

read_input(test2) -> read_input("apps/day7/src/test2.txt");
read_input(test) -> read_input("apps/day7/src/test.txt");
read_input(main) -> read_input("apps/day7/src/input.txt");
read_input(Filename) ->
    {ok, Data} = file:read_file(Filename),
    binary:split(Data, <<"\n">>, [global, trim]).

split(Bin, Term) ->
    binary:split(Bin, Term, [global, trim_all]).
