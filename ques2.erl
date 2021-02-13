-module(ques2).
-export([main/1, inputGraph/1, takeEdges/3]).

%% Edge_list is a dictionary Vertex -> [Edge], where Edge = {To, Weight}
inputGraph(InF) ->
	{ok, Input_file} = file:open(InF, read), % open the input file
	{ok, Line1} = file:read_line(Input_file), % read line from input file
	{Processes, _} = string:to_integer(string:trim(Line1)),
	{ok, Line2} = file:read_line(Input_file),
	% converts the string to tokens by splitting using space " "
	String_list = string:tokens(string:trim(Line2)," "),
	% converts list of strings to list of integers
	L = lists:map( fun(X) -> list_to_integer(X) end, String_list),
	[Vertices, Edges] = L,
	Edge_list = dict:new(),
	{Edge_list1, Source} = takeEdges(Input_file, Edge_list, 0),
	file:close(Input_file),
	dict:fetch(2, Edge_list1).

takeEdges(InF, Edge_list, Source) ->
	case io:get_line(InF, "") of
		eof -> {Edge_list, Source};

		Line -> String_list = string:tokens(string:trim(Line)," "),
			% converts list of strings to list of integers
			L = lists:map( fun(X) -> list_to_integer(X) end, String_list),
			Elements = string:len(L),
			case Elements == 1 of
				true ->
					[S] = L,
					takeEdges(InF, Edge_list, S);
				false ->
					[From, To, W] = L,
					Edge_list1 = dict:append(From, {To, W}, Edge_list),
					Edge_list2 = dict:append(To, {From, W}, Edge_list1),
					takeEdges(InF, Edge_list2, Source)
			end  % This is very important (learn how to use nested Case, otherwise u r doomed)
	end.

% dijkstra() ->

main([InF, OutF]) ->
	Val = inputGraph(InF),
	{ok, Out} = file:open(OutF, [write]), % open the output file
	io:format(Out, "~w", [Val]),
	file:close(Out).
