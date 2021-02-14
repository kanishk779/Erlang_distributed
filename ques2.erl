-module(ques2).
-export([main/1, inputGraph/1, takeEdges/3, eachProcess/0]).

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
	{Edge_list1, Source, Processes, Vertices, Edges}.

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

eachProcess() ->
	1.
% dijkstra() ->


createProcesses(Num_Id, Curr, Processes) ->
	Pid = spawn('ques2', eachProcess, []),
	Num_Id1 = dict:store(Curr, Pid, Num_Id),
	if
		Curr == Processes ->
			Num_Id1;
		true ->
			createProcesses(Num_Id1, Curr+1, Processes)
	end.

main([InF, OutF]) ->
	{Edge_list, Source, Processes, Vertices, Edges} = inputGraph(InF),
	N = dict:new(),
	Num = dict:store(1, self(), N),
	Num_Id = createProcesses(Num, 1, Processes),
	{ok, Out} = file:open(OutF, [write]), % open the output file
	io:format(Out, "~w~n", Num_Id),
	file:close(Out).
