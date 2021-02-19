-module(ques2).
-export([main/1, inputGraph/1, takeEdges/3, eachProcess/0, runProcess/6, createProcesses/3, splitVertices/5]).

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
% Processes -> no. of processes , K ->, Vertices -> no. of vertices, 
runProcess(Edge_list, Processes, K, Vertices, Status, Root_pid) ->
	io:format("~w ~n", [1]),
	1.
eachProcess() ->
	receive {Edge_list, Processes, K, Vertices, Status, Root_pid} -> ok end,
	runProcess(Edge_list, Processes, K, Vertices, Status, Root_pid),
	1.
% dijkstra() ->


createProcesses(Num_Id, Curr, Processes) ->
	Pid = spawn(?MODULE, eachProcess, []),
	Num_Id1 = dict:store(Curr, Pid, Num_Id),
	if
		Curr == Processes ->
			Num_Id1;
		true ->
			createProcesses(Num_Id1, Curr+1, Processes)
	end.

splitVertices(VertexDict, L, N, Curr, Processes) ->
	if Curr == Processes ->
		VertexDict1 = dict:store(Curr, L, VertexDict),
		VertexDict1;
	true ->
		{L1, L2} = lists:split(N, L),
		VertexDict1 = dict:store(Curr, L1, VertexDict),
		splitVertices(VertexDict1, L2, N, Curr+1, Processes)
	end.
	
main([InF, OutF]) ->
	{Edge_list, Source, Processes, Vertices, Edges} = inputGraph(InF),
	N = dict:new(),
	% Num_Id is the dictionary which maps process number to process PID
	Num_Id = createProcesses(N, 2, Processes),
	Id_list = dict:to_list(Num_Id),
	{ok, Out} = file:open(OutF, [write]), % open the output file
	% io:format(Out, "~w~n", Num_Id),
	io:format(Out, "~w~n", [Id_list]),
	Process_Nums = lists:seq(1, Processes),
	Status = lists:foreach(fun(Key) -> {if Key == 1 -> 0; true -> infinity end, not_visited} end, Process_Nums),
	Worker = fun({K, V}) -> V ! {Edge_list, Processes, K, Vertices, Status, self()} end,
	lists:foreach(Worker, Id_list),
	file:close(Out).
