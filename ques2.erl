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

% This reads the data from input file and creates Edge_list and returns the source vertex as well
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
	% compute min
	% Send min to root  lists:foldl(fun({X, W}, {Y, Z}) -> if X < Y -> {X, W}; true -> {Y, Z} end end, hd(T), tl(T)).
	% Receive global min(will receive stop on completion)
	% Update Status 
	io:format("~w ~n", [1]),
	1.

eachProcess() ->
	receive {Edge_list, Processes, K, Vertices, Status, Root_pid} -> ok end,
	receive MyVertices -> ok end,
	runProcess(Edge_list, Processes, K, Vertices, Status, Root_pid),
	1.
% dijkstra() ->

% creates processes and stores their PID in Num_Id dictionary
createProcesses(Num_Id, Curr, Processes) ->
	Pid = spawn(?MODULE, eachProcess, []),
	Num_Id1 = dict:store(Curr, Pid, Num_Id),
	if
		Curr == Processes ->
			Num_Id1;
		true ->
			createProcesses(Num_Id1, Curr+1, Processes)
	end.
% Split the vertices equally among all the processes
splitVertices(VertexDict, L, N, Curr, Processes) ->
	if Curr == Processes ->
		VertexDict1 = dict:store(Curr, L, VertexDict),
		VertexDict1;
	true ->
		{L1, L2} = lists:split(N, L),
		VertexDict1 = dict:store(Curr, L1, VertexDict),
		splitVertices(VertexDict1, L2, N, Curr+1, Processes)
	end.
rootProcess() ->
	% first find the minimum vertex(unvisited)
	% receive from all the process their minimum vertex, distance, their process num
	% if minimum is infinity than end the process
	% update the status dictionary by using dijkstra algo
	% send the updated dictionary to all other processes
	1.
%% each process will send infinity if there is no unvisited vertex
main([InF, OutF]) ->
	{Edge_list, Source, Processes, Vertices, Edges} = inputGraph(InF),
	N = dict:new(),
	% Num_Id is the dictionary which maps process number to process PID
	Num_Id = createProcesses(N, 2, Processes),
	{ok, Out} = file:open(OutF, [write]), % open the output file

	Process_Nums = lists:seq(1, Processes),
	VertexDict = splitVertices(dict:new(), lists:seq(1, Vertices), floor(Vertices/Processes), 1, Processes),
	Sta = [if Y > 1 -> {Y, {infinity, unvisited}}; true -> {Y, {0, unvisited}} end || Y <- Process_Nums],
	Status = dict:from_list(Sta),

	lists:foreach(fun(K) -> dict:fetch(K, Num_Id) ! {Edge_list, Processes, K, Vertices, Status, self()} end, lists:seq(2, Processes)),
	lists:foreach(fun(K) -> dict:fetch(K, Num_Id) ! dict:fetch(K, VertexDict) end, lists:seq(2, Processes)),
	file:close(Out).
