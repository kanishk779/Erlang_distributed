-module(ques2).
-export([main/1, inputGraph/1, takeEdges/3, eachProcess/0, runProcess/3, createProcesses/3, splitVertices/5, sendSplitStatus/3, rootProcess/4]).
%% We assume that the graph is connected
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
			end  % This is very important (learn how to use nested Case/if 's, otherwise u r doomed)
	end.

statusUpdate([], _, _, Status) ->
	Status;
statusUpdate([H | T], D, U, Status) ->
	{To, Weight} = H,
	Res = dict:find(To, Status),
	if
		Res /= error  ->
			{Dist, Lab} = dict:fetch(To, Status),
			if
				Dist > (Weight + D) ->
					Status1 = dict:store(To, {Weight + D,Lab}, Status),
					statusUpdate(T, D, U, Status1);
				true ->
					statusUpdate(T, D, U, Status)
			end;
		true ->
			statusUpdate(T, D, U, Status)
	end.

% Status is a dict Vertex -> {distance, visited/unvisited}
runProcess(Edge_list, Status, Root_pid) ->
	% create list of un-visited vertices (use list comprehension)
	StatusList = dict:to_list(Status),
	Unvisited = [{D, V} || {V, {D, X}} <- StatusList, X == unvisited],
	Len = length(Unvisited),
	% compute min
	MinVal = case Len > 0 of
		true ->
			lists:foldl(fun({X, V1}, {Y, V2}) -> if X < Y -> {X, V1}; true -> {Y, V2} end end, hd(Unvisited), tl(Unvisited));
		false ->
			{infinity, 0}
	end,
	% Send min to root
	Root_pid ! MinVal,
	% Receive global min(will receive stop on completion)
	receive GlobalMin -> ok end,
	{D, U} = GlobalMin,
	if
		D /= infinity ->
			% Update Status
			NewStatus = statusUpdate(dict:fetch(U, Edge_list), D, U, Status),
			% mark U as visited if present in Status
			Res = dict:find(U, Status),
			if
				Res /= error ->
					{ok, {VD, _}} = Res,
					runProcess(Edge_list, dict:store(U , {VD, visited}, NewStatus), Root_pid);
				true ->
					runProcess(Edge_list, NewStatus, Root_pid)
			end;
		true ->
			Final = [{V, Dist} || {V, {Dist, _}} <- dict:to_list(Status)],
			Root_pid ! Final % Send final status containing distances of every vertex to root
	end.

eachProcess() ->
	receive Sta -> ok end,
	Status = dict:from_list(Sta),
	receive {Edge_list, Root_pid} -> ok end,
	runProcess(Edge_list, Status, Root_pid).

% creates processes and stores their PID in Num_Id dictionary
createProcesses(Num_Id, Curr, Processes) when Curr =< Processes ->
	Pid = spawn(?MODULE, eachProcess, []),
	Num_Id1 = dict:store(Curr, Pid, Num_Id),
	if
		Curr == Processes ->
			Num_Id1;
		true ->
			createProcesses(Num_Id1, Curr+1, Processes)
	end;
createProcesses(Num_Id, Curr, Processes) when Curr > Processes ->
	Num_Id. % If there is only one process, then no need to spawn aditional processes

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

sendSplitStatus(Status, VertexList, PID) ->
	PID ! [dict:fetch(X, Status) || X <- VertexList].

receiveMinimum(MinList, Curr, Processes) when Curr =< Processes ->
	receive {Dist, Vertex} -> ok end,
	NewList = MinList ++ [{Dist, Vertex}], % Append the received data to the MinList
	if
		Curr == Processes ->
			NewList;
		true ->
			receiveMinimum(NewList, Curr+1, Processes)
	end;
receiveMinimum(MinList, Curr, Processes) when Curr > Processes ->
	MinList.

receiveFinalStatus(Curr, Processes, DistList) when Curr =< Processes ->
	receive PartialList -> ok end,
	NewList = DistList ++ PartialList,
	if
		Curr == Processes ->
			NewList;
		true ->
			receiveFinalStatus(Curr+1, Processes, NewList)
	end;
receiveFinalStatus(Curr, Processes, DistList) when Curr > Processes ->
	DistList.

rootProcess(Status, Edge_list, Processes, Num_Id) ->
	% first find the minimum vertex(unvisited)
	StatusList = dict:to_list(Status),
	Unvisited = [{D, V} || {V, {D, X}} <- StatusList, X == unvisited],
	Len = length(Unvisited),
	% compute min
	MinVal = case Len > 0 of
		true ->
			lists:foldl(fun({X, V1}, {Y, V2}) -> if X < Y -> {X, V1}; true -> {Y, V2} end end, hd(Unvisited), tl(Unvisited));
		false ->
			{infinity, 0}
	end,
	% receive from all the process their minimum vertex, distance.
	MinList = receiveMinimum([MinVal], 2, Processes),
	% if minimum is infinity than end the algorithm and inform other processes as well(Just by sending infinity)
	Len1 = length(MinList),
	GlobalMin = case Len1 > 0 of
		true ->
			lists:foldl(fun({X, V1}, {Y, V2}) -> if X < Y -> {X, V1}; true -> {Y, V2} end end, hd(MinList), tl(MinList));
		false ->
			{infinity, 0}
	end,
	io:format("global min ~w ~n", [GlobalMin]),
	% send the Distance and Node of GloabalMin to all other processes
	if
		Processes > 1 ->
			lists:foreach(fun(K) -> dict:fetch(K, Num_Id) ! GlobalMin end, lists:seq(2, Processes));
		true -> ok
	end,
	% update the status dictionary by using dijkstra algo
	{D, U} = GlobalMin,
	if
		D /= infinity ->
			% Update Status
			NewStatus = statusUpdate(dict:fetch(U, Edge_list), D, U, Status),
			% mark U as visited if present in Status
			Res = dict:find(U, Status),
			if
				Res /= error ->
					{ok, {VD, _}} = Res,
					rootProcess(dict:store(U, {VD, visited}, NewStatus), Edge_list, Processes, Num_Id);
				true ->
					rootProcess(NewStatus, Edge_list, Processes, Num_Id)
			end;
		true ->
			Final = [{V, Dist} || {V, {Dist, _}} <- dict:to_list(Status)],
			DistList = receiveFinalStatus(2, Processes, Final),
			DistList
	end.

%% each process will send infinity if there is no unvisited vertex
main([InF, OutF]) ->
	{Edge_list, Source, Processes, Vertices, _} = inputGraph(InF),
	{ok, Out} = file:open(OutF, [write]), % open the output file
	
	N = dict:new(),
	% Num_Id is the dictionary which maps process number to process PID
	Num_Id = createProcesses(N, 2, Processes),
	
	VList = lists:seq(1, Vertices), % list of numbers of vertices
	VertexDict = splitVertices(dict:new(), VList, floor(Vertices/Processes), 1, Processes),
	Sta = [if Y /= Source -> {Y, {Y, {infinity, unvisited}}}; true -> {Y, {Y,{0, unvisited}}} end || Y <- VList],
	Status = dict:from_list(Sta),
	% Send data to other processes only if they exist
	if
		Processes > 1 ->
			lists:foreach(fun(K) -> sendSplitStatus(Status, dict:fetch(K, VertexDict), dict:fetch(K, Num_Id)) end, lists:seq(2, Processes)),
			lists:foreach(fun(K) -> dict:fetch(K, Num_Id) ! {Edge_list, self()} end, lists:seq(2, Processes));
		true -> ok
	end,
	RootSta = [dict:fetch(X, Status) || X <- dict:fetch(1, VertexDict)], % create status for root process
	RootStatus = dict:from_list(RootSta),
	Result = rootProcess(RootStatus, Edge_list, Processes, Num_Id), % get the final Distance list
	lists:foreach(fun({V, D}) -> io:format(Out, "~w ~w ~n",[V, D]) end, lists:sort(Result)), % print the sorted list (sorted by vertex number)
	file:close(Out).
