-module (ques1).
-export ([main/1, passing/4]).

passing(Num, Limit, OutF, Root_pid) ->
	% receive the token
	receive Token -> ok end,
	io:format(OutF, "Process ~w recieved token ~w from Process ~w.~n", [Num, Token, Num-1]),
	if
		Num < Limit-1 ->
			% send token to next process
			Pid = spawn(fun() -> passing(Num+1, Limit, OutF, Root_pid) end),
			Pid ! Token;
		true ->
			% This is the last process and hence will send token to root process
			Root_pid ! Token
	end.
	

main([InF, OutF]) ->
	{ok, Input_file} = file:open(InF, read), % open the input file
	{ok, Line} = file:read_line(Input_file), % read line from input file
	file:close(Input_file),
	{ok, Out} = file:open(OutF, [write]), % open the output file
	
	% converts the string to tokens by splitting using space " "
	String_list = string:tokens(string:trim(Line)," "),
	% converts list of strings to list of integers
	L = lists:map( fun(X) -> list_to_integer(X) end, String_list),
	
	% extract the processes and token
	[Processes, Token] = L,
	% start the process of sending the token to the next process
	Pid = spawn('ques1', passing, [1, Processes, Out, self()]),
	Pid ! Token,
	% receive the token from the last process
	receive Token_r -> ok end,
	io:format(Out,"Process 0 recieved token ~w from Process ~w.~n", [Token_r, Processes-1]),
	file:close(Out).