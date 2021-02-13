-module(tut3).
-export([merge/2, main/1, sum/1]).

sum(L) -> sum(L, 0).
sum([], N)    -> N;
sum([H|T], N) -> sum(T, H+N).

merge(A, []) ->
	A;
merge([], B) ->
	B;

merge([A|Ca], [B|Cb]) ->
	if
		A < B ->
			[A| merge(Ca, [B|Cb])];
		true ->
			[B| merge([A|Ca], Cb)]
	end.
main([InF, OutF]) ->
	%io:fwrite("~w~n", A),
	%io:fwrite("~w~n", B).
	{ok, In} = file:open(InF, read),
	{ok, Ln} = file:read_line(In),
	file:close(In),
	Sl = string:tokens(string:trim(Ln)," "),
	L = lists:map( fun(X) -> list_to_integer(X) end, Sl),
	io:fwrite("~w~n", [L]).
	%merge(A, B).
