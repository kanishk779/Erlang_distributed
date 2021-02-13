-module (tut5).
-export([one/1, main/0]).

one(L) ->
	io:fwrite("value of L is ~w~n", [L]).

main() ->
	L = 5,
	ok = one(L).
