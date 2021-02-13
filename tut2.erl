-module(tut2).
-export([fac/1, convert/2, give/1]).

fac(1) ->
	1;
fac(N) ->
	N * fac(N-1).

convert(N, inch) ->
	N/2.54;
convert(N, centi) ->
	N*2.54.

give([]) ->
	0;
give([F|R]) ->
	1 + give(R).

