-module (tut4).
-export ([loop/0, start/0]).

loop() ->
	receive
		{rectangle, W, H} ->
			io:fwrite("Area of rectange ~w ~n", [W*H]);
		{circle, R} ->
			io:fwrite("Area of circle ~w ~n", [3*R*R]);
		Other ->
			io:fwrite("Unknown")
	end.

start() ->
   Pid = spawn(fun() -> loop() end),
   Pid ! {circle, 6}.
