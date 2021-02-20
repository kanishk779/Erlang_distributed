# DS_assign2
Erlang assignment

## Problem 1
In this problem we need to pass tokens between processes in circular fashion.
The function 
```
passing(Num, Limit, OutF, Root_pid) ->
```
is used for spawning the next process and sending the token to it. In case of
last process it need not spawn a new process as it will pass the token to the
root process.

## Problem 2
In this problem we need to implement parallel Dijkstra shortest path algorithm
(single source). I have used the approach mentioned in below PDF.
https://www8.cs.umu.se/kurser/5DV050/VT10/handouts/F10.pdf

```
inputGraph(InF) ->
```
Above function is responsible for reading the input.

```
takeEdges(InF, Edge_list, Source) ->
```
Above function is used for taking the input of edges, it also reads in the
source for Dijkstra algorithm. It returns the **Edge_list** dictionary and **Source**.

