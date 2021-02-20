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

```
statusUpdate(List, D, U, Status) ->
```
Above function helps in updating the **Status** dictionary which stores the distance
and **visited/unvisited** information for each vertex assigned to a process. This is
the main step of Dijkstra algorithm, distance of a vertex is updated if the
current distance is greater than distance of the vertex responsible for updating
the **Status** *plus* the weight of the edge connecting both these vertices.

```
splitVertices(VertexDict, L, N, Curr, Processes) ->
```
Above function splits the vertices equally among all the processes. It takes the
first **N** elements of the list **L** and assigns to the current process. If
the current process happens to be the last one than all the elements of the list
**L** is assigned to it.

```
createProcesses(Num_Id, Curr, Processes) ->
```
It spawns **Processes** number of processes and stores their **PID** in
**Num_Id** dictionary. Each newly spawned process executes *eachProcess* function.

```
eachProcess() ->
```
This function receives the initial **Status** dictionary and other useful
information like **Edge_list** and **Root_pid**. Finally it invokes the
recursive function **runProcess**.

```
runProcess(Edge_list, Status, Root_pid) ->
```
Above function is executed by every other process except the root.
1. First finds the unvisited vertices assigned to it.
2. Finds the minimum among them w.r.t distance of each vertex.
3. Sends the minimum vertex found to the root.
4. Receives the global minimum vertex and it's distance from the root.
5. Update it's own status according to the global minimum received.
6. Updates the status of **U** which is the global minimum vertex to **visited**
   from **unvisited** if it is assigned to this process.
7. If the global minimum was **infinity** this means the algorithm is complete
   and the process sends it **Status** dictionary to root process.


```
rootProcess(Status, Edge_list, Processes, Num_Id) ->
```
Above function is executed by the root process.
1. First finds the unvisited vertices assigned to it.
2. Finds the minimum among them w.r.t. distance of each vertex.
3. Receives the minimum from other processes as well.
4. Finds the global minimum using it's own minimum and minimums that it received
   from other processes as well.
5. Broadcasts the global minimum to other processes.
6. Updates it's own status and status of **U** which is the global minimum
   vertex to **visited** if it is assigned to root process.
7. If the global minimum was **infinity** this means the algorithm is complete
   and root process collects the **Status** from all other processes.




