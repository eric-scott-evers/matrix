-module(matrix).
-compile([export_all]).

start() ->
  A = [ [0.0,   0.2,   0.3,   0.5],
        [0.2,   0.0,   0.3,   0.5],
        [0.3,   0.2,   0.0,   0.5],
        [0.2,   0.3,   0.5,   0.0]],
  % B = transpose(A),
  C = make_square(lists:seq(1,40000)),
  D = transpose(C),
  Row_Items = [{X,Y} || X<-C, Y<-D],
  F = fun(Z) -> dot(Z) end,
  Output = pmap(F, Row_Items),
  X = make_square(Output),
  % C = [[1,2,3],[4,5,6],[7,8,9]],
  mPow(A,10),
  Z = mMul(C, C),
  Pure = remove_ok_list(X),
  [{mMul,Z}, {pmap,X}, {pure,Pure}].

mMul(M1,M2) ->
  A = M1,
  B = transpose(M2),
  Row_Items = [{X,Y} || X<-A, Y<-B],
  F = fun(Z) -> dot(Z) end,
  Output = pmap(F,Row_Items),
  Square = make_square(Output),
  Pure = remove_ok_list(Square),
  Pure.

mPow(M,1) ->
  M;
mPow(M,N) ->
  A = M,
  B = transpose(A),
  Row_Items = [{X,Y} || X<-A, Y<-B],
  F = fun(Z) -> dot(Z) end,
  Output = lists:map(F,Row_Items),
  Out = make_square(Output),
  mPow(Out,N-1).

% dot product of two vectors

dot({L1,L2}) ->
  dot(0,{L1,L2}).

dot(Sum,{[],[]}) -> Sum;
dot(Sum,{[H1|T1],[H2|T2]})->
  NSum = Sum + H1*H2,
  dot(NSum,{T1,T2}).

% A is a new row [a11,a21]
% temp is leftover rows [[a12],[a22]]
% X is matrix input

transpose(X) ->
  transpose(X, [], [], []).

transpose( [Row|T], A, Temp, Output)
  when length(Row) > 0 ->
  [H|Rest] = Row,
  transpose(T, A ++ [H], Temp ++ [Rest], Output);

transpose( [], [], [], Output) ->
  Output;

transpose( [], A, Temp, Output) ->
  Len = length(lists:flatten(Temp)),
  % matches when Temp = [[],[],[]] etc
  if Len == 0 ->
    NTemp = [];
  true ->
    NTemp = Temp
  end,
  transpose( NTemp, [], [], Output ++ [A]).

make_square(X) ->
  Side = round(math:sqrt(length(X))),
  make_square(Side, X, [], []).

make_square(_Side, [], [], Output) ->
  Output;

make_square(Side, X, Row, Output)
  when length(Row) < Side ->
  % rpc:call(node(), io, fwrite,["~w ~n ", [{x, X, row, Row, output, Output}]]),
  [H|T] = X,
  make_square(Side, T, Row ++ [H], Output);

make_square(Side, X, Row, Output)
  when length(Row) == Side ->
  make_square(Side, X, [], Output ++ [Row]).

% ===========================

pmap(F, Es) ->
   Parent = self(),
   Running = [
     spawn_monitor(fun() -> Parent ! {self(), F(E)} end)
       || E <- Es],
   collect(Running, 5000).

% The idea here is we spawn each work unit as a function,
%  and attach a monitor to the spawned function.
%  Once the computation is done, we send the result
%  back to the invoker of pmap/2 together with the pid()
%  of the spawned function. This monitor/pid acts
%  like a future[0] which we can use to later collect
%  the running processes:

collect([], _Timeout) -> [];
collect([{Pid, MRef} | Next], Timeout) ->
  receive
    {Pid, Res} ->
      erlang:demonitor(MRef, [flush]),
      [{ok, Res} | collect(Next, Timeout)];
    {'DOWN', MRef, process, Pid, Reason} ->
      [{error, Reason} | collect(Next, Timeout)]
  after Timeout ->
    exit(pmap_timeout)
  end.

% fuction removes ok added by pmap
% example:
%   input  [[{ok,4},{ok,2}],[{ok,1},{ok,8}]]
%   output [[4,2],[1,8]]
%
remove_ok_list(L) ->
  remove_ok_list(L, []).

% remove ok from list of rows
remove_ok_list([], Output) -> Output;
remove_ok_list([Row|T], A) ->
  Pure_row = remove_ok_row(Row, []),
  remove_ok_list(T, A++[Pure_row]).

% remove ok from row
remove_ok_row([], Output) -> Output;
remove_ok_row([H|T], Output) ->
  {ok,Number} = H,
  remove_ok_row(T, Output++[Number]).
