%
% Erlang module to implement a simple echo operation
%

-module(echo).
-export([start/0, print/1, stop/0, echo_proc/0]).

start() ->
    PidA = spawn(?MODULE, echo_proc, []),
    register(echo, PidA).

stop() ->
    echo ! stop.

print(Term) ->
    echo ! {self(), Term}.

echo_proc() ->
    receive
        {_, Msg} -> Msg, echo_proc();
        stop -> true
    end.
