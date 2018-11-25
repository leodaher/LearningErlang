%
% Erlang module to implement a process ring.
%

-module(procring).
-export([start/3, create_processes/2, rcv_loop/0, send_messages/3]).

% Main function
start(M, N, Message) ->
    % Create list of pids and bound it to the variable List
    List = create_processes([],N),

    % Add the main process at last
    ProcList = lists:append(List, [self()]),

    % Start sending messages
    send_messages(M, ProcList, Message).

% Tail-recursive function to create list of pids
create_processes(Pids, 1) ->
    Pids;
create_processes(Pids, N) ->
    create_processes([spawn(?MODULE, rcv_loop, []) | Pids], N-1).

% Tail-recursive function for the main process to send messages
send_messages(0, [Next|Pids], _) ->
    io:format("~w (~w) -> ~w~n",[self(), quit, Next]),
    % No messages left, send quit
    Next ! {quit, Pids},
    % Receive quit and exit
    receive
        {quit, _} -> true
    end;
send_messages(M, [Next|Pids], Message) ->
    % Print message was sent
    io:format("~w (~w) -> ~w~n",[self(), Message, Next]),
    % Send message to next process
    Next ! {Message, Pids},
    receive
        % Receive message from last process
        {Message, _} -> send_messages(M-1, [Next | Pids], Message)
    end.

% Loop for the receiving processes
rcv_loop() ->
    receive
        % Receive message and send to next process
        {Message, [Next|Pids]} -> io:format("~w (~w) -> ~w~n",[self(), Message, Next]),
                                  Next ! {Message, Pids},
                                  rcv_loop()
    end.
