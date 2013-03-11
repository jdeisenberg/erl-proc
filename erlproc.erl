-module(erlproc).
-export([go/0, erlproc_init/0, erlproc_loop/1, sketch/2,
  draw/0,
  rect/4,
  mouse/0,
  pmouse/0
]).

go() ->
  spawn(?MODULE, erlproc_init, []).
  % This code is when we want to see crashes
  % Pid = spawn(?MODULE, erlproc_init, []),
  % link(Pid).
  
erlproc_init() ->
  case lists:member(erlproc, registered()) of
    true -> true;
    false -> register(erlproc, self())  % this is my mailbox name.
  end,
  process_flag(trap_exit, true),
  Port = open_port({spawn, "java -cp .:/usr/lib/erlang/lib/jinterface-1.5.6/priv/OtpErlang.jar:core.jar ErlProc"},
    [{line, 256}]),
	
  % Although I could communicate between Erlang and Java via
  % process name / node tuples, I felt it would be better to use
  % Pids. Thus, the following handshaking kabuki to exchange
  % Pids.

  % First, I send the name of this node to Java via the port
  port_command(Port, list_to_binary(atom_to_list(node()) ++ "\n" )),
  
  % It's going to send me back its Pid and node name
  receive 
    {kabuki, ProcessingPid, ProcessingNodeName} ->
      io:format("Got Java external pid ~p at ~p~n",
        [ProcessingPid, ProcessingNodeName]),
      link(ProcessingPid), % link to it...
      
      % and send it a message by name (which has my pid attached)
      {processing, ProcessingNodeName} ! {kabuki, message_has_my_pid},    
      
      erlproc_loop(ProcessingPid);
      
    _X ->
      io:format("Received unexpected message ~p~n", [_X])
  end.

erlproc_loop(ProcessingPid) ->
  receive
    {call, Command} ->
      ProcessingPid ! {Command},
      erlproc_loop(ProcessingPid);
    {call, Command, Data} ->
      io:format("erlproc_loop got ~p Data|~p|~n", [Command, Data]),
      ProcessingPid ! {Command, Data},
      erlproc_loop(ProcessingPid);
    kabuki_complete ->
      io:format("Handshaking with Java is complete~n"),
      erlproc_loop(ProcessingPid);
    {'EXIT', _, _} ->
      io:format("Java program exits. Ending erlang loop~n");
    X ->
      io:format("Erlang process loop receives message ~p~n", [X])
  end.
  
      
sketch(W, H) ->
  erlproc ! {call, sketch, [W, H]}.

draw() -> erlproc ! {call, redraw}.

rect(X, Y, W, H) ->
  erlproc ! {call, rect, [X, Y, W, H]}.

mouse() ->
  erlproc ! {call, mouse},
  receive
    {mouse, [X, Y]} -> [X, Y];
    _ -> [undefined, undefined]
  end.
  
pmouse() ->
  erlproc ! {call, pmouse},
  receive
    {pmouse, [X, Y]} -> [X, Y];
    _ -> [undefined, undefined]
  end.
