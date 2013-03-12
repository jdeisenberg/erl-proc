-module(erlproc).
-export([go/0, erlproc_init/0, erlproc_loop/1,
  sketch/1,
  sketch/2,
  redraw/0,
  line/1, line/4,
  rect/1, rect/4,
  triangle/1, triangle/6,
  quad/1, quad/8,
  ellipse/1, ellipse/4,
  fill/1, no_fill/0,
  stroke/1, no_stroke/0,
  background/1,
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
      io:format("Erlang gets Java's pid: ~p at ~p~n",
        [ProcessingPid, ProcessingNodeName]),
      link(ProcessingPid), % link to it...
      
      % and send it a message by name (which has my pid attached)
      {processing, ProcessingNodeName} ! {kabuki, message_has_my_pid},  
      
      % and wait for it to respond to me
      receive
        kabuki_complete ->
        io:format("Erlang reports kabuki complete~n")
      end,
     
      erlproc_loop(ProcessingPid);
      
    _X ->
      io:format("Erlang received unexpected message ~p~n", [_X])
  end.

%% Process commands destined for Java at ProcessingPid.
%% If I receive a tuple starting with {call...}, I just send the
%% command and any data it might have to Java; I don't need a reply.
%% If I receive a tuple starting with {request, RequesterPid...}, I
%% send the command to Java, and then wait for its response, which I
%% immediately forward to RequesterPid
%%
%% The kabuki_complete message is the last part of the handshaking
%% that exchanges Erlang and Processing's pids

erlproc_loop(ProcessingPid) ->
  receive
    {request, RequesterPid, Command} ->
      io:format("erlang: request ~p~n", [Command]),
      ProcessingPid ! {Command},
      receive
        Reply -> RequesterPid ! Reply
      end,
      erlproc_loop(ProcessingPid);
    {request, RequesterPid, Command, Data} ->
      io:format("erlang: request ~p ~p~n", [Command, Data]),
      ProcessingPid ! {Command, Data},
      receive
        Reply -> RequesterPid ! Reply
      end,
      erlproc_loop(ProcessingPid);
    {call, Command} ->
      io:format("erlang: call ~p~n", [Command]),
      ProcessingPid ! {Command},
      erlproc_loop(ProcessingPid);
    {call, Command, Data} ->
      io:format("erlang: call ~p ~p~n", [Command, Data]),
      ProcessingPid ! {Command, Data},
      erlproc_loop(ProcessingPid);
    {'EXIT', _, _} ->
      io:format("Java program exits. Ending erlang loop~n");
    X ->
      io:format("Erlang process loop receives message ~p~n", [X])
  end.
  
%% Convenience methods for setting      
sketch([W, H]) ->
  erlproc ! {call, sketch, [W, H]}.
sketch(W, H) ->
  erlproc ! {call, sketch, [W, H]}.

redraw() -> erlproc ! {call, redraw}.

line([X1, Y1, X2, Y2]) -> erlproc ! {call, line, [X1, Y1, X2, Y2]}.
line(X1, Y1, X2, Y2) -> erlproc ! {call, line, [X1, Y1, X2, Y2]}.

rect([X, Y, W, H]) ->  erlproc ! {call, rect, [X, Y, W, H]}.
rect(X, Y, W, H) ->  erlproc ! {call, rect, [X, Y, W, H]}.

triangle([X1, Y1, X2, Y2, X3, Y3]) ->
  erlproc ! {call, triangle, [X1, Y1, X2, Y2, X3, Y3]}.
triangle(X1, Y1, X2, Y2, X3, Y3) ->
  erlproc ! {call, triangle, [X1, Y1, X2, Y2, X3, Y3]}.

quad([X1, Y1, X2, Y2, X3, Y3, X4, Y4]) ->
  erlproc ! {call, quad, [X1, Y1, X2, Y2, X3, Y3, X4, Y4]}.
quad(X1, Y1, X2, Y2, X3, Y3, X4, Y4) ->
  erlproc ! {call, quad, [X1, Y1, X2, Y2, X3, Y3, X4, Y4]}.


ellipse([X, Y, W, H]) ->  erlproc ! {call, ellipse, [X, Y, W, H]}.
ellipse(X, Y, W, H) ->  erlproc ! {call, ellipse, [X, Y, W, H]}.
  
background(ColorList) -> erlproc ! {call, background, ColorList}.

fill(ColorList) -> erlproc ! {call, fill, ColorList}.
no_fill() -> erlproc ! {call, noFill}.

stroke(ColorList) -> erlproc ! {call, stroke, ColorList}.
no_stroke() -> erlproc ! {call, noStroke}.

 
mouse() ->
  erlproc ! {request, self(), mouse},
  receive
    {mouse, [X, Y]} -> [X, Y];
    _ -> [undefined, undefined]
  end.
  
pmouse() ->
  erlproc ! {request, self(), pmouse},
  receive
    {pmouse, [X, Y]} -> [X, Y];
    _ -> [undefined, undefined]
  end.
