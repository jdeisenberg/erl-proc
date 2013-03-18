%% @author J David Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Create graphics from Erlang using Processing's core functions.
%% @copyright 2013 by J David Eisenberg under Erlang Public License (same as Erlang)
%% @version 0.1

%% The system works on the basis of three processes:
%% 1) The hub process (this module), which communicates with the other
%%    two processes.
%% 2) The Java process. It gets a list of things to draw from the hub,
%%    and sends the drawing environment to the hub.
%% 3) The sketch process. It sends the hub things that it wants to draw,
%%    and awaits the hub's response. This is necessary to make sure
%%    that calls to the hub stay in order.

-module(erlproc).
-export([
  sketch/2,
  hub/2, hub_loop/5,
  redraw/0,
  no_loop/0,
  point/1,
  line/1,
  rect/1,
  ellipse/1,
  triangle/1,
  quad/1,
  arc/1,
  fill/1, no_fill/0,
  stroke/1, no_stroke/0,
  stroke_weight/1,
  background/1,
  smooth/0,
  mouse/0,
  pmouse/0,
  mouse_pressed/0,
  mouse_button/0,
  rect_mode/1,
  ellipse_mode/1
]).


%% @doc Start a sketch with the given dimensions and name of a module
%% that contains a setup() and draw() function.

-spec(sketch(atom(), [float()]) -> atom()).

sketch(ModuleName, Dimensions) ->
  %% Set up environment.
  
  %G = [{mouseX, 0}, {mouseY, 0}, {pmouseX, 0}, {pmouseY, 0},
  %  {mousePressed, false}, {mouseButton, none}, {width, 0},
  %  {height, 0}],

  %%If already running the hub process, do nothing, otherwise spawn it.
  case lists:member(hub, registered()) of
    true -> ok;
    false ->
      Pid = spawn_link(?MODULE, hub, [ModuleName, Dimensions]),
      register(hub, Pid)
      % io:format(standard_error, "erlproc registered as ~p~n", [Pid])
  end.

hub(Module, Dimensions) ->
  process_flag(trap_exit, true),
  %% Kill the Java process if it's running.
  case lists:member(java_process, registered()) of
    true ->
      % exit(whereis(erlproc_java), kill),
      port_close(java_process);
    false -> ok
  end,

  Port = open_port({spawn, "java -cp .:/usr/local/lib/erlang/lib/jinterface-1.5.8/priv/OtpErlang.jar:core.jar ErlProc"},
    [{line, 256}]),
  link(Port), % link to it
  register(java_process, Port), % and register it
	
  %% Although I could communicate between Erlang and Java via
  %% process name / node tuples, I felt it would be better to use
  %% Pids. Thus, the following handshaking kabuki to exchange
  %% Pids.

  %% First, I send the name of this node to Java via the port
  port_command(Port, list_to_binary(atom_to_list(node()) ++ "\n" )),
  
  %% Java will send me back its Pid and node name
  receive 
    {kabuki, JavaPid, _JavaNodeName} ->
      % io:format(standard_error, "Erlang gets Java's pid: ~p and name ~p~n",
      %  [JavaPid, _JavaNodeName]),
      % io:format(standard_error, "Sending message to Java~n", []),
      
      %% and send Java a message which has my pid, and the sketch dimensions
      JavaPid ! {kabuki, self(), Dimensions},  
      
      %% and wait for Java to respond to me
      receive
        kabuki_complete ->
          % io:format(standard_error, "Erlang reports kabuki complete~n", []),
     
          %% Start the hub's main loop
          hub_loop(Module, undefined, JavaPid, [], []) 
      end;
      
    _X ->
      io:format(standard_error,
        "Erlang received unexpected handshake message ~p~n", [_X])
  end.

hub_loop(SketchModule, SketchPid, JavaPid, Environment, DrawList) ->
  receive
    %% Java sends me its environment; I acknowledge.
    {environment, NewEnvironment} ->
      % io:format(standard_error, "Received environment ~p~n", [NewEnvironment]),
      JavaPid ! ok,
      hub_loop(SketchModule, SketchPid, JavaPid, NewEnvironment, DrawList);
    
    setup ->
      % io:format(standard_error, "From java: setup/0 on ~p~n", [SketchModule]),
      SetupFcnPid = spawn_link(SketchModule, setup, []),
      % io:format(standard_error, "Spawned setup at PID ~p~n", [SetupFcnPid]),
      hub_loop(SketchModule, SetupFcnPid, JavaPid, Environment, DrawList);
    
    draw ->
      % io:format(standard_error, "From java: draw/0~n", []),
      DrawFcnPid = spawn_link(SketchModule, draw, []),
      % io:format(standard_error, "Spawned draw at pid ~p~n", [DrawFcnPid]),
      hub_loop(SketchModule, DrawFcnPid, JavaPid, Environment, DrawList);
    
    {draw_cmd, Command}  when SketchPid /= undefined ->
      % io:format(standard_error, "Add ~p to draw list~n", [Command]),
      NewDrawList = [{Command, []} | DrawList],
      SketchPid ! ok,
      hub_loop(SketchModule, SketchPid, JavaPid, Environment, NewDrawList);

    {draw_cmd, Command, Data} when SketchPid /= undefined ->
      % io:format(standard_error, "Add ~p ~p to draw list~n", [Command, Data]),
      NewDrawList = [{Command, Data} | DrawList],
      SketchPid ! ok,
      hub_loop(SketchModule, SketchPid, JavaPid, Environment, NewDrawList);
    
    {draw_request, mouse} ->
      {_Mx, X} = lists:keyfind(mouseX, 1, Environment),
      {_My, Y} = lists:keyfind(mouseY, 1, Environment),
      SketchPid ! [X, Y],
      hub_loop(SketchModule, SketchPid, JavaPid, Environment, DrawList);

    {draw_request, pmouse} ->
      {_Px, X} = lists:keyfind(pmouseX, 1, Environment),
      {_Py, Y} = lists:keyfind(pmouseY, 1, Environment),
      SketchPid ! [X, Y],
      hub_loop(SketchModule, SketchPid, JavaPid, Environment, DrawList);

    {draw_request, mouse_pressed} ->
      {_K, Status} = lists:keyfind(mousePressed, 1, Environment),
      SketchPid ! Status,
      hub_loop(SketchModule, SketchPid, JavaPid, Environment, DrawList);

    {draw_request, mouse_button} ->
      {_K, Button} = lists:keyfind(mouseButton, 1, Environment),
      SketchPid ! Button,
      hub_loop(SketchModule, SketchPid, JavaPid, Environment, DrawList);

    {'EXIT', SketchPid, normal} ->
      % io:format(standard_error, "Sketch function ~p exits normally.~n",
      % [SketchPid]),
      JavaPid ! {execute_commands, DrawList},
      hub_loop(SketchModule, undefined, JavaPid, Environment, []);
    
    {'EXIT', JavaPid, _} ->
      io:format(standard_error, "Java program exits. Ending erlang loop~n", []);

    X ->
      io:format(standard_error, "Hub loop gets unknown message ~p~n", [X]),
      exit(JavaPid, kill)
  end.

await() ->
  receive
    _X -> % io:format(standard_error, "Acknowledgment: ~p~n", [_X]),
      _X
  end.
  
%% These are drawing functions that will be called from the sketch.
%% Each one tells the hub to add a command to the draw list,
%% and then waits for a reply from the hub before continuing.

redraw() ->
  hub ! {draw_cmd, redraw},
  await().
  
no_loop() ->
  hub ! {draw_cmd, noLoop},
  await().

point(Coords) when is_list(Coords) ->
  hub ! {draw_cmd, point, Coords},
  await().

line(Coords) when is_list(Coords) ->
  hub ! {draw_cmd, line, Coords},
  await().

rect(Coords) when is_list(Coords) ->
  hub ! {draw_cmd, rect, Coords},
  await().

rect_mode(Mode) when is_atom(Mode) ->
  hub ! {draw_cmd, rect_mode, Mode},
  await().

triangle(Coords) when is_list(Coords) ->
  hub ! {draw_cmd, triangle, Coords},
  await().

quad(Coords) when is_list(Coords) ->
  hub ! {draw_cmd, quad, Coords},
  await().

arc(Coords) when is_list(Coords) ->
  hub ! {draw_cmd, arc, Coords},
  await().

ellipse(Coords) when is_list(Coords) -> 
  hub ! {draw_cmd, ellipse, Coords},
  await().
  
ellipse_mode(Mode) when is_atom(Mode) ->
  hub ! {draw_cmd, ellipse_mode, Mode},
  await().

background(ColorList) when is_list(ColorList) ->
  hub ! {draw_cmd, background, ColorList},
  await().

smooth() ->
  hub ! {draw_cmd, smooth},
  await().

fill(ColorList) when is_list(ColorList) ->
  hub ! {draw_cmd, fill, ColorList},
  await().

no_fill() ->
  hub ! {draw_cmd, noFill},
  await().

stroke(ColorList) when is_list(ColorList) ->
  hub ! {draw_cmd, stroke, ColorList},
  await().

no_stroke() ->
  hub ! {draw_cmd, noStroke},
  await().

stroke_weight(W) ->
  hub ! {draw_cmd, strokeWeight, [W]},
  await().

mouse() ->
  hub ! {draw_request, mouse},
  receive
    [X, Y] -> [X, Y];
    _Other -> [0.0, 0.0]
  end.

pmouse() ->
  hub ! {draw_request, pmouse},
  receive
    [X, Y] -> [X, Y];
    _Other -> [0.0, 0.0]
  end.

mouse_pressed() ->
  hub ! {draw_request, mouse_pressed},
  receive
    Status -> Status
  end.

mouse_button() ->
  hub ! {draw_request, mouse_button},
  receive
    Button -> Button
  end.

