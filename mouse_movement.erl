-module(mouse_movement).
-export([setup/0, draw/0]).

setup() ->
  erlproc:background([255, 0, 255]).

draw() ->
  erlproc:background([255]),
  [X, Y] = erlproc:mouse(),
  erlproc:fill([X, (X + Y) / 2, Y]),
  erlproc:no_stroke(),
  erlproc:ellipse([X, Y, 50, 50]),
  ok.
  
