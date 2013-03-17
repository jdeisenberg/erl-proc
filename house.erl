-module(house).
-export([setup/0, draw/0]).

setup() ->
  % io:format(standard_error, "Begin setup in module house~n", []),
  erlproc:background([255, 255, 255]).

draw() ->
  % io:format(standard_error, "Begin draw in module house~n",[]),
  erlproc:background([255, 192, 255]),
  erlproc:rect([100, 100, 100, 100]),
  erlproc:fill([0, 128, 0]),
  erlproc:triangle([100, 100, 150, 50, 200, 100]),
  erlproc:fill([255, 255, 0]),
  erlproc:no_stroke(),
  erlproc:ellipse([60, 60, 50, 50]),
  erlproc:no_loop(),
  % io:format(standard_error, "End house~n",[]),
  ok.
  
