-module(house).
-export([draw_house/0]).

% You have to type this into the shell first:
% erlproc:go().
% and wait for it to return to you. I'll fix that later.

draw_house() ->
  erlproc:sketch(300, 300),
  erlproc:background([255, 192, 255]),
  erlproc:rect([100, 100, 100, 100]),
  erlproc:fill([0, 128, 0]),
  erlproc:triangle([100, 100, 150, 50, 200, 100]),
  erlproc:fill([255, 255, 0]),
  erlproc:no_stroke(),
  erlproc:ellipse([60, 60, 50, 50]),
  erlproc:redraw(),
  erlproc:noloop(),
  ok.
  
