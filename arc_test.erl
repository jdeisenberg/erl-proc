-module(arc_test).
-export([setup/0, draw/0]).

setup() ->
  erlproc:background([255, 255, 0]),
  erlproc:smooth().

draw() ->
  erlproc:stroke_weight(2),
  erlproc:fill([255]),
  erlproc:rect([10, 10, 200, 100]),
  erlproc:stroke_weight(1),
  erlproc:fill([192, 255, 255]),
  erlproc:arc([50, 50, 40, 40, 0, math:pi() / 3]),
  erlproc:arc([100, 50, 40, 40, 0, math:pi() / 2, chord]),
  erlproc:arc([150, 50, 40, 40, math:pi() / 4, math:pi() / 2, pie]),
  erlproc:no_loop().
  
