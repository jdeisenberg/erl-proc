-module(modes).
-export([setup/0, draw/0]).

setup() ->
  erlproc:background([255]),
  erlproc:smooth().

draw() ->
  erlproc:fill([255, 0, 0]),
  erlproc:rect_mode(corner),
  erlproc:rect([50, 50, 30, 30]),
  erlproc:rect_mode(corners),
  erlproc:rect([100, 50, 130, 80]),
  erlproc:rect_mode(center),
  erlproc:rect([165, 65, 30, 30]),
  erlproc:rect_mode(radius),
  erlproc:rect([215, 65, 15, 15]),

  erlproc:fill([0, 128, 128]),
  erlproc:ellipse_mode(corner),
  erlproc:ellipse([50, 150, 30, 30]),
  erlproc:ellipse_mode(corners),
  erlproc:ellipse([100, 150, 130, 180]),
  erlproc:ellipse_mode(center),
  erlproc:ellipse([165, 165, 30, 30]),
  erlproc:ellipse_mode(radius),
  erlproc:ellipse([215, 165, 15, 15]),
  erlproc:no_loop(),
  ok.
  
