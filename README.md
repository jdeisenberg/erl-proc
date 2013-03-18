erl-proc
========

Create graphics from Erlang by calling Processing's `core.jar`
([Processing](http://processing.org/) "is an open source programming language and environment...to create images...and to teach fundamentals of computer programming within a visual context.")

To use the `erlproc` module, you have to be running `erl` with a node name:

    erl -sname some_node_name

You write your graphics code in a module that contains `setup/0` and
`draw/0` functions. Here is a short example:

    -module(example).
    -export([setup/0, draw/0]).
    
    setup() ->
      background([255]).
    
    draw() ->
      rect([100, 100, 50, 50]),
      fill([255, 0, 0]),
      ellipse([125, 125, 30, 40]),
      no_loop().  % this prevents redrawing 60 times per second

To start the sketch, call the `erlproc:sketch/2` with the module name
and the sketch size:

    erlproc:sketch(example, [300, 300]).

There are two sample modules with this code. The first one draws a
simple house and sun; the second draws a circle that follows the mouse.

    erlproc:sketch(house, [300, 300]).
    erlproc:sketch(mouse_movement, [200, 200]).
