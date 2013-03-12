erl-proc
========

Drive Processing's core.jar from Erlang

To make this work, you have to be running `erl` with a node name:

    erl -sname example

Before you run a demo, you have to establish communication with a Java program that
has Processing's `core.jar`:

    > erlproc:go().

Then you can issue commands to Processing from `erl` , like:

    erlproc:sketch([300, 300]).
    [Mx, My] = erlproc:mouse().
    erlproc:fill([255, 0, 0]).
    erlproc:rect([Mx, My, 30, 30]).
    erlproc:redraw().

That last command is necessary to display what you've drawn.

At the moment, if you do the preceding commands from a module, the redraw will
show the circle, and then immediately erase the window. I'm working on that.

