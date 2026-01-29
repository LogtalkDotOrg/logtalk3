.. _library_process:

``process``
===========

This library provides a portable abstraction over process handling
predicates found in some backend Prolog systems. Currently supports
ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog, Trealla Prolog, and
XVM.

API documentation
-----------------

Open the
`../../docs/library_index.html#process <../../docs/library_index.html#process>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(process(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(process(tester)).

Usage
-----

The ``process`` object provides the following predicates for portable
process handling:

- ``create(Executable, Arguments, Options)`` - Creates a new process
- ``wait(Process, Status)`` - Waits for a process to terminate
- ``kill(Process, Signal)`` - Terminates a process with a specific
  signal
- ``kill(Process)`` - Terminates a process using the default signal
  (SIGKILL)

The ``create/3`` predicate supports the following options:

- ``process(Pid)`` - Unifies Pid with the process identifier (an opaque
  ground term)
- ``stdin(Stream)`` - Binds Stream to the process standard input
- ``stdout(Stream)`` - Binds Stream to the process standard output
- ``stderr(Stream)`` - Binds Stream to the process standard error

Note: Process identifiers (PIDs) should be treated as opaque ground
terms. Their internal representation varies between backend Prolog
systems.

Example:

::

   | ?- process::create('/bin/echo', ['hello.'], [stdout(Out), process(PID)]),
        read_term(Out, Term, []),
        close(Out).
   Out = ..., PID = ..., Term = hello.
