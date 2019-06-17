``profiler``
============

This tool contains simple wrappers for selected Prolog profiler tools.

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(profiler(loader)).

For sample queries, please see the `SCRIPT.txt <SCRIPT.txt>`__ file in
the tool directory.

Supported backend Prolog compilers
----------------------------------

Currently, this tool supports the profilers provided with SICStus Prolog
4, SWI-Prolog, and YAP. The tool includes two files:

-  | ``yap_profiler.lgt``
   | simple wrapper for the YAP count profiler

-  | ``sicstus_profiler.lgt``
   | simple wrapper for the SICStus Prolog 4 profiler

Logtalk also supports the YAP tick profiler (using the latest YAP
development version) and the SWI-Prolog XPCE profiler. When using the
XPCE profiler, you can avoid profiling the Logtalk compiler (which is
invoked e.g. when you use the ``(::)/2`` message-sending operator at the
top-level) by typing:

::

   | ?- logtalk << (prolog_statistics:profile(... :: ...)).

Given that ``prolog_statistics:profile/1`` is a meta-predicate, Logtalk
will compile its argument before calling it. Don't forget, however, to
load the ``prolog_statistics`` module *before* using or compiling calls
to the ``profile/1`` predicate by typing:

::

   | ?- use_module(library(statistics)).

The profiler support makes no attempt to conceal the internal Logtalk
compiler/runtime predicates or the generated predicates that implement
predicate inheritance. Calls to internal compiler and runtime predicates
have functors starting with ``$lgt_``. Calls to predicates with functors
such as ``_def``, ``_dcl``, or ``_super``, used to implement
inheritance, may be listed when your code uses dynamic binding.

Compiling source code for profiling
-----------------------------------

In order to get user-level object and predicate names instead of
compiler generated names when using the SWI-Prolog and YAP profilers you
must set ``code_prefix`` flag to a value other than the default ``$``.
For example:

::

   | ?- set_logtalk_flag(code_prefix, '.').

See also the ``settings-sample`` file for automating the necessary setup
at Logtalk startup.

Other notes
-----------

All source files are formatted using tabs (the recommended setting is a
tab width equivalent to 4 spaces).
