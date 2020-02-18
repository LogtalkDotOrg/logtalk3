``profiler``
============

This tool contains simple wrappers for selected Prolog profiler tools.

Loading
-------

This tool can be loaded using the query:

::

   ?- logtalk_load(profiler(loader)).

For sample queries, please see the ``SCRIPT.txt`` file in the tool
directory.

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
invoked e.g. when you use the ``::/2`` message-sending operator at the
top-level interpreter) by compiling your code with the ``optimize`` flag
turned on:

::

   ?- set_logtalk_flag(optimize, on).
   true.

   ?- use_module(library(statistics)).
   true.

   ?- profile(... :: ...).
   ...

Given that ``prolog_statistics:profile/1`` is a meta-predicate, Logtalk
will compile its argument before calling it thanks to the
``goal_expansion/2`` hook predicate definitions in the adapter file.
Without this hook definition, you would need to use instead (to avoid
profiling the compiler itself):

::

   ?- logtalk << (prolog_statistics:profile(... :: ...)).
   ...

In either case, Don't forget, however, to load the ``prolog_statistics``
module *before* using or compiling calls to the ``profile/1`` to allow
the Logtalk compiler to access its meta-predicate template.

The profiler support attempts to conceal internal Logtalk
compiler/runtime predicates and the generated entity predicates that
implement predicate inheritance. Calls to internal compiler and runtime
predicates have functors starting with ``$lgt_``. Calls to predicates
with functors such as ``_def``, ``_dcl``, or ``_super``, used to
implement inheritance, may still be listed in a few cases. Note that the
time and the number of calls/redos of concealed predicates is added to
the caller predicates.

Compiling source code for profiling
-----------------------------------

In order to get user-level object and predicate names instead of
compiler generated internal names when using the SWI-Prolog and YAP
profilers, you must set ``code_prefix`` flag to a character other than
the default ``$`` before compiling your source code. For example:

::

   ?- set_logtalk_flag(code_prefix, '.').

See also the ``settings-sample.lgt`` file for automating the necessary
setup at Logtalk startup.

Other notes
-----------

All source files are indented using tabs (a common setting is a tab
width equivalent to 4 spaces).
