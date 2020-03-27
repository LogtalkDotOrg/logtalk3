..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _errors_errors:

Error handling
==============

Error handling is accomplished in Logtalk by using the standard ``catch/3``
and ``throw/1`` predicates [ISO95]_ together with a set of built-in methods
that simplify generating errors decorated with expected context.

Errors thrown by Logtalk have, whenever possible, the following format:

::

   error(Error, logtalk(Goal, ExecutionContext))

In this exception term, ``Goal`` is the goal that triggered the error
``Error`` and ``ExecutionContext`` is the context in which ``Goal`` is
called. For example:

::

   error(
       permission_error(modify,private_predicate,p),
       logtalk(foo::abolish(p/0), _)
   )

Note, however, that ``Goal`` and ``ExecutionContext`` can be unbound or only
partially instantiated when the corresponding information is not available
(e.g. due to compiler optimizations that throw away the necessary error context
information). The ``ExecutionContext`` argument is an opaque term that
can be decoded using the
:ref:`logtalk::execution_context/7 <logtalk/0::execution_context/7>` predicate.

Raising Exceptions
------------------

The :ref:`error handling section <error_handling_methods>` in the reference
manual lists a set of convenient built-in methods that generate ``error/2``
exception terms with the expected context argument. For example, instead of
manually constructing a type error as in:

::

   ...,
   context(Context),
   throw(error(type_error(atom, 42), Context)).

we can simply write:

::

   ...,
   type_error(atom, 42).

The provided error built-in methods cover all standard error types found in
the ISO Prolog Core standard.

Type-checking
-------------

One of the most common case where errors may be generated is when
type-checking predicate arguments and input data before processing it.
The standard library includes a :ref:`type <apis:type/0>` object that 
defines an extensive set of types, together with predicates for validating
and checking terms. The set of types is user extensible and new types can
be defined by adding clauses for the ``type/1`` and ``check/2`` multifile
predicates. For example, assume that we want to be able to check
*temperatures* expressed in Celsius, Fahrenheit, or Kelvin scales. We
start by declaring (in an object or category) the new type:

::

   :- multifile(type::type/1).
   type::type(temperature(_Unit)).

Next, we need to define the actual code that would verify that a temperature
is valid. As the different scales use a different value for absolute zero,
we can write:

::

   :- multifile(type::check/2).
   type::check(temperature(Unit), Term) :-
       check_temperature(Unit, Term).

   % given that temperature has only a lower bound, we make use of the library
   % property/2 type to define the necessary test expression for each unit
   check_temperature(celsius, Term) :-
       type::check(property(float, [Temperature]>>(Temperature >= -273.15)), Term).
   check_temperature(fahrenheit, Term) :-
       type::check(property(float, [Temperature]>>(Temperature >= -459.67)), Term).
   check_temperature(kelvin, Term) :-
       type::check(property(float, [Temperature]>>(Temperature >= 0.0)), Term).

With this definition, a term is first checked that it is a float value before
checking that it is in the expected open interval. But how do we use this new
type? If we want just to test if a temperature is valid, we can write:

::

   ..., type::valid(temperature(celsius), 42.0), ...

The :ref:`type::valid/2 <apis:type/0::valid/2>` predicate succeeds or fails
depending on the second argument being of the type specified in the first
argument. If instead of success or failure we want to generate an error for
invalid values, we can use the :ref:`type::check/2 <apis:type/0::check/2>`
predicate instead:

::

   ..., type::check(temperature(celsius), 42.0), ...

If we require an ``error/2`` exception term with the error context, we can
use instead the :ref:`type::check/3 <apis:type/0::check/3>` predicate:

::

   ...,
   context(Context),
   type::check(temperature(celsius), 42.0, Context),
   ...

Note that ``context/1`` calls are inlined and messages to the library
``type`` object use :term:`static binding` when compiling with the
:ref:`optimize flag <flag_optimize>` turned on, thus enabling efficient
type-checking.

Expected terms
--------------

Support for representing and handling *expected terms* is provided by the
:doc:`../libraries/expecteds` library. Expected terms allows defering errors
to later stages of an application in alternative to raising an exception as
soon as an error is detected.

Compiler warnings and errors
----------------------------

The current Logtalk compiler uses the standard ``read_term/3`` built-in
predicate to read and compile a Logtalk source file. This improves the
compatibility with :term:`backend Prolog compilers <backend Prolog compiler>`
and their proprietary syntax extensions and standard compliance quirks. But one
consequence of this design choice is that invalid Prolog terms or syntax errors
may abort the compilation process with limited information given to the user
(due to the inherent limitations of the ``read_term/3`` predicate).

Assuming that all the terms in a source file are valid, there is a set of
errors and potential errors, described below, that the compiler will try
to detect and report, depending on the used compiler flags (see the
:ref:`programming_flags` section of this manual on lint flags for details).

.. _errors_unknown:

Unknown entities
~~~~~~~~~~~~~~~~

The Logtalk compiler warns about any referenced entity that is not
currently loaded. The warning may reveal a misspell entity name or just
an entity that it will be loaded later. Out-of-oder loading should be
avoided when possible as it prevents some code optimizations such as
:term:`static binding` of messages to methods.

.. _errors_singletons:

Singleton variables
~~~~~~~~~~~~~~~~~~~

Singleton variables in a clause are often misspell variables and, as
such, one of the most common errors when programming in Prolog.
Assuming that the :term:`backend Prolog compiler` implementation of the
``read_term/3`` predicate supports the standard ``singletons/1``
option, the compiler warns about any singleton variable found while
compiling a source file.

.. _errors_prolog:

Redefinition of Prolog built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Logtalk compiler will warn us of any redefinition of a Prolog
built-in predicate inside an object or category. Sometimes the redefinition
is intended. In other cases, the user may not be aware that a particular
:term:`backend Prolog compiler` may already provide the predicate
as a built-in predicate or may want to ensure code portability among
several Prolog compilers with different sets of built-in predicates.

.. _errors_redefinition_predicates:

Redefinition of Logtalk built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similar to the redefinition of Prolog built-in predicates, the Logtalk
compiler will warn us if we try to redefine a Logtalk built-in. But the
redefinition will probably be an error in most (if not all) cases.

.. _errors_redefinition_methods:

Redefinition of Logtalk built-in methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An error will be thrown if we attempt to redefine a Logtalk built-in
method inside an entity. The default behavior is to report the error and
abort the compilation of the offending entity.

.. _errors_misspell:

Misspell calls of local predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A warning will be reported if Logtalk finds (in the body of a predicate
definition) a call to a local predicate that is not defined, built-in
(either in Prolog or in Logtalk) or declared dynamic. In most cases
these calls are simple misspell errors.

.. _errors_portability:

Portability warnings
~~~~~~~~~~~~~~~~~~~~

A warning will be reported if a predicate clause contains a call to a
non-standard built-in predicate or arithmetic function, Portability
warnings are also reported for non-standard flags or flag values. These
warnings often cannot be avoided due to the limited scope of the ISO
Prolog standard.

.. _errors_deprecated:

Deprecated elements
~~~~~~~~~~~~~~~~~~~

A warning will be reported if a deprecated directive, control construct,
or predicate is used. These warnings should be fixed as soon as possible
as support for any deprecated features will likely be discontinued in
future versions.

.. _errors_missing_directives:

Missing directives
~~~~~~~~~~~~~~~~~~

A warning will be reported for any missing dynamic, discontiguous,
meta-predicate, and public predicate directive.

.. _errors_duplicated_directives:

Duplicated directives
~~~~~~~~~~~~~~~~~~~~~

A warning will be reported for any duplicated scope, multifile, dynamic,
discontiguous, meta-predicate, and meta-non-terminal directives. Note
that conflicting directives for the same predicate are handled as
errors, not as duplicated directive warnings.

.. _errors_duplicated_clauses:

Duplicated clauses
~~~~~~~~~~~~~~~~~~

A warning will be reported for any duplicated entity clauses. This check
is computationally heavy, however, and usually turned off by default.

.. _errors_always_true_or_false_goals:

Goals that are always true or false
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A warning will be reported for any goal that is always true or false.
This is usually caused by typos in the code. For example, writing
``X == y`` instead of ``X == Y``.

.. _errors_trivial_fails:

Trivial fails
~~~~~~~~~~~~~

A warning will be reported for any call to a local static predicate with
no matching clause.

.. _errors_suspicious_calls:

Suspicious calls
~~~~~~~~~~~~~~~~

A warning will be reported for calls that are syntactically correct but most
likely a semantic error. An example is :ref:`control_send_to_self_1` calls in
clauses that apparently are meant to implement recursive predicate definitions
where the user intention is to call the local predicate definition.

.. _errors_lambda_variables:

Lambda variables
~~~~~~~~~~~~~~~~

A warning will be reported for :term:`lambda expressions <lambda expression>`
with unclassified variables (not listed as either :term:`lambda free <lambda free variable>`
or :term:`lambda parameter` variables), for variables playing a dual role
(as both lambda free and lambda parameter variables), and for lambda parameters
used elsewhere in a clause.

.. _errors_predicate_redefinition:

Redefinition of predicates declared in ``uses/2`` or ``use_module/2`` directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A error will be reported for any attempt to define locally a predicate
that is already declared in an :ref:`directives_uses_2` or
:ref:`directives_use_module_2` directive.

.. _errors_others:

Other warnings and errors
~~~~~~~~~~~~~~~~~~~~~~~~~

The Logtalk compiler will throw an error if it finds a predicate clause
or a directive that cannot be parsed. The default behavior is to report
the error and abort the compilation.

.. _errors_runtime:

Runtime errors
--------------

This section briefly describes runtime errors that result from misuse of
Logtalk built-in predicates, built-in methods or from message sending.
For a complete and detailed description of runtime errors please consult
the Reference Manual.

.. _errors_predicates:

Logtalk built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most Logtalk built-in predicates checks the type and mode of the calling
arguments, throwing an exception in case of misuse.

.. _errors_methods:

Logtalk built-in methods
~~~~~~~~~~~~~~~~~~~~~~~~

Most Logtalk built-in method checks the type and mode of the calling
arguments, throwing an exception in case of misuse.

.. _errors_sending:

Message sending
~~~~~~~~~~~~~~~

The message sending mechanisms always check if the receiver of a message
is a defined object and if the message corresponds to a declared
predicate within the scope of the sender. The built-in protocol
:ref:`forwarding <apis:forwarding/0>` declares a predicate,
:ref:`methods_forward_1`, which is automatically called (if defined) by
the runtime for any message that the receiving object does not understand.
The usual definition for this error handler is to delegate or forward the
message to another object that might be able to answer it:

::

   forward(Message) :-
       % forward the message while preserving the sender
       [Object::Message].

If preserving the original sender is not required, this definition can
be simplified to:

::

   forward(Message) :-
       Object::Message.

More sophisticated definitions are, of course, possible.
