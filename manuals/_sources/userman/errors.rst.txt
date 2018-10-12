
.. _errors_errors:

==============
Error handling
==============

All error handling is done in Logtalk by using the ISO defined
``catch/3`` and ``throw/1`` predicates [ISO95]_.
Errors thrown by Logtalk have the following format:

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
can be decoded using the ``logtalk::execution_context/7`` predicate.

Compiler warnings and errors
----------------------------

The current Logtalk compiler uses the ``read_term/3`` ISO Prolog defined
built-in predicate to read and compile a Logtalk source file. One
consequence of this is that invalid Prolog terms or syntax errors may
abort the compilation process with limited information given to the user
(due to the inherent limitations of the ``read_term/3`` predicate).

If all the terms in a source file are valid, then there is a set of
errors or potential errors, described below, that the compiler will try
to detect and report, depending on the used compiler flags (see the
:ref:`programming_flags` section of this manual for details).

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
such, one of the most common errors when programming in Prolog. When the
backend Prolog compiler complies with the Prolog ISO standard or at
least supports the ISO predicate ``read_term/3`` called with the option
``singletons(S)``, the Logtalk compiler warns about any singleton
variable found while compiling a source file.

.. _errors_prolog:

Redefinition of Prolog built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Logtalk compiler will warn us of any redefinition of a Prolog
built-in predicate inside an object or category. Sometimes the
redefinition is intended. In other cases, the user may not be aware that
the used backend Prolog compiler may already provide the predicate as a
built-in or may want to ensure code portability among several Prolog
compilers with different sets of built-in predicates.

.. _errors_redefinion_predicates:

Redefinition of Logtalk built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similar to the redefinition of Prolog built-in predicates, the Logtalk
compiler will warn us if we try to redefine a Logtalk built-in. The
redefinition will probably be an error in almost all (if not all) cases.

.. _errors_redefinion_methods:

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
non-ISO specified built-in predicate or arithmetic function, Portability
warnings are also reported for non-standard flags or flag values. These
warnings often cannot be avoided due to the limited scope of the ISO
Prolog standard.

.. _errors_deprecated:

Deprecated elements
~~~~~~~~~~~~~~~~~~~

A warning will be reported if a deprecated directive or control
construct is used.

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

A warning will be reported for calls that are syntactically correct but
most likely a semantic error. An example is ``::/1`` calls in clauses
that apparently are meant to implement recursive predicate definitions
where the user intention is to call the local predicate.

.. _errors_lambda_variables:

Lambda variables
~~~~~~~~~~~~~~~~

A warning will be reported for :term:`lambda expressions <lambda expression>`
with unclassified variables (not listed as either :term:`lambda free <lambda free variable>`
or :term:`lambda parameter` variables) or where variables play a dual role
(as both lambda free and lambda parameter variables).

.. _errors_predicate_redefinition:

Redefinition of predicates declared in ``uses/2`` and ``use_module/2`` directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A error will be reported for any attempt to define locally a predicate
that is already listed in an :ref:`directives_uses_2` or in an
:ref:`directives_use_module_2` directive.

.. _errors_others:

Other warnings and errors
~~~~~~~~~~~~~~~~~~~~~~~~~

The Logtalk compiler will throw an error if it finds a predicate clause
or a directive that cannot be parsed. The default behavior is to report
the error and abort the compilation of the offending entity.

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
``forwarding`` declares a predicate, :ref:`methods_forward_1`, which is
automatically called (if defined) by the runtime for any message that
the receiving object does not understand. The usual definition for this
error handler is to delegate or forward the message to another object
that might be able to answer it:

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
