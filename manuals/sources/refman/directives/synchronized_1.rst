
.. index:: synchronized/1
.. _directives_synchronized_1:

synchronized/1
==============

Description
-----------

::

   synchronized(Name/Arity)
   synchronized((Functor1/Arity1, ...))
   synchronized([Functor1/Arity1, ...])

   synchronized(Name//Arity)
   synchronized((Functor1//Arity1, ...))
   synchronized([Functor1//Arity1, ...])

Declares synchronized predicates and synchronized grammar rule
non-terminals. A synchronized predicate (or synchronized non-terminal)
is protected by a mutex in order to allow for thread synchronization
when proving a call to the predicate (or non-terminal). All predicates
(and non-terminals) declared in the same synchronized directive share
the same mutex. In order to use a separate mutex for each predicate
(non-terminal) so that they are independently synchronized, a
per-predicate synchronized directive must be used.

Declaring a predicate synchronized implicitly makes it deterministic.
When using a single-threaded back-end Prolog compiler, calls to
synchronized predicates behave as wrapped by the standard ``once/1``
meta-predicate.

Note that synchronized predicates cannot be declared dynamic (when
necessary, declare the predicates updating the dynamic predicates as
synchronized).

Template and modes
------------------

::

   synchronized(+predicate_indicator_term)
   synchronized(+non_terminal_indicator_term)

Examples
--------

::

   :- synchronized(db_update/1).

   :- synchronized((write_stream/2, read_stream/2)).

   :- synchronized([add_to_queue/2, remove_from_queue/2]).

.. seealso::

   :ref:`methods_predicate_property_2`
