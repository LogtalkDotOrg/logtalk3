
.. index:: {}/1
.. _control_external_call_1:

{}/1
====

Description
-----------

::

   {Term}
   {Goal}

This control construct allows the programmer to bypass the Logtalk
compiler. It can also be used to wrap a source file term (either a clause or
a directive) to bypass the term-expansion mechanism. Similarly, it can
also be used to wrap a goal to bypass the goal-expansion mechanism. When used
to wrap a goal, it is opaque to cuts and the argument is called within
the context of the pseudo-object :ref:`user <objects_user>`. It is also possible
to use ``{Closure}`` as the first argument of :ref:`methods_call_N` calls. In
this case, ``Closure`` will be extended with the remaining arguments of
the ``call/2-N`` call in order to construct a goal that will be called
within the context of ``user``. It can also be used as a message to any
object. This is useful when the message is e.g. a conjunction of
messages, some of which being calls to Prolog built-in predicates.

This control construct may also be used in place of an object identifier
when sending a message. In this case, the result of proving its argument
as a goal (within the context of the pseudo-object ``user``) is used as
an object identifier in the message sending call. This feature is mainly
used with :term:`parametric objects <parametric object>` when their
identifiers correspond to predicates defined in ``user``.

Template and modes
------------------

::

   {+callable}

Errors
------

Term or Goal is a variable:
   ``instantiation_error``
Term is neither a variable nor a callable term:
   ``type_error(callable, Term)``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

::

   {:- load_foreign_resource(file)}.

   N1/D1 < N2/D2 :-
       {N1*D2 < N2*D1}.

   call_in_user(F, X, Y, Z) :-
       call({F}, X, Y, Z).

   | ?- {circle(Id, Radius, Color)}::area(Area).
   ...

   | ?- logtalk::{write('Hello world!'), nl}.
   Hello world!
   yes
