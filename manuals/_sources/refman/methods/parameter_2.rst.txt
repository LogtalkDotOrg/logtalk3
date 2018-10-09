
.. index:: parameter/2
.. _methods_parameter_2:

parameter/2
===========

Description
-----------

::

   parameter(Number, Term)

Used in :term:`parametric objects <parametric entity>` (and
parametric categories), this private method provides runtime access to
the parameter values of the entity that contains the predicate clause
whose body is being executed by using the argument number in the entity
identifier. This predicate is implemented as a unification between its
second argument and the corresponding implicit execution-context
argument in the predicate containing the call. This unification occurs
at the clause head when the second argument is not instantiated (the
most common case). When the second argument is instantiated, the
unification must be delayed to runtime and thus occurs at the clause
body.

Entity parameters can also be accessed using *parametric variables*,
which use the syntax ``_VariableName_``. The compiler recognizes
occurrences of these variables in entity clauses. Parametric variables
allows us to abstract parameter positions thus simplifying code
maintenance.

Template and modes
------------------

::

   parameter(+integer, ?term)

Errors
------

Number is a variable:
   ``instantiation_error``
Number is neither a variable nor an integer value:
   ``type_error(integer, Number)``
Number is smaller than one or greater than the parametric entity identifier arity:
   ``domain_error(out_of_range, Number)``
Entity identifier is not a compound term:
   ``type_error(compound, Entity)``

Examples
--------

::

   :- object(box(_Color, _Weight)).

       ...
       
       color(Color) :-              % this clause is translated into a fact
           parameter(1, Color).     % upon compilation 

       heavy :-
           parameter(2, Weight),    % after compilation, the >/2 call will be
           Weight > 10.             % the first condition on the clause body

       ...

The same example using *parametric variables*:

::

   :- object(box(_Color_, _Weight_)).

       ...
       
       color(_Color_).

       heavy :-
           _Weight_ > 10.

       ...

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_self_1`,
   :ref:`methods_sender_1`,
   :ref:`methods_this_1`
