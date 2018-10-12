
.. index:: coinductive_success_hook/1-2
.. _methods_coinductive_success_hook_1_2:

coinductive_success_hook/1-2
============================

Description
-----------

::

   coinductive_success_hook(Head, Hypothesis)
   coinductive_success_hook(Head)

User-defined hook predicates that are automatically called in case of
coinductive success when proving a query for a coinductive predicates.
The hook predicates are called with the head of the coinductive
predicate on coinductive success and, optionally, with the hypothesis
used that to reach coinductive success.

When both hook predicates are defined, the
``coinductive_success_hook/1`` clauses are only used if no
``coinductive_success_hook/2`` clause applies. The compiler ensures zero
performance penalties when defining coinductive predicates without a
corresponding definition for the coinductive success hook predicates.

The compiler assumes that these hook predicates are defined as static
predicates in order to optimize their use.

Template and modes
------------------

::

   coinductive_success_hook(+callable, +callable)
   coinductive_success_hook(+callable)

Errors
------

(none)

Examples
--------

(none)

.. seealso::

   :ref:`directives_coinductive_1`
