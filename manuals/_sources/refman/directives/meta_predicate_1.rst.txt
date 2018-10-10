
.. index:: meta_predicate/1
.. _directives_meta_predicate_1:

meta_predicate/1
================

Description
-----------

::

   meta_predicate(MetaPredicateTemplate)
   meta_predicate((MetaPredicateTemplate1, ...))
   meta_predicate([MetaPredicateTemplate1, ...])

   meta_predicate(Entity::MetaPredicateTemplate)
   meta_predicate((Entity1::MetaPredicateTemplate1, ...))
   meta_predicate([Entity1::MetaPredicateTemplate1, ...])

   meta_predicate(Module:MetaPredicateTemplate)
   meta_predicate((Module1:MetaPredicateTemplate1, ...))
   meta_predicate([Module1:MetaPredicateTemplate1, ...])

Declares meta-predicates, i.e., predicates that have arguments that will
be called as goals. An argument may also be a *closure* instead of a
goal if the meta-predicate uses the :ref:`methods_call_N` Logtalk built-in
methods to construct and call the actual goal from the closure and the
additional arguments.

Meta-arguments which are goals are represented by the integer ``0``.
Meta-arguments which are closures are represented by a positive integer,
``N``, representing the number of additional arguments that will be
appended to the closure in order to construct the corresponding
meta-call. Normal arguments are represented by the atom ``*``.
Meta-arguments are always called in the meta-predicate calling context,
not in the meta-predicate definition context.

Logtalk allows the use of this directive to override the original
meta-predicate directive. This is sometimes necessary when calling
Prolog module meta-predicates due to the lack of standardization of the
syntax of the meta-predicate templates.

Template and modes
------------------

::

   meta_predicate(+meta_predicate_template_term)

   meta_predicate(+object_identifier::+meta_predicate_template_term)
   meta_predicate(+category_identifier::+meta_predicate_template_term)

   meta_predicate(+module_identifier:+meta_predicate_template_term)

Examples
--------

::

   :- meta_predicate(findall(*, 0, *)).

   :- meta_predicate(forall(0, 0)).

   :- meta_predicate(maplist(2, *, *)).

.. seealso::

   :ref:`directives_meta_non_terminal_1`,
   :ref:`methods_predicate_property_2`
