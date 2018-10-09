
.. index:: info/2
.. _directives_info_2:

info/2
======

Description
-----------

::

   info(Name/Arity, List)
   info(Name//Arity, List)

Documentation directive for predicates and grammar rule non-terminals.
The first argument is either a predicate indicator or a grammar rule
non-terminal indicator. The second argument is a list of pairs using the
format *Key is Value*. See the :ref:`documenting_predicate`
section for a description of the default keys.

Template and modes
------------------

::

   info(+predicate_indicator, +predicate_info_list)
   info(+non_terminal_indicator, +predicate_info_list)

Examples
--------

::

   :- info(empty/1, [
       comment is 'True if the argument is an empty list.',
       argnames is ['List']
   ]).
       
   :- info(sentence//0, [
       comment is 'Rewrites a sentence into a noun phrase and a verb phrase.'
   ]).

.. seealso::

   :ref:`directives_info_1`,
   :ref:`directives_mode_2`,
   :ref:`methods_predicate_property_2`
