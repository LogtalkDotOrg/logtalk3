
.. index:: set_logtalk_flag/2
.. _directives_set_logtalk_flag_2:

set_logtalk_flag/2
==================

Description
-----------

::

   set_logtalk_flag(Flag, Value)

Sets Logtalk flag values. The scope of this directive is the entity or
the source file containing it. For global scope, use the corresponding
:ref:`predicates_set_logtalk_flag_2` built-in predicate called from an
:ref:`directives_initialization_1` directive.

Template and modes
------------------

::

   set_logtalk_flag(+atom, +nonvar)

Errors
------

Flag is a variable:
   ``instantiation_error``
Value is a variable:
   ``instantiation_error``
Flag is not an atom:
   ``type_error(atom, Flag)``
Flag is neither a variable nor a valid flag:
   ``domain_error(flag, Flag)``
Value is not a valid value for flag Flag:
   ``domain_error(flag_value, Flag + Value)``
Flag is a read-only flag:
   ``permission_error(modify, flag, Flag)``

Examples
--------

::

   :- set_logtalk_flag(unknown_entities, silent).
