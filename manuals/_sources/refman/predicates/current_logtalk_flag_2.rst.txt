
.. index:: current_logtalk_flag/2
.. _predicates_current_logtalk_flag_2:

current_logtalk_flag/2
======================

Description
-----------

::

   current_logtalk_flag(Flag, Value)

Enumerates, by backtracking, the current Logtalk flag values.

Template and modes
------------------

::

   current_logtalk_flag(?atom, ?atom)

Errors
------

Flag is neither a variable nor an atom:
   ``type_error(atom, Flag)``
Flag is an atom but an invalid flag:
   ``domain_error(flag, Value)``

Examples
--------

::

   | ?- current_logtalk_flag(source_data, Value).

.. seealso::

   :ref:`predicates_create_logtalk_flag_3`,
   :ref:`predicates_set_logtalk_flag_2`
