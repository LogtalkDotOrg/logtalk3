
.. index:: logtalk_make_target_action/1
.. _predicates_logtalk_make_target_action_1:

logtalk_make_target_action/1
============================

Description
-----------

::

   logtalk_make_target_action(Target)

Multifile and dynamic hook predicate that allows defining user actions
for the ``logtalk_make/1`` targets. The user defined actions are run
after the default ones using a failure driven loop.

Template and modes
------------------

::

   logtalk_make_target_action(+atom)

Errors
------

``(none)``

Examples
--------

::

   % integrate the dead_code_scanner tool with logtalk_make/1

   :- multifile(logtalk_make_target_action/1).
   :- dynamic(logtalk_make_target_action/1).

   logtalk_make_target_action(check) :-
       dead_code_scanner::all.

.. seealso::

   :ref:`predicates_logtalk_make_1`,
   :ref:`predicates_logtalk_make_0`
