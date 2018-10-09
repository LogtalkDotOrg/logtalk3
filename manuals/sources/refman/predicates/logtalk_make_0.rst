
.. index:: logtalk_make/0
.. _predicates_logtalk_make_0:

logtalk_make/0
==============

Description
-----------

::

   logtalk_make

Reloads all Logtalk source files that have been modified since the time
they are last loaded. Only source files loaded using the
``logtalk_load/1-2`` predicates are reloaded. Non-modified files will
also be reloaded when there is a change to the compilation mode (i.e.
when the files were loaded without explicit ``debug/1`` or
``optimize/1`` flags and the default values of these flags changed after
loading; no check is made, however, for other implicit compiler flags
that may have changed since loading). When an included file is modified,
this predicate reloads its main file (i.e. the file that contains the
``include/1`` directive).

Depending on the back-end Prolog compiler, the shortcut ``{*}`` may be
used in alternative. Check the adapter files for the availability of
these shortcuts as they are not part of the language (and thus should
only be used at the top-level interpreter).

This predicate can be extended by the user by defining clauses for the
``logtalk_make_target_action/1`` multifile and dynamic hook predicate
using the argument ``all``. The additional user defined actions are run
after the default one.

Template and modes
------------------

::

   logtalk_make

Errors
------

``(none)``

Examples
--------

::

   | ?- logtalk_make.

.. seealso::

   :ref:`predicates_logtalk_compile_1`,
   :ref:`predicates_logtalk_compile_2`,
   :ref:`predicates_logtalk_load_1`,
   :ref:`predicates_logtalk_load_2`,
   :ref:`predicates_logtalk_make_1`,
   :ref:`predicates_logtalk_make_target_action_1`
