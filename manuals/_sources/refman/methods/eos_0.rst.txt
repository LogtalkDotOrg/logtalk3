
.. index:: eos//0
.. _methods_eos_0:

eos//0
======

Description
-----------

::

   eos

This non-terminal matches the end-of-input. It is implemented by
checking that the implicit difference list unifies with ``[]-[]``.

Template and modes
------------------

::

   eos

Errors
------

``(none)``

Examples
--------

::

   abc --> a, b, c, eos.

.. seealso::

   :ref:`methods_call_1`,
   :ref:`methods_phrase_1`,
   :ref:`methods_phrase_2`,
   :ref:`methods_phrase_3`
