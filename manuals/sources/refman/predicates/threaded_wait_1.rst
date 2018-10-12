
.. index:: threaded_wait/1
.. _predicates_threaded_wait_1:

threaded_wait/1
===============

Description
-----------

::

   threaded_wait(Term)
   threaded_wait([Term| Terms])

Suspends the thread making the call until a notification is received
that unifies with ``Term``. The call must be made within the same object
(:term:`this`) containing the calls to the
:ref:`predicates_threaded_notify_1` predicate that will
eventually send the notification. The argument may also be a list of
notifications, ``[Term| Terms]``. In this case, the thread making the
call will suspend until all notifications in the list are received.

Template and modes
------------------

::

   threaded_wait(?term)
   threaded_wait(+list(term))

Errors
------

(none)

Examples
--------

Wait until the ``data_available`` notification is received:
   ``threaded_wait(data_available)``

.. seealso::

   :ref:`predicates_threaded_notify_1`
