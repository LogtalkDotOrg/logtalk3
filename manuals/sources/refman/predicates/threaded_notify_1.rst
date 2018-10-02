
.. index:: threaded_notify/1
.. _predicates_threaded_notify_1:

threaded_notify/1
=================

Description
-----------

::

   threaded_notify(Term)
   threaded_notify([Term| Terms])

Sends ``Term`` as a notification to any thread suspended waiting for it
in order to proceed. The call must be made within the same object
(:term:`this`) containing the calls to the
:ref:`predicates_threaded_wait_1` predicate waiting for the
notification. The argument may also be a list of notifications,
``[Term| Terms]``. In this case, all notifications in the list will be
sent to any threads suspended waiting for them in order to proceed.

Template and modes
------------------

::

   threaded_notify(@term)
   threaded_notify(@list(term))

Errors
------

``(none)``

Examples
--------

Send the notification ``data_available``:
   ``threaded_notify(data_available)``

See also
--------

:ref:`predicates_threaded_wait_1`
