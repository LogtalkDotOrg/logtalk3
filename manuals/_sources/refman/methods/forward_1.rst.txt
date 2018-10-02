
.. index:: forward/1
.. _methods_forward_1:

forward/1
=========

Description
-----------

::

   forward(Message)

User-defined method for forwarding unknown messages sent to an object
(using the :ref:`control_send_to_object_2` control
construct), automatically called by the runtime when defined. This
method is declared in the ``forwarding`` built-in protocol as a public
predicate. Note that you can make its scope protected or private by
using, respectively, protected or private implementation of the
``forwarding`` protocol.

Template and modes
------------------

::

   forward(+callable)

Errors
------

``(none)``

Examples
--------

::

   :- object(proxy,
       implements(forwarding),
       ...).

       forward(Message) :-
           % delegate the unknown message to other object
           [real::Message].

See also
--------

:ref:`control_delegate_message_1`
