
.. index:: ::/1
.. _control_send_to_self_1:

::/1
====

Description
-----------

::

   ::Message

Sends a message to :term:`self`. Can only used in
the body of a predicate definition. The argument should match a public
or protected predicate of *self*. It may also match a private predicate
if the predicate is within the scope of the object where the method
making the call is defined, if imported from a category, if used from
within a category, or when using private inheritance. When the predicate
is declared but not defined, the message simply fails (as per the
closed-world assumption).

The lookups for the message declaration and the corresponding method are
performed using a depth-first strategy. A message to *self* necessarily
implies the use of dynamic binding but a caching mechanism is used to
improve performance in subsequent messages.

Template and modes
------------------

::

   ::+callable

Errors
------

Message is a variable:
   ``instantiation_error``
Message is neither a variable nor a callable term:
   ``type_error(callable, Message)``
Message, with predicate indicator Name/Arity, is declared private:
   ``permission_error(access, private_predicate, Name/Arity)``
Message, with predicate indicator Name/Arity, is not declared:
   ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

::

   area(Area) :-
       ::width(Width),
       ::height(Height),
       Area is Width*Height.

.. seealso::

   :ref:`control_send_to_object_2`,
   :ref:`control_call_super_1`,
   :ref:`control_delegate_message_1`
