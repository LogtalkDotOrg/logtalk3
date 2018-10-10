
.. index:: []/1
.. _control_delegate_message_1:

[]/1
====

Description
-----------

::

   [Object::Message]
   [{Proxy}::Message]

This control construct allows the programmer to send a message to an
object while preserving the original sender. It is mainly used in the
definition of object handlers for unknown messages. This functionality
is usually known as *delegation* but be aware that this is an overloaded
word that can mean different things in different object-oriented
programming languages.

To prevent using of this control construct to break object
encapsulation, an attempt to delegate a message to the original sender
results in an error. The remaining error conditions are the same as the
:ref:`control_send_to_object_2` control construct.

Note that, despite the correct functor for this control construct being
(traditionally) ``'.'/2``, we refer to it as ``[]/1`` simply to
emphasize that the syntax is a list with a single element.

Template and modes
------------------

::

   [+object_identifier::+callable]
   [{+object_identifier}::+callable]

Errors
------

Object and the original sender are the same object:
   ``permission_error(access, object, Sender)``

Either Object or Message is a variable:
   ``instantiation_error``
Object is neither a variable nor an object identifier:
   ``type_error(object_identifier, Object)``
Message is neither a variable nor a callable term:
   ``type_error(callable, Message)``
Message, with predicate indicator Name/Arity, is declared private:
   ``permission_error(access, private_predicate, Name/Arity)``
Message, with predicate indicator Name/Arity, is declared protected:
   ``permission_error(access, protected_predicate, Name/Arity)``
Message, with predicate indicator Name/Arity, is not declared:
   ``existence_error(predicate_declaration, Name/Arity)``
Object does not exist:
   ``existence_error(object, Object)``

Proxy is a variable:
   ``instantiation_error``
Proxy is neither a variable nor an object identifier:
   ``type_error(object_identifier, Proxy)``
Proxy, with predicate indicator Name/Arity, does not exist in the *user* pseudo-object:
   ``existence_error(procedure, Name/Arity)``

Examples
--------

::

   forward(Message) :-
       [backup::Message].

.. seealso::

   :ref:`control_send_to_object_2`,
   :ref:`control_send_to_self_1`,
   :ref:`control_call_super_1`,
   :ref:`methods_forward_1`
