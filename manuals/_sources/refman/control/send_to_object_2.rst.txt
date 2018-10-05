
.. index:: ::/2
.. _control_send_to_object_2:

::/2
====

Description
-----------

::

   Object::Message
   {Proxy}::Message

Sends a message to an object. The message argument must match a public
predicate of the receiver object. When the message corresponds to a
protected or private predicate, the call is only valid if the
:term:`sender` matches the :term:`predicate scope container`. When the
predicate is declared but not defined, the message simply fails (as per
the closed-world assumption).

The ``{Proxy}::Message`` syntax allows simplified access to parametric
object *proxies*. Its operational semantics is equivalent to the goal
conjunction ``(call(Proxy), Proxy::Message)``. I.e. ``Proxy`` is proved
within the context of the pseudo-object :ref:`user <objects_user>` and,
if successful, the goal term is used as a parametric object identifier.
Exceptions thrown when proving ``Proxy`` are handled by the ``::/2``
control construct. This syntax construct supports backtracking over the
``{Proxy}`` goal.

The lookups for the message declaration and the corresponding method are
performed using a depth-first strategy. Depending on the value of the
``optimize`` flag, these lookups are performed at compile time whenever
sufficient information is available. When the lookups are performed at
runtime, a caching mechanism is used to improve performance in
subsequent messages.

Template and modes
------------------

::

   +object_identifier::+callable
   {+object_identifier}::+callable

Errors
------

Either Object or Message is a variable:
   ``instantiation_error``
Object is neither a variable nor a valid object identifier:
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
Proxy is neither a variable nor a callable term:
   ``type_error(callable, Proxy)``
Proxy, with predicate indicator Name/Arity, does not exist in the *user* pseudo-object:
   ``existence_error(procedure, Name/Arity)``

Examples
--------

::

   | ?- list::member(X, [1, 2, 3]).

   X = 1 ;
   X = 2 ;
   X = 3
   yes

See also
--------

:ref:`control_send_to_self_1`,
:ref:`control_call_super_1`,
:ref:`control_delegate_message_1`
