..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _inheritance_inheritance:

Inheritance
===========

The inheritance mechanisms found on object-oriented programming
languages allow us the specialization of previously defined objects,
avoiding the unnecessary repetition of code and allowing the definition
of common predicates for sets of objects. In the context of logic
programming, we can interpret inheritance as a form of theory extension:
an object will virtually contain, besides its own predicates, all the
predicates inherited from other objects that are not redefined by
itself.

Logtalk uses a depth-first lookup procedure for finding predicate
declarations and predicate definitions, as explained below. The lookup
procedures locate the entities holding the predicate declaration and the
predicate definition using, respectively, the predicate indicator and
the predicate template (constructed from the predicate indicator).
The :ref:`directives_alias_2` predicate directive may be used to defining
alternative names for inherited predicates, for solving inheritance
conflicts, and for giving access to all inherited definitions (thus
overriding the default lookup procedure).

.. _inheritance_protocol:

Protocol inheritance
--------------------

Protocol inheritance refers to the inheritance of predicate declarations
(:term:`scope directives <predicate scope directive>`). These can be contained
in objects, in protocols, or in categories. Logtalk supports single and
multi-inheritance of protocols: an object or a category may implement several
protocols and a protocol may extend several protocols.

.. _inheritance_protocol_prototype:

Search order for prototype hierarchies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The search order for predicate declarations is first the object, second
the implemented protocols (and the protocols that these may extend),
third the imported categories (and the protocols that they may
implement), and last the objects that the object extends. This search is
performed in depth-first order. When an object inherits two different
declarations for the same predicate, by default, only the first one will
be considered.

.. _inheritance_protocol_class:

Search order for class hierarchies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The search order for predicate declarations starts in the object
classes. Following the classes declaration order, the search starts in
the classes implemented protocols (and the protocols that these may
extend), third the classes imported categories (and the protocols that
they may implement), and last the superclasses of the object classes.
This search is performed in depth-first order. If the object inherits
two different declarations for the same predicate, by default only the
first one will be considered.

.. _inheritance_implementation:

Implementation inheritance
--------------------------

Implementation inheritance refers to the inheritance of predicate
definitions. These can be contained in objects or in categories. Logtalk
supports multi-inheritance of implementation: an object may import
several categories or extend, specialize, or instantiate several
objects.

.. _inheritance_implementation_prototype:

Search order for prototype hierarchies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The search order for predicate definitions is similar to the search for
predicate declarations except that implemented protocols are ignored (as
they can only contain predicate directives).

.. _inheritance_implementation_class:

Search order for class hierarchies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The search order for predicate definitions is similar to the search for
predicate declarations except that implemented protocols are ignored (as
they can only contain predicate directives) and that the search starts
at the instance itself (that received the message) before proceeding, if
no predicate definition is found there, to the instance classes and then
to the class superclasses.

.. _inheritance_implementation_redefinition:

Redefining inherited predicate definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we define a predicate that is already inherited from other object,
the inherited definitions are hidden by the new definitions. This is
called overriding inheritance: a local definition overrides any inherited
definitions. For example, assume that we have the following two objects:

::

   :- object(root).

       :- public(bar/1).
       bar(root).

       :- public(foo/1).
       foo(root).

   :- end_object.


   :- object(descendant,
       extends(root)).

       foo(descendant).

   :- end_object.

After compiling and loading these objects, we can check the overriding
behavior by trying the following queries:

.. code-block:: text

   | ?- root::(bar(Bar), foo(Foo)).

   Bar = root
   Foo = root
   yes


   | ?- descendant::(bar(Bar), foo(Foo)).

   Bar = root
   Foo = descendant
   yes

However, we can explicitly code other behaviors. Some examples follow.

.. _inheritance_specialization:

Specializing inherited predicate definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Specialization of inherited definitions: the new definition uses the
inherited definitions, adding new code. This is accomplished by
calling the :ref:`control_call_super_1` operator
in the new definition. For example, assume a ``init/0`` predicate
that must account for object specific initializations along the
inheritance chain:

::

   :- object(root).

       :- public(init/0).

       init :-
           write('root init'), nl.

   :- end_object.


   :- object(descendant,
       extends(root)).

       init :-
           write('descendant init'), nl,
           ^^init.

   :- end_object.


   | ?- descendant::init.

   descendant init
   root init

   yes

.. _inheritance_union:

Union of inherited and local predicate definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Union of the new with the inherited definitions: all the definitions are
taken into account, the calling order being defined by the inheritance
mechanisms. This can be accomplished by writing a clause that just calls,
using the :ref:`control_call_super_1` operator, the inherited definitions.
The relative position of this clause among the other definition clauses
sets the calling order for the local and inherited definitions. For example:

::

   :- object(root).

       :- public(foo/1).

       foo(1).
       foo(2).

   :- end_object.


   :- object(descendant,
       extends(root)).

       foo(3).
       foo(Foo) :-
           ^^foo(Foo).

   :- end_object.


   | ?- descendant::foo(Foo).

   Foo = 3 ;
   Foo = 1 ;
   Foo = 2 ;
   no

.. _inheritance_selective:

Selective inheritance of predicate definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The selective inheritance of predicate definitions (also known as
differential inheritance) is normally used in the representation
of exceptions to inherited default definitions. We can use the
:ref:`control_call_super_1` operator to test and possibly reject
some of the inherited definitions. A common example is representing
flightless birds:

::

   :- object(bird).

       :- public(mode/1).

       mode(walks).
       mode(flies).

   :- end_object.


   :- object(penguin,
       extends(bird)).

       mode(swims).
       mode(Mode) :-
           ^^mode(Mode),
           Mode \= flies.

   :- end_object.


   | ?- penguin::mode(Mode).

   Mode = swims ;
   Mode = walks ;

   no

.. _inheritance_types:

Public, protected, and private inheritance
------------------------------------------

To make all :term:`public predicates<public predicate>`
declared via implemented protocols, imported categories, or ancestor
objects :term:`protected predicates <protected predicate>` or to make
all public and protected predicates
:term:`private predicates <private predicate>` we prefix the entity's
name with the corresponding keyword. For example:

::

   :- object(Object,
       implements(private::Protocol)).

       % all the Protocol public and protected
       % predicates become private predicates
       % for the Object clients

       ...

   :- end_object.

or:

::

   :- object(Class,
       specializes(protected::Superclass)).

       % all the Superclass public predicates become
       % protected predicates for the Class clients

       ...

   :- end_object.

Omitting the scope keyword is equivalent to using the public scope
keyword. For example:

::

   :- object(Object,
       imports(public::Category)).
       ...
   :- end_object.

This is the same as:

::

   :- object(Object,
       imports(Category)).
       ...
   :- end_object.

This way we ensure backward compatibility with older Logtalk versions
and a simplified syntax when protected or private inheritance are not
used.

.. _inheritance_composition:

Composition versus multiple inheritance
---------------------------------------

It is not possible to discuss inheritance mechanisms without referring
to the long and probably endless debate on single versus multiple
inheritance. The single inheritance mechanism can be implemented
efficiently but it imposes several limitations on reusing, even
if the multiple characteristics we intend to inherit are orthogonal. On
the other hand, the multiple inheritance mechanisms are attractive in
their apparent capability of modeling complex situations. However, they
include a potential for conflict between inherited definitions whose
variety does not allow a single and satisfactory solution for all the
cases.

Until now, no solution that we might consider satisfactory for all the
problems presented by the multiple inheritance mechanisms has been
found. From the simplicity of some extensions that use the Prolog search
strategy like [McCabe92]_ or [Moss94]_ and to the sophisticated algorithms
of CLOS [Bobrow_et_al_88]_, there is no
adequate solution for all the situations. Besides, the use of multiple
inheritance carries some complex problems in the domain of software
engineering, particularly in the reuse and maintenance of the
applications. All these problems are substantially reduced if we
preferably use in our software development composition mechanisms
instead of specialization mechanisms [Taenzer89]_. Multiple inheritance is
best used as an analysis and project abstraction, rather than
as an implementation technique [Shan_et_al_93]_. Logtalk provides first-class
support for software composition using :ref:`categories_categories`.

Nevertheless, Logtalk supports multi-inheritance by enabling an object
to extend, instantiate, or specialize more than one object. The
:ref:`directives_alias_2` predicate directive can always be used
to solve multi-inheritance conflicts. It should also be noted that the
multi-inheritance support does not affect performance when we use
single-inheritance.
