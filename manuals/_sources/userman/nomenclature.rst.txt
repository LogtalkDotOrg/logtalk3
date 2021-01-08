..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _nomenclature_nomenclature:

Nomenclature
============

Depending on your logic programming and object-oriented programming background
(or lack of it), you may find Logtalk nomenclature either familiar or at odds
with the terms used in other languages. In addition, being a superset of Prolog,
terms such as *predicate* and *method* are often used interchangeably. Logtalk
inherits most of its nomenclature from Prolog and Smalltalk.

Note that the same terms can have different meanings in different languages.
A good example is *class*. The support for meta-classes in e.g. Smalltalk
translates to a concept of class that is different is key aspects from the
concept of class in e.g. Java or C++. Other terms that can have different
meanings are *delegation* and *forwarding*. There are also cases where the
same concept is found under different names in some languages (e.g. *self*
and *this*) but that can also mean different concepts in other languages.
Always be aware of these differences and be cautious with assumptions carried
from other programming languages.

In this section, we map nomenclatures from Prolog and popular OOP languages
such as Smalltalk, C++, Java, and Python to the Logtalk nomenclature. The
Logtalk distribution includes several examples of how to implement common
concepts found in other languages, complementing the information in this
section. There is also an extensive :ref:`glossary <glossary>` providing
the exact meaning of the names commonly used in Logtalk programming.

.. _nomenclature_prolog:

Prolog nomenclature
-------------------

Being a superset of Prolog, Logtalk inherits its nomenclature. But Logtalk
also aims to fix several Prolog shortcomings, thus introducing new concepts
or refining existing Prolog concepts. Logtalk object-oriented nature also
introduces names and concepts that are not common when discussing logic
programming semantics. We mention here the most relevant ones, notably
those where semantics or common practice differ. Further details can be
found elsewhere in this Handbook.

**arbitrary goals as directives**
   Although not ISO Prolog Core standard compliant, several Prolog systems
   accept using arbitrary goal as directives. This is not supported in
   Logtalk source files. Always use an ``initialization/1`` to wrap those
   goals. This ensure that any initialization goals, which often have
   side-effects, are only called if the source file is successfully
   compiled and loaded.

**calling a predicate**
   Sending a message to an object is similar to *calling a goal* with the
   difference that the actual predicate that is called is determined not
   just by the message *term* but also by the object receiving the message.
   This is also different from calling a Prolog module predicate: a message
   may result e.g. in calling a predicate inherited by the object but calling
   a module predicate requires the predicate to exist in (or be reexported by)
   the module.

**closed world assumption semantics**
   Logtalk provides clear closed world assumption semantics: messages or calls
   for declared but undefined predicates fail. Messages or calls for unknown
   (i.e. not declared) predicates throw an error. Crucially, this semantics
   apply to both *static* and *dynamic* predicates. But in Prolog workarounds
   are required to have a static predicate being known by the runtime without
   it being also defined (so that calling it would fail instead of throwing a
   predicate existence error).

**compiling and loading source files**
   Logtalk provides its own built-in predicates for compiling and loading
   source files. It also provides convenient top-level interpreter shorthands
   for these and other frequent operations. In general, the traditional
   Prolog built-in predicates and top-level interpreter shorthands cannot
   be used to load Logtalk source files.

**debugging**
   In most (if not all) Prolog systems, debugging support is a built-in
   feature made available using a set of built-in predicates like ``trace/0``
   and ``spy/1``. But in Logtalk the default debugger is a regular application,
   implemented using a public reflection API. This means that the debugger
   must be explicitly loaded (either automatically from a *settings* file or
   from the top-level). It also means that the debugger can be easily extended
   or replaced by an alternative application.

**directive operators**
   Some Prolog systems declare directive names as operators (e.g. ``dynamic``,
   ``multifile``, ...). This is not required by the ISO Prolog Core standard.
   It's a practice that should be avoided as it makes code non-portable.

**encapsulation**
   Logtalk enforces encapsulation of object predicates, generating a permission
   error when a predicate is not within the scope of the caller. In contrast,
   most Prolog module systems allow any module predicate to be called by using
   explicit qualification, even if not exported. Worse, some Prolog systems
   also allow defining clauses for a module predicate outside the module,
   without declaring the predicate as multifile, by simply writing clauses
   with explicit module-qualified heads.

**entity loading**
   When using Prolog modules, ``use_module/1-2`` (or equivalent) directives
   both load the module files and declare that the (implicitly or explicitly)
   imported predicates can be used with implicit module qualification.
   But Logtalk separates entity (object, protocol, category, or module)
   predicate *usage* declarations (via ``uses/2`` or its own ``use_module/1-2``
   directives) from *loading* goals, which use an explicit and disciplined
   approach with *loader* files.

**flags scope**
   The ``set_logtalk_flag/2`` **directive** is always local to the entity or
   source file that contains it. Only calls to the ``set_logtalk_flag/2``
   **predicate** set the global default value for a flag. This distinction
   is lacking in Prolog (where directives usually have a global scope) and
   Prolog modules (where some flags are local to modules in some systems
   and global in other systems).

**meta-predicate call semantics**
   Logtalk provides consistent meta-predicate call semantics: meta-arguments
   are always called in the meta-predicate *calling context*. This contrasts
   with Prolog module meta-predicates where the semantics of implicitly
   qualified calls is different from explicitly qualified calls.

**operators scope**
   Operators declared inside an entity (object, protocol, or category) are
   local to the entity.

**predicates scope**
   In plain Prolog, all predicates are visible. In a Prolog module, a
   predicate can be exported or local. In Logtalk, a predicate can be
   *public*, *protected*, *private*, or *local*.

**predicate declaration**
   Logtalk provides a clear distinction between *declaring* a predicate and
   *defining* a predicate. This is a fundamental requirement for the concept
   of *protocol* (aka interface) in Logtalk: we must be able to *declare* a
   predicate without necessarily *defining* it. This clear distinction is
   missing in Prolog and Prolog modules. Notably, it's a compiler error for
   a module to try to export a predicate that it does not define.

**predicate loading conflicts**
   Logtalk does not use predicate import/export semantics. Thus, there are
   never conflicts when loading entities (objects, protocols, or categories)
   that declare the same public predicates. But attempting to load two Prolog
   modules that export the same predicate results in a conflict, usually a
   compilation error (this is specially problematic when the ``use_module/1``
   directive is used; e.g. adding a new exported predicate can break
   applications that use the module but not the new predicate).

.. _nomenclature_smalltalk:

Smalltalk nomenclature
----------------------

The Logtalk name originates from a combination of the Prolog and Smalltalk
names. Smalltalk had a significant influence in the design of Logtalk and
thus inherits some of its ideas and nomenclature. The following list relates
the most commonly used Smalltalk terms with their Logtalk counterparts.

**abstract class**
   Similar to Smalltalk, an abstract class is just a class not meant to be
   instantiated by not understanding a message to create instances.

**assignment statement**
   Logtalk, as a superset of Prolog, uses *logic variables* and *unification*
   and thus provides no equivalent to the Smalltalk assignment statement.

**block**
   Logtalk supports lambda expressions and meta-predicates, which can be used
   to provide similar functionality to Smalltalk blocks.

**class**
   In Logtalk, *class* is a just a *role* that an object can play. This is
   similar to Smalltalk where classes are also objects.

**class method**
   Class methods in Logtalk are simply instance methods declared and defined
   in the class metaclass.

**class variable**
   Logtalk objects, which can play the roles of class and instance,
   encapsulate predicates, not state. Class variables, which in Smalltalk are
   really shared instance variables, can be emulated in a class by defining a
   predicate locally instead of defining it in the class instances.

**inheritance**
   While Smalltalk only supports single inheritance, Logtalk supports
   single inheritance, multiple inheritance, and multiple instantiation.

**instance**
   While in Smalltalk every object is an *instance* of same class, objects
   in Logtalk can play different roles, including the role of a prototype
   where the concepts of instance and class don't apply. Moreover, instances
   can be either created dynamically or defined statically.

**instance method**
   Instance methods in Logtalk are simply predicates declared and defined
   in a class and thus inherited by the class instances.

**instance variable**
   Logtalk being a *declarative* language, objects encapsulate a set of
   predicates instead of encapsulating *state*. But different objects may
   provide different definitions of the same predicates. Mutable internal
   state as in Smalltalk can be emulated by using dynamic predicates.

**message**
   Similar to Smalltalk, a *message* is a request for an operation, which is
   interpreted in Logtalk as a logic query, asking for the construction of a
   proof that something is true.

**message selector**
   Logtalk uses the predicate template (i.e. the predicate callable term with
   all its arguments unbound) as message selector. The actual type of the
   message arguments is not considered. Like Smalltalk, Logtalk uses *single
   dispatch* on the message receiver.

**metaclass**
   Metaclasses are optional in Logtalk (except for a root class) and can be
   shared by several classes. When metaclasses are used, infinite regression
   is simply avoided by making a class an instance of itself.

**method**
   Same as in Smalltalk, a *method* is the actual code (i.e. predicate
   definition) that is run to answer a message. Logtalk uses the words
   *method* and *predicate* interchangeably.

**method categories**
   There is no support in Logtalk for partitioning the methods of an object
   in different categories. The Logtalk concept of *category* (a first-class
   entity) was, however, partially inspired by Smalltalk method categories.

**object**
   Unlike Smalltalk, where *everything* is an object, Logtalk language
   constructs includes both *terms* (as in Prolog representing e.g. numbers
   and structures) and three first-class entities: objects, protocols, and
   categories.

*pool variables**
   Logtalk, as a superset of Prolog, uses *predicates* with no distinction
   between *variables* and *methods*. Categories can be used to share a set
   of predicate definitions between any number of objects.

**protocol**
   In Smalltalk, an object *protocol* is the set of messages it understands.
   The same concept applies in Logtalk. But Logtalk also supports protocols
   as first-class entities where a protocol can be implemented by multiple
   objects and an object can implement multiple protocols.

**self**
   Logtalk uses the same definition of *self* found in Smalltalk: the object
   that received the message being processed. Note, however, that *self* is
   not a keyword in Logtalk but implicit in the :ref:`control_send_to_self_1`
   message to *self* control construct.

**subclass**
   Same definition in Logtalk.

**super**
   As in Smalltalk, the idea of *super* is to allow calling an inherited
   predicate (that is usually being redefined). Note, however, that *super* is
   not a keyword in Logtalk, which provides instead a :ref:`control_call_super_1`
   *super* call control construct.

**superclass**
   Same definition in Logtalk. But while in Smalltalk a class can only have a
   single superclass, Logtalk support for multiple inheritance allows a class
   to have multiple superclasses.

.. _nomenclature_cpp:

C++ nomenclature
----------------

There are several C++ glossaries available on the Internet. The list
that follows relates the most commonly used C++ terms with their Logtalk
equivalents.

**abstract class**
   Logtalk uses an *operational* definition of abstract class: any class
   that does not inherit a method for creating new instances can be
   considered an abstract class. Moreover, Logtalk supports
   :term:`interfaces/protocols <protocol>`, which are often a better way to provide the
   functionality of C++ abstract classes.

**base class**
   Logtalk uses the term :term:`superclass` with the same meaning.

**data member**
   Logtalk uses :term:`predicates <predicate>` for representing both behavior and data.

**constructor function**
   There are no special methods for creating new objects in Logtalk.
   Instead, Logtalk provides a built-in predicate, :ref:`predicates_create_object_4`,
   which can be used as a building block to define more sophisticated
   object creation predicates.

**derived class**
   Logtalk uses the term :term:`subclass` with the same meaning.

**destructor function**
   There are no special methods for deleting new objects in Logtalk.
   Instead, Logtalk provides a built-in predicate, :ref:`predicates_abolish_object_1`,
   which is often used to define more sophisticated object deletion
   predicates.

**friend function**
   Not supported in Logtalk. Nevertheless, see the User Manual section on
   :ref:`meta-predicates <predicates_meta>`.

**instance**
   In Logtalk, an instance can be either created dynamically at runtime
   or defined statically in a source file in the same way as classes.

**member**
   Logtalk uses the term :term:`predicate`.

**member function**
   Logtalk uses :term:`predicates <predicate>` for representing both behavior
   and data.

**namespace**
   Logtalk does not support multiple identifier namespaces. All Logtalk
   entity identifiers share the same namespace (Logtalk entities are
   objects, categories, and protocols).

**nested class**
   Logtalk does not support nested classes.

**static member**
   Logtalk does not support a ``static`` keyword. But the equivalent to
   static members can be declared in a class metaclass.

**template**
   Logtalk supports :ref:`parametric objects <objects_parametric>`, which
   allows you to get the similar functionality of templates at runtime.

**this**
   Logtalk uses the built-in context method :ref:`methods_self_1` for retrieving
   the instance that received the message being processed. Logtalk also provides
   a :ref:`methods_this_1` method but for returning the class containing the
   method being executed. Why the name clashes? Well, the notion of :term:`self`
   was inherited from Smalltalk, which predates C++.

**virtual member function**
   There is no ``virtual`` keyword in Logtalk. Any inherited or imported
   predicate can be redefined (either overridden or specialized).
   Logtalk can use :term:`static binding` or :term:`dynamic binding` for
   locating both method declarations and method definitions. Moreover,
   methods that are declared but not defined simply fail when called
   (as per :term:`closed-world assumption`).

.. _nomenclature_java:

Java nomenclature
-----------------

There are several Java glossaries available on the Internet. The list
that follows relates the most commonly used Java terms with their
Logtalk equivalents.

**abstract class**
   Logtalk uses an *operational* definition of abstract class: any class
   that does not inherit a method for creating new instances is an
   abstract class. I.e. there is no ``abstract`` keyword in Logtalk.

**abstract method**
   In Logtalk, you may simply declare a method (:term:`predicate`) in a
   class without defining it, leaving its definition to some descendant
   subclass.

**assertion**
   There is no ``assertion`` keyword in Logtalk. Assertions are
   supported using Logtalk compilation hooks and developer tools.

**class**
   Logtalk objects can play the role of classes, instances, or protocols
   (depending on their relations with other objects).

**extends**
   There is no ``extends`` keyword in Logtalk. Class inheritance is
   indicated using *specialization relations*. Moreover, the *extends
   relation* is used in Logtalk to indicate protocol, category, or
   prototype extension.

**interface**
   Logtalk uses the term :term:`protocol` with similar meaning. But note
   that Logtalk objects and categories declared as implementing a protocol
   are not required to provide definitions for the declared predicates
   (:term:`closed-world assumption`).

**callback method**
   Logtalk supports :ref:`event-driven programming <events_events>`,
   the most common usage context of callback methods. Callback methods
   can also be implemented using :term:`meta-predicates <meta-predicate>`.

**constructor**
   There are no special methods for creating new objects in Logtalk.
   Instead, Logtalk provides a built-in predicate, :ref:`predicates_create_object_4`,
   which is often used to define more sophisticated object creation
   predicates.

**final**
   There is no ``final`` keyword in Logtalk. Predicates can always be
   redeclared and redefined in subclasses (and instances!).

**inner class**
   Inner classes are not supported in Logtalk.

**instance**
   In Logtalk, an instance can be either created dynamically at runtime
   or defined statically in a source file in the same way as classes.

**method**
   Logtalk uses the term :term:`predicate` interchangeably with the term
   *method*.

**method call**
   Logtalk usually uses the expression *message sending* for method
   calls, true to its Smalltalk heritage.

**method signature**
   Logtalk selects the method/predicate to execute in order to answer a
   method call based only on the method name and number of arguments.
   Logtalk (and Prolog) are not typed languages in the same sense as Java.

**package**
   There is no concept of packages in Logtalk. All Logtalk entities
   (objects, protocols, categories) share a single namespace. But
   Logtalk does support a concept of :term:`library` that allows
   grouping of entities whose source files share a common path prefix.

**reflection**
   Logtalk features a *white box* API supporting *structural* reflection
   about :ref:`entity contents <enumerating_entity_property_predicates>`,
   a *black box* API supporting *behavioral* reflection about
   :ref:`object protocols <reflection_methods>`, and an
   :ref:`events <event_handling_predicates>` API for reasoning about messages
   exchanged at runtime.

**static**
   There is no ``static`` keyword in Logtalk. See the entries below on
   *static method* and *static variable*.

**static method**
   Static methods may be implemented in Logtalk by using a :term:`metaclass`
   for the class and defining the static methods in the metaclass. I.e. static
   methods are simply instance methods of the class metaclass.

**static variable**
   Static variables are *shared instance variables* and can simply be both
   declared and defined in a class. The built-in database methods can be
   used to implement destructive updates if necessary by accessing and
   updated a single clause of a dynamic predicate stored in the class.

**super**
   Instead of a ``super`` keyword, Logtalk provides a super operator and
   control construct, :ref:`control_call_super_1`, for calling overridden
   methods.

**synchronized**
   Logtalk supports :ref:`multi-threading programming <threads_threads>` in
   selected Prolog compilers, including a :ref:`directives_synchronized_1`
   predicate directive. Logtalk allows you to synchronize a predicate or a
   set of predicates using per-predicate or per-predicate-set *mutexes*.

**this**
   Logtalk uses the built-in context method :ref:`methods_self_1` for retrieving
   the instance that received the message being processed. Logtalk also provides
   a :ref:`methods_this_1` method but for returning the class containing the
   method being executed. Why the name clashes? Well, the notion of :term:`self`
   was inherited from Smalltalk, which predates C++.

.. _nomenclature_python:

Python nomenclature
-------------------

The list that follows relates the commonly used Python concepts with
their Logtalk equivalents.

**abstract class**
   Logtalk uses a different definition of abstract class: a class that
   does not inherit a method for creating new instances. Notably, the
   presence of *abstract methods* (i.e. predicates that are declared
   but not defined) does not make a class abstract.

**abstract method**
   Logtalk uses the term *predicate* interchangeably with *method*. Predicates
   can be declared without being also defined in an object (or category).

**class**
   Logtalk objects can play the role of classes, instances, or protocols
   (depending on their relations with other objects).

**dictionary**
   There is no native, built-in associative data type. But the library
   provides several implementations of a dictionary protocol.

**function**
   The closest equivalent is a predicate defined in ``user``, a pseudo-object
   for predicates not defined in regular objects, and thus callable from
   anywhere without requiring a scope directive.

**function object**
   Predicates calls (goals) can be passed or returned from other predicates
   and unified with other terms (e.g. variables).

**import path**
   Logtalk uses the term *library* to refer to a directory of source files
   and supports defining aliases (symbolic names) to library paths to abstract
   the actual locations.

**lambda**
   Logtalk natively supports lambda expressions.

**list**
   Lists are compound terms with native syntax support.

**list comprehensions**
   There is no native, built-in support for list comprehensions. But the
   standard ``findall/3`` predicate can be used to construct a list by
   calling a goal that generates the list elements.

**loader**
   Logtalk uses the term *loader* to refer to source files whose main or
   sole purpose is to load other source files.

**loop**
   There are no native loop control constructs based on a counter. But the
   library provides implementations of several loop predicates.

**metaclass**
   Logtalk objects play the role of metaclasses when instantiated by
   objects that play the role of classes.

**method**
   Logtalk uses the terms *method* and *predicate* interchangeably.
   Predicates can be defined in objects (and categories). The value
   of *self* is implicit unlike in Python where it is the first parameter
   of any method.

**method resolution order**
   Logtalk uses a depth-first algorithm to lookup method (predicate)
   declarations and definitions. It's possible to use predicate *aliases*
   to access predicate declarations and definitions other than the first
   ones found by the lookup algorithm.

**object**
   Objects are first-class entities that can play multiple roles, including
   prototype, class, instance, and metaclass.

**package**
   Logtalk uses the term *library* to refer to a directory of source files
   defining objects, categories, and protocols.

**set**
   There is no native, built-in set type. But the library provides set
   implementations.

**string**
   The interpretation of text between double-quotes depends on the
   ``double_quotes`` flag. Depending on this flag, double-quoted text
   can be interpreted as a list of characters, a list of character codes,
   or an atom. Some backend Prolog compilers allow double-quoted text
   to be interpreted as a string in the Python sense.

**tuple**
   Compound terms can be used to represent tuples of any complexity.

**variable**
   Logtalk works with *logical variables*, which are close to the
   mathematical concept of variables and distinct from variables in
   imperative or imperative-based OOP languages where they are
   symbolic names for memory locations. Logical variables can be
   *unified* with any term, including other variables.

**while loop**
   The built-in ``forall/2`` predicate implements a *generate-and-test*
   loop.
