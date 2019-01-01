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


.. _features_features:

Main features
=============

Several years ago, I decided that the best way to learn object-oriented
programming was to build my own object-oriented language. Prolog being
always my favorite language, I chose to extend it with object-oriented
capabilities. Strong motivation also come from my frustration with
Prolog shortcomings for writing large applications. Eventually this work
has led to the Logtalk programming language as its know today. The first
system to use the name Logtalk appeared in February 1995. At that
time, Logtalk was mainly an experiment in computational reflection with
a rudimentary runtime and no compiler. Based on feedback by users and on
the author subsequent work, the name was retained and Logtalk as created
as a full programming language focusing on using object-oriented concepts
for code encapsulation and reuse. Development started on January 1998 with
the first public alpha version released in July 1998. The first stable
release (2.0) was published in February 1999. Development of the third
generation of Logtalk started in 2012 with the first public alpha version
in August 2012 and the first stable release (3.0.0) in January 2015.

Logtalk provides the following features:

.. _features_logic:

Integration of logic and object-oriented programming
----------------------------------------------------

   Logtalk tries to bring together the main advantages of these two
   programming paradigms. On one hand, the object orientation allows us
   to work with the same set of entities in the successive phases of
   application development, giving us a way of organizing and
   encapsulating the knowledge of each entity within a given domain. On
   the other hand, logic programming allows us to represent, in a
   declarative way, the knowledge we have of each entity. Together,
   these two advantages allow us to minimize the distance between an
   application and its problem domain, turning the writing and
   maintenance of programming easier and more productive.

   From a pragmatic perspective, Logtalk objects provide Prolog with
   the possibility of defining several namespaces, instead of the
   traditional Prolog single database, addressing some of the needs
   of large software projects.

.. _features_events:

Integration of event-driven and object-oriented programming
-----------------------------------------------------------

   Event-driven programming enables the building of reactive systems,
   where computing which takes place at each moment is a result of the
   observation of occurring events. This integration complements
   object-oriented programming, in which each computing is initiated by
   the explicit sending of a message to an object. The user dynamically
   defines what events are to be observed and establishes monitors for
   these events. This is specially useful when representing
   relationships between objects that imply constraints in the state of
   participating objects [Rumbaugh87]_, [Rumbaugh88]_, [Fornarino_et_al_89]_,
   [Razek92]_. Other common uses are
   reflective applications like code debugging or profiling [Maes87]_.
   Predicates can be implicitly
   called when a spied event occurs, allowing programming solutions
   which minimize object coupling. In addition, events provide support
   for behavioral reflection and can be used to implement the concepts
   of *pointcut* and *advice* found on Aspect-Oriented Programming.

.. _features_categories:

Support for component-based programming
---------------------------------------

   Predicates can be encapsulated inside *categories* which can be
   imported by any object, without any code duplication and irrespective
   of object hierarchies. A category is a first-class encapsulation
   entity, at the same level as objects and protocols, which can be used
   as a component when building new objects. Thus, objects may be
   defined through composition of categories, which act as fine-grained
   units of code reuse. Categories may also extend existing objects.
   Categories can be used to implement *mixins* and *aspects*.
   Categories allows for code reuse between non-related objects,
   independent of hierarchy relations, in the same vein as protocols
   allow for interface reuse.

.. _features_both:

Support for both prototype and class-based systems
--------------------------------------------------

   Almost any (if not all) object-oriented languages available today are
   either class-based or prototype-based [Lieberman86]_, with a strong predominance
   of class-based languages. Logtalk provides support for both hierarchy
   types. That is, we can have both prototype and class hierarchies in
   the same application. Prototypes solve a problem of class-based
   systems where we sometimes have to define a class that will have only
   one instance in order to reuse a piece of code. Classes solves a dual
   problem in prototype based systems where it is not possible to
   encapsulate some code to be reused by other objects but not by the
   encapsulating object. Stand-alone objects, that is, objects that do
   not belong to any hierarchy, are a convenient solution to encapsulate
   code that will be reused by several unrelated objects.

.. _features_multiple:

Support for multiple object hierarchies
---------------------------------------

   Languages like Smalltalk-80 [Goldberg83]_, Objective-C [Cox86]_ and Java 
   [Joy_et_al_00]_ define a single hierarchy rooted
   in a class usually named ``Object``. This makes it easy to ensure
   that all objects share a common behavior but also tends to result in
   lengthy hierarchies where it is difficult to express objects which
   represent exceptions to default behavior. In Logtalk we can have
   multiple, independent, object hierarchies. Some of them can be
   prototype-based while others can be class-based. Furthermore,
   stand-alone objects provide a simple way to encapsulate utility
   predicates that do not need or fit in an object hierarchy.

.. _features_interface:

Separation between interface and implementation
-----------------------------------------------

   This is an expected (should we say standard ?) feature of almost any
   modern programming language. Logtalk provides support for separating
   interface from implementation in a flexible way: predicate directives
   can be contained in an object, a category or a protocol (first-order
   entities in Logtalk) or can be spread in both objects, categories and
   protocols.

.. _features_inheritance:

Private, protected and public inheritance
-----------------------------------------

   Logtalk supports private, protected and public inheritance in a
   similar way to C++ [Stroustrup86]_, enabling us to restrict
   the scope of inherited, imported or implemented predicates (by
   default inheritance is public).

.. _features_predicates:

Private, protected and public object predicates
-----------------------------------------------

   Logtalk supports data hiding by implementing private, protected and
   public object predicates in a way similar to C++ [Stroustrup86]_. Private predicates can
   only be called from the container object. Protected predicates can be
   called by the container object or by the container descendants.
   Public predicates can be called from any object.

.. _features_parametric:

Parametric objects
------------------

   Object names can be compound terms (instead of atoms), providing a
   way to parameterize object predicates. Parametric objects are
   implemented in a similar way to ``L&O`` [McCabe92]_, ``OL(P)``
   [Fromherz93]_ or ``SICStus Objects`` [SICStus95]_ (however, access to
   parameter values is done via a built-in method instead of making the
   parameters scope global over the whole object). Parametric objects
   allows us to treat any predicate clause as defining an
   *instantiation* of a parametric object. Thus, a parametric object
   allows us to encapsulate and associate any number of predicates with
   a compound term.

.. _features_threading:

High level multi-threading programming support
----------------------------------------------

   High level multi-threading programming is available when running
   Logtalk with selected backend Prolog compilers, allowing objects to
   support both synchronous and asynchronous messages. Logtalk allows
   programmers to take advantage of modern multi-processor and
   multi-core computers without bothering with the details of creating
   and destroying threads, implement thread communication, or
   synchronizing threads.

.. _features_learning:

Smooth learning curve
---------------------

   Logtalk has a smooth learning curve, by adopting standard Prolog
   syntax and by enabling an incremental learning and use of most of its
   features.

.. _features_compatibility:

Compatibility with most Prolog systems and the ISO standard
-----------------------------------------------------------

   The Logtalk system has been designed to be compatible with most
   Prolog compilers and, in particular, with the ISO Prolog standard
   [ISO95]_. It runs in almost any computer system with a modern Prolog
   compiler.

.. _features_performance:

Performance
-----------

   The current Logtalk implementation works as a trans-compiler: Logtalk
   source files are first compiled to Prolog source files, which are
   then compiled by the chosen Prolog compiler. Therefore, Logtalk
   performance necessarily depends on the backend Prolog compiler. The
   Logtalk compiler preserves the programmers choices when writing
   efficient code that takes advantage of tail recursion and
   first-argument indexing.

   As an object-oriented language, Logtalk can use both :term:`static binding`
   and :term:`dynamic binding` for matching messages and methods. Furthermore,
   Logtalk entities (objects, protocols, and categories) are
   independently compiled, allowing for a very flexible programming
   development. Entities can be edited, compiled, and loaded at runtime,
   without necessarily implying recompilation of all related entities.

   When dynamic binding is used, the Logtalk runtime engine implements
   caching of method lookups (including messages to *self* and *super*
   calls), ensuring a performance level close to what could be achieved
   when using static binding.

.. _features_scope:

Logtalk scope
-------------

Logtalk, being a superset of Prolog, shares with it the same preferred
areas of application but also extends them with those areas where
object-oriented features provide an advantage compared to plain Prolog.
Among these areas we have:

**Logic and object-oriented programming teaching and researching**
   Logtalk smooth learning curve, combined with support for both
   prototype and class-based programming, protocols, components or
   aspects via category-based composition, and other advanced
   object-oriented features allow a smooth introduction to
   object-oriented programming to people with a background in Prolog
   programming. The distribution of Logtalk source code using an
   open-source license provides a framework for people to learn and then
   modify to try out new ideas on object-oriented programming research.
   In addition, the Logtalk distribution includes plenty of programming
   examples that can be used in the classroom for teaching logic and
   object-oriented programming concepts.

**Structured knowledge representations and knowledge-based systems**
   Logtalk objects, coupled with event-driven programming features,
   enable easy implementation of frame-like systems and similar
   structured knowledge representations.

**Blackboard systems, agent-based systems, and systems with complex object relationships**
   Logtalk support for event-driven programming can provide a basis for
   the dynamic and reactive nature of blackboard type applications.

**Highly portable applications**
   Logtalk is compatible with most modern Prolog systems that support
   official and de facto standards. Used as a way to provide Prolog with
   namespaces, it avoids the porting problems of most Prolog module
   systems. Platform, operating system, or compiler specific code can be
   isolated from the rest of the code by encapsulating it in objects
   with well-defined interfaces.

**Alternative to a Prolog module system**
   Logtalk can be used as an alternative to a Prolog compiler module
   system. Most Prolog applications that use modules can be converted
   into Logtalk applications, improving portability across Prolog
   systems and taking advantage of the stronger encapsulation and reuse
   framework provided by Logtalk object-oriented features.

**Integration with other programming languages**
   Logtalk support for most key object-oriented features helps users
   integrating Prolog with object-oriented languages like C++, Java, or
   Smalltalk by facilitating a high-level mapping between the two
   languages.
