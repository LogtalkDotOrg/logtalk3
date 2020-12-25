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


.. _declarative_declarative:

Declarative object-oriented programming
=======================================

Logtalk is a *declarative object-oriented logic programming language*. This means
that Logtalk shares key concepts with other object-oriented programming languages
but abstracts and reinterprets these concepts in the context of declarative logic
programming.

The key concepts in *declarative* object-oriented programming are *encapsulation*
and *reuse patterns*. Notably, the concept of *mutable state*, which is an *imperative*
concept, is not a significant concept in *declarative* object-oriented programming.
Declarative object-oriented programming concepts can be materialized in both
logic and functional languages. In this section, we focus only in declarative
object-oriented logic programming.

The first critical generalization of object-oriented programming concepts is the
concept of object itself. What an object encapsulates depends on the *base programming
paradigm* where we apply object-oriented programming concepts. When these concepts
are applied to an imperative language, where mutable state and destructive assignment
are central, objects naturally encapsulate and abstract mutable state, providing
disciplined access and modification. When these concepts are applied to a declarative
logic language such as Prolog, objects naturally encapsulate predicates. Therefore, an
object can be seen as a *theory*, expressed by a set of related predicates. Theories
are usually static and thus Logtalk objects are static by default. This contrasts with
imperative object-oriented languages where usually classes are static and objects are
dynamic. This view of an object as a set of predicates also forgo a distinction
between *data* and *procedures* that is central to imperative object-oriented
languages but moot in declarative, homoiconic logic languages.

The second critical generalization concerns the relation between objects and other
entities such as protocols (interfaces) and ancestor objects. The idea is that entity
relations define *reuse patterns* and the *roles* played by the participating entities.
A common reuse pattern is *inheritance*. In this case, an entity inherits, and thus
reuses, resources from an ancestor entity. In a reuse pattern, each participating entity
plays a specific *role*. The same entity, however, can play multiple roles
depending on its relations with other entities. For example, an object can play
the role of a class for its instances, the role of a subclass for its superclasses,
and the role of an instance for its metaclass. Another common reuse pattern is
*protocol implementation*. In this case, an object implementing a protocol reuses
its predicate declarations by providing an implementation for those predicates and
exposing those predicates to its clients. An essential consequence of this
generalization is that protocols, objects, and categories are first-class entities
while e.g. *prototype*, *parent*, *class*, *instance*, *metaclass*, *subclass*,
*superclass*, or *ancestor* are just *roles* that an object can play. Moreover, a
language can provide multiple reuse patterns instead of selecting a set of patterns
and supporting this set as a design choice that excludes other reuse patterns. For
example, most imperative object-oriented languages are either class-based or
prototype-based. In contrast, Logtalk naturally supports both classes and prototypes
by providing the corresponding reuse patterns using objects as first-class entities
capable of playing multiple roles.
