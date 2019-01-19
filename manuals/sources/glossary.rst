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


Glossary
========

.. glossary::
   :sorted:

   ancestor
      A :term:`class` or a parent :term:`prototype` that contributes (via
      inheritance) to the definition of an object. For class-based hierarchies,
      the ancestors of an instance are its class(es) and all the superclasses
      of its class(es). For prototype-based hierarchies, the ancestors of
      a prototype are its parent(s) and the ancestors of its parent(s).

   category
      A set of predicates directives and clauses that can be (virtually)
      imported by any object. Categories support composing objects using
      fine-grained units of code reuse and also :term:`hot patching` of
      existing objects. A category should be functionally-cohesive,
      defining a single functionality.

   complementing category
      A category used for :term:`hot patching` an existing object (or a
      set of objects).

   parametric category
      See :term:`parametric entity`.

   class
      An :term:`object` that specializes another object, interpreted as its
      superclass. Classes define the common predicates of a set of objects
      that instantiates it. An object can also be interpreted as a class
      when it instantiates itself.

   abstract class
      A :term:`class` that cannot be instantiated. Usually used to contain
      common predicates that are inherited by other classes.

   metaclass
      The :term:`class` of a class, when interpreted as an instance.
      Metaclass instances are themselves classes.

   subclass
      A :term:`class` that is a specialization, direct or indirectly, of
      another class.

   superclass
      A :term:`class` from which another class is a specialization
      (directly or indirectly via another class).

   closed-world assumption
      The assumption that what cannot be proved true is false. Therefore,
      sending a message corresponding to a *declared* but not *defined*
      predicate, or calling a declared predicate with no clauses, fails.
      But messages or calls to undeclared predicates generate an error.

   directive
      A source file term that affects the interpretation of source code.
      Directives use the ``(:-)/1`` prefix operator as functor.

   entity directive
      A directive that affects how Logtalk entities (:term:`objects <object>`,
      :term:`categories <category>`, or :term:`protocols <protocol>`) are
      used or compiled.

   predicate directive
      A directive that affects how predicates are called or compiled.

   source file directive
      A directive that affects how a source file is compiled.

   encapsulation
      The hiding of an object implementation. This promotes software reuse
      by isolating the object clients from its implementation details.
      Encapsulation is enforced in Logtalk by using
      :term:`predicate scope directives <predicate scope directive>`.

   entity
      Generic name for Logtalk compilation units: :term:`objects <object>`,
      :term:`categories <category>`, and :term:`protocols <protocol>`.
      Entities share a single namespace (i.e. entity
      :term:`identifiers <identity>` must be unique).

   parametric entity
      An :term:`object` or :term:`category` whose :term:`identifier <identity>`
      is a compound term possibly containing free variables that can be used
      to parameterize the entity predicates. Parameters are *logical variables*
      implicitly shared by all the entity clauses. Note that the identifier
      of a parametric entity is its functor, irrespective of the possible
      values of its arguments (e.g. ``foo(bar)`` and ``foo(baz)`` are
      different parameterizations of the same parametric entity, ``foo/1``).

   event
      The sending of a :term:`message` to an object. An event can be
      expressed as an ordered tuple: ``(Event, Object, Message, Sender)``.
      Logtalk distinguish between the sending of a message — ``before``
      event — and the return of control to the sender — ``after`` event.

   grammar rule
      An alternative notation for predicates used to parse or generate
      sentences on some language. This notation hides the arguments used to
      pass the sequences of tokens being processed, thus simplifying the
      representation of grammars. Grammar rules are represented using as
      functor the infix operator ``(-->)/2`` instead of the ``(:-)/2``
      operator used with predicate clauses.

   grammar rule non-terminal
      A syntactic category of words or phrases. A non-terminal is
      identified by its *non-terminal indicator*, i.e. by its name and
      number of arguments using the notation ``Name//Arity``.

   grammar rule terminal
      A word or basic symbol of a language.

   identity
      Property of an entity that distinguishes it from every other entity.
      The identifier of an entity is its functor (i.e. its name and arity),
      which must be unique. Object and :term:`category` identifiers can be
      atoms or compound terms. Protocol identities must be atoms. All Logtalk
      entities (objects, protocols, and categories) share the same namespace.

   inheritance
      An entity inherits predicate directives and clauses from related
      entities. In the particular case of objects, when an object extends
      other object, we have prototype-based inheritance. When an object
      specializes or instantiates another object, we have class-based
      inheritance.

   private inheritance
      All public and protected predicates are inherited as private
      predicates.

   protected inheritance
      All public predicates are inherited as protected. No change for
      protected or private predicates.

   public inheritance
      All inherited predicates maintain their declared scope.

   instance
      An object that instantiates another object, interpreted as its class.

   instantiation
      The process of creating a new class instance. In Logtalk, this does
      not necessarily imply dynamic creation of an object at runtime; an
      instance may also be defined as a static object in a source file.

   library
      A directory containing source files. The library name can be used as
      an alias to the directory path when compiling and loading source
      files using the notation ``library_name('source_file_relative_path')``.
      Library names and their paths are defined using the
      :ref:`predicates_logtalk_library_path_2` predicate.

   module
      A Prolog entity characterized by an identity and a set of predicate
      directives and clauses. Prolog modules are usually static although
      some Prolog systems allow the creation of dynamic modules at runtime.
      Prolog modules can be seen as prototypes.

   message
      A query sent to an object. In logical terms, a message can be seen as
      a request for proof construction using an object database and the
      databases of related entities.

   message to self
      A message sent to the object that received the original message under
      processing. Messages to self require :term:`dynamic binding` as the
      value of self is only know at runtime.

   meta-interpreter
      A program capable of running other programs written in the same
      language.

   method
      The predicate definition used to answer a message sent to an object.
      Logtalk supports both :term:`static binding` and dynamic binding to
      find which method to run to answer a message.

   abstract method
      A :term:`method` implementing an algorithm whose step corresponds
      to calls to methods defined in the descendants of the object (or
      :term:`category`) containing it.

   built-in method
      A predefined :term:`method` that can be called from within any object
      or :term:`category`. Built-in methods cannot be redefined.

   singleton method
      A :term:`method` defined in an :term:`instance` itself. Singleton
      methods are supported in Logtalk and can also be found in other
      object-oriented programming languages.

   template method
      See :term:`abstract method`.

   monitor
      Any object, implementing the ``monitoring`` built-in protocol, that
      is notified by the runtime when a spied event occurs. The spied
      :term:`events <event>` can be set by the monitor itself or by any
      other object.

   object
      An entity characterized by an :term:`identity` and a set of predicate
      directives and clauses. Logtalk objects can be either static or
      dynamic. Logtalk objects can play the *role* of classes, instances,
      or prototypes. The role or roles an object plays are a function of
      its relations with other objects.

   object database
      The set of predicates locally defined inside an object.

   doclet object
      An object specifying the steps necessary to (re)generate the API
      documentation for a project. See the ``doclet`` and ``lgtdoc``
      tools for details.

   hook object
      An object, implementing the ``expanding`` built-in protocol,
      defining term- and goal-expansion predicates, used in the compilation
      of Logtalk or Prolog source files. A hook object can be specified
      using the :ref:`hook <flag_hook>` compiler flag. It can also be
      specified using a :ref:`directives_set_logtalk_flag_2` directive in
      the source files to be expanded.

   parametric object
      See :term:`parametric entity`.

   parametric object proxy
      A compound term (usually represented as a plain Prolog fact) with
      the same name and number of arguments as the identifier of a parametric
      object.

   parameter
      An argument of a parametric object or a parametric category identifier.
      Parameters are *logical variables* implicitly shared by all the entity
      predicate clauses.

   parameter variable
      A variable used as parameter in a parametric object or a parametric
      category using the syntax ``_VariableName_``. Occurrences of
      parameter variables in entity clauses are implicitly unified with the
      corresponding entity parameters.

   parent
      A prototype that is extended by another prototype.

   predicate
      Predicates describe what is true about the application domain. A
      predicate is identified by its *predicate indicator*, i.e. by its
      name and number of arguments using the notation ``Name/Arity``.

   predicate alias
      An alternative functor (``Name/Arity``) for a predicate. Predicate
      aliases can be defined for any inherited predicate using the ``alias/2``
      directive and for predicates listed in ``uses/2`` and ``use_module``
      directives. Predicate aliases can be used to solve inheritance conflicts
      and to improve code clarity by using alternative names that are more
      meaningful in the calling context.

   built-in predicate
      A predefined predicate that can be called from anywhere. Built-in
      predicates can be redefined within objects and
      :term:`categories <category>`.

   coinductive predicate
      A predicate whose calls are proved using greatest fixed point
      semantics. Coinductive predicates allows reasoning about
      infinite rational entities such as cyclic terms and ω-automata.

   local predicate
      A predicate that is defined in an object (or in a :term:`category`) but
      that is not listed in a scope directive. These predicates behave
      like private predicates but are invisible to the reflection
      :term:`built-in methods <built-in method>`. Local predicates are usually
      auxiliary predicates and only relevant to the entity where they are defined.

   meta-argument
      A predicate argument that is called as a goal, used as a closure
      to construct a goal that will be called, or that is handled in a
      way that requires awareness of the predicate calling context.

   meta-predicate
      A predicate with one or more :term:`meta-arguments <meta-argument>`.
      For example, :ref:`methods_call_N` and :ref:`methods_findall_3` are
      built-in meta-predicates.

   predicate scope directive
      A directive that declares a predicate by specifying its visibility
      (public, protected, or private).

   predicate scope container
      The object that inherits a predicate declaration from an imported
      :term:`category` or an implemented :term:`protocol`.

   private predicate
      A predicate that can only be called from the object that contains
      the scope directive.

   protected predicate
      A predicate that can only be called from the object containing the
      scope directive or from an object that inherits the predicate.

   public predicate
      A predicate that can be called from any object.

   primary predicate declaration
      See :term:`multifile predicate`.

   multifile predicate
      A predicate whose clauses can be defined in multiple
      :term:`entities <entity>`. The object or category holding the
      directive without an entity prefix qualifying the predicate holds
      the multifile predicate *primary declaration*, which consists of
      both a :term:`scope directive <predicate scope directive>` and a
      ``multifile/1`` directive for the predicate.

   synchronized predicate
      A synchronized predicate is protected by a mutex ensuring that, in
      a multi-threaded application, it can only be called by a single
      thread at a time.

   visible predicate
      A predicate that is within scope, a locally defined predicate, a
      :term:`built-in method`, a Logtalk built-in predicate, or a Prolog
      built-in predicate.

   profiler
      A program that collects data about other program performance.

   protocol
      An entity that contains predicate declarations. A predicate is
      declared using a scope directive. It may be further specified by
      additional predicate directives. Protocols support the separation
      between interface and implementation, can be implemented by both
      objects and categories, and can be extended by other protocols. A
      protocol should be functionally-cohesive, specifying a single
      functionality.

   prototype
      A self-describing object that may extend or be extended by other
      objects. An object with no instantiation or specialization relations
      with other objects is always interpreted as a prototype.

   self
      The object that received the message under processing.

   sender
      An object that sends a message to other object. When a message is
      sent from within a :term:`category`, the *sender* is the object
      importing the category.

   super call
      Call of an inherited (or imported) predicate definition. Mainly used
      when redefining an inherited (or imported) predicate to call the
      overridden definition while making additional calls. Super calls preserve
      self and may require :term:`dynamic binding` if the predicate is dynamic.

   specialization
      A :term:`class` is specialized by defining a new class that inherit its
      predicates and possibly add new ones.

   source file
      A text file defining Logtalk and/or Prolog code. Multiple Logtalk
      entities may be defined in a single source file. Plain rolog code
      may be intermixed with Logtalk entity definitions.

   adapter file
      A Prolog source file defining a minimal abstraction layer between
      the Logtalk compiler/runtime and a specific backend Prolog
      compiler.

   doclet file
      A source file whose main purpose is to generate documentation for
      e.g. a library or an application.

   loader file
      A source file whose main purpose is to load a set of source files.

   settings file
      A source file, compiled and loaded at Logtalk startup, mainly
      defining default values for compiler flags that override the
      defaults found on the backend Prolog compiler
      :term:`adapter files <adapter file>`.

   tester file
      A source file whose main purpose is to load and a run a set of
      unit tests.

   this
      The object that contains the predicate clause under execution. When
      the predicate clause is contained in a :term:`category`, *this* is a
      reference to the object importing the category for which the
      predicate clause is being executed.

   dynamic binding
      Runtime lookup of a predicate declaration and definition to verify
      the validity of a message (or a :term:`super call`) and find the predicate
      definition that will be used to answer the message (or the super call).
      Also known as *late binding*. See also :term:`static binding`.

   static binding
      Compile time lookup of a predicate declaration and definition when
      compiling a message sending call (or a :term:`super call`). Dynamic
      binding is used whenever static binding is not possible (e.g. due to
      the predicate being dynamic or due to lack of enough information).
      Also known as *early binding*. See also :term:`dynamic binding`.

   lambda expression
      A compound term that can be used in place of a goal or closure
      meta-argument and that abstracts a predicate definition by listing
      its variables and a callable term that implements the definition.
      Lambda expressions help avoiding the need of naming and defining
      auxiliary predicates.

   lambda parameter
      A term (usually a variable or a non-ground compound term) that is
      local to a :term:`lambda expression`. All lambda parameters must
      be explicitly enumerated in a lambda expression.

   lambda free variable
      A variable that is global to a :term:`lambda expression`. All
      used global variables must be explicitly listed in a lambda
      expression.

   hot patching
      The act of fixing entity directives and predicates or adding new
      entity directives and predicates to loaded entities in a running
      application without requiring access to the entities source code
      or restarting the application.

   threaded engine
      A computing thread running a goal whose solutions can be lazily and
      concurrently computed and retrieved. A threaded engine also supports
      a term queue that allows passing arbitrary terms to the engine. This
      queue can be used to pass e.g. data and new goals to the engine.
