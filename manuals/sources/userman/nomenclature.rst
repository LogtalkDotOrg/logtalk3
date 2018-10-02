
.. _nomenclature_nomenclature:

============
Nomenclature
============

Depending on your Object-oriented Programming background (or lack of
it), you may find Logtalk nomenclature either familiar or at odds with
the terms used in other languages. In addition, being a superset of
Prolog, terms such as *predicate* and *method* are often used
interchangeably. Logtalk inherits most of its nomenclature from
Smalltalk, arguably (and somehow sadly) not the most popular OOP
language nowadays. In this section, we map nomenclatures from popular
OOP languages such as C++ and Java to the Logtalk nomenclature.

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
   *interfaces*/*protocols*, which are often a better way to provide the
   functionality of C++ abstract classes.

**base class**
   Logtalk uses the term *superclass* with the same meaning.

**data member**
   Logtalk uses *predicates* for representing both behavior and data.

**constructor function**
   There are no special methods for creating new objects in Logtalk.
   Instead, Logtalk provides a built-in predicate, ``create_object/4``,
   which can be used as a building block to define more sophisticated
   object creation predicates.

**derived class**
   Logtalk uses the term *subclass* with the same meaning.

**destructor function**
   There are no special methods for deleting new objects in Logtalk.
   Instead, Logtalk provides a built-in predicate, ``abolish_object/1``,
   which is often used to define more sophisticated object deletion
   predicates.

**friend function**
   Not supported in Logtalk. Nevertheless, see the manual section on
   *meta-predicates.*

**instance**
   In Logtalk, an instance can be either created dynamically at runtime
   or defined statically in a source file in the same way as classes.

**member**
   Logtalk uses the term predicate.

**member function**
   Logtalk uses predicates for representing both behavior and data.

**namespace**
   Logtalk does not support multiple identifier namespaces. All Logtalk
   entity identifiers share the same namespace (Logtalk entities are
   objects, categories, and protocols).

**nested class**
   Logtalk does not support nested classes.

**template**
   Logtalk supports *parametric objects*, which allows you to get the
   similar functionality of templates at runtime.

**this**
   Logtalk uses the built-in context method ``self/1`` for retrieving
   the current instance. Logtalk also provides a ``this/1`` method but
   for returning the class containing the method being executed. Why the
   name clashes? Well, the notion of *self* was inherited from
   Smalltalk, which predates C++.

**virtual member function**
   There is no ``virtual`` keyword in Logtalk. Any inherited or imported
   predicate can be redefined (either overridden or specialized).
   Logtalk can use static binding or dynamic binding for locating both
   method declarations and method definitions. Moreover, methods that
   are declared but not defined simply fail when called.

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
   In Logtalk, you may simply declare a method (predicate) in a class
   without defining it, leaving its definition to some descendant
   sub-class.

**assertion**
   There is no ``assertion`` keyword in Logtalk. Assertions are
   supported using Logtalk compilation hooks and developer tools.

**extends**
   There is no ``extends`` keyword in Logtalk. Class inheritance is
   indicated using *specialization* relations. Moreover, the *extends*
   relation is used in Logtalk to indicate protocol, category, or
   prototype extension.

**interface**
   Logtalk uses the term *protocol* with the same meaning.

**callback method**
   Logtalk supports *event-driven programming*, the most common use
   context of callback methods.

**class method**
   Class methods may be implemented in Logtalk by using a metaclass for
   the class and defining the class methods in the metaclass. I.e. class
   methods are simply instance methods of the class metaclass.

**class variable**
   True class variables may be implemented in Logtalk by using a
   metaclass for the class and defining the class variables in the
   class. I.e. class variables are simply instance variables of the
   class metaclass. Shared instance variables may be implemented by
   using the built-in database methods (which can be used to implement
   variable assignment) to access and updated a single occurrence of the
   variable stored in the class (there is no ``static`` keyword in
   Logtalk).

**constructor**
   There are no special methods for creating new objects in Logtalk.
   Instead, Logtalk provides a built-in predicate, ``create_object/4``,
   which is often used to define more sophisticated object creation
   predicates.

**final**
   There is no ``final`` keyword in Logtalk; methods may always be
   redefined in subclasses (and instances!).

**inner class**
   Inner classes are not supported in Logtalk.

**instance**
   In Logtalk, an instance can be either created dynamically at runtime
   or defined statically in a source file in the same way as classes.

**method**
   Logtalk uses the term *predicate* interchangeably with the term
   *method*.

**method call**
   Logtalk usually uses the expression *message sending* for method
   calls, true to its Smalltalk heritage.

**method signature**
   Logtalk selects the method/predicate to execute in order to answer a
   method call based only on the method name (functor) and number of
   arguments (arity). Logtalk (and Prolog) are not typed languages in
   the same sense as Java.

**reflection**
   Logtalk supports both *structural reflection* (using a set of
   built-in predicates and built-in methods) and *behavioral reflection*
   (using event-driven programming).

**static**
   There is no ``static`` keyword in Logtalk. See the entries on *class
   methods* and *class variables*.

**super**
   Instead of a ``super`` keyword, Logtalk provides a *super* operator,
   ``^^/1``, for calling overridden methods.

**synchronized**
   Logtalk supports *multi-threading programming* in selected Prolog
   compilers, including a ``synchronized/1`` predicate directive.
   Logtalk allows you to synchronize a predicate or a set of predicates
   using per-predicate or per-predicate-set *mutexes*.

**this**
   Logtalk uses the built-in context method ``self/1`` for retrieving
   the current instance. Logtalk also provides a ``this/1`` method but
   for returning the class containing the predicate clause being
   executed. Why the name clashes? Well, the notion of *self* was
   inherited from Smalltalk, which predates Java.
