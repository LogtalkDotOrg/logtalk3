.. _library_tutor:

``tutor``
=========

This tool adds explanations and suggestions to selected compiler warning
and error messages. It's specially useful for new users not yet familiar
with the compiler and runtime warning and error messages.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#tutor <../../docs/library_index.html#tutor>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(tutor(loader)).

Usage
-----

Simply load the tool at startup (e.g. from a settings file). As an
example, with this tool loaded, instead of terse compiler warnings such
as:

::

   *     No matching clause for goal: baz(a)
   *       while compiling object main_include_compiler_warning
   *       in file logtalk/examples/errors/include_compiler_warning.lgt between lines 37-38
   *     
   *     Duplicated clause: b(one)
   *       first found at or above line 45
   *       while compiling object main_include_compiler_warning
   *       in file logtalk/examples/errors/include_compiler_warning.lgt at or above line 48

the user will get:

::

   *     No matching clause for goal: baz(a)
   *       while compiling object main_include_compiler_warning
   *       in file logtalk/examples/errors/include_compiler_warning.lgt between lines 37-38
   *     Calls to locally defined predicates without a clause with a matching head
   *     fail. Typo in a predicate argument? Predicate definition incomplete?
   *     
   *     Duplicated clause: b(one)
   *       first found at or above line 45
   *       while compiling object main_include_compiler_warning
   *       in file logtalk/examples/errors/include_compiler_warning.lgt at or above line 48
   *     Duplicated clauses are usually a source code editing error and can
   *     result in spurious choice-points, degrading performance. Delete or
   *     correct the duplicated clause to fix this warning.
