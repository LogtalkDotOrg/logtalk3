``assertions``
==============

The ``assertions.lgt`` file contains definitions for two
meta-predicates, ``assertion/1-2``, which allows using of assertions on
your source code to print warning and error messages (using the message
printing mechanism). The ``assertions_messages.lgt`` file defines the
default message translations generated on assertions succeed, fail, or
throw an exception.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#assertions <../../docs/library_index.html#assertions>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(assertions(loader)).

Adding assertions to your source code
-------------------------------------

The ``assertion/1`` predicate takes a goal as argument. For example:

::

   foo(L) :-
       assertions::assertion(non_empty_list(L)),
       ...

The ``assertion/2`` predicate takes as arguments a term for passing
context information and a goal. Using again a unit test as an example:

::

   foo(L) :-
       assertions::assertion(foo_list_alerts, non_empty_list(L)),
       ...

When using a large number of assertions, you can use a lighter syntax by
adding a ``uses/2`` directive. For example:

::

   :- uses(assertions, [assertion/1, assertion/2]).

Automatically adding file and line context information to assertions
--------------------------------------------------------------------

The ``assertions/1`` parametric object can be used as a hook object to
automatically add file and line context information, represented by the
term ``file_lines(File, BeginLine-EndLine)``, to calls to the
``assertion/1`` predicate by goal-expanding it to calls to the
``assertion/2`` predicate (the expansion assumes that a ``uses/2``
directive is being used in the code that will be expanded to direct
``assertion/1`` calls to the ``assertions`` object). For example,
assuming the file using assertions is named ``source``, it would be
compiled and loaded using the call:

::

   logtalk_load(source, [hook(assertions(debug))])

Suppressing assertion calls from source code
--------------------------------------------

The ``assertions/1`` parametric object can be used as a hook object to
suppress calls to the ``assertion/1-2`` predicates using goal-expansion
(the expansion assumes ``assertions::assertion/1-2`` messages). For
example, assuming the file using assertions is named ``source``, it
would be compiled and loaded using the call:

::

   logtalk_load(source, [hook(assertions(production))])

Redirecting assertion failure messages
--------------------------------------

By default, assertion failures and errors are printed to the standard
output stream. These messages, however, can be intercepted by defining
the ``logtalk::message_hook/4`` multifile predicate. For example:

::

   :- category(redirect_assertions_messages).

       :- multifile(logtalk::message_hook/4).
       :- dynamic(logtalk::message_hook/4).

       logtalk::message_hook(Message, error, assertions, _) :-
           writeq(my_log_file, Message), write(my_log_file, '.\n').

   :- end_category.

Converting assertion failures into errors
-----------------------------------------

If you want an assertion failure to result in a runtime error, you can
intercept the assertion failure messages, optionally still printing
them, and throw an error. For example:

::

   :- category(assertions_failures_to_errors).

       :- multifile(logtalk::message_hook/4).
       :- dynamic(logtalk::message_hook/4).

       logtalk::message_hook(Message, error, assertions, Tokens) :-
           % uncomment the next two lines to also print the default message
           % logtalk::message_prefix_stream(error, assertions, Prefix, Stream),
           % logtalk::print_message_tokens(Stream, Prefix, Tokens),
           throw(error(Message, _)).

   :- end_category.

In alternative, if you want assertions to always trigger an exception,
use instead the ``lgtunit`` tool ``assertions/1-2`` public predicates.

Other notes
-----------

All source files are indented using tabs (a common setting is a tab
width equivalent to 4 spaces).
