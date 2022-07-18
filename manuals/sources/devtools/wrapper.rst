.. _library_wrapper:

``wrapper``
===========

This is a prototype tool to help port a plain Prolog application to
Logtalk. It can also be used to enable applying other Logtalk developer
tools, such as the documenting and diagramming tools, to plain Prolog
code.

The tool takes a directory of Prolog files or a list of Prolog files,
loads and wraps the code in each file using an object wrapper, and
advises on missing directives to be added to those objects by using the
compiler lint checker and the reflection API. The user can then either
save the generated wrapper objects or copy and paste the printed advice
into the Prolog files (updating them to Logtalk files by adding the
object opening and closing directives to the Prolog files). The wrapper
objects use ``include/1`` directives to include the Prolog files and can
be loaded for testing and for use with other tools. The wrapped Prolog
files are not modified and thus require only read permission.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#wrapper <../../docs/library_index.html#wrapper>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(wrapper(loader)).

Workflows
---------

The typical porting workflow is simply:

::

   | ?- wrapper::rdirectory(root_directory_of_prolog_code).
   ...
   | ?- wrapper::save.
   ...

See the next section on how to customize the API calls for more flexible
processing.

Customization
-------------

The tool can be customized by extending the ``wrapper`` object. A common
scenario is when wrapping plain Prolog code just to take advantage, for
example, of the documenting tool or for generating cross-referencing
diagrams. In this case, we can workaround any compiler errors by
specializing the inherited definitions for the ``term_expansion/2`` and
``goal_expansion/2`` predicates and then load the wrapper objects for
further processing by using the ``include_wrapped_files(false)`` option
described below.

The API predicates also accept a set of options for customization:

-  | ``prolog_extensions(Extensions)``
   | list of file name extensions used to recognize Prolog source files
     (default is ``['.pl', '.pro', '.prolog']``)

-  | ``logtalk_extension(Extension)``
   | Logtalk file name extension to be used for the generated wrapper
     files (default is ``'.lgt'``)

-  | ``exclude_files(Files)``
   | list of Prolog source files to exclude (default is ``[]``)

-  | ``exclude_directories(Files)``
   | list of sub-directories to exclude (default is ``[]``)

-  | ``include_wrapped_files(Boolean)``
   | generate ``include/1`` directives for the wrapped Prolog source
     files (default is ``true``)

Current limitations
-------------------

-  The tool cannot deal with syntax errors in the Prolog files. These
   errors usually occur when using a backend Prolog system different
   from the one used to compile the original plain Prolog code. A common
   cause of syntax errors are operator definitions. These can often be
   solved by defining those operators for the Prolog backend used to run
   Logtalk and this tool. An alternative is to preload the Prolog files
   where those operators are declared. Preloading the plain Prolog
   application can also help in wrapping it by ensuring that its
   dependencies are also loaded.

-  The tool assumes that all files to be wrapped have different names
   (even if found in different directories). If that is not the case,
   the name conflicts must be manually solved before using the tool.

-  There isn't yet any support for dealing with meta-predicates and
   advise on missing meta-predicate directives.
