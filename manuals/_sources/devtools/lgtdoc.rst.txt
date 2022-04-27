``lgtdoc``
==========

This is the default Logtalk documenting tool for generating API
documentation for libraries and applications. It uses the structural
reflection API to extract and output in XML format relevant
documentation about a source file, a library or directory of source
files, or all loaded source files. The tool predicates accept several
options for generating the XML files, including the output directory.

The ``lgtdoc/xml`` directory contains several ready to use Bash and
PowerShell scripts for converting the XML documenting files into final
formats including (X)HTML, PDF, Markdown, and reStructuredText (for use
with Sphinx), or plain text files. The scripts are described in their
``man`` pages and made available in the system path by default. See also
the ``lgtdoc/xml/NOTES.md`` for details, including the **required
third-party software**.

API documentation
-----------------

This tool API documentation is available at:

`../../docs/library_index.html#lgtdoc <../../docs/library_index.html#lgtdoc>`__

Loading
-------

This tool can be loaded using the query:

::

   | ?- logtalk_load(lgtdoc(loader)).

Testing
-------

To test this tool, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(lgtdoc(tester)).

Documenting source code
-----------------------

For information on documenting your source code, notably on documenting
directives, consult the documenting section of the User Manual:

`../../manuals/userman/documenting.html <../../manuals/userman/documenting.html>`__

Extracting documenting information from your source code using with this
tool requires compiling the source files using the ``source_data(on)``
compiler flag. For example:

::

   | ?- logtalk_load(source_file, [source_data(on)]).

Usually, this flag is set for all application source files in the
corresponding loader file. In alternative, you may also turn on the
``source_data`` flag globally by typing:

::

   | ?- set_logtalk_flag(source_data, on).

The tool API allows generating documentation for libraries, directories,
and files, complemented with library, directory, entity, and predicate
indexes. Note that the source files to be documented **must** be loaded
prior to using this tool predicates to generate the documentation.

Generating documentation
------------------------

For a simple application, assuming a library alias is defined for it
(e.g. ``my_app``), and at the top-level interpreter, we can generate the
application documentation by typing:

::

   | ?- {my_app(loader)}.
   ...

   | ?- {lgtdoc(loader)}.
   ...

   | ?- lgtdoc::library(my_app).
   ...

By default, the documenting XML files are created in a ``xml_docs``
directory in the current working directory. But usually all documenting
files are collected for both the application and the libraries it uses
in a common directory so that all documentation links resolved properly.
The ``lgtdoc`` predicates can take a list of options to customize the
generated XML documenting files. See the remarks section in the
`lgtdocp <https://logtalk.org/docs/library_index.html#lgtdoc>`__
protocol documentation for details on the available options.

After generating the XML documenting files, these can be easily
converted into final formats using the provided scripts. For example,
assuming that we want to generate HTML documentation:

::

   $ cd xml_docs
   $ lgt2html -t "My app"

To generate the documentation in Sphinx format instead (as used by
Logtalk itself for its APIs):

::

   $ cd xml_docs
   $ lgt2rst -s -- -q -p "Application name" -a "Author name" -v "Version X.YZ.P"
   $ make html

In this case, the generated documentation will be in the
``xml_docs/_build/html/`` directory. See the scripts man pages or call
them using the ``-h`` option to learn more about their supported
options.

For more complex applications, you can use the ``doclet`` tool to define
a *doclet* to automate all the steps required to generate documentation.
The *doclet* message that triggers the process can also be sent
automatically when the ``make`` tool is used with the ``documentation``
target.
