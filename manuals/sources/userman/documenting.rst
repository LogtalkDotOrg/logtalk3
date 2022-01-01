..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _documenting_documenting:

Documenting
===========

Assuming that the :ref:`source_data <flag_source_data>` flag is turned on, the
compiler saves all relevant documenting information collected when compiling
a source file. The provided :doc:`../devtools/lgtdoc` tool can access this
information by using the :ref:`reflection <reflection_reflection>` support
and generate a documentation file for each compiled entity (object, protocol,
or category) in XML format. Contents of the XML file include the entity name,
type, and compilation mode (static or dynamic), the entity relations with
other entities, and a description of any declared predicates (name,
compilation mode, scope, ...). The XML documentation files can be enriched
with arbitrary user-defined information, either about an entity or about its
predicates, by using the two directives described in the next section. The
``lgtdoc`` tool includes POSIX and Windows scripts for converting the XML
documentation files to several final formats (such as HTML and PDF).

.. _documenting_directives:

Documenting directives
----------------------

Logtalk supports two documentation directives for providing arbitrary
user-defined information about an entity or a predicate. These two
directives complement other directives that also provide important
documentation information such as the :ref:`directives_mode_2` and
:ref:`directives_meta_predicate_1` directives.

.. _documenting_entity:

Entity directives
~~~~~~~~~~~~~~~~~

Arbitrary user-defined entity information can be represented using the
:ref:`directives_info_1` directive:

::

   :- info([
       Key1 is Value1,
       Key2 is Value2,
       ...
   ]).

In this pattern, keys should be atoms and values should be bound terms.
The following keys are predefined and may be processed specially by
Logtalk tools:

``comment``
   Comment describing the entity purpose (an atom). As a style guideline,
   don't use overly long comments. If you need to provide additional
   details, use the ``remarks`` key.
``author``
   Entity author(s) (an atom or a compound term ``{entity}`` where
   ``entity`` is the name of an XML entity in a user defined
   ``custom.ent`` file).
``version``
   Version number (a ``Major:Minor:Patch`` compound term) Following the
   `Semantic Versioning guidelines <https://semver.org>`_ is strongly advised.
``date``
   Date of last modification in ISO 8601 standard format (``Year-Month-Day``
   where ``Year``, ``Month``, and ``Day`` are integers).
``parameters``
   Parameter names and descriptions for parametric entities (a list of
   ``Name-Description`` pairs where both names and descriptions are atoms).
``parnames``
   Parameter names for parametric entities (a list of atoms; a simpler
   version of the previous key, used when parameter descriptions are
   deemed unnecessary).
``copyright``
   Copyright notice for the entity source code (an atom or a compound
   term ``{entity}`` where ``entity`` is the name of an XML entity
   defined in a user defined ``custom.ent`` file).
``license``
   License terms for the entity source code; usually, just the license
   name (an atom or a compound term ``{entity}`` where ``entity`` is the
   name of an XML entity in a user defined ``custom.ent`` file).
``remarks``
   List of general remarks about the entity using ``Topic-Text`` pairs
   where both the topic and the text must be atoms.
``see_also``
   List of related entities (using the entity identifiers, which can
   be atoms or compound terms).

For example:

::

   :- info([
       version is 2:1:0,
       author is 'Paulo Moura',
       date is 2000-11-20,
       comment is 'Building representation.',
       diagram is 'UML Class Diagram #312'
   ]).

Use only the keywords that make sense for your application and remember that
you are free to invent your own keywords. All key-value pairs can be retrieved
programmatically using the :ref:`reflection API <reflection_reflection>` and
are visible to the :doc:`../devtools/lgtdoc` tool (which includes them in the
generated documentation).

.. _documenting_predicate:

Predicate directives
~~~~~~~~~~~~~~~~~~~~

Arbitrary user-defined predicate information can be represented using
the :ref:`directives_info_2` directive:

::

   :- info(Name/Arity, [
       Key1 is Value1,
       Key2 is Value2,
       ...
   ]).

The first argument can also a grammar rule non-terminal indicator,
``Name//Arity``. Keys should be atoms and values should be bound terms.
The following keys are predefined and may be processed specially by
Logtalk tools:

``comment``
   Comment describing the predicate (or non-terminal) purpose (an atom).
   As a style guideline, don't use overly long comments. If you need to
   provide additional details, use the ``remarks`` key.
``arguments``
   Names and descriptions of predicate arguments for pretty print output
   (a list of ``Name-Description`` pairs where both names and descriptions
   are atoms).
``argnames``
   Names of predicate arguments for pretty print output (a list of
   atoms; a simpler version of the previous key, used when argument
   descriptions are deemed unnecessary).
``allocation``
   Objects where we should define the predicate. Some possible values
   are ``container``, ``descendants``, ``instances``, ``classes``,
   ``subclasses``, and ``any``.
``redefinition``
   Describes if predicate is expected to be redefined and, if so, in
   what way. Some possible values are ``never``, ``free``,
   ``specialize``, ``call_super_first``, ``call_super_last``.
``exceptions``
   List of possible exceptions throw by the predicate using
   ``Description-Exception`` pairs. The description must be an
   atom. The exception term must be a ground term.
``examples``
   List of typical predicate call examples using the format
   ``Description-Goal-Bindings``. The description must be an atom
   with the goal sharing variables with the bindings. The
   variable bindings term uses the format ``{Variable = Term, ...}``.
   When there are no variable bindings, the success or failure of
   the predicate call should be represented by the terms ``{yes}``
   or ``{no}``, respectively.
``remarks``
   List of general remarks about the predicate using ``Topic-Text``
   pairs where both the topic and the text must be atoms.
``see_also``
   List of related predicates and non-terminals (using the predicate
   and non-terminal indicators).

For example:

::

   :- info(color/1, [
       comment is 'Table of defined colors.',
       argnames is ['Color'],
       constraint is 'Up to four visible colors allowed.'
   ]).

As with the ``info/1`` directive, use only the keywords that make sense
for your application and remember that you are free to invent your own
keywords. All key-value pairs can also be retrieved programmatically
using the :ref:`reflection API <reflection_reflection>` and are visible
to the :doc:`../devtools/lgtdoc` tool (which includes them in the generated
documentation).

.. _documenting_processing:

Processing and viewing documenting files
----------------------------------------

The :doc:`../devtools/lgtdoc` tool generates an XML documenting file per
entity. It can also generate library, directory, entity, and predicate
indexes when documenting libraries and directories. For example, assuming
the default filename extensions, a ``trace`` object and a ``sort(_)``
parametric object will result in ``trace_0.xml`` and ``sort_1.xml`` XML
files.

Each entity XML file contains references to two other files, an XML
specification file and a XSLT style-sheet file. The XML specification
file can be either a DTD file (``logtalk_entity.dtd``) or an XML Scheme
file (``logtalk_entity.xsd``). The XSLT style-sheet file is responsible
for converting the XML files to some desired format such as HTML or PDF.
The default names for the XML specification file and the XSL style-sheet
file are defined by the :doc:`../devtools/lgtdoc` tool but can be
overridden by passing a list of options to the tool predicates. The
``lgtdoc/xml`` sub-directory in the Logtalk installation directory contains
the XML specification files described above, along with several sample XSL
style-sheet files and sample scripts for converting XML documenting files
to several formats (e.g. reStructuredText, Markdown, HTML, and PDF). For
example, assume that you want to generate the API documentation for the
``types`` library:

.. code-block:: text

   | ?- {types(loader)}.
   ....

   | ?- {lgtdoc(loader)}.
   ....

   | ?- lgtdoc::library(types).
   ...

The above queries will result in the creation of a ``xml_docs`` in your
current directory by default. Assuming that we want to generate
Sphinx-based documentation and that we are using a POSIX operating-system,
the next steps would be:

.. code-block:: bash

   $ cd xml_docs
   $ lgt2rst -s -m

The ``lgt2rst`` script will ask a few questions (project name, author,
version, ...). After its completion, the generated HTML files will be
found in the ``_build/html`` directory by default:

.. code-block:: bash

   $ open _build/html/index.html

On Windows operating-systems, the JScript scripts can be used. For example,
assuming that we want to generate HTML documentation, we could run in a
``cmd.exe`` window:

.. code-block:: text

   cd xml_docs
   cscript "%LOGTALKHOME%\tools\lgtdoc\xml\lgt2html.js" /p:saxon

Or simply use the ``.bat`` script alternatives:

.. code-block:: text

   cd xml_docs
   lgt2html /p:saxon

After completion, the generated HTML files will be found in the ``xml_docs``
directory by default.

See the ``NOTES`` file in the tool directory for details, specially on the
XSLT processor dependencies. You may use the supplied sample files as a
starting point for generating the documentation of your Logtalk applications.

The Logtalk DTD file, ``logtalk_entity.dtd``, contains a reference to a
user-customizable file, ``custom.ent``, which declares XML entities for
source code author names, license terms, and copyright string. After
editing the ``custom.ent`` file to reflect your personal data, you may
use the XML entities on ``info/1`` documenting directives. For example,
assuming that the XML entities are named *author*, *license*, and
*copyright* we may write:

::

   :- info([
       version is 1:1:0,
       author is {author},
       license is {license},
       copyright is {copyright}
   ]).

The entity references are replaced by the value of the corresponding XML
entity when the XML documenting files are processed (**not** when they
are generated; this notation is just a shortcut to take advantage of XML
entities).

The :doc:`../devtools/lgtdoc` tool supports a set of options that can be
used to control the generation of the XML documentation files. See the
tool documentation for details. There is also a :doc:`../devtools/doclet`
tool that allows automating the steps required to generate the documentation
for an application.

.. _documenting_formatting:

Inline formatting in comments text
----------------------------------

Inline formatting in comments text can be accomplished by using Markdown
or reStructuredText syntax and converting XML documenting files to Markdown
or reStructuredText files (and these, if required, to e.g. HTML, ePub, or
PDF formats). Note that Markdown and reStructuredText common syntax elements
are enough for most API documentation:

.. code-block:: text

   Mark *italic text* with one asterisk.
   Mark **bold text** with two asterisks.
   Mark ``monospaced text`` with two backquotes.

Rendering this block as markup gives:

   Mark *italic text* with one asterisk. Mark **bold text** with
   two asterisks. Mark ``monospaced text`` with two backquotes.

Diagrams
--------

The :doc:`../devtools/diagrams` tool supports a wide range of diagrams that
can also help in documenting an application. The generated diagrams can
include URL links to both source code and API documentation. They can also
be linked, connecting for example high level diagrams to detail diagrams.
These features allow diagrams to be an effective solution for navigating and
understanding the structure and implementation of an application. This tool
uses the same :ref:`reflection API <reflection_reflection>` as the ``lgtdoc``
tool and thus have access to the same source data. See the tool documentation
for details. 
