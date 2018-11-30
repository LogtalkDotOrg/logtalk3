..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _installing_installing:

Installing Logtalk
==================

This page provides an overview of Logtalk installation requirements and
instructions and a description of the files contained on the Logtalk
distribution. For detailed, up-to-date installation and configuration
instructions, please see the ``README.md``, ``INSTALL.md``, and
``CUSTOMIZE.md`` files distributed with Logtalk. The broad compatibility
of Logtalk, both with Prolog compilers and operating-systems, together
with all the possible user scenarios, means that installation can vary
from very simple by running an installer or a couple of scripts to the
need of patching both Logtalk and Prolog compilers to workaround the
lack of strong Prolog standards or to cope with the requirements of less
common operating-systems.

The preferred installation scenario is to have Logtalk installed in a
system-wide location, thus available for all users, and a local copy of
user-modifiable files on each user home directory (even when you are the
single user of your computer). This scenario allows each user to
independently customize Logtalk and to freely modify the provided
libraries and programming examples. Logtalk installers, installation
shell scripts, and Prolog integration scripts favor this installation
scenario, although alternative installation scenarios are always
possible. The installers set two environment variables, ``LOGTALKHOME``
and ``LOGTALKUSER``, pointing, respectively, to the Logtalk installation
folder and to the Logtalk user folder.

User applications should preferable be kept outside of the Logtalk user
folder created by the installation process, however, as updating Logtalk
often results in updating the contents of this folder. If your
applications depend on customizations to the distribution files, backup
those changes before updating Logtalk.

.. _installing_requirements:

Hardware and software requirements
----------------------------------

.. _installing_computer:

Computer and operating system
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Logtalk is compatible with almost any computer/operating-system with a
modern, standards compliant, Prolog compiler available.

.. _installing_compiler:

Prolog compiler
~~~~~~~~~~~~~~~

Logtalk requires a backend Prolog compiler supporting official and de
facto standards. Capabilities needed by Logtalk that are not defined in
the official ISO Prolog Core standard include:

-  access to predicate properties
-  operating-system access predicates
-  de facto standard predicates not (yet) specified in the official
   standard

Logtalk needs access to the predicate property ``built_in`` to properly
compile objects and categories that contain Prolog built-in predicates
calls. In addition, some Logtalk built-ins need to know the
dynamic/static status of predicates to ensure correct application. The
ISO standard for Prolog modules defines a ``predicate_property/2``
predicate that is already implemented by most Prolog compilers. Note
that if these capabilities are not built-in the user cannot easily
define them.

For optimal performance, Logtalk requires that the Prolog compiler
supports **first-argument indexing** for both static and dynamic code
(most modern compilers support this feature).

Since most Prolog compilers are moving closer to the ISO Prolog standard
[ISO95]_, it is advisable that you try
to use the most recent version of your favorite Prolog compiler.

.. _installing_installers:

Logtalk installers
------------------

Logtalk installers are available for macOS, Linux, and Microsoft
Windows. Depending on the chosen installer, some tasks (e.g. setting
environment variables or integrating Logtalk with some Prolog compilers)
may need to be performed manually.

.. _installing_sources:

Source distribution
-------------------

Logtalk sources are available in a ``tar`` archive compressed with
``bzip2``, ``lgt3xxx.tar.bz2``. You may expand the archive by using a
decompressing utility or by typing the following commands at the
command-line:

::

   % tar -jxvf lgt3xxx.tar.bz2

This will create a sub-directory named ``lgt3xxx`` in your current
directory. Almost all files in the Logtalk distribution are text files.
Different operating-systems use different end-of-line codes for text
files. Ensure that your decompressing utility converts the end-of-lines
of all text files to match your operating system.

.. _installing_organization:

Directories and files organization
----------------------------------

In the Logtalk installation directory, you will find the following files
and directories:

``BIBLIOGRAPHY.bib`` – Logtalk bibliography in BibTeX format

``CUSTOMIZE.md`` – Logtalk end-user customization instructions

``INSTALL.md`` – Logtalk installation instructions

``LICENSE.txt`` – Logtalk user license

``NOTICE.txt`` – Logtalk copyright notice

``QUICK_START.md`` – Quick start instructions for those that do not like
to read manuals

``README.md`` – several useful information

``RELEASE_NOTES.md`` – release notes for this version

``UPGRADING.md`` – instructions on how to upgrade your programs to the
current Logtalk version

``VERSION.txt`` – file containing the current Logtalk version number
(used for compatibility checking when upgrading Logtalk)

``loader-sample.lgt`` – sample loader file for user applications

``settings-sample.lgt`` – sample file for user-defined Logtalk settings

``tester-sample.lgt`` – sample file for helping to automate running user
application unit tests

``adapters``
   ``NOTES.md`` – notes on the provided adapter files
   ``template.pl`` – template adapter file
   ``...`` – specific adapter files

``coding``
   ``NOTES.md`` – notes on syntax highlighter and text editor support
   files providing syntax coloring for publishing and editing Logtalk
   source code
   ``...`` – syntax coloring support files

``contributions``
   ``NOTES.md`` – notes on the user-contributed code
   ``...`` – user-contributed code files

``core``
   ``NOTES.md`` – notes on the current status of the compiler and
   runtime
   ``...`` – core source files

``docs``
   ``NOTES.md`` – notes on the provided documentation for core, library,
   tools, and contributions entities
   ``index.html`` – root document for all entities documentation
   ``...`` – other entity documentation files

``examples``
   ``NOTES.md`` – short description of the provided examples

   ``bricks``
      ``NOTES.md`` – example description and other notes
      ``SCRIPT.txt`` – step by step example tutorial
      ``loader.lgt`` – loader utility file for the example objects
      ``...`` – bricks example source files

   ``...`` – other examples

``integration``
   ``NOTES.md`` – notes on scripts for Logtalk integration with Prolog
   compilers
   ``...`` – Prolog integration scripts

``library``
   ``NOTES.md`` – short description of the library contents
   ``all_loader.lgt`` – loader utility file for all library entities
   ``...`` – library source files

``man``
   ``...`` – POSIX man pages for the shell scripts

``manuals``
   ``NOTES.md`` – notes on the provided documentation
   ``bibliography.html`` – bibliography
   ``glossary.html`` – glossary
   ``index.html`` – root document for all documentation
   ``...`` – other documentation files

``paths``
   ``NOTES.md`` – description on how to setup library and examples paths
   ``paths.pl`` – default library and example paths

``scratch``
   ``NOTES.md`` – notes on the scratch directory

``scripts``
   ``NOTES.md`` – notes on scripts for Logtalk user setup, packaging,
   and installation
   ``...`` – packaging, installation, and setup scripts

``tests``
   ``NOTES.md`` – notes on the current status of the unit tests
   ``...`` – unit tests for built-in features

``tools``
   ``NOTES.md`` – notes on the provided programming tools
   ``...`` – programming tools

.. _installing_adapters:

Adapter files
~~~~~~~~~~~~~

Adapter files provide the glue code between the Logtalk compiler/runtime
and a Prolog compiler. Each adapter file contains two sets of
predicates: ISO Prolog standard predicates and directives not built-in
in the target Prolog compiler and Logtalk specific predicates.

Logtalk already includes ready to use adapter files for most academic
and commercial Prolog compilers. If an adapter file is not available for
the compiler that you intend to use, then you need to build a new one,
starting from the included ``template.pl`` file. Start by making a copy
of the template file. Carefully check (or complete if needed) each
listed definition. If your Prolog compiler conforms to the ISO standard,
this task should only take you a few minutes. In most cases, you can
borrow code from the predefined adapter files. If you are unsure
that your Prolog compiler provides all the ISO predicates needed by
Logtalk, try to run the system by setting the unknown predicate error
handler to report as an error any call to a missing predicate. Better
yet, switch to a modern, ISO compliant, Prolog compiler. If you send me
your adapter file, with a reference to the target Prolog compiler, maybe
I can include it in the next release of Logtalk.

The adapter files specifies *default* values for most of the Logtalk
compiler flags. Most of these compiler flags are described in the
`next <programming.html#programming_flags>`__ section. A few of these
flags have read-only values and cannot be changed at runtime. These are:

.. index:: single: settings_file flag
.. _flag_settings_file:

``settings_file``
   Allows or disables loading of a :term:`settings file` at startup.
   Possible values are ``allow``, ``restrict``, and ``deny``. The usual
   default value is ``allow`` but it can be changed by editing the adapter
   file when e.g. embedding Logtalk in a compiled application. With a value
   of ``allow``, settings files are searched in the startup directory,
   in the Logtalk user directory, and in the user home directory. With a
   value of ``restrict``, settings files are only searched in the
   Logtalk user directory and in the user home directory.

.. index:: single: prolog_dialect flag
.. _flag_prolog_dialect:

``prolog_dialect``
   Name of the backend Prolog compiler (an atom). This flag can be used
   for :ref:`conditional compilation <conditional_compilation_directives>`
   of Prolog specific code.

.. index:: single: prolog_version flag
.. _flag_prolog_version:

``prolog_version``
   Version of the backend Prolog compiler (a compound term,
   ``v(Major, Minor, Patch)``, whose arguments are integers). This flag
   availability depends on the Prolog compiler. Checking the value of
   this flag fails for any Prolog compiler that does not provide access
   to version data.

.. index:: single: prolog_compatible_version flag
.. _flag_prolog_compatible_version:

``prolog_compatible_version``
   Compatible version of the backend Prolog compiler (a compound term,
   usually with the format ``@>=(v(Major, Minor, Patch))``, whose
   arguments are integers). This flag availability depends on the Prolog
   compiler. Checking the value of this flag fails for any Prolog
   compiler that does not provide access to version data.

.. index:: single: prolog_conformance flag
.. _flag_prolog_conformance:

``prolog_conformance``
   Level of conformance of the backend Prolog compiler with the ISO
   Prolog Core standard. The possible values are ``strict`` for
   compilers claiming strict conformance and ``lax`` for compilers
   claiming only broad conformance.

.. index:: single: unicode flag
.. _flag_unicode:

``unicode``
   Informs Logtalk if the backend Prolog compiler supports the Unicode
   standard. Possible flag values are ``unsupported``, ``full`` (all
   Unicode planes supported), and ``bmp`` (supports only the Basic
   Multilingual Plane).

.. index:: single: encoding_directive flag
.. _flag_encoding_directive:

``encoding_directive``
   Informs Logtalk if the backend Prolog compiler supports the
   :ref:`directives_encoding_1` directive.
   This directive is used for declaring the text encoding of source
   files. Possible flag values are ``unsupported``, ``full`` (can be
   used in both Logtalk source files and compiler generated Prolog
   files), and ``source`` (can be used only in Logtalk source files).

.. index:: single: tabling flag
.. _flag_tabling:

``tabling``
   Informs Logtalk if the backend Prolog compiler provides tabling
   programming support. Possible flag values are ``unsupported`` and
   ``supported``.

.. index:: single: engines flag
.. _flag_engines:

``engines``
   Informs if the backend Prolog compiler provides the required low
   level multi-threading programming support for Logtalk
   :term:`threaded engines <threaded engine>`. Possible flag values
   are ``unsupported`` and ``supported``.

.. index:: single: threads flag
.. _flag_threads:

``threads``
   Informs if the backend Prolog compiler provides the required low
   level multi-threading programming support for all high-level Logtalk
   :ref:`multi-threading features <threads_threads>`. Possible flag
   values are ``unsupported`` and ``supported``.

.. index:: single: modules flag
.. _flag_modules:

``modules``
   Informs Logtalk if the backend Prolog compiler provides suitable
   module support. Possible flag values are ``unsupported`` and
   ``supported`` (Logtalk provides limited support for compiling Prolog
   modules as objects).

.. index:: single: coinduction flag
.. _flag_coinduction:

``coinduction``
   Informs Logtalk if the backend Prolog compiler provides the minimal
   support for cyclic terms necessary for working with
   :term:`coinductive predicates <coinductive predicate>`. Possible flag
   values are ``unsupported`` and ``supported``.

.. _installing_settings:

Settings files
~~~~~~~~~~~~~~

Although is always possible to edit the backend Prolog compiler adapter
files, the recommended solution to customize compiler flags is to edit
the ``settings.lgt`` file in the Logtalk user folder or in the user home
folder. Depending on the backend Prolog compiler and on the
operating-system, is also possible to define per-project settings files
by creating a ``settings.lgt`` file in the project directory and by
starting Logtalk from this directory. At startup, Logtalk tries to load
a ``settings.lgt`` file from the startup directory (assuming that the
read-only :ref:`settings_file <flag_settings_file>` flag is set to
``allow``). If not found, Logtalk tries to load a ``settings.lgt`` file
from the Logtalk user folder. If still not found, Logtalk tries to load a
``settings.lgt`` file from the user home folder. When no settings files are
found, Logtalk will use the default compiler flag values set on the backend
Prolog compiler adapter files. When limitations of the backend Prolog
compiler or on the operating-system prevent Logtalk from finding the
settings files, these can always be loaded manually after Logtalk startup.

Settings files are normal Logtalk source files (although when
automatically loaded by Logtalk they are compiled silently with any
errors being simply ignored). The usual contents is an
``initialization/1`` Prolog directive containing calls to the
:ref:`predicates_set_logtalk_flag_2`
Logtalk built-in predicate and asserting clauses for the
:ref:`predicates_logtalk_library_path_2`
multifile dynamic predicate. Note that the
:ref:`directives_set_logtalk_flag_2`
directive cannot be used as its scope is local to the source file being
compiled. For example, one of the troubles of writing portable
applications is the different feature sets of Prolog compilers. A
typical issue is the lack of support for tabling. Using the Logtalk
support for conditional compilation you could write:

::

   :- if(current_logtalk_flag(tabling, supported)).

       % add tabling directives to the source code
       :- table(foo/1).
       :- table(bar/2).

   :- endif.

The :ref:`prolog_dialect <flag_prolog_dialect>` flag may also be used
with the conditional compilation directives in order to define a single
settings file that can be used with several backend Prolog compilers.
For example:

::

   :- if(current_logtalk_flag(prolog_dialect, yap)).

       % YAP specific settings
       ...

   :- elif(current_logtalk_flag(prolog_dialect, gnu)).

       % GNU Prolog specific settings
       ...

   :- else.

       % generic Prolog settings

   :- endif.

.. _installing_runtime:

Logtalk compiler and runtime
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``core`` sub-directory contains the Prolog and Logtalk source files that
implement the Logtalk compiler and the Logtalk runtime. The compiler and
the runtime may be split in two (or more) separate files or combined in
a single file, depending on the Logtalk release that you are installing.

.. _installing_library:

Library
~~~~~~~

Starting from version 2.7.0, Logtalk contains a standard library of
useful objects, categories, and protocols. Read the corresponding
``NOTES.md`` file for details about the library contents.

.. _installing_examples:

Examples
~~~~~~~~

Logtalk 2.x and 3.x contain new implementations of some of the examples
provided with previous 1.x versions. The sources of each one of these
examples can be found included in a subdirectory with the same name,
inside the directory examples. The majority of these examples include a
file named ``SCRIPT.txt`` that contains cases of simple utilization.
Some examples may depend on other examples and library objects to work
properly. Read the corresponding ``NOTES.md`` file for details before
running an example.

.. _installing_entities:

Logtalk source files
~~~~~~~~~~~~~~~~~~~~

Logtalk source files are text files containing one or more entity
definitions (objects, categories, or protocols). The Logtalk source
files may also contain plain Prolog code. The extension ``.lgt`` is
normally used. Logtalk compiles these files to plain Prolog by appending
to the file name a suffix derived from the extension and by replacing
the ``.lgt`` extension with ``.pl`` (``.pl`` is the default Prolog
extension; if your Prolog compiler expects the Prolog source filenames
to end with a specific, different extension, you can set it in the
corresponding adapter file).
