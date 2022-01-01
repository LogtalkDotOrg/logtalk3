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

Logtalk requires a :term:`backend Prolog compiler` supporting official and
de facto standards. Capabilities needed by Logtalk that are not defined in
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

Distribution overview
---------------------

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

``ports``
   ``NOTES.md`` – description of included ports of third-party software
   ``...`` – ports

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

The adapter files specify *default* values for most of the Logtalk
:ref:`compiler flags <programming_flags>`. They also specify values for
read-only flags that are used to describe Prolog backend specific features.

.. _installing_runtime:

Compiler and runtime
~~~~~~~~~~~~~~~~~~~~

The ``core`` sub-directory contains the Prolog and Logtalk source files that
implement the Logtalk compiler and the Logtalk runtime. The compiler and
the runtime may be split in two (or more) separate files or combined in
a single file, depending on the Logtalk release that you are installing.

.. _installing_library:

Library
~~~~~~~

The Logtalk distribution includes a standard library of useful objects,
categories, and protocols. Read the corresponding ``NOTES.md`` file for
details about the library contents.

.. _installing_examples:

Examples
~~~~~~~~

The Logtalk distribution includes a large number of programing examples.
The sources of each one of these examples can be found included in a
subdirectory with the same name, inside the directory examples. The
majority of these examples include tests and a file named ``SCRIPT.txt``
with sample calls. Some examples may depend on other examples and
library objects to work properly. Read the corresponding ``NOTES.md``
file for details before running an example.

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
