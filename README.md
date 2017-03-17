________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


README
======

[![Join the chat at https://gitter.im/LogtalkDotOrg/logtalk](https://badges.gitter.im/LogtalkDotOrg/logtalk.svg)](https://gitter.im/LogtalkDotOrg/logtalk?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


ABOUT
-----

Logtalk is an *object-oriented logic programming language* that extends and
leverages the Prolog language with a feature set suitable for programming
in the large. Logtalk supports modern code encapsulation and code reuse
mechanisms without compromising the declarative programming features of
Prolog. Logtalk is implemented in highly portable code and can use most
modern and standards compliant Prolog implementations as a back-end compiler.

As a multi-paradigm language, Logtalk includes support for both prototypes
and classes, protocols (interfaces), categories (components and hot-patching),
event-driven programming, coinduction, lambda expressions, and high-level
multi-threading programming. Logtalk uses standard Prolog syntax with the
addition of some operators and directives for a smooth learning path.


LOGTALK WEB SITE
----------------

The latest release of the Logtalk is always available at the URL:

<http://logtalk.org/>

At this address you can also find additional documentation and information
about Logtalk.


REGISTRATION
------------

To register as a Logtalk user either use the registration form found at
the Logtalk web site:

<http://logtalk.org/regform.html>

or send an email message to:

<registration@logtalk.org>

with the following information:

email address, first and last name, organization, organization type
(education, commercial, government, ...), Prolog compilers you use
(optional), platforms (mac, pc, unix, ...) (optional), projects where
you intend to use Logtalk (optional)


SUPPORT
-------

For support options, please consult the web page:

<http://logtalk.org/support.html>


INSTALLATION
------------

Logtalk can be installed either from sources by running a couple of shell
scripts  or by using one of the provided installers, depending on your
operating system. For manual installation, see the [INSTALL.md](INSTALL.md)
file for detailed instructions.

See the user manual for a description of the source files organization
and for using instructions (to read the user manual open the
[index](manuals/index.html) file in a web browser). Most files are
formatted using tabs (the recommended setting is a tab width equivalent
to 4 spaces).


CUSTOMIZATION
-------------

The [CUSTOMIZE.md](CUSTOMIZE.md) file provides detailed instructions for
customizing the Logtalk installation and working environment.


RUNNING
-------

The [QUICK_START.md](QUICK_START.md) file provides quick instructions for
those of you in a hurry to run Logtalk, provided that your favorite Prolog
compiler is supported and installed.


DOCUMENTATION
-------------

A quick introduction for experienced programmers is available at the
[Learn X in Y minutes](https://learnxinyminutes.com/docs/logtalk/) website.

The reference and user manuals and the tutorial are provided in XHTML and
PDF formats and can be found in the [manuals](manuals/) directory.

The [RELEASE_NOTES.md](RELEASE_NOTES.md) file contains descriptions of all
Logtalk updates since the first public version. Check it carefully if you
have been using a previous Logtalk version.

The documentation for the core, library, tools, and contributions entities
is provided in XHTML format and can be found in the [docs](docs/) directory.

Most directories include `NOTES.md` documenting files, which can be viewed
as plain text in any text editor but also nicely rendered as Markdown files
(for e.g. easy conversion to HTML).

On POSIX systems, there's also a man page for each shell script. A list of
all the scripts can be generated using the `apropos logtalk` command.


UPGRADING
---------

If you are upgrading from a previous Logtalk version, please check the
[UPGRADING.md](UPGRADING.md) file for instructions on how to upgrade your
programs or your custom adapter files to run under this new version.


CITATIONS
---------

If you find Logtalk useful, please include a citation on your publications.
Consult the [BIBLIOGRAPHY.bib](BIBLIOGRAPHY.bib) file for bibliographic
references in BibTeX format (the Logtalk technical report published on 2000
and the 2003 PhD thesis on Logtalk are good choices).


CONTRIBUTIONS
-------------

Contributions, constructive criticism, patches, bug reports, and suggestions
are always welcome. Major code and documentation contributions require the
contributor to sign and submit a contributor license agreement and should
be made available under the Logtalk license without any additional terms or
conditions. Contributions using other licensing terms may also be distributed
with Logtalk with the understanding that the terms of their use depends solely
on the authors chosen licensing terms and may require a separate, independent,
agreement between users and authors.


LEGAL
-----

Logtalk is copyrighted by Paulo Moura and made available under the Apache
License 2.0. See the [LICENSE.txt](LICENSE.txt) file for the license terms.
The copyright notice and license applies to all files in this release unless
otherwise explicitly stated. See the [NOTICE.txt](NOTICE.txt) for detailed
copyright information.

Some files that are part of the Logtalk distribution are distributed using
a different license and/or are copyrighted by a Logtalk contributor.

Some of the included examples are adaptations to Logtalk of Prolog examples
found elsewhere (e.g. in manuals, tutorials, books, and public mailing list
discussions). The copyright in the examples original code should be assumed
to belong to the original authors.

Logtalk is a registered trademark of Paulo Moura.
