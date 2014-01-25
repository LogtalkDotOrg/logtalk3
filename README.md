________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Additional licensing terms apply per Section 7 of the GNU General
Public License 3. Consult the `LICENSE.txt` file for details.
________________________________________________________________________


README
======

1. About
2. Logtalk web site
3. Registration
4. Support
5. Installation
6. Customization
7. Running
8. Documentation
9. Upgrading
10. Citations
11. Contributions
12. Legal


1. ABOUT
--------

Logtalk is an *object-oriented logic programming language* that extends and
leverages the Prolog language with a feature set suitable for programming
in the large. As a multi-paradigm language, it includes support for both
prototypes and classes, protocols (interfaces), component-based programming
through category-based composition, event-driven programming, coinduction,
and high-level multi-threading programming. Logtalk uses standard Prolog
syntax with the addition of some operators and directives and can be used
with any Prolog implementation compliant with official and de facto
standards as the back-end compiler.


2. LOGTALK WEB SITE
-------------------

The latest release of the Logtalk is always available at the URL:

    http://logtalk.org/

At this address you can also find additional documentation and information
about Logtalk.


3. REGISTRATION
---------------

To register as a Logtalk user either use the registration form found at 
the Logtalk web site:

    http://logtalk.org/regform.html

or send an email message to:

	registration@logtalk.org

with the following information:

email address, first and last name, organization, organization type 
(education, commercial, government, ...), Prolog compilers you use 
(optional), platforms (mac, pc, unix,...) (optional), projects where 
you intend to use Logtalk (optional)


4. SUPPORT
----------

For support options, please consult the page:

    http://logtalk.org/support.html


5. INSTALLATION
---------------

Logtalk can be installed either from sources by running a few shell scripts 
or by using one of the provided installers, depending on your operating 
system. For manual installation, see the file `INSTALL.md` for detailed 
instructions.

See the user manual for a description of the source files organization 
and for using instructions (to read the user manual open the file 
`manuals/index.html` with a web browser). Most files are formatted using
tabs (the recommended setting is a tab width equivalent to 4 spaces).


6. CUSTOMIZATION
----------------

The file `CUSTOMIZE.md` provides detailed instructions for customizing the
Logtalk installation and working environment.


7. RUNNING
----------

The file `QUICK_START.md` provides quick instructions for those of you
in a hurry to run Logtalk, provided that your favorite Prolog compiler
is supported and installed.


8. DOCUMENTATION
----------------

The reference and user manuals and the tutorial are provided in XHTML format 
and can be found in the `manuals` directory. PDF versions of the reference
and user manuals can be downloaded from the Logtalk web site.

The file `RELEASE_NOTES.md` contains descriptions of all Logtalk updates 
since the first public version. Read it carefully if you have been using
a previous Logtalk version.

The documentation for the core, library, tools, and contributions entities
is provided in XHTML format and can be found in the `docs` directory.

Most directories include `NOTES.md` documenting files, which can be viewed
as plain text in any text editor but also nicely rendered as Markdown files
(for e.g. easy conversion to HTML).


9. UPGRADING
------------

If you are upgrading from a previous Logtalk version, please check the file 
`UPGRADING.md` for instructions on how to upgrade your programs or your 
custom adapter files to run under this new version.


10. CITATIONS
-------------

If you find Logtalk useful, please include a citation on your publications.
Consult the file `BIBLIOGRAPHY.bib` for bibliographic references in BibTeX 
format (the Logtalk technical report published on 2000 and the 2003 PhD
thesis on Logtalk are good choices).


11. CONTRIBUTIONS
-----------------

Contributions, constructive criticism, patches, bug reports, and suggestions
are always welcome. However, major code contributions to core files, such as
the compiler, runtime, adapter files, library files, or tools can only be
accepted with a transfer of copyright or if the submitted code is explicitly
declared as public domain code. This simplifies the selling of commercial
licenses, which provide a revenue source for the sustained development of
Logtalk.


12. LEGAL
---------

Logtalk is copyrighted by Paulo Moura. The Logtalk use and distribution
license can be found in the `LICENSE.txt` file. Logtalk follows the GNU
General Public License 3, plus some additional terms as per Section 7.
The copyright notice and license applies to all files in this release
unless otherwise explicitly stated.

Some files that are part of the Logtalk distribution are distributed using
a different license and/or are copyrighted by a Logtalk contributor.

Some of the included examples are adaptations to Logtalk of Prolog examples
found elsewhere (e.g. in manuals, tutorials, books, and public mailing list
discussions). The copyright in the examples original code should be assumed
to belong to the original authors.

Logtalk is a registered trademark of Paulo Moura.
