________________________________________________________________________

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
________________________________________________________________________


README
======

[![Build Status](https://travis-ci.org/LogtalkDotOrg/logtalk3.svg?branch=master)](https://travis-ci.org/LogtalkDotOrg/logtalk3)
[![Documentation Status](https://readthedocs.org/projects/logtalk3/badge/?version=latest)](https://logtalk3.readthedocs.io/en/latest/?badge=latest)
[![Join the chat at https://gitter.im/LogtalkDotOrg/logtalk3](https://badges.gitter.im/LogtalkDotOrg/logtalk3.svg)](https://gitter.im/LogtalkDotOrg/logtalk3?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


About
-----

Logtalk is a *declarative object-oriented logic programming language* that
extends and leverages the Prolog language with a feature set suitable for
programming in the large. Logtalk supports modern code encapsulation and
code reuse mechanisms without compromising the declarative programming
features of Prolog.

Logtalk is implemented as a trans-compiler in highly portable code and can
use most modern and standards compliant Prolog implementations as a backend
compiler.

As a multi-paradigm language, Logtalk includes support for both *prototypes*
and *classes*, *protocols* (*interfaces*), *categories* (components and
hot-patching), *event-driven programming*, *coinduction*, *lambda expressions*,
and *high-level multi-threading programming*. Logtalk uses standard Prolog
syntax with the addition of some operators and directives for a smooth learning
path.

Logtalk is distributed under a commercial friendly license and includes full
documentation, portable libraries, portable developer tools, and numerous
programming examples to help get you started.

Logtalk development adheres to the Contributor Covenant [code of conduct](CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report
unacceptable behavior to contact@logtalk.org.


Logtalk website
---------------

The latest stable release of the Logtalk is always available at:

https://logtalk.org/

At this address you can download installers for your operating-system and also
find additional documentation and information about Logtalk.


Installation
------------

Logtalk can be installed either from sources by running a couple of shell
scripts or by using an [installer](https://logtalk.org/download.html) for your
operating system. For manual installation, see the [INSTALL.md](INSTALL.md)
file for detailed instructions.

See the [user manual](manuals/index.html) for a description of the source
files organization and for usage instructions.

If you are upgrading from a previous Logtalk version, please check the
[UPGRADING.md](UPGRADING.md) file for instructions on how to upgrade your
programs or your custom adapter files to run under this new version.


Customization
-------------

The [CUSTOMIZE.md](CUSTOMIZE.md) file provides detailed instructions for
customizing the Logtalk installation and working environment.


Running
-------

The [QUICK_START.md](QUICK_START.md) file provides quick instructions for
those of you in a hurry to run Logtalk, provided that your favorite Prolog
compiler is supported and installed.


Documentation
-------------

A quick and highly recommended introduction for users comfortable with Prolog
and with general knowledge about object-oriented programming is available at
the [Learn X in Y minutes](https://learnxinyminutes.com/docs/logtalk/) website.

The reference and user manuals are provided in HTML, PDF, and ePub formats and
can be found in the [manuals](manuals/) directory. They are also available
online at:

https://logtalk.org/manuals/index.html

The [RELEASE_NOTES.md](RELEASE_NOTES.md) file contains descriptions of all
Logtalk updates since the first public version. Please check it if you are
upgrading from a previous Logtalk version.

The API documentation for the core, library, tools, and contributions is
provided in HTML format and can be found in the [docs](docs/) directory and
also available online at:

https://logtalk.org/docs/index.html

Most directories include `NOTES.md` or `NOTES.txt` documentation files.

On POSIX systems, there's also a [man page](man/man1) for most shell scripts.
A list of these scripts can be generated using the `apropos logtalk` command.
[HTML versions](https://logtalk.org/man/) of the man pages are also available
at the Logtalk website.

Additional useful information is available in the official repository
[wiki](https://github.com/LogtalkDotOrg/logtalk3/wiki) at GitHub.


Registration
------------

To register as a Logtalk user either use the registration form found at
the Logtalk website:

https://logtalk.org/regform.html

Registration is optional. But it's also a way of showing your support and
an opportunity for us to learn about the cool projects where you will be
using Logtalk.


Support
-------

Support channels include:

* [Professional services](https://logtalk.pt/)
* [Community discussion forums](https://forums.logtalk.org/)
* [Community live chat room](https://gitter.im/LogtalkDotOrg/logtalk3)

For more information on support options, please consult the web page:

https://logtalk.org/support.html


Citations
---------

If you find Logtalk useful, please include a citation on your publications
(also cite the used backend Prolog compilers). The [BIBLIOGRAPHY.bib](BIBLIOGRAPHY.bib)
file includes bibliographic references in BibTeX format (the Logtalk user
and reference manuals and the 2003 PhD thesis on Logtalk are good choices).


Contributions
-------------

Contributions are most welcome! See the [CONTRIBUTING.md](CONTRIBUTING.md) file
for details.


Legal
-----

Logtalk is copyrighted by Paulo Moura and made available under the Apache
License 2.0. See the [LICENSE.txt](LICENSE.txt) file for the license terms.
The copyright notice and license applies to all files in this release unless
otherwise explicitly stated. See the [NOTICE.txt](NOTICE.txt) for detailed
copyright information.

Some files that are part of the Logtalk distribution are distributed using
a different license and/or are copyrighted by a Logtalk contributor.

Some examples are adaptations to Logtalk of Prolog examples found elsewhere
(e.g. in manuals, tutorials, books, and public mailing list discussions).
The copyright in the examples original code should be assumed to belong to
the original authors.

Logtalk is a registered trademark of Paulo Moura.
