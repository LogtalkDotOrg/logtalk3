________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
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
________________________________________________________________________


This directory contains Prolog integration shell scripts and supporting
Prolog files. The POSIX scripts assume that Bash is installed and that
the Prolog compilers are available in the system path.

On POSIX systems, the Logtalk installers make the following integration
scripts available from the command-line (you may need to adjust your
system path):

* B-Prolog (8.1 or later):           `bplgt`
* Ciao Prolog (1.20.0 or later):     `ciaolgt`    (experimental; first run may require `sudo`)
* CxProlog (0.98.1 or later):        `cxlgt`
* ECLiPSe (6.1#143 or later):        `eclipselgt`
* GNU Prolog (1.4.5 or later):       `gplgt`
* JIProlog (4.1.7.1 or later):       `jiplgt`     (first run may require `sudo`)
* LVM (1.7.0 or later):              `lvmlgt`
* Quintus Prolog (3.3 or later):     `quintuslgt`
* Scryer Prolog (0.8.128 or later):  `scryerlgt`  (experimental)
* SICStus Prolog (4.1.0 or later):   `sicstuslgt`
* SWI-Prolog (6.6.0 or later):       `swilgt`
* Tau Prolog (0.3.1 or later):       `taulgt`
* Trealla Prolog (1.17.0 or later):  `tplgt`      (experimental)
* XSB (3.8.0 or later):              `xsblgt`     (first run may require `sudo`)
* YAP (6.3.4 or later):              `yaplgt`

For more information about these scripts and their dependencies, consult
the corresponding `man` page (e.g. `man yaplgt`). The `man` pages are also
available at:

https://logtalk.org/man/

On Windows systems, the Logtalk installer makes the Prolog integration
shortcuts available from the `Start Menu/Programs/Logtalk` menu. But
the POSIX shell scripts above can also be used by installing a bash
shell implementation for Windows. The easiest way is to install Git for
Windows from:

http://msysgit.github.io/

After installation, you can start the Bash shell by selecting `Git Bash`
from the context menu. You will also need to add the `$LOGTALKHOME/scripts`
and `$LOGTALKHOME/integration` directories plus the backend Prolog compiler
executable directories to the system path environment variable. This can be
accomplished by defining e.g. a `~/.profile` file with the necessary export
commands. The integration scripts will need to be called, however, without
omitting the `.sh` extension.

The first run of the Ciao Prolog, XSB, or JIProlog integration scripts may
require a user with administrative rights. On POSIX systems, run them **once**
as root or using `sudo`. In Windows systems, the first run of the integration
shortcuts must be made from an administrative account (right-click on the
shortcut and select the "Run as administrator" option).

The GNU Prolog integration script provides adequate performance for
development. But for production environments, improved performance can be
achieved by generating a new GNU-Prolog top-level that includes Logtalk.
See the `adapters/NOTES.md` file for details.

The Tau Prolog integration scripts requires that the `NODE_PATH` environment
variable be set to the `node_modules` directory path where you installed
Tau Prolog and its dependencies.

The environment variables `LOGTALKHOME` and `LOGTALKUSER` should be defined
in order to run the integration scripts (see the `INSTALL.md` file for
details on setting these variables). When the scripts detect an outdated
Logtalk user directory, they create a new one by running the script
`logtalk_user_setup.sh` (a backup is automatically created of the old
directory).

Note that the integration scripts and shortcuts may fail if you use non-
standard locations for your Prolog compilers. Edit the scripts and the
shortcuts if necessary.

Depending on the size and complexity of your Logtalk applications and the
used Prolog backend compiler, you may need to change the integration scripts
in order to allocate more memory to the backend Prolog compilers. Please
consult the documentation on the backend Prolog compilers you intend to use
for details.

All the scripts accept command-line options, which are passed straight to
the backend Prolog compiler. For example (on a POSIX operating-system and
using SWI-Prolog as the backend compiler):

	% swilgt -g "write('Hello world'), nl"

Keep in mind, however, that the integration scripts already use the backend
Prolog command-line option that allows an initialization file to be loaded in
order to bootstrap Logtalk. See the script files for details.
