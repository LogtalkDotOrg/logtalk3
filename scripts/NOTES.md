________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>

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


This directory contains shell scripts used for Logtalk testing, packaging,
installation, and integration with Prolog compilers. Those with extension
`.sh` are bash shells scripts for MacOS X, Linux, and similar systems.
Those with extension `.js` are JScript command-line scripts for Windows;
they require WSH 5.6 or later version to be installed and should be run
using `cscript.exe` from a DOS command line shell (you may download WSH 5.6
from `http://msdn.microsoft.com/downloads/list/webdev.asp`).

Man pages are provided for all POSIX shell scripts, which can be listed
using the `apropos logtalk` command.

- `build_release.sh`  
	helper script for building most of the distribution files of a new 
	Logtalk release; accepts as an optional argument a version identifier
	(e.g. 3.00.0-a8)

- `cleandist.sh`  
	script for cleaning a Logtalk distribution in preparation for packaging;
	expects to be called from the parent directory of the `scripts` directory

- `install.sh`  
	shell script for installing Logtalk in POSIX operating systems. When
	using the default installation directory prefix, it must be run from
	this directory by a user with administration privileges (for example,
	`sudo ./install.sh`). The default prefix is `/opt/local` on Darwin
	(MacOS X), `/usr` on Debian systems, and `/usr/local` on other POSIX
	systems, resulting in Logtalk being installed in `$prefix/share` with
	useful scripts written to `$prefix/bin`, which should be in your path);
	the script also accepts as an optional argument a prefix for the
	installation directory (for example, `./install.sh -p $HOME`)

- `uninstall.sh`  
	shell script for de-installing Logtalk in Unix and Unix-like operating 
	systems (must be run from this directory by a user with administration 
	privileges)

- `logtalk_tester.sh`  
	shell script for automating running unit tests e.g. in the `examples` and
	`tests` directories; it recurses through all sub-directories of a directory
	looking for either `tester.lgt` or `tester.logtalk` files;
	in its default output format, it reports, besides test results, compilation
	warnings and errors (please note that, depending on the tests and on the
	compilation mode, these warnings and errors might be expected);
	it can also write test results in the TAP and xUnit formats, generating files
	that can then be processed by continuous integration servers;
	know issue: the output of some of the multi-threading examples may interfere
	with the computation of the test/skipped/passed/failed totals;
	you can use this script on Windows operating-systems by installing Git for
	Windows (which provides a Bash shell implementation and is available from
	<http://msysgit.github.io>) and by adding the `$LOGTALKHOME/scripts` and
	`$LOGTALKHOME/integration` directories plus the backend Prolog compiler
	executable directories to the system path environment variable;
	if the script detects either a `timeout` or a `gtimeout` command (provided
	by the GNU coreutils package), it will use it to run each test set if the
	`timeout` option is set to a value greater than zero

- `logtalk_doclet.sh`  
	shell script for automating running doclets; it recurses through all
	sub-directories of a directory looking for either `doclet.lgt` or
	`doclet.logtalk` files;
	you can use this script on Windows operating-systems by installing Git for
	Windows (which provides a Bash shell implementation and is available from
	<http://msysgit.github.io>) and by adding the `$LOGTALKHOME/scripts` and
	`$LOGTALKHOME/integration` directories plus the backend Prolog compiler
	executable directories to the system path environment variable;
	if the script detects either a `timeout` or a `gtimeout` command (provided
	by the GNU coreutils package), it will use it to run each doclet if the
	`timeout` option is set to a value greater than zero

- `logtalk_version_select.sh`  
	shell script for switching between installed Logtalk versions for POSIX
	operating-systems; works with version 2.36.0 or later; doesn't change the
	Logtalk user folder; this script is loosely based on the `python_select`
	script

- `logtalk_backend_select.sh`  
	experimental shell script for defining an alias, logtalk, to a chosen
	back-end Prolog integration script for POSIX operating-systems; the
	alias is created in same directory where the `*lgt` integration scripts
	are found

- `debian`  
	directory containing support files for building a Debian package
	(work in progress; experimental)

- `freedesktop`  
	directory containing support files for adding the Logtalk mime-type
	to the freedesktop.org shared mime-info database

- `linux`  
	directory containing files used when building

- `macosx`  
	directory containing files used when building MacOS X installer 
	packages

- `pack`  
	support files for creating a `logtalk` SWI-Prolog pack

- `windows`  
	directory containing files used when building Windows GUI installers

- `logtalk_user_setup.sh` and `logtalk_user_setup.js`  
	end-user scripts for copying the Logtalk user-modifiable files and 
	directories to the location pointed by the environment variable 
	`LOGTALKUSER` (defaults to `~/logtalk` on POSIX operating-systems 
	and to `My Documents\Logtalk` on Windows when the variable is not 
	defined); must be run by each end-user in order to ensure proper 
	permissions for the copied files; the `LOGTALKHOME` environment 
	variable must be defined (pointing to the Logtalk installation 
	directory)
- `logtalk_user_setup.bat`  
	wrapper for the `logtalk_user_setup.js` script to simplify its use

- `update_html_docs.sh`  
	shell script for updating the library and tools HTML documentation

- `update_svg_diagrams.sh`  
	shell script for updating the core entities, library, and development
	tools SVG diagrams
