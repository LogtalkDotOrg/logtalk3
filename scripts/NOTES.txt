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


This directory contains shell scripts used for Logtalk testing, packaging,
installation, and integration with Prolog compilers. Those with extension
`.sh` are bash shells scripts for MacOS X, Linux, and similar systems.
Those with extension `.js` are JScript command-line scripts for Windows;
they require WSH 5.6 or later version to be installed and should be run
using `cscript.exe` from a DOS command line shell (you may download WSH 5.6
from `http://msdn.microsoft.com/downloads/list/webdev.asp`).

- `build_release.sh`  
	helper script for building most of the distribution files of a new 
	Logtalk release; accepts as an optional argument a version identifier
	(e.g. 3.00.0-a8)

- `cleandist.sh`  
	script for cleaning a Logtalk distribution in preparation for packaging

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
	shell script for automating running unit tests in the `examples` and
	`tests` directories; besides test results, this script also reports
	compilation warnings and errors (note that, depending on the tests,
	these warnings and errors might be expected); know issue: the output
	of some of the multi-threading examples may interfere with the computation
	of the test/skipped/passed/failed totals; you can use this script on
	Windows operating-systems by installing Git for Windows (which provides a
	bash shell implementation and is available from <http://msysgit.github.io>)
	and by adding the `$LOGTALKHOME/scripts` and `$LOGTALKHOME/integration`
	directories plus the backend Prolog compiler executable directories to the
	system path environment variable

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
