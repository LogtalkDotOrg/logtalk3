________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>

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


This is a SWI-Prolog pack specific README file. The actual Logtalk
README file can be found at `../logtalk-3.00.2/README.md`. Follows
some notes on this pack version of Logtalk.

Installing this pack simplifies loading Logtalk on-demand by simply
using the directive (or the corresponding query):

	:- use_module(library(logtalk)).

Note, however, that Logtalk is not packaged as a module and that the
`logtalk` module provided contains just an initialization directive
that loads Logtalk in exactly the same way as when Logtalk is installed
using one of its prebuilt installers.

When using the directive above, the `LOGTALKHOME` and `LOGTALKUSER`
shell environment variables are set, just for the duration of the
SWI-Prolog process, to the full path of the `../logtalk-3.00.2`
directory.

For easy access to the contents of the Logtalk installation folder
(e.g. documentation or examples) you can create a symbolic link to the
`../logtalk-3.00.01` directory (in e.g. your home directory). Its full
path can be easily found by using the query:

	?- pack_info(logtalk).

The `../logtalk-3.00.2` directory contains a `settings.lgt` file that
makes loading of Logtalk silent. If you make other changes to this file,
or to other contents of the `../logtalk-3.00.2` directory, be sure to
make a backup before upgrading or removing this pack.
