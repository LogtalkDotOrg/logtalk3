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


This is a SWI-Prolog pack specific README file. The actual Logtalk
README file can be found at `../logtalk-3.13.0/README.md`. Follows
some notes on this pack version of Logtalk.

The pack version of Logtalk is mainly targeted for the *deployment*
of applications. For *development*, using the installers available
from the Logtalk website are advisable as they provide a better user
experience.

Installing this pack simplifies loading Logtalk on-demand by simply
using the directive (or the corresponding query):

	:- use_module(library(logtalk)).

Note, however, that Logtalk is not packaged as a module and that the
`logtalk` module provided contains just an initialization directive
that loads Logtalk in exactly the same way as when Logtalk is installed
using one of its prebuilt installers.

When using the directive above, the `LOGTALKHOME` and `LOGTALKUSER`
shell environment variables are set, just for the duration of the
SWI-Prolog process, to the full path of the `../logtalk-3.13.0`
directory.

For easy access to the contents of the Logtalk installation folder
(e.g. documentation or examples) you can create a symbolic link to the
`../logtalk-3.13.0` directory (in e.g. your home directory). Its full
path can be easily found by using the query:

	?- pack_info(logtalk).

The `../logtalk-3.13.0` directory contains a `settings.lgt` file that
makes loading of Logtalk silent. If you make other changes to this file,
or to other contents of the `../logtalk-3.13.0` directory, be sure to
make a backup before upgrading or removing this pack.

The `../logtalk-3.13.0` directory contains several handy scripts but due
to a limitation of the `archive` library in SWI-Prolog 7.3.28 and older
versions used for extracting the pack files, the executable permission
of the script files was not preserved. If you're not running SWI-Prolog
7.3.29 or a later version, this issue can be manually fixed after installing
the pack using the following steps (adjust the `logtalk-3.13.0` directory
full path if necessary for your installation):

	$ cd $HOME/lib/swipl/pack/logtalk/logtalk-3.13.0
	$ chmod a+x scripts/cleandist.sh
	$ ./scripts/cleandist.sh
