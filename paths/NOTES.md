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


This folder contains a Prolog file, `paths.pl` file, that is automatically
loaded by the Prolog integration scripts and shortcuts. This file defines
essential library paths for starting Logtalk and library paths for the
standard library, developer tools, and also for the supplied examples
and contributions. Note that, in Logtalk, a library is simply a directory
containing source files.

Also provided in this folder are four Prolog files, `paths_core.pl`,
`paths_tools.pl`, `paths_contributions.pl`, and `paths_examples.pl`.
Together, these files provide the same path definitions of the `paths.pl`
file. They can be used when embedding or deploying Logtalk applications
where e.g. tools or example paths should not included. Note that the
obvious solution of having `paths.pl` including or loading the other
four Prolog files is not portable!

Basic library aliases defined in the `paths.pl` file include:

- `home`  
	user home directory
- `startup`  
	Logtalk startup directory
- `logtalk_home`  
	Logtalk installation directory
- `logtalk_user`  
	Logtalk user directory
- `library`  
	Logtalk standard library directory (a sub-directory of the Logtalk user directory)

You might need to edit the `paths.pl` file in order to adapt it to 
reflect your Logtalk installation, Prolog compiler, and operating-system 
requirements. However, as defined, the provided `paths.pl` file is already
compatible with the supported back-end Prolog compilers running on MacOS X,
Windows, Linux, Unix, and Unix-like operating-systems.

Although you could customize this file by adding the paths to your own
"libraries" (in order to easily load your own source code), this is
preferably accomplished by customizing the settings file in the Logtalk
user directory or in your Logtalk application directory.

When manually loading Logtalk (instead of using the provided integration
scripts and shortcuts), you will need to load the `paths.pl` into your
Prolog compiler before loading the Logtalk compiler/runtime to ensure
Logtalk proper startup and to easily compile and load library and example
source files using the notation `<library>(<file>)`.
