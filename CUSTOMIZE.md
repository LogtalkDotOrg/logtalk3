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


CUSTOMIZATION INSTRUCTIONS
==========================

This file contains detailed instructions for customizing your Logtalk 
installation and working environment. Customization is usually done on
a per-user basis by editing a settings file in the user homer folder
or in the Logtalk user folder (its path is stored on the `LOGTALKUSER`
environment variable; this folder can be (re)created by running the
`logtalk_user_setup` shell script; on Windows systems, you can use the
installer and choose the corresponding install option). The default
path for the Logtalk user folder is:

* POSIX systems  
    `$HOME/logtalk`
* Windows  
    `My Documents\Logtalk`

The Logtalk user folder is updated when you update Logtalk. Therefore,
it shouldn't be used to store your Logtalk application's source code.


1. DEFINING A DEFAULT PROLOG BACKEND COMPILER
---------------------------------------------

Users of POSIX systems may use the `logtalk_backend_select` shell script
to define an alias, `logtalk`, for one of the provided backend Prolog
compiler integration scripts.


2. SETTING LIBRARY PATHS
------------------------

In Logtalk, a library is simply a directory containing source files. Library 
paths can be declared using the `logtalk_library_path/2` dynamic and multifile
predicate. This allows compiling and loading of libraries and library files
using the library names instead of the full library paths. It also makes it
easy to relocate libraries.

Inside your Logtalk user folder, you will find a `paths` folder containing 
a sample file which, when loaded, defines the library paths for the standard
library, developer tools, contributions, and supplied examples. For details,
see the `paths/NOTES.md` file.

Library paths for your own source files directories are preferably defined in
your settings file, described next.


3. CUSTOMIZING LOGTALK SETTINGS
-------------------------------

Logtalk interfaces with a specific backend Prolog compiler using a adapter
file that can be found on the `adapters` folder in the Logtalk installation
folder. These adapter files define default values of the flags that are used
by Logtalk when compiling source files (for a full description of these flags, 
consult the `Writing, Running, and Debugging Logtalk Programs` section of
the User Manual). 

The default compiler flag settings are appropriated for the *development*
(but not necessarily for the *deployment*) of applications. Check the example
settings on the `settings-sample.lgt` for configuration suggestions.

You may customize the Logtalk compiler flags and add your own library paths
by coping or renaming the `settings-sample.lgt` file in your Logtalk user
folder to `settings.lgt` and editing it. The `settings.lgt` file may also be
stored in your home directory. Settings in this file override the default
values in the adapter files. Some of the default flag values that you may
want to change include:

* `report`  
    for less verbose startup and compilation reports

* `scratch_directory`  
    to move compiler generated temporary files out of the way or to
    collect them in a single place for embedding Logtalk applications

* `portability` and `underscore_variables`  
    essential if you're writing portable Logtalk applications

Check the `adapters/NOTES.md` file for Prolog specific compatibility notes.
Some backend Prolog compilers don't support all the possible compilation
flags values. In addition, some backend Prolog compilers provide limited
support for settings files in some operating-systems.

Settings filescan be debugged by compiling them with the `logtalk_compile/1-2`
built-in predicates.


4. CUSTOMIZING DOCUMENTATION PROCESSING SUPPORT
-----------------------------------------------

Inside your Logtalk user folder, you will find a `tools/lgtdoc/xml` folder
containing a set of shell scripts, CSS and XSLT style-sheets, and DTD and
XML Schema files for processing the XML documenting files that are generated 
from the source data collected when compiling source files. You may want to
customize the CSS and XSLT files to modify the layout or style of the resulting
Markdown/XML/PDF/(X)HTML files or to write new scripts and transformations to
generate other formats. You may also edit the file `custom.ent` in order to
specify XML entities for your personal data that can be used with the Logtalk
documenting directives. For details, see the `tools/lgtdoc/NOTES.md` file.


5. ADDING LOGTALK SUPPORT TO TEXT EDITORS
-----------------------------------------

Inside your Logtalk user folder, you will find a `coding` folder, containing 
support files for several text editors, which add support for syntax 
highlighting and other text editing services for Logtalk source files.
Support for several syntax highlighters (used e.g. in web pages and in wikis
and issue trackers) is also included. For details, see the `coding/NOTES.md`
file.
