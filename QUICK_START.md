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


QUICK START INSTRUCTIONS
========================

Starting up Logtalk
-------------------

Install Logtalk by using either the installer provided for your 
operating-system (when available) or by following the instructions 
on the `INSTALL.md` file.

On POSIX operating-systems, the following shell scripts are installed 
by default for running Logtalk with selected backend Prolog compilers 
(which must be up-to-date and properly installed before running these
scripts!):

* B-Prolog:       `bplgt`
* CxProlog:       `cxlgt`
* ECLiPSe:        `eclipselgt`
* GNU Prolog:     `gplgt`
* JIProlog:       `jiplgt`     (first run may require `sudo`)
* Lean Prolog:    `lplgt`      (experimental)
* Qu-Prolog:      `qplgt`
* Quintus Prolog: `quintuslgt` (experimental)
* SICStus Prolog: `sicstuslgt`
* SWI-Prolog:     `swilgt`
* XSB:            `xsblgt`     (first run may require `sudo`)
* XSB MT:         `xsbmtlgt`   (first run may require `sudo`)
* YAP:            `yaplgt`

On macOS systems, `/opt/local/bin` must be in your PATH to run the scripts.
Terminal command files for running Logtalk with selected backend Prolog 
compilers are also available on the Logtalk installation folder (by default,
`/opt/local/share/logtalk/scripts/macosx/command_files`; for easy access, the
installer creates an alias to the Logtalk folder in the Applications folder.

On Windows systems, shortcuts for running Logtalk with selected backend 
Prolog compilers are created on the `Start Menu/Programs/Logtalk` menu.
The first run of the JIProlog and XSB integration shortcuts may require
administrator privileges depending on the JIProlog and XSB installation
(right-click on the shortcut and select the "Run as administrator" option).

If you get an unexpected failure when using one of the Prolog integration
scripts or shortcuts, consult the `adapters/NOTES.md` file in the Logtalk
installation folder for compatibility notes. For the integration scripts,
see also the integration script `man` page.

Open the `manuals/index.html` file with a web browser and select the
`Tutorial` link. This will provide you with a basic understanding of
some of the main Logtalk concepts. You can then go back to the `index.html`
file, select the `User Manual` link, then the `Installing Logtalk` and
`Writing and running applications` links. This will provide you with a
basic understanding of how to start Logtalk as well as how to compile and
load Logtalk code.

Basic help on Logtalk usage at the top-level interpreter
--------------------------------------------------------

Start Logtalk and call the goal `{help(loader)}` followed by `help::help`.
This will provide you with an overview on how to get help and how to load
and debug your code.

Running an example
------------------

You may now try some examples. The `examples/NOTES.md` file contains a
brief description of each example.

1. Select and open one  of the examples sub-directory.

2. Read the example `NOTES.md` file for a description of the example.

3. Open the `SCRIPT.txt` file for instructions on how to load the example 
and for sample queries that you may try by copying-and-pasting them to 
your Prolog interpreter top-level.


Writing your own programs
-------------------------

Ready to start writing your own programs?

1. Read the User Manual sections on `Programming in Logtalk` and
`Running and debugging Logtalk programs`.

2. Take a look at the `coding` sub-directory. There you will find syntax 
support files for popular text editors which enable syntax coloring and
other text services when editing Logtalk source files. There's also
support for syntax highlighters used for publishing source code.

3. The `tools` directory contains a comprehensive set of developer tools
to help you test, debug, analyze, and document your applications.

4. Create a directory (preferably outside of your Logtalk user folder,
which is updated when you update Logtalk) with a suitable name to hold
all the files of your application. 

5. Copy or rename the `settings-sample.lgt` file to `settings.lgt`, and
modify it to define a library alias for your application directory and
for defining default compiler flags (see the file `CUSTOMIZE.md` for
details).

6. Copy to your application directory the `loader-sample.lgt` file, rename
it to `loader.lgt`, and modify it to load your application source files.
You may also copy the `tester-sample.lgt` and `tests-sample.lgt` files,
renaming them to `tester.lgt` and `tests.lgt`, and editing them to define
and run your application unit tests.

7. Have fun!
