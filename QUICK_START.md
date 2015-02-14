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


QUICK START INSTRUCTIONS
========================

Starting up Logtalk
-------------------

Install Logtalk by using either the installer provided for your 
operating-system (when available) or by following the instructions 
on the `INSTALL.md` file.

On POSIX operating-systems, the following shell scripts are installed 
by default for running Logtalk with selected back-end Prolog compilers 
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

On MacOS X systems, `/opt/local/bin` must be in your PATH to run the scripts.
Terminal command files for running Logtalk with selected back-end Prolog 
compilers are also available on the Logtalk installation folder (by default,
`/opt/local/share/logtalk/scripts/macosx/command_files`; for easy access, the
installer creates an alias to the Logtalk folder in the Applications folder.

On Windows systems, shortcuts for running Logtalk with selected back-end 
Prolog compilers are created on the `Start Menu/Programs/Logtalk` menu.
The first run of the XSB integration shortcuts may require administrator
privileges depending on the XSB installation (right-click on the shortcut
and select the "Run as administrator" option).

If you get an unexpected failure when using one of the Prolog integration
scripts or shortcuts, consult the `adapters/NOTES.md` file in the Logtalk
installation folder for compatibility notes. For the integration scripts,
see also the integration script man page.

Open the `manuals/index.html` file with a web browser and select the
`Tutorial` link. This will provide you with a basic understanding of
some of the main Logtalk concepts. You can then go back to the `index.html`
file, select the `User Manual` link, then the `Installing Logtalk` and
`Writing, Running, and Debugging Logtalk Programs` links. This will
provide you with a basic understanding of how to start Logtalk as well
as how to compile and load Logtalk code.


Running the examples
--------------------

You may now try some of the provided examples:

1. Open the `examples` sub-directory. There you find several sub-directories
with ready to run examples and a `NOTES.md` file containing general 
instructions and a brief description of each example. Select and open one 
of the examples sub-directory.

2. Read the example `NOTES.md` file for a description of the example.

3. Open the `SCRIPT.txt` file for instructions on how to load the example 
and for sample queries that you may try by copying-and-pasting them to 
your Prolog interpreter top-level.


Writing your own programs
-------------------------

Ready to start writing your own programs?

1. Read the User Manual sections on `Programming in Logtalk` and
`Running and debugging Logtalk programs`.

2. Take a look to the `adapters/NOTES.md` file for important compatibility
information about your chosen backend Prolog compiler and for any defined
shorthands for commonly used load and make predicates.

3. Take a look at the `coding` sub-directory. There you will find syntax 
support files for popular text editors which enable syntax coloring and
other text services when editing Logtalk source files. There's also
support for syntax highlighters used for publishing source code.

4. The `tools` directory contains a comprehensive set of developer tools
to help you test, debug, analyze, and document your applications.

5. Create a directory (preferably outside of your Logtalk user folder,
which is updated when you update Logtalk) with a suitable name to hold
all the files of your application. 

6. Copy or rename the `settings-sample.lgt` file to `settings.lgt`, and
modify it to define a library alias for your application directory and
for defining default compiler flags (see the file `CUSTOMIZE.md` for
details).

7. Copy to your application directory the `loader-sample.lgt` file, rename
it to `loader.lgt`, and modify it to load your application source files.
You may also copy the `tester-sample.lgt` file, renaming it to `tester.lgt`,
and editing it to run your application unit tests.

8. Have fun!
