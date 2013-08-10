________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>

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

1. Install Logtalk by using either the installer provided for your 
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
* Qu-Prolog:      `qplgt`
* SICStus Prolog: `sicstuslgt`
* SWI-Prolog:     `swilgt`
* XSB:            `xsblgt`     (first run may require `sudo`)
* XSB 64 bits:    `xsb64lgt`   (first run may require `sudo`)
* XSB MT:         `xsbmtlgt`   (first run may require `sudo`)
* XSB MT 64 bits: `xsbmt64lgt` (first run may require `sudo`)
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
scripts or shortcuts, consult the `adapters/NOTES.txt` file in the Logtalk
installation folder for compatibility notes. For the integration scripts,
see also the integration script man page.

2. Open the `manuals/index.html` file with a web browser.

3. Select the `Tutorial` link. This will provide you with a basic 
understanding of some of the main Logtalk concepts.

4. Go back to the `index.html` file, select the `User Manual` link, then
the `Installing Logtalk` and `Writing, Running, and Debugging Logtalk
Programs` links. This will provide you with a basic understanding of 
how to start Logtalk as well as how to compile and load Logtalk code.


Running the examples
--------------------

You may now try some of the provided examples:

1. Open the `examples` sub-directory. There you find several sub-directories
with ready to run examples and a `NOTES.txt` file containing general 
instructions and a brief description of each example. Select and open one 
of the examples sub-directory.

2. Read the example `NOTES.txt` file for a description of the example.

3. Open the `SCRIPT.txt` file for instructions on how to load the example 
and for sample queries that you may try by copying-and-pasting them to 
your Prolog interpreter top-level.


Writing your own programs
-------------------------

Ready to start writing your own programs?

1. Read the User Manual sections on `Programming in Logtalk` and `Running 
and debugging Logtalk programs`.

2. Take a look at the `coding` sub-directory. There you will find syntax 
support files for popular text editors which enable syntax coloring and
other text services when editing Logtalk source files. There's also
support for syntax highlighters used for publishing code.

3. Create a sub-directory with a suitable name to hold all the files of 
your application. You may want to define the application directory path
in a `settings.lgt` file in order to easily load your application (see 
the file `CUSTOMIZE.md` for details).

4. Copy to this sub-directory a loader file from one of the example 
directories and modify it to load your own source files.

5. Have fun!
