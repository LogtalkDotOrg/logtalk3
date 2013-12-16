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


MANUAL INSTALLATION INSTRUCTIONS
================================

The recommended way of installing Logtalk is to use, whenever possible, one 
of the provided installers. This file contains detailed instructions for 
*manual* installation and configuration of Logtalk. You should also consult 
the `scripts/NOTES.txt` and `integration/NOTES.txt` files for a description 
of a set of shell scripts that might be used for Logtalk installation on 
some operating-systems and for easy Logtalk integration with popular Prolog 
compilers.

The POSIX shell scripts assume that `/bin/bash` is available.


1. LOGTALK BASIC INSTALLATION
-----------------------------

Manual installation of Logtalk can be accomplished by decompressing the
downloaded archive (or by cloning the development git server), running an
installation script, and defining a couple of environment variables. You can
install Logtalk in any user accessible location. Whenever possible, it is
recommended that Logtalk be installed by a user with administrative rights,
as described below. This leads to a setup where each Logtalk user may freely
try and modify the provided examples, library, and adapter files with the 
option of, at any time, restoring the files to its original state by simply
running one of the provided scripts.


* Installing for a single user with no administrative rights:

For POSIX systems, first, open a terminal, change the current directory to
the Logtalk directory, and then type:

	% cd scripts
	% ./install.sh -p $HOME

This will install Logtalk into the `$HOME/share` and `$HOME/bin` directories
(the `$HOME/bin` path must be in your `PATH` environment variable).

If you're using Windows, you can simply use the provided installer (which
supports both admin and non-admin users) to perform a full installation.
The Windows installer is built using Inno Setup, which is freely available.
If you need to customize the installer (e.g. to recognize Prolog compilers
installed in non-standard locations), edit the Inno Setup script found on
the `scripts/windows` directory and regenerate the installer.


* Installing for one or more users by a user with administrative rights:

For POSIX systems, first, open a terminal, change the current directory to
the Logtalk directory, and then type:

	% cd scripts
	% sudo ./install.sh

This installation script makes all files read-only for non-admin users in 
order to avoid user tempering. This is a convenient setup for computer labs, 
given that making directories world-writable is a security risk. The install
script accepts an installation prefix as argument. For example:

	% sudo ./install.sh -p /opt/local

If no prefix is given, the default installation prefix depends on the
operating-system:

* Mac OS X:				`/opt/local`
* Debian distributions:	`/usr`
* Other POSIX systems:	`/usr/local`

The script installs Logtalk in the `$prefix/share` directory with Prolog
integration and other useful shell scripts written to the `$prefix/bin`
directory, which should be in your path. The `install.sh` shell script also
creates a symbolic link, `$prefix/share/logtalk`, which can be used for e.g.
defining the `LOGTALKHOME` environment variable described below.

If you're using Windows, you can simply use the provided GUI installer (which
supports both admin and non-admin users) to perform a full installation. You
can also easily generate the GUI installer yourself. See the instructions on
the `scripts/windows/NOTES.txt` file.


2. SETTING LOGTALK ENVIRONMENT VARIABLES
----------------------------------------

You need to set two environment variables, `LOGTALKHOME` and `LOGTALKUSER`.
The environment variable `LOGTALKHOME` should be set to the Logtalk installation 
directory. The environment variable `LOGTALKUSER` should point to a directory 
in your home directory where you want to store the user-specific Logtalk files
(by default, `$HOME/logtalk` on POSIX systems and `My Documents\Logtalk` on 
Windows). Both environment variables may be set for all users by a user with 
administration privileges.

For POSIX systems, add the following lines to your `~/.profile` file:

	LOGTALKHOME=/your/logtalk/installation/directory
	LOGTALKUSER=$HOME/logtalk
	PATH=$PATH:$LOGTALKHOME/tools/lgtdoc/xml:$LOGTALKHOME/scripts:$LOGTALKHOME/integration
	MANPATH=$MANPATH:$LOGTALKHOME/man
	export LOGTALKHOME LOGTALKUSER PATH MANPATH

If you use instead a csh shell, add the following line to your `~/.cshrc` file:

	setenv LOGTALKHOME /your/logtalk/installation/directory
	setenv LOGTALKUSER $HOME/logtalk
	setenv PATH $PATH:$LOGTALKHOME/tools/lgtdoc/xml:$LOGTALKHOME/scripts:$LOGTALKHOME/integration
	setenv MANPATH $MANPATH:$LOGTALKHOME/man

Don't use relative paths such as `../` or `./` in the definition of the environment
variables. Some Prolog compilers don't expand environment variables, resulting
in `file not found` errors when attempting to use the Logtalk integration scripts.

When using the provided shell script for installing Logtalk, a symbolic link 
to the Logtalk installation directory is automatically created. The link is 
named `logtalk`. In this case, you may use this symbolic link to define the 
`LOGTALKHOME` environment variable in order to avoid breaking it when upgrading 
Logtalk.

If you're using Windows, the provided GUI installer (which supports both admin
and non-admin users) takes care of the definition of the environment variables.


3. END-USER SETUP (COPYING LOGTALK USER-MODIFIABLE FILES TO USERS HOME DIRS)
----------------------------------------------------------------------------

If you installed Logtalk on your home directory, then skip this step if and only
if you have set both Logtalk environment variables (`LOGTALKHOME` and `LOGTALKUSER`)
to point to the same directory.

Each user must make a local copy of the Logtalk user-modifiable files (examples,
libraries, and other supporting files) in his/her home directory. These copies
can be easily made by running the `logtalk_user_setup` shell script (see the
`scripts/NOTES.txt` file for details):

* POSIX systems  
	`% logtalk_user_setup`
* Windows  
	`C:\> logtalk_user_setup`

The local copies made by the `logtalk_user_setup` scripts have both read and
write permissions for the user running the script. When used with one of the 
back-end Prolog compilers for which an integration script is provided on 
the `integration` directory, this setup as the advantage of allowing each 
end-user to independently customize default compilation options, library 
paths, and modify and experiment with the provided libraries and examples.

Windows (admin and non-admin) users may also use the Logtalk GUI installer
to setup their Logtalk user folder and the `LOGTALKUSER` environment variable.


4. CREATING NEW PROLOG TOP-LEVELS FOR AUTOMATIC LOADING OF LOGTALK
------------------------------------------------------------------

Most Prolog compilers allows the user to define an initialization file that
is automatically consulted at startup. These initialization files may contain
directives for loading other files, such as the Logtalk adapter file and the
Logtalk compiler/runtime. The `$LOGTALKHOME/integration` sub-directory 
contains several pre-made scripts (named `*lgt.*`) for running Logtalk with 
selected back-end Prolog compilers. You can use these scripts as examples 
when creating initialization files for other Prolog compilers. Be sure to 
read the `adapters/NOTES.txt` file notes on the Prolog compilers that you 
intend to use.


5. CUSTOMIZING LOGTALK
----------------------

Please see the file `CUSTOMIZE.md` for details on how to customize your 
Logtalk installation and working environment.
