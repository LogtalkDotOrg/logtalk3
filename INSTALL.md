________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

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


MANUAL INSTALLATION INSTRUCTIONS
================================

The recommended way of installing Logtalk is to use, whenever possible,
one of the provided installers that can be downloaded from the
[Logtalk website](https://logtalk.org). Installers are provided for macOS,
Windows, and Linux.

This file contains detailed instructions for **manual** installation and
configuration of Logtalk. You should also consult
the [scripts/NOTES.md](scripts/NOTES.md) and
[integration/NOTES.md](integration/NOTES.md) files for a description of a
set of shell scripts that might be used for Logtalk installation on some
operating-systems and for easy Logtalk integration with popular Prolog
compilers.

The POSIX shell scripts assume that `bash` is available.


Logtalk basic installation
--------------------------

Manual installation of Logtalk can be accomplished by decompressing the
sources archive (or by cloning the development git server), optionally running
an installation script, and defining a couple of environment variables. You can
install Logtalk in any user accessible location. Whenever possible, it is
recommended that Logtalk be installed by a user with administrative rights,
as described below. This leads to a setup where each Logtalk user may freely
run and modify the provided examples, library, and tool files with the option
of, at any time, restoring the files to its original state by simply running
one of the provided scripts.

**Using the sources or git clone directory**

In this case, there is no installation procedure other than decompressing the
sources (or cloning the git repository) into a convenient directory. Simply
skip to the section below on setting the Logtalk environment variables and
use the directory full path as the value for both the `LOGTALKHOME` and
`LOGTALKUSER` variables. Note that in this case, on POSIX systems, you will
need to call the provided scripts without omitting the `.sh` extension (e.g.
`swilgt.sh` instead of `swilgt`).

**Installing for a single user with no administrative rights**

If you want to keep a pristine copy of the sources (or avoid possible merge
conflicts when updating your git clone) use the provided installation script
to copy the files that typically you want to play with and modify elsewhere.

For **POSIX** systems (macOS, Linux, ...), first, open a terminal, change the
current directory to the Logtalk directory, and then type:

	% cd scripts
	% ./install.sh -p $HOME

This will install Logtalk into the `$HOME/share` and `$HOME/bin` directories
(the `$HOME/bin` path must be in your `PATH` environment variable; also, the
`$HOME/share/man` directory should be in your `MANPATH` environment variable).

If you're using **Windows**, you can simply use the provided installer (which
supports both admin and non-admin users) to perform a full installation.
The Windows installer is built using Inno Setup, which is freely available.
If you need to customize the installer (e.g. to recognize Prolog compilers
installed in non-standard locations), edit the Inno Setup script found on
the [scripts/windows](scripts/windows) directory and regenerate the installer.


**Installing for one or more users by a user with administrative rights**

For **POSIX** systems, first, open a terminal, change the current directory
to the Logtalk directory, and then type:

	% cd scripts
	% sudo ./install.sh

This installation script makes all files read-only for non-admin users
in order to avoid user tampering. This is a convenient setup for computer
labs, given that making directories world-writable is a security risk. The
installation script accepts an installation prefix as argument. For example:

	% sudo ./install.sh -p /opt/local

If no prefix is given, the default installation prefix depends on the
operating-system:

* macOS:				`/opt/local`
* Debian distributions:	`/usr`
* Other POSIX systems:	`/usr/local`

The script installs Logtalk in the `$prefix/share` directory with Prolog
integration and other useful shell scripts written to the `$prefix/bin`
directory, which should be in your `PATH` environment variable. The script
also creates a symbolic link, `$prefix/share/logtalk`, which can be used
for e.g. defining the `LOGTALKHOME` environment variable described below.
Man pages are installed on the `$prefix/share/man` directory, which should
be in your `MANPATH` environment variable.

If you're using **Windows**, you can simply use the provided GUI installer
(which supports both admin and non-admin users) to perform a full installation.
You can also easily generate the GUI installer yourself. See the instructions
on the `scripts/windows/NOTES.md` file. If the installer fails to detect the
installation of the backend Prolog compiler you want to use, you can manually
create a Windows integration shortcut by finding the full path to the Prolog
executable and using the Prolog specific command-line options to load the
integration file (see the Bash integration scripts in the `integration`
directory for the options and file to be loaded). For example assume that
you're using a non-standard and non-default installation of SWI-Prolog in
a removable disk mounted as `G:\`. The shortcut would use a path such as:

	G:\swipl\bin\swipl-win.exe -s "%LOGTALKHOME%\integration\logtalk_swi.pl"

Alternatively, on Windows 10 or Windows Server 2019, you can use the
Windows Subsystem for Linux (WSL) and install Logtalk using one of the
Linux installers or by following the instructions above for POSIX systems.


Setting Logtalk environment variables
-------------------------------------

You need to set two environment variables, `LOGTALKHOME` and `LOGTALKUSER`.
The environment variable `LOGTALKHOME` should be set to the Logtalk installation
directory. The environment variable `LOGTALKUSER` should point to a directory
in your home directory where you want to store the user-specific Logtalk files
(by default, `$HOME/logtalk` on POSIX systems and `My Documents\Logtalk` on
Windows). Both environment variables may be set for all users by a user with
administration privileges.

For POSIX systems using a bash shell, edit and add the following lines to your
`~/.profile` file:

	export LOGTALKHOME=/your/logtalk/installation/directory
	export LOGTALKUSER=$HOME/logtalk

In case you're using the clone directory itself to run Logtalk without running
the `install.sh` script, you will also need to add:

	PATH=$PATH:$LOGTALKHOME/tools/diagrams
	PATH=$PATH:$LOGTALKHOME/tools/lgtdoc/xml
	PATH=$PATH:$LOGTALKHOME/scripts
	PATH=$PATH:$LOGTALKHOME/integration
	MANPATH=$MANPATH:$LOGTALKHOME/man
	export PATH MANPATH

If you use instead a csh shell, edit and add the following lines to your
`~/.cshrc` file:

	setenv LOGTALKHOME /your/logtalk/installation/directory
	setenv LOGTALKUSER "${HOME}"/logtalk

In case you're using the clone directory itself to run Logtalk without running
the `install.sh` script, you will also need to add:

	setenv PATH "${PATH}":"${LOGTALKHOME}"/tools/diagrams
	setenv PATH "${PATH}":"${LOGTALKHOME}"/tools/lgtdoc/xml
	setenv PATH "${PATH}":"${LOGTALKHOME}"/scripts
	setenv PATH "${PATH}":"${LOGTALKHOME}"/integration
	setenv MANPATH "${MANPATH}":"${LOGTALKHOME}"/man

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


End-user setup
--------------

Skip this step if you have set both Logtalk environment variables
(`LOGTALKHOME` and `LOGTALKUSER`) to point to the same directory.

Each user must make a local copy of the Logtalk user-modifiable files (examples,
libraries, tools, and other supporting files) in his/her home directory. These
copies can be easily made by running the `logtalk_user_setup` shell script (see
the `scripts/NOTES.md` file for details):

* POSIX systems  
	`% logtalk_user_setup`
* Windows  
	`C:\> logtalk_user_setup`

The local copies made by the `logtalk_user_setup` scripts have both read and
write permissions for the user running the script. When used with one of the
backend Prolog compilers for which an integration script is provided on
the `integration` directory, this setup as the advantage of allowing each
end-user to independently customize default compilation flags, library paths,
and modify and experiment with the provided libraries and examples.

Windows (admin and non-admin) users may also use the Logtalk GUI installer
to setup their Logtalk user folder and the `LOGTALKUSER` environment variable.

User applications should preferable be kept outside of the Logtalk user folder
created by the scripts above as updating Logtalk often results in updating the
contents of this folder. If your applications depend on customization of the
distribution files, backup those changes before updating Logtalk (the scripts
above automatically make a backup of any existing Logtalk user folder but you
should rely on your own backups).


Automatically loading Logtalk at Prolog startup
-----------------------------------------------

The `$LOGTALKHOME/integration` sub-directory contains shell scripts for running
Logtalk with all supported backend Prolog compilers. You can use these scripts
as examples when creating initialization files for other Prolog compilers. Be
sure to read the `adapters/NOTES.md` file notes on the Prolog compilers that
you intend to use.

There's also a `logtalk_backend_select` script that creates a `logtalk` alias
to the Prolog integration script of your choice.


Customizing Logtalk
-------------------

Please see the file [CUSTOMIZE.md](CUSTOMIZE.md) for details on how to
customize your Logtalk installation and working environment.
