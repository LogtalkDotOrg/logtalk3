________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains files used when building Windows GUI installers
using Inno Setup 6.0.2 Unicode (or a later version):

http://www.jrsoftware.org/isinfo.php

The Inno Setup script, `logtalk.iss`, assumes that a clone of the Logtalk
git repository exists in the `C:\lgt3git` directory.

The default location for the Logtalk system files installation when the
`%LOGTALKHOME%` environment variable is not defined is:

- For admin user installations:
	* `C:\Program Files (x86)\Logtalk` on 64-bit Windows
	* `C:\Program Files\Logtalk` on 32-bit Windows

- For non-admin user installations:	
	* `%LOCALAPPDATA%\Logtalk`

The default location for the Logtalk user files installation when the
`%LOGTALKUSER%` environment variable is not defined is:

- `%USERPROFILE%\Documents\Logtalk`

This `%LOGTALKUSER%` directory can be re-created by re-running the
installer and choosing the corresponding installation option or by
running the `logtalk_user_setup.bat` script.

The installer makes available PowerShell integration scripts from the
Windows system directory. Requires PowerShell 7.3 or a later version.

The installer also creates integration shortcuts that start Logtalk
in the `%LOGTALKUSER%` directory. You can edit the building script
and change the `Start in` setting to `%CD%`. However, this works on
Windows XP but is ignored on Windows 7.

Note that the integration shortcuts are only created for the Prolog
systems that are detected when running the installer. Most Prolog
installers write entries in the Windows registry that are read by
the Logtalk installer. Some Prolog systems, however, use environment
variables. If a shortcut is not being created for a supported and
installed Prolog compiler, possibly due to a non-standard install,
check the registry and environment variable assumptions made by the
installer.

As Logtalk first looks at startup for a `settings.lgt` file in the
current directory, in order to use a project-specific settings file,
copy the desired shortcut to the project directory and use its path
as the value of the `Start in` shortcut setting.

The installer can be run in silent mode when executed at the command-line
by using the option `/SILENT` (progress window is shown) or `/VERYSILENT`
(no progress window). Currently, the installer cannot be run in system
context.

The installation directory can be set at the command-line by using
the option `/DIR=path` or by defining the value of the `LOGTALKHOME`
environment variable prior to running the installer.
