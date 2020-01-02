________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


PDT is a free Prolog IDE provided as a plug-in for the Eclipse IDE
that's available at:

    https://sewiki.iai.uni-bonn.de/research/pdt/start

PDT is being extended with Logtalk support. The default settings file
contains some example settings for Logtalk development using PDT.

PDT currently requires a recent version of SWI-Prolog. It's possible
to start up Logtalk automatically by configuring PDT runtime preferences.
The configuration depends on the operating-system.

After installing Logtalk and setting the LOGTALKHOME and LOGTALKUSER
environment variables, open Eclipse preferences, select PDT runtime
preferences, and enter the following configuration data for the SWI-Prolog
executable and for the extra environment variables (change the paths to
match your SWI-Prolog and Logtalk installations and your home directory):

macOS

/Users/pmoura/bin/swipl -s /opt/local/share/logtalk/integration/logtalk_swi.pl
DISPLAY=:0.0, HOME=/Users/pmoura, LOGTALKHOME=/opt/local/share/logtalk, LOGTALKUSER=/Users/pmoura/logtalk

Notes: full paths should be used; no quotes should be used.

Windows

cmd.exe /c start "cmdwindow" /min "C:\Program Files\pl\bin\swipl-win.exe" -s "%LOGTALKHOME%\integration\logtalk_swi.pl"

Linux

/usr/bin/swipl -L4m -G4m -T4m -s /usr/share/logtalk/integration/logtalk_swi.pl
LOGTALKHOME=/usr/share/logtalk, LOGTALKUSER=/home/pmoura/logtalk

Notes: full paths should be used; no quotes should be used.

Logtalk 2.43.0 or a later version with the source_data compiler flag
turned on is necessary for using PDT. Currently supported PDT features
include consulting Logtalk source files, the source file outline (which
requires the source code to be consulted first), and selecting a goal
and searching for all declarations and definitions (you must select the
whole goal and control-click on the selection).
