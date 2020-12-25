________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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


This directory contains files that provide snippets and tools for
working with Logtalk source code using Gnome's Gedit text editor.
For more information, visit the Gedit plugin websites:

	http://live.gnome.org/Gedit/Plugins/Snippets
	http://live.gnome.org/Gedit/ToolLauncherPlugin

For syntax coloring support see the `../gtksourceview2` and
`../gtksourceview3` folders (depending on the Gedit version).


To install the snippets support, copy the file `logtalk.xml` to  the
following system-wide directory:

	${prefix}/share/gedit/plugins/snippets

The ${prefix} can be e.g. `/usr`, `/usr/local`, or `/opt`, depending 
on your system configuration.

Alternatively, you can copy the `logtalk.xml` file to one of the following 
locations on your home directory:

	Gedit 2.x: ~/.gnome2/gedit/snippets/
	Gedit 3.x: ~/.config/gedit/snippets

To install the tools support, copy the files in the `tools` directory to
the following system-wide directory:

	${prefix}/share/gedit/plugins/tools

The ${prefix} can be e.g. `/usr`, `/usr/local`, or `/opt`, depending on
your system configuration.

Alternatively, you can copy the files in the `tools` directory to one of
the following locations on your home directory:

	Gedit 2.x: ~/.gnome2/gedit/tools/
	Gedit 3.x: ~/.config/gedit/tools/

Before using the provided tools, edit and customize them in order to
use your favorite backend Prolog compiler and to set your choices
for the (X)HTML and PDF generation scripts. The `xdg-open` command
must available to use most of the provided commands.
