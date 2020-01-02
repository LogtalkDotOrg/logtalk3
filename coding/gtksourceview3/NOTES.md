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


This directory contains files that provide syntax highlighting for 
GtkSourceView 3.x, which is a text widget used in text editors such 
as recent versions of Gnome's Gedit and in and IDEs such as Anjuta 
and MonoDevelop. For more information, visit the GtkSourceView 
website:

	http://gtksourceview.sourceforge.net/


These support files are dual-licensed under the Apache License 2.0 and
the GtkSourceView 3.x license.

Recent versions of GtkSourceView may already contain support for Logtalk.
However, this directory may contain updated support files. If that is the
case, install the Logtalk support files as described next.

To install, copy the file `../gtksourceview2/logtalk.lang` to the
following system-wide directory:

	${prefix}/share/gtksourceview-3.0/language-specs/

The ${prefix} can be e.g. `/usr`, `/usr/local`, or `/opt`, depending 
on your system configuration.

Alternatively, you can copy the `logtalk.lang` file to the following 
location on your home directory:

	 ~/.local/share/gtksourceview-3.0/language-specs/
