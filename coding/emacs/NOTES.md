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


This directory contains a `logtalk.el` file that provides syntax 
highlighting for editing Logtalk source files with the Emacs text 
editor:

	http://www.gnu.org/software/emacs/emacs.html

These support files are dual-licensed under the Apache License 2.0 and the
Emacs license.

To install follow the instructions contained in the file itself.

Emacs regular expressions don't support look-ahead assertions, which
result is syntax coloring errors in valid code such as the `0'"` term.