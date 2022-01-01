________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains files that provide:

- Support for syntax highlighting and other text editing services for text
editors and IDEs.

- Support for syntax highlighters (used in e.g in wikis, code repositories,
source code browsers, bug trackers, and shells).

- Support for setting Logtalk projects using common version control systems
(e.g. git).

- Sample source files for testing syntax highlighting support.

The Logtalk syntax coloring support is also usable for Prolog coding.

The files in this directory can be regarded as contributions to third-party
text editors and syntax highlighters. Thus, as a general rule, these files
are dual-licensed under the Logtalk license and the license used by the
third-party software.

Some text editors already include support for Logtalk. If that is the case
of your favorite editor, you may want to check if the supporting files
provided in this directory are newer than the editor ones.

Logtalk source files (including the library entities and the programming
examples) are indented using tabs and are expected to remain perfectly 
indented no matter your tab width preference (a common setting is a tab
width equivalent to 4 spaces).

You may notice that support for some text editors, notably for Windows-only 
editors, is relatively poor. Depending on the text editor, proper syntax
highlighting support for Prolog and Logtalk may require support for
regular expressions with zero-width look-ahead and look-behind assertions,
which some editors lack.

Some text editors supporting regular expressions contain bugs and/or 
hard-coded limitations that prevent full optimization of the syntax 
coloring patterns.


Online code formatters
----------------------

http://hilite.me is an online service that converts code snippets into
pretty-printed HTML format. It supports many programming languages,
including Logtalk and Prolog.

