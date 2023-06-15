________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains files that provides basic syntax coloring, 
entity and public predicate index, and code completion for editing 
Logtalk source files with the text editor SuperEdi 3.8.1 or later 
version:

	http://www.wolosoft.com/en/superedi/index.html

Install the supporting files by performing the following steps:

1.	Copy the file `Logtalk.syn` to the `Syntax` directory in your 
	SuperEdi installation directory (replacing any existing older file).

2.	Copy the file `Logtalk.col` to the `Syntax` directory in your 
	SuperEdi installation directory (replacing any existing older file).

3.	Go to the Tools/Options dialog and create a new file type named 
	`Logtalk`. Set the file extensions to `lgt` and `logtalk` and the
	syntax and color scheme to `Logtalk`. Set the tab size to `4`.

THESE SYNTAX COLORING SUPPORT FILES ARE UNDER DEVELOPMENT.
