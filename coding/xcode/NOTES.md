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


This directory contains some files that provide basic syntax highlighting 
and project and template files for editing Logtalk source files with macOS 
Xcode 1.1 or later version.


To install:

1. Copy the files "logtalk.pbfilespec" and "logtalk.pblangspec" to the 
directory:

	~/Library/Application Support/Apple/Developer Tools/Specifications 

2. Copy the directory "Logtalk" to the directory:
 
	~/Library/Application Support/Apple/Developer Tools/File Templates 

3. Create (if it does not exist) the directory:

	~/Library/Application Support/Apple/Developer Tools/Project Templates/Logtalk

and copy to it the directory Logtalk Application


Although the file logtalk.pblangspec provides basic syntax coloring for the 
Logtalk language, it is recommended that you configure Xcode to use an 
external text editor such as SubEthaEdit or TextMate for editing Logtalk 
source files.


SUPPORT FOR THIS IDE IS A WORK IN PROGRESS.
