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


This directory contains a `Logtalk.gitignore` file for versioning Logtalk
projects using git. To use this file, copy it to the root of your git
repository and rename it to `.gitignore`.

For providing better `git diff` hunk header context, create or edit your
`.gitattributes` and add the following lines:

	*.lgt diff=logtalk
	*.logtalk diff=logtalk

Next, edit your `~/.gitconfig` and add the following lines:

	[diff "logtalk"]
	    xfuncname ="^[ \t]*(:- (object|protocol|category)\\((.*)(\\)[.]|[,]))$"
