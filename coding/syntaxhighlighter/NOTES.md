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


This directory contains files that provide support for using version 3.0.83 
or later of the SyntaxHighlighter package by Alex Gorbatchev with Logtalk 
source files. A detailed description on the SyntaxHighlighter package is 
available from:

	http://alexgorbatchev.com/SyntaxHighlighter/

These support files are dual-licensed under the Apache License 2.0 and the
SyntaxHighlighter license.

In order to check if your SyntaxHighlighter distribution already includes 
support for Logtalk, look for a file named `shBrushLogtalk.js` in the
`scripts` folder. If support for Logtalk is not included or if it is
outdated, copy the file `shBrushLogtalk.js` to the `scripts` folder.

The file `source.html` is a test file based on the `../tests/source.lgt`
file. In order to use this test file, copy the referenced SyntaxHighlighter
files (`shCore.js`, `shCore.css`, and `shThemeEclipse.css`) to the same
directory and open it in your favorite web browser.
