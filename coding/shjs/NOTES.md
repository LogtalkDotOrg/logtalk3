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


This directory contains the `sh_logtalk.js` file that provides syntax 
coloring support for SHJS (version 0.6 or later):

	http://shjs.sourceforge.net/

These support files are dual-licensed under the Apache License 2.0 and the
SHJS license.

If support for Logtalk is not included or if it is outdated in your SHJS
distribution, just replace the file `sh_logtalk.js` in the SHJS distribution
`lang` directory with the newer file from this directory.

The file `source.html` is a test file based on the `../tests/source.lgt`
file. To use this test file, copy it to the root of your SHJS distribution
directory and open it in your favorite web browser.

SHJS expects the source code to be wrapped in `pre` tags and doesn't seem to
provide any control over tab settings. Therefore, for best results, convert
the tabs in your source code to spaces before copying it to your HTML files.
