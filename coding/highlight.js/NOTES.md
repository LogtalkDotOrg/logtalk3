________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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


This directory contains files that provide support for using version 9.12.0
or later of highlight.js with Logtalk source files. A detailed description
of highlight.js is available from:

	https://highlightjs.org/

These support files are licensed under the highlight.js license.

Recent versions of highlight.js may already contain support for Logtalk.
However, this directory may contain updated support files. If that is the
case, install the Logtalk support files by performing the following steps:

1. Copy the file `logtalk.js` to the `src/languages` sub-directory in 
your highlight.js source distribution (replacing any existing older file).

2. Copy the folder `logtalk` to the `test/detect` sub-directory in your
highlight.js source distribution (replacing any existing older folder).

3. Build a new `highlight.pack.js` file for use in your web pages. For
example, assuming that you are only highlighting Logtalk code, run the
following shell command from the root of the source distribution:

	$ node tools/build.js -n logtalk

The new `highlight.pack.js` file will be created in the `build` folder.
See the highlight.js for detailed build instructions.
