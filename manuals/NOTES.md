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


To consult the Logtalk documentation, open the file `index.html` with a
web browser.

The HTML, PDF, ePub, Texinfo, and Markdown versions of the documentation
are generated from reStructuredText sources in the `sources` directory
using Sphinx. See the `sources/NOTES.md` file for details. Only the HTML
version is included in the sources distribution. The PDF, ePub, Texinfo,
and Markdown versions can be downloaded from the Logtalk website (for the
latest stable release) at:

https://logtalk.org/documentation.html

The HTML, PDF, and ePub versions (for the current git version) can be
downloaded from the Read the Docs website at:

https://logtalk3.readthedocs.io/en/latest/

This website also provides improved search features.

The Markdown version is a LLM friendly single file. Depending on the LLM
pr AI coding tool, you may need to split in several files for indexing due
to file size limitations. For example, using the POSIX `split` utility
command:

	$ split -p '## (Reference Manual|Developer Tools)' TheLogtalkHandbook-3.90.1.md TheLogtalkHandbook_
	$ for file in "TheLogtalkHandbook_"*; do mv "$file" "${file}.md"; done

Experiment with the section headers until all generated files are below
the maximum size.
