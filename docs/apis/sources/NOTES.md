________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains files used by the Logtalk API documentation. The
sources are generated automatically in reStructuredText markup language
using the `build.*` scripts and then converted to HTML, PDF, ePub, Texinfo,
and Markdown formats using Sphinx:

http://sphinx-doc.org/

The conversion uses the `sphinx_rtd_theme` theme:

https://github.com/rtfd/sphinx_rtd_theme

The Sphinx configuration file, `conf.py`, is renamed in this directory
to `_conf.py` to avoid being picked up by the Read the Docs website
builds. This file is temporarily renamed to the correct name by the
`build.*` scripts.

The `_templates/layout.html` file adds the links to the SVG diagrams
and the index to the sidebar of the generated HTML documentation.

The required Python packages can be installed using the commands:

	$ python3 -m pip install --upgrade pygments
	$ python3 -m pip install --upgrade sphinx
	$ python3 -m pip install --upgrade sphinx_rtd_theme

Required versions are:

- Sphinx 7.4.7 or later
- RTD theme 2.0.0 or later
- Pygments 2.19.1 or later

The APIs are also documented using SVG diagrams. These can be generated
using the `update_svg_diagrams.*` scripts, which must be called from a
Logtalk git clone directory	as the generated diagrams link to the files
at the latest commit at GitHub. [GraphViz](https://www.graphviz.org/) is
required (version 2.43.20191029.1313 or later).
