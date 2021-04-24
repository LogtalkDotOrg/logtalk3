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


This directory contains files used by the Logtalk API documentation. The
sources are generated automatically in reStructuredText markup language
using the `update_html_docs.sh` script and then converted to HTML, PDF,
and ePub formats using Sphinx:

http://sphinx-doc.org/

The conversion uses the `sphinx_rtd_theme` theme:

https://github.com/rtfd/sphinx_rtd_theme

The Sphinx configuration file, `conf.py`, is renamed in this directory
to `_conf.py` to avoid being picked up by the Read the Docs website
builds. This file is temporarily renamed to the correct name by the
`update_html_docs.sh` script.

The `_templates/layout.html` file adds the links to the SVG diagrams
and the index to the sidebar of the generated HTML documentation.

The required Python packages can be installed using the commands:

	$ sudo pip install --upgrade pygments
	$ sudo pip install --upgrade sphinx
	$ sudo pip install --upgrade sphinx_rtd_theme
