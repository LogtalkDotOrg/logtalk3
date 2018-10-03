________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


This directory contains the sources for the Logtalk documentation (which
include the User Manual, Reference Manual, FAQ, and Glossary). The sources
are written using the reStructuredText markup language and converted to
HTML, PDF, and ePub formats using Sphinx with the `sphinx_rtd_theme` theme:

http://sphinx-doc.org/

The included Makefile allows exporting the documentation in the final
formats:

	$ make clean && make html && make latexpdf && make epub

After completion, the exported files are found in the `_build` directory.
The `build_docs.sh` bash shell script can be used to both export the final
formats and move the final files to the `manuals` root directory in the
distribution.
