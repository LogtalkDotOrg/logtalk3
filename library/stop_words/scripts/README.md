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


Stop-word source scripts
========================

The `update_stopwords.sh` script downloads and converts an individual
[`stopwords-iso`](https://github.com/stopwords-iso) language repository:

	$ ./update_stopwords.sh REPOSITORY COMMIT

For example:

	$ ./update_stopwords.sh stopwords-fr 069f556be89f2f08de2f1b8313084bb4610f30b4

The repository name determines the source filename, generated object, and
output filename. For example, `stopwords-fr` produces `stopwords_fr.lgt` defining
the `stopwords_fr` object. A full commit identifier is required so regeneration
is reproducible.

The updater stores the source list and original license under
`sources/REPOSITORY/`, then calls `generate_stopwords.sh`. The generator validates
the repository name, commit identifier, MIT license, nonempty lowercase source
entries, and uniqueness before writing indexed `stop_word/1` facts. For the
English source, it also omits four malformed non-ASCII reflexive-pronoun entries
whose correct forms are already present, producing an ASCII-only object.
