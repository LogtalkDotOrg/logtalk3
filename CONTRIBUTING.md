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


CONTRIBUTING
============

Thanks for considering contributing to Logtalk. All contributions are most
welcome, including code patches, bug reports, documentation fixes, feature
suggestions, portability improvements, new libraries, new examples, and
improved support for text editors and IDEs. No contribution is too small.
The [ACKNOWLEDGMENTS.md](ACKNOWLEDGMENTS.md) file lists past contributions
and their authors. Looking forward to seeing your name there. Contributors
are expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md).


How to make a contribution
--------------------------

For bug reports and suggestions, create a ticket in our issue tracker at:

https://github.com/LogtalkDotOrg/logtalk3/issues

If you prefer to discuss your contribution first with other developers,
you can join us for a live discussion in our chat room:

https://gitter.im/LogtalkDotOrg/logtalk3

In alternative, you can also join our discussion forums at:

https://github.com/LogtalkDotOrg/logtalk3/discussions

For contributing improvements and fixes to the Handbook, open it in your
web browser, click on the link on the top right that says "Edit on GitHub",
make the proposed changes, and submit them as a pull request.

In the specific case of developer tools and libraries documentation, note
that their Handbook sections are automatically generated from the tool and
library directory `NOTES.md` files.

In the case of code contributions, you are required to follow the coding
guidelines described at:

https://github.com/LogtalkDotOrg/logtalk3/wiki/Coding-Style-Guidelines

Submit your code contribution by forking Logtalk, working on a topic branch,
and creating a pull request. If you're not familiar with pull request based
contributions to open source projects, see e.g.

https://egghead.io/courses/how-to-contribute-to-an-open-source-project-on-github

When contributing compiler, runtime, or library enhancements, ensure there
are no regressions by testing your contribution. For details, see:

https://github.com/LogtalkDotOrg/logtalk3/wiki/Testing

Ideally, new code should come with a comprehensive test set. For writing tests,
see our unit testing framework documentation at:

https://github.com/LogtalkDotOrg/logtalk3/blob/master/tools/lgtunit/NOTES.md

Please note that **portability** is a main Logtalk feature. As a general rule,
new code should depend only on standard or de facto standard features available
from the supported backend Prolog systems. But contributions that enhance the
integration with a particular backend Prolog system are also sought as long
they don't break support for other systems.

Integrating your contribution
-----------------------------

Major code and documentation contributions require the contributor to sign-off
that they adhere to the Developer Certificate of Origin (DCO):

https://developercertificate.org

Contributions should be made available under the Logtalk license without any
additional terms or conditions. Contributions using other licensing terms may
also be distributed with Logtalk with the understanding that the terms of their
use depends solely on the authors chosen license terms and may require a
separate and independent agreement between users and authors.
