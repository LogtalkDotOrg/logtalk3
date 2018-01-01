________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
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


CONTRIBUTING
============

Thanks for considering contributing to Logtalk. All contributions are most
welcome, including code patches, bug reports, documentation fixes, feature
suggestions, portability improvements, new libraries, new examples, and
improved support for text editors and IDEs. No contribution is too small.
The [ACKNOWLEDGMENTS.md](ACKNOWLEDGMENTS.md) file lists past contributions
and their authors. Looking forward to see your name there.


How to make a contribution
--------------------------

Bur bug reports and suggestions, create a ticket in our issue tracker at:

	https://github.com/LogtalkDotOrg/logtalk3/issues

If you prefer to discuss your contribution first with other developers,
you can join us for live discussion at our chat room:

	https://gitter.im/LogtalkDotOrg/logtalk3

In alternative, you can also join our discussion forums at:

	http://forums.logtalk.org/

In the case of code contributions, follow the coding guidelines described at:

	https://github.com/LogtalkDotOrg/logtalk3/wiki/Coding-Style-Guidelines

Submit your code contribution preferably by forking Logtalk, working on a topic
branch, and creating a pull request. If you're not familiar with pull request
based contributions to open source projects, see e.g.

	https://egghead.io/courses/how-to-contribute-to-an-open-source-project-on-github

When contributing compiler, runtime, or library enhancements, ensure there
are no regressions by testing your contribution. For details, see:

	https://github.com/LogtalkDotOrg/logtalk3/wiki/Testing

Ideally, new code should come with a comprehensive test set. For writing tests,
see our unit testing framework documentation at:

	https://github.com/LogtalkDotOrg/logtalk3/blob/master/tools/lgtunit/NOTES.md

Please note that portability is main Logtalk feature. As a general rule, new code
should depend only on standard or de factor standard features available from the
supported backend Prolog systems. But contributions that enhance the integration
with a particular backend Prolog system are also sought as long they don't break
support for other systems.

Integrating your contribution
-----------------------------

Major code and documentation contributions may require the contributor to sign
and submit a contributor license agreement and should be made available under
the Logtalk license without any additional terms or conditions. Contributions
using other licensing terms may also be distributed with Logtalk with the
understanding that the terms of their use depends solely on the authors chosen
licensing terms and may require a separate, independent, agreement between
users and authors.
