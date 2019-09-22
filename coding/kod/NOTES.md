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


Kod is a free macOS text editor available from:

	http://kodapp.com/

Kod already includes support for syntax highlight of Logtalk source files.
The syntax highlighting support uses GNU Syntax Highlight, which is also
supported by Logtalk. In order to check if Kod built-in support for Logtalk
is out-of-date, control-click on the Kod application icon, select the "Show
Package Contents" menu item, and compare the modification date of the
"Contents/Resources/lang/logtalk.lang" file with the GNU Syntax Highlight
"logtalk.lang" distributed with Logtalk. Be aware that Kod adds a header to
the "*.lang" files that you must preserve when updating. In the Kod 0.0.2
beta version this header is:

# @title Logtalk
# @matchuti public.logtalk-source
# @matchext lgt
