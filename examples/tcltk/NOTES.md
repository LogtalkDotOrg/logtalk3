________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 2021 Paul Brown <pbrown@optimusprime.ai>
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


This folder contains an example of using Tcl/Tk to provide a GUI for the
`symdiff` example. It assumes that the `logtalk_backend_select` script
was run to make the `logtalk` shell script available. If not, you will
need to edit `query.tcl` file definition of the `load_cmd` variable and
replace `logtalk` by the name of the integration script you want to use
(e.g. `gplgt`) if using a POSIX system. On Windows, `logtalk` needs to
be replaced by the integration shortcut command for the backend you
intend to use. That command must not use any environment variables and
backslashes, spaces, and square brackets must be properly escaped as
per Tcl requirements.

To run the example, open the `gui.tcl` file using the `wish` application
provided by your Tk installation.

This example as successfully tested with Tcl/Tk version 8.6.11 and `tcllib`
version 1.20 on Linux and macOS. Tests on Windows have so far only worked
with some backends. The issue seems to be how Windows applications handle
the standard input/output streams that can break the current solution for
connecting the background Logtalk process to the Tk interface for some
backends.
