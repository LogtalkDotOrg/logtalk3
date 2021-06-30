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
need to edit `lgt_query.tcl` file definition of the `load_cmd` variable
and replace `logtalk` by the name of the integration script you want to
use (e.g. `gplgt`).

To run the example, open the `gui.tcl` file using the `wish` application
provided by your Tk installation. On POSIX systems, you can also simple
call the `run` script from a terminal window:

     $ ./run

Note: This example requires Tcl/Tk and `tcllib` are also installed. Tested
with Tcl/Tk version 8.6 and `tcllib` version 1.20.
