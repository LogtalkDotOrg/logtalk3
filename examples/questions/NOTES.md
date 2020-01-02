________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This example illustrates the question asking mechanism, which complements
the message printing mechanism to allow questions to be abstracted and
intercepted. The question used in this example is taken from Douglas Adams
book "The Hitchhiker's Guide to the Galaxy".

The `loader.lgt` file loads code that asks the question in the top-level
interpreter.

The `loader_gui.lgt` file loads code that asks the question using a Java
GUI dialog. It can be used with JIProlog, SWI-Prolog, or YAP. Running on
SWI-Prolog or YAP requires a functional JPL library installation.

When running the GUI dialog on the macOS Terminal application, you may get
a Java error saying that the AWT cannot be started. In alternative, try
to run the GUI version using either JIProlog or from within the SWI-Prolog
macOS application instead of using the shell integration script. This issue
is due to a macOS Java issue that's orthogonal to both SWI-Prolog/YAP and
Logtalk.
