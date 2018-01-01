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


To load this example and for sample queries, please see the `SCRIPT.txt`
file. Running all the examples and the benchmarks requires SWI-Prolog with
the JPL library installed. YAP most likely would be usable when the old JPL
library bundled with it is updated. Some of the examples (but not the
benchmarks) can also be used with JIProlog.

This folder contains examples, most of them adapted from the JPL library
distributed with SWI-Prolog and YAP, of calling Java from Logtalk. It uses
a lightweight abstraction included in the Logtalk library for calling Java.

Adaptations of the JColorChooser and JOptionPane dialog examples and the
JTable example from the JPL distribution are included.

When running the GUI examples on the Mac OS X Terminal application, you may
get a Java error saying that the AWT cannot be started. In alternative, try
to run the example from within the SWI-Prolog Mac OS X application instead
of using the shell integration script. This issue is due to a Mac OS X Java
issue that's orthogonal to both SWI-Prolog/YAP and Logtalk.
