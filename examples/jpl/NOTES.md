________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains a minimal abstraction of the JPL library distributed
with SWI-Prolog and YAP. This abstraction makes use of Logtalk parametric
objects and allows creating Java object, accessing Java class fields, and
calling Java class and object methods using a more Logtalk-like syntax.

For demonstration, adaptations of the JColorChooser and JOptionPane dialog
examples and the JTable example from the JPL distribution are included.

There are two loader files in this example, The `loader.lgt` file loads
only the JPL library and the Logtalk abstraction of the JPL API. The
`examples.lgt` file loads base files and the examples.

When running the GUI examples on the Mac OS X Terminal application, you may
get a Java error saying that the AWT cannot be started. In alternative, try
to run the example from within the SWI-Prolog Mac OS X application instead
of using the shell integration script. This issue is due to a Mac OS X Java
issue that's orthogonal to both SWI-Prolog/YAP and Logtalk.
