________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt` file.

This example illustrates how to use Logtalk event-driven programming support
for implementing a simple message logger for messages sent from the command-
line (i.e. from the pseudo-object `user`). If you need more than one message 
logger, just create a new prototype as an extension of the object `msglog`.
