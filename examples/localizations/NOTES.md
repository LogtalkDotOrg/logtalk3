________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  

Logtalk is free software. You can redistribute it and/or modify it under
the terms of the FSF GNU General Public License 3  (plus some additional
terms per section 7).        Consult the `LICENSE.txt` file for details.
________________________________________________________________________


To load this example and for sample queries, please see the `SCRIPT.txt`
file.

This folder contains an example of using the message printing mechanism
to provide software localization in several natural languages. Each language
localization is defined in its own category. Typically, only the category
for the selected language would be loaded, possibly with the help of
conditional compilation directives. But we load them all in this example.
The selected language can be represented in several ways. Here we use a
single object parameter. Another alternative would be to use any mechanism
for setting the default language (e.g. a simple global fact). The main
point is that an application core logic should be decoupled from the
natural language used when interacting with the user. Customizing the
application for another natural language should ideally be as simple as
defining a set of entities holding the message text translations and
loading these localization files at application startup. The solution
used in this example support loading more than one localization at the
same time but this is usually not required. In the simple case where
a single localization would be loaded at any given time, the code can be
simplified by removing the country code parametrization from the core
logic.
