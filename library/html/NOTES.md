________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paul Brown <pbrown@optimusprime.ai> and
                    Paulo Moura <pmoura@logtalk.org>
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


`html`
======

This library provides predicates for generating HTML content using either
HTML 5 or XHTML 1.1 formats from a term representation. The library performs
minimal validation, checking only that all elements are valid. No attempt is
made to generate nicely indented output.

Normal elements are represented using a compound term with one argument (the
element content) or two arguments (the element attributes represented by a
list of `Key=Value` or `Key-Value` pairs and the element content). The element
content can be another element or a list of elements. For example:

	ol([type-a], [li(foo), li(bar), li(baz)])

The two exceptions are the `pre` or `code` elements whose content is never
interpreted as an element or a list of elements. For example, the fragment:

	pre([foo,bar,baz])
	
is translated to:

	<pre>
	[foo,bar,baz]
	</pre>

Void elements are represented using a compound term with one argument, the
(possibly empty) list of attributes represented by a list of `Key=Value`
or `Key-Value` pairs. For example:

	hr([class-separator])

Atomic arguments of the compound terms are interpreted as element content.
Non atomic element content can be represented as a quoted atom or by using
the `pre` or `code` elements as explained above.

This library is a work in progress.


API documentation
-----------------

Open the [../../docs/library_index.html#os](../../docs/library_index.html#html)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(html(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(html(tester)).


Generating a HTML document
--------------------------

HTML documents can be generated from a compound term representation and written
to a file or a stream. For example, assuming we want to generate a HTML 5 file:

	| ?- html5::generate(
	         file('hello.html'),
	         html([lang=en], [head(title('Hello world!')), body(p('Bye!'))])
	     ).

When the second argument is a `html/1` or `html/2` compound term, a *doctype*
is automatically written. If we prefer instead e.g. a XHTML 1.1 document, we
use the `xhtml11` object:

	| ?- xhtml11::generate(
	         file('hello.html'),
	         html([lang=en], [head(title('Hello world!')), body(p('Bye!'))])
	     ).


Generating a HTML fragment
--------------------------

It's also possible to generate just a fragment of a (X)HTML document by using
a list of compound terms or a compound term for an element other then `html`.
For example:

	| ?- current_output(Stream),
	     html5::generate(stream(Stream), ul([li(foo), li(bar), li(baz)])).

	<ul>
	<li>
	foo</li>
	<li>
	bar</li>
	<li>
	baz</li>
	</ul>

	Stream = ...
