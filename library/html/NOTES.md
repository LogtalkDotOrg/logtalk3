________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2021 Paul Brown <pbrown@optimusprime.ai>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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
Non-atomic element content can be represented as a quoted atom or by using
the `pre` or `code` elements as explained above.

This library is a work in progress.


API documentation
-----------------

Open the [../../apis/library_index.html#html](../../apis/library_index.html#html)
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
a list of compound terms or a compound term for an element other than `html`.
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


Working with callbacks to generate content
------------------------------------------

Often we need to programmatically generate HTML content from queries. In other
cases, we may have fixed content that we don't want to keep repeating (e.g., a
navigation bar). The library supports a `(::)/2` pseudo-element that sends a
message to an object to retrieve content. As an example, assume the following
predicate definition in `user`:

	content(strong('Hello world!')).

This predicate can then be called from the HTML term representation. For
example:

	| ?- current_output(Stream),
	     html5::generate(stream(Stream), span(user::content)).

	<span><strong>Hello world!</strong></span>

	Stream = ...

Note that the callback always takes the form `Object::Closure` where `Closure`
is extended with a single argument (to be bound to the generated content).
More complex callbacks are possible by using lambda expressions.


Working with resource declarations
---------------------------------

The renderer accepts explicit resource declarations in addition to the
usual HTML elements. The supported resource declarations are:

- `resource(Type, Target)`
- `resource(Type, Target, Attributes)`
- `css(Href)`
- `js(Src)`
- `resources(Declarations)`
- `dependency(Dependent, Required)`

Here, `Type` is `css` or `js`, `Target` is the stylesheet or script location,
`Attributes` is an optional list of HTML attributes added to the generated
`<link>` or `<script>` element, `Href` and `Src` are shorthand forms for the
corresponding stylesheet or script target, `Declarations` is a list of
resource specifications to aggregate, and `dependency(Dependent, Required)`
expresses that the resource `Dependent` must be emitted after `Required` when
ordering the generated output.

The atom `stylesheet` can also be used as an alternative to `css`. The atoms
`javascript` and `script` can also be used as an alternative to `js`.

For example, the following term renders standard stylesheet and JavaScript
links directly:

	| ?- current_output(Stream),
	     html5::generate(stream(Stream),
	         resources([
	             css('base.css'),
	             resource(js, 'app.js', [defer-true])
         ])).

This produces `<link rel="stylesheet" href="base.css" />` and
`<script defer src="app.js"></script>`-style output.


Aggregating and ordering resources
----------------------------------

A list of resource declarations can be wrapped with `resources/1` to
normalize and aggregate them into a deterministic output order. Repeated
entries are removed automatically, and `dependency(Dependent, Required)` can
be used to force a resource to be emitted after its prerequisite.

For example:

	| ?- current_output(Stream),
	     html5::generate(stream(Stream),
	         resources([
	             dependency(resource(css, 'theme.css'), resource(css, 'base.css')),
	             css('theme.css'),
	             css('base.css'),
	             css('theme.css')
         ])).

This renders the `base.css` stylesheet before `theme.css`, while also
removing the duplicated `theme.css` entry from the final output.


Working with custom elements
----------------------------

The `html5` and `xhtml11` objects recognize the same set of standard HTML 5
normal and void elements and generate an error for non-standard elements. If
you need to generate HTML content containing custom elements, define a new
object that extends one of the library objects. For example:

	:- object(html5custom,
		extends(html5)).

		normal_element(foo, inline).
		normal_element(bar, block).
		normal_element(Name, Display) :-
			^^normal_element(Name, Display).

	:- end_object.
