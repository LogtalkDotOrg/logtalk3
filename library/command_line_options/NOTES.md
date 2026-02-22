________________________________________________________________________
This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
SPDX-FileCopyrightText: 2011-2015 Marcus Uneson <marcus.uneson@ling.lu.se>
SPDX-License-Identifier: BSD-2-Clause

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
________________________________________________________________________


`command_line_options`
======================

This library provides command-line parsing predicates where command-line
options are defined as objects that import the `command_line_option`
category. Predicates are also provided for generating help text.

Parsed options are returned by default as a list of `Name(Value)` terms.
A parser option, `output_functor(Functor)`, allows returning options as
`Functor(Key, Value)` terms instead.

Adapted with significant changes from the SWI-Prolog `optparse` library
by Marcus Uneson. Most documentation, parsing options, and help options
retained from the original library. Command line options specification
changed to an object-based representation. Parsing simplified, no longer
using DCGs. Comprehensive set of tests added, based on documentation
examples. Full portability to all supported backends.

API documentation
-----------------

Open the [../../apis/library_index.html#command_line_options](../../apis/library_index.html#command_line_options)
file in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(command_line_options(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(command_line_options(tester)).

Terminology
-----------

The terminology is partly borrowed from Python (see
http://docs.python.org/library/optparse.html#terminology). _Arguments_
are what you provide on the command line and for many Prolog systems
show up as a list of atoms `Args` in `current_prolog_flag(argv, Args)`.
They can be divided into:

- _Runtime arguments_, which control the Prolog runtime; conventionally
  terminated by `--`.
- _Options_, which are key-value pairs (with a boolean value possibly
  implicit) intended to control your program.
- _Positional arguments_, which are what remains after all runtime
  arguments and options have been removed (with implicit arguments
  `true`/`false` for booleans filled in).

Positional arguments are used for mandatory arguments without sensible
defaults (e.g., input file names). Options offer flexibility by letting
you change a default setting. This library has no notion of mandatory
or required options.

Defining option objects
-----------------------

Define command-line options by creating objects that import the
`command_line_option` category. Each object represents one command-line
option and overrides the predicates to specify its properties.

Create an object that imports `command_line_option` and override the
following predicates:

- `key/1`: The key used to identify this option in the result.
  This predicate must be overridden.

- `short_flags/1`: List of single-character short flags
  (e.g., `[v]` for `-v`). Default: `[]`.

- `long_flags/1`: List of long flags
  (e.g., `[verbose]` for `--verbose`). Default: `[]`.

- `type/1`: Option value type. One of `boolean`, `atom`, `integer`,
  `float`, or `term`. Default: `term`.

- `default/1`: Default value. Optional.

- `meta/1`: Metasyntactic variable name for help text.
  Default: `''`.

- `help/1`: Help text (atom or list of atoms for pre-broken lines).
  Default: `''`.

Validating option definitions
-----------------------------

Option objects provide two predicates for validating their definitions:

- `check/0`: Validates the option definition, throwing an error if
  invalid. The validation checks:
  - The `key/1` predicate is defined (not just using the default).
  - All short flags are single characters.
  - The type is a known type (supported by the `type` library).
  - If a default value is defined and the type is not `term`, the
    default value matches the declared type.

- `valid/0`: Succeeds deterministically if the option definition
  is valid, fails otherwise. This is useful when you want to check
  validity without handling exceptions.

The `parse/4-5` and `help/2-3` predicates automatically call `check/0` on all
option objects before processing, ensuring invalid definitions are caught
early with clear error messages.

Example:

	| ?- verbose_option::check.
	yes

	| ?- verbose_option::valid.
	yes

Example option objects
----------------------

	:- object(verbose_option,
		imports(command_line_option)).

		key(verbose).
		short_flags([v]).
		long_flags([verbosity]).
		type(integer).
		default(2).
		meta('V').
		help('verbosity level, 1 <= V <= 3').

	:- end_object.


	:- object(mode_option,
		imports(command_line_option)).

		key(mode).
		short_flags([m]).
		long_flags([mode]).
		type(atom).
		default('SCAN').
		help(['data gathering mode, one of',
		      '  SCAN: do this',
		      '  READ: do that',
		      '  MAKE: fabricate numbers',
		      '  WAIT: do nothing']).

	:- end_object.


	:- object(cache_option,
		imports(command_line_option)).

		key(cache).
		short_flags([r]).
		long_flags(['rebuild-cache']).
		type(boolean).
		default(true).
		help('rebuild cache in each iteration').

	:- end_object.


	:- object(outfile_option,
		imports(command_line_option)).

		key(outfile).
		short_flags([o]).
		long_flags(['output-file']).
		type(atom).
		meta('FILE').
		help('write output to FILE').

	:- end_object.


	% Configuration parameter without command-line flags
	:- object(path_option,
		imports(command_line_option)).

		key(path).
		default('/some/file/path/').

	:- end_object.

Parsing command-line arguments
-------------------------------

Use the `command_line_options` object `parse/4-5` predicates:

	| ?- command_line_options::parse(
	         [verbose_option, mode_option, cache_option, outfile_option],
	         ['-v', '5', '-m', 'READ', '-ooutput.txt', 'input.txt'],
	         Options,
	         PositionalArguments
	     ).

	Options = [verbose(5), mode('READ'), cache(true), outfile('output.txt')],
	PositionalArguments = ['input.txt'].

Generating help text
--------------------

Use the `command_line_options::help/2` predicate for default formatting:

	| ?- command_line_options::help([verbose_option, mode_option, cache_option], Help).

Or use `command_line_options::help/3` with custom help options:

	| ?- command_line_options::help([verbose_option, mode_option], Help, [line_width(100)]).

Help options
------------

The `help/3` predicate supports the following help options:

- `line_width(Width)`: Maximum line width for help text.
  Default: `80`.

- `min_help_width(Width)`: Minimum width for help text column.
  Default: `40`.

- `break_long_flags(Boolean)`: If `true`, break long flags across
  multiple lines. Default: `false`.

- `suppress_empty_meta(Boolean)`: If `true` (default), suppress empty
  metasyntactic variables in help output.

Parse options
-------------

The `parse/5` predicate supports the following parse options:

- `output_functor(Functor)`: When defined, options are returned as
  `Functor(Key, Value)` terms instead of `Key(Value)` terms. No default.

- `duplicated_flags(Keep)`: How to handle duplicate options.
  One of `keepfirst`, `keeplast`, `keepall`. Default: `keeplast`.

- `allow_empty_flag_spec(Boolean)`: If `true` (default), options
  without flags are allowed (useful for configuration parameters).
  Set to `false` to raise errors on empty flags.

Notes and tips
--------------

- The default type is `term`, which subsumes `integer`, `float`, and
  `atom`. However, always specifying types is recommended for reliable
  parsing and clear error messages.

- `-sbar` is taken as `-s bar`, not as `-s -b -a -r` (no clustering).

- `-s=foo` is disallowed.

- Duplicate flags default to `keeplast` (controllable via the
  `duplicated_flags` parse option).

- Unknown flags (not in the specification) will raise errors.
