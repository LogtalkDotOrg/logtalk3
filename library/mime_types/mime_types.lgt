%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(mime_types).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-17,
		comment is 'MIME type registry and convenience predicates for mapping file names and URLs to media types.',
		remarks is [
			'Default behavior' - 'Convenience predicates default to lenient lookup by consulting both built-in standard mappings and built-in common mappings.',
			'Path decomposition' - 'File path decomposition uses the ``os`` library ``decompose_file_name/4`` predicate.',
			'External overlays' - 'Additional ``mime.types``-style files can be loaded into the in-memory registry using the ``load/1-2`` predicates.'
		]
	]).

	:- public(guess_type/3).
	:- mode(guess_type(+atom, -atom, -atom), one).
	:- info(guess_type/3, [
		comment is 'Guesses the MIME type and content encoding for a URL or file name using lenient lookup. Unknown values are returned as the empty atom.',
		argnames is ['Resource', 'Type', 'Encoding']
	]).

	:- public(guess_type/4).
	:- mode(guess_type(+atom, -atom, -atom, +boolean), one).
	:- info(guess_type/4, [
		comment is 'Guesses the MIME type and content encoding for a URL or file name. When ``Strict`` is ``true``, only built-in standard mappings and strict runtime overlays are consulted.',
		argnames is ['Resource', 'Type', 'Encoding', 'Strict']
	]).

	:- public(guess_file_type/3).
	:- mode(guess_file_type(+atom, -atom, -atom), one).
	:- info(guess_file_type/3, [
		comment is 'Guesses the MIME type and content encoding for a file path using lenient lookup. Unknown values are returned as the empty atom.',
		argnames is ['Path', 'Type', 'Encoding']
	]).

	:- public(guess_file_type/4).
	:- mode(guess_file_type(+atom, -atom, -atom, +boolean), one).
	:- info(guess_file_type/4, [
		comment is 'Guesses the MIME type and content encoding for a file path.',
		argnames is ['Path', 'Type', 'Encoding', 'Strict']
	]).

	:- public(extension_type/2).
	:- mode(extension_type(+atom, -atom), zero_or_one).
	:- info(extension_type/2, [
		comment is 'Returns the MIME type associated with a file extension using lenient lookup.',
		argnames is ['Extension', 'Type']
	]).

	:- public(extension_type/3).
	:- mode(extension_type(+atom, -atom, +boolean), zero_or_one).
	:- info(extension_type/3, [
		comment is 'Returns the MIME type associated with a file extension.',
		argnames is ['Extension', 'Type', 'Strict']
	]).

	:- public(guess_extension/2).
	:- mode(guess_extension(+atom, -atom), zero_or_one).
	:- info(guess_extension/2, [
		comment is 'Returns the preferred file extension associated with a MIME type using lenient lookup.',
		argnames is ['Type', 'Extension']
	]).

	:- public(guess_extension/3).
	:- mode(guess_extension(+atom, -atom, +boolean), zero_or_one).
	:- info(guess_extension/3, [
		comment is 'Returns the preferred file extension associated with a MIME type.',
		argnames is ['Type', 'Extension', 'Strict']
	]).

	:- public(guess_all_extensions/2).
	:- mode(guess_all_extensions(+atom, -list(atom)), one).
	:- info(guess_all_extensions/2, [
		comment is 'Returns all known file extensions associated with a MIME type using lenient lookup.',
		argnames is ['Type', 'Extensions']
	]).

	:- public(guess_all_extensions/3).
	:- mode(guess_all_extensions(+atom, -list(atom)), one).
	:- info(guess_all_extensions/3, [
		comment is 'Returns all known file extensions associated with a MIME type.',
		argnames is ['Type', 'Extensions', 'Strict']
	]).

	:- public(add_type/2).
	:- mode(add_type(+atom, +atom), one).
	:- info(add_type/2, [
		comment is 'Adds a lenient runtime mapping from a MIME type to a file extension.',
		argnames is ['Type', 'Extension']
	]).

	:- public(add_type/3).
	:- mode(add_type(+atom, +atom, +boolean), one).
	:- info(add_type/3, [
		comment is 'Adds a runtime mapping from a MIME type to a file extension.',
		argnames is ['Type', 'Extension', 'Strict']
	]).

	:- public(read_mime_types/2).
	:- mode(read_mime_types(+atom, -list(pair(atom,atom))), one).
	:- info(read_mime_types/2, [
		comment is 'Reads a ``mime.types``-style file returning a list of ``Extension-Type`` pairs.',
		argnames is ['File', 'Pairs']
	]).

	:- public(load/1).
	:- mode(load(+atom), one).
	:- info(load/1, [
		comment is 'Loads a ``mime.types``-style file as a lenient runtime overlay.',
		argnames is ['File']
	]).

	:- public(load/2).
	:- mode(load(+atom, +boolean), one).
	:- info(load/2, [
		comment is 'Loads a ``mime.types``-style file as a strict or lenient runtime overlay.',
		argnames is ['File', 'Strict']
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Clears all runtime MIME type overlays.'
	]).

	:- public(suffix_alias/2).
	:- mode(suffix_alias(?atom, ?atom), zero_or_more).
	:- info(suffix_alias/2, [
		comment is 'Returns suffix aliases used before splitting encoding and type suffixes.',
		argnames is ['Alias', 'Expanded']
	]).

	:- public(encoding_suffix/2).
	:- mode(encoding_suffix(?atom, ?atom), zero_or_more).
	:- info(encoding_suffix/2, [
		comment is 'Returns content-encoding suffix mappings.',
		argnames is ['Extension', 'Encoding']
	]).

	:- private(runtime_type_/3).
	:- dynamic(runtime_type_/3).
	:- mode(runtime_type_(?boolean, ?atom, ?atom), zero_or_more).
    :- info(runtime_type_/3, [
        comment is 'Table of runtime MIME type overlays.',
        argnames is ['Strict', 'Extension', 'Type']
    ]).

	:- uses(list, [
		append/3, reverse/2
	]).

	guess_type(Resource, Type, Encoding) :-
		guess_type(Resource, Type, Encoding, false).

	guess_type(Resource, Type, Encoding, Strict) :-
		guess_file_type(Resource, Type, Encoding, Strict).

	guess_file_type(Path, Type, Encoding) :-
		guess_file_type(Path, Type, Encoding, false).

	guess_file_type(Path, Type, Encoding, Strict) :-
		atom(Path),
		strip_query_fragment(Path, StrippedPath),
		guess_path_type(StrippedPath, Type, Encoding, Strict).

	extension_type(Extension, Type) :-
		extension_type(Extension, Type, false).

	extension_type(Extension, Type, Strict) :-
		normalize_extension(Extension, NormalizedExtension),
		extension_type_lookup(Strict, NormalizedExtension, Type),
		!.

	guess_extension(Type, Extension) :-
		guess_extension(Type, Extension, false).

	guess_extension(Type, Extension, Strict) :-
		guess_all_extensions(Type, Extensions, Strict),
		Extensions = [Extension| _].

	guess_all_extensions(Type, Extensions) :-
		guess_all_extensions(Type, Extensions, false).

	guess_all_extensions(Type, Extensions, Strict) :-
		normalize_type(Type, NormalizedType),
		findall(Extension, type_extension_lookup(Strict, NormalizedType, Extension), Extensions0),
		unique(Extensions0, Extensions).

	add_type(Type, Extension) :-
		add_type(Type, Extension, false).

	add_type(Type, Extension, Strict) :-
		normalize_type(Type, NormalizedType),
		normalize_extension(Extension, NormalizedExtension),
		asserta(runtime_type_(Strict, NormalizedExtension, NormalizedType)).

	read_mime_types(File, Pairs) :-
		open(File, read, Stream),
		catch(
			read_mime_types_stream(Stream, Pairs),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	load(File) :-
		load(File, false).

	load(File, Strict) :-
		read_mime_types(File, Pairs),
		load_pairs(Pairs, Strict).

	reset :-
		retractall(runtime_type_(_, _, _)).

	guess_path_type(Path, Type, Encoding, Strict) :-
		expand_alias(Path, Expanded),
		os::decompose_file_name(Expanded, _, Name, Extension0),
		normalize_extension(Extension0, Extension),
		(   encoding_suffix(Extension, Encoding) ->
			guess_name_type(Name, Type, Strict)
		;   Encoding = '',
			guess_extension_type_or_empty(Extension, Type, Strict)
		).

	guess_name_type('', '', _) :-
		!.
	guess_name_type(Name, Type, Strict) :-
		expand_alias(Name, Expanded),
		os::decompose_file_name(Expanded, _, _, Extension0),
		normalize_extension(Extension0, Extension),
		guess_extension_type_or_empty(Extension, Type, Strict).

	guess_extension_type_or_empty('', '', _) :-
		!.
	guess_extension_type_or_empty(Extension, Type, Strict) :-
		(   extension_type(Extension, Type, Strict) ->
			true
		;   Type = ''
		).

	extension_type_lookup(true, Extension, Type) :-
		runtime_type_(true, Extension, Type).
	extension_type_lookup(true, Extension, Type) :-
		standard_type(Extension, Type).
	extension_type_lookup(false, Extension, Type) :-
		runtime_type_(true, Extension, Type).
	extension_type_lookup(false, Extension, Type) :-
		runtime_type_(false, Extension, Type).
	extension_type_lookup(false, Extension, Type) :-
		standard_type(Extension, Type).
	extension_type_lookup(false, Extension, Type) :-
		common_type(Extension, Type).

	type_extension_lookup(true, Type, Extension) :-
		runtime_type_(true, Extension, Type).
	type_extension_lookup(true, Type, Extension) :-
		standard_type(Extension, Type).
	type_extension_lookup(false, Type, Extension) :-
		runtime_type_(true, Extension, Type).
	type_extension_lookup(false, Type, Extension) :-
		runtime_type_(false, Extension, Type).
	type_extension_lookup(false, Type, Extension) :-
		standard_type(Extension, Type).
	type_extension_lookup(false, Type, Extension) :-
		common_type(Extension, Type).

	read_mime_types_stream(Stream, Pairs) :-
		reader::line_to_codes(Stream, Codes),
		(   Codes == end_of_file ->
			Pairs = []
		;   atom_codes(Line, Codes),
			parse_mime_types_line(Line, LinePairs),
			append(LinePairs, RestPairs, Pairs),
			read_mime_types_stream(Stream, RestPairs)
		).

	parse_mime_types_line(Line, Pairs) :-
		strip_comment(Line, WithoutComment),
		string(atom)::trim(WithoutComment, Trimmed),
		(   Trimmed == '' ->
			Pairs = []
		;   string(atom)::split_string(Trimmed, ' \t', ' \t', Tokens),
			parse_mime_types_tokens(Tokens, Pairs)
		).

	parse_mime_types_tokens([], []).
	parse_mime_types_tokens([TypeToken| ExtensionTokens], Pairs) :-
		sub_atom(TypeToken, _, 1, _, '/'),
		!,
		normalize_type(TypeToken, Type),
		parse_extension_tokens(ExtensionTokens, Type, Pairs).
	parse_mime_types_tokens(_, []).

	parse_extension_tokens([], _, []).
	parse_extension_tokens([ExtensionToken| ExtensionTokens], Type, [Extension-Type| Pairs]) :-
		normalize_extension(ExtensionToken, Extension),
		Extension \== '',
		!,
		parse_extension_tokens(ExtensionTokens, Type, Pairs).
	parse_extension_tokens([_| ExtensionTokens], Type, Pairs) :-
		parse_extension_tokens(ExtensionTokens, Type, Pairs).

	load_pairs([], _).
	load_pairs([Extension-Type| Pairs], Strict) :-
		add_type(Type, Extension, Strict),
		load_pairs(Pairs, Strict).

	strip_query_fragment(Input, Output) :-
		trim_at_delimiter(Input, '#', FragmentTrimmed),
		trim_at_delimiter(FragmentTrimmed, '?', Output).

	trim_at_delimiter(Input, Delimiter, Output) :-
		(   sub_atom(Input, Before, _, _, Delimiter) ->
			sub_atom(Input, 0, Before, _, Output)
		;   Output = Input
		).

	strip_comment(Line, WithoutComment) :-
		trim_at_delimiter(Line, '#', WithoutComment).

	expand_alias(Path, Expanded) :-
		os::decompose_file_name(Path, _, Name, Extension),
		(   suffix_alias(Extension, Replacement) ->
			atom_concat(Name, Replacement, Expanded)
		;   Expanded = Path
		).

	normalize_type(Type, Normalized) :-
		atom(Type),
		string(atom)::trim(Type, Trimmed),
		string(atom)::string_lower(Trimmed, Normalized).

	normalize_extension(Extension, Normalized) :-
		atom(Extension),
		string(atom)::trim(Extension, Trimmed),
		normalize_extension_atom(Trimmed, Normalized).

	normalize_extension_atom('', '') :-
		!.
	normalize_extension_atom(Extension, Normalized) :-
		string(atom)::string_lower(Extension, Lowercase),
		(   sub_atom(Lowercase, 0, 1, _, '.') ->
			Normalized = Lowercase
		;   atom_concat('.', Lowercase, Normalized)
		).

	unique(List, Unique) :-
		unique(List, [], Unique).

	unique([], Acc, Unique) :-
		reverse(Acc, Unique).
	unique([Head| Tail], Acc, Unique) :-
		member_of(Head, Acc),
		!,
		unique(Tail, Acc, Unique).
	unique([Head| Tail], Acc, Unique) :-
		unique(Tail, [Head| Acc], Unique).

	member_of(Item, [Head| _]) :-
		Item == Head,
		!.
	member_of(Item, [_| Tail]) :-
		member_of(Item, Tail).

	encoding_suffix('.gz', gzip).
	encoding_suffix('.bz2', bzip2).
	encoding_suffix('.xz', xz).
	encoding_suffix('.br', br).
	encoding_suffix('.zst', zstd).
	encoding_suffix('.z', compress).

	suffix_alias('.tgz', '.tar.gz').
	suffix_alias('.taz', '.tar.gz').
	suffix_alias('.tbz2', '.tar.bz2').
	suffix_alias('.txz', '.tar.xz').
	suffix_alias('.svgz', '.svg.gz').

	standard_type('.avif', 'image/avif').
	standard_type('.css', 'text/css').
	standard_type('.csv', 'text/csv').
	standard_type('.gif', 'image/gif').
	standard_type('.html', 'text/html').
	standard_type('.htm', 'text/html').
	standard_type('.ico', 'image/vnd.microsoft.icon').
	standard_type('.jpeg', 'image/jpeg').
	standard_type('.jpg', 'image/jpeg').
	standard_type('.js', 'text/javascript').
	standard_type('.json', 'application/json').
	standard_type('.jsonld', 'application/ld+json').
	standard_type('.md', 'text/markdown').
	standard_type('.mp3', 'audio/mpeg').
	standard_type('.mp4', 'video/mp4').
	standard_type('.mjs', 'text/javascript').
	standard_type('.pdf', 'application/pdf').
	standard_type('.png', 'image/png').
	standard_type('.svg', 'image/svg+xml').
	standard_type('.text', 'text/plain').
	standard_type('.toml', 'application/toml').
	standard_type('.tsv', 'text/tab-separated-values').
	standard_type('.txt', 'text/plain').
	standard_type('.wasm', 'application/wasm').
	standard_type('.wav', 'audio/wav').
	standard_type('.webmanifest', 'application/manifest+json').
	standard_type('.webp', 'image/webp').
	standard_type('.xml', 'application/xml').
	standard_type('.yaml', 'application/yaml').
	standard_type('.yml', 'application/yaml').
	standard_type('.zip', 'application/zip').

	common_type('.7z', 'application/x-7z-compressed').
	common_type('.cjs', 'text/javascript').
	common_type('.tar', 'application/x-tar').
	common_type('.ts', 'text/typescript').
	common_type('.tsx', 'text/tsx').

:- end_object.
