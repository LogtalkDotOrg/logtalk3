%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(lgtdoc,
	implements(lgtdocp),
	imports(options)).

	:- info([
		version is 6:2:1,
		author is 'Paulo Moura',
		date is 2021-10-19,
		comment is 'Documenting tool. Generates XML documenting files for loaded entities and for library, directory, entity, and predicate indexes.'
	]).

	:- private(library_entity_/4).
	:- dynamic(library_entity_/4).
	:- mode(library_entity_(?atom, ?nonvar, ?nonvar, ?atom), zero_or_more).
	:- info(library_entity_/4, [
		comment is 'Table of documented entities per library.',
		argnames is ['Library', 'PrimarySortKey', 'SecondarySortKey', 'Entity']
	]).

	:- private(directory_entity_/4).
	:- dynamic(directory_entity_/4).
	:- mode(directory_entity_(?atom, ?nonvar, ?nonvar, ?atom), zero_or_more).
	:- info(directory_entity_/4, [
		comment is 'Table of documented entities per directory.',
		argnames is ['Directory', 'PrimarySortKey', 'SecondarySortKey', 'Entity']
	]).

	:- private(type_entity_/4).
	:- dynamic(type_entity_/4).
	:- mode(type_entity_(?atom, ?nonvar, ?nonvar, ?atom), zero_or_more).
	:- info(type_entity_/4, [
		comment is 'Table of documented entities per type.',
		argnames is ['Type', 'PrimarySortKey', 'SecondarySortKey', 'Entity']
	]).

	:- private(predicate_entity_/4).
	:- dynamic(predicate_entity_/4).
	:- mode(predicate_entity_(?predicate_indicator, ?nonvar, ?nonvar, ?entity_identifier), zero_or_more).
	:- info(predicate_entity_/4, [
		comment is 'Table of public predicates for all documented entities.',
		argnames is ['Predicate', 'PrimarySortKey', 'SecondarySortKey', 'Entity']
	]).

	:- uses(list, [
		member/2, memberchk/2, sort/4
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(varlist, [
		memberchk/2 as member_var/2
	]).

	rlibrary(Library, UserOptions) :-
		^^check_options(UserOptions),
		reset,
		logtalk::expand_library_path(Library, TopPath),
		^^merge_options(UserOptions, Options),
		memberchk(xml_docs_directory(XMLDirectory), Options),
		os::make_directory(XMLDirectory),
		output_rlibrary(TopPath, Options).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopPath, Options) :-
		memberchk(exclude_paths(ExcludedPaths), Options),
		memberchk(exclude_prefixes(ExcludedPrefixes), Options),
		forall(
			sub_library(TopPath, ExcludedPaths, ExcludedPrefixes, LibraryPath),
			output_directory_files(LibraryPath, Options)
		),
		write_indexes(Options).

	sub_library(TopPath, ExcludedPaths, ExcludedPrefixes, LibraryPath) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, LibraryPath),
		\+ (
			member(ExcludedPrefix, ExcludedPrefixes),
			sub_atom(LibraryPath, 0, _, _, ExcludedPrefix)
		),
		atom_concat(TopPath, RelativePath, LibraryPath),
		\+ member(RelativePath, ExcludedPaths).

	library(Library, UserOptions) :-
		^^check_options(UserOptions),
		reset,
		logtalk::expand_library_path(Library, Path),
		^^merge_options(UserOptions, Options),
		memberchk(xml_docs_directory(XMLDirectory), Options),
		os::make_directory(XMLDirectory),
		output_directory_files(Path, Options),
		write_indexes(Options).

	library(Library) :-
		library(Library, []).

	rdirectory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		reset,
		os::absolute_file_name(Directory, Path),
		^^merge_options(UserOptions, Options),
		memberchk(xml_docs_directory(XMLDirectory), Options),
		os::make_directory(XMLDirectory),
		output_rdirectory(Path, Options).

	rdirectory(Directory) :-
		rdirectory(Directory, []).

	output_rdirectory(Directory, Options) :-
		memberchk(exclude_paths(ExcludedPaths), Options),
		memberchk(exclude_prefixes(ExcludedPrefixes), Options),
		setof(
			SubDirectory,
			sub_directory(Directory, ExcludedPaths, ExcludedPrefixes, SubDirectory),
			SubDirectories
		),
		forall(
			member(SubDirectory, SubDirectories),
			output_directory_files(SubDirectory, Options)
		),
		write_indexes(Options).

	sub_directory(Directory, ExcludedPaths, ExcludedPrefixes, SubDirectory) :-
		logtalk::loaded_file(Path),
		\+ (
			member(ExcludedPrefix, ExcludedPrefixes),
			sub_atom(Path, 0, _, _, ExcludedPrefix)
		),
		os::decompose_file_name(Path, SubDirectory, _),
		atom_concat(Directory, RelativePath, SubDirectory),
		\+ member(RelativePath, ExcludedPaths).

	directory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		reset,
		os::absolute_file_name(Directory, Path),
		^^merge_options(UserOptions, Options),
		memberchk(xml_docs_directory(XMLDirectory), Options),
		os::make_directory(XMLDirectory),
		output_directory_files(Path, Options),
		write_indexes(Options).

	directory(Directory) :-
		directory(Directory, []).

	output_directory_files(Directory, Options) :-
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		memberchk(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(DirectorySlash)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		\+ member(Path, ExcludedFiles),
		\+ member(Basename, ExcludedFiles),
		os::decompose_file_name(Path, _, Name, _),
		\+ member(Name, ExcludedFiles),
		logtalk::loaded_file_property(Path, text_properties(StreamOptions)),
		process(Basename, DirectorySlash, Options, StreamOptions),
		fail.
	output_directory_files(_, _).

	file(Source, UserOptions) :-
		^^check_options(UserOptions),
		reset,
		locate_file(Source, Basename, Directory, StreamOptions),
		^^merge_options(UserOptions, Options),
		memberchk(xml_docs_directory(XMLDirectory), Options),
		os::make_directory(XMLDirectory),
		process(Basename, Directory, Options, StreamOptions).

	file(Source) :-
		file(Source, []).

	all(UserOptions) :-
		^^check_options(UserOptions),
		reset,
		^^merge_options(UserOptions, Options),
		memberchk(xml_docs_directory(XMLDirectory), Options),
		memberchk(exclude_prefixes(ExcludedPrefixes), Options),
		os::make_directory(XMLDirectory),
		(	logtalk::loaded_file_property(Path, directory(Directory)),
			\+ (
				member(ExcludedPrefix, ExcludedPrefixes),
				sub_atom(Directory, 0, _, _, ExcludedPrefix)
			),
			logtalk::loaded_file_property(Path, basename(File)),
			logtalk::loaded_file_property(Path, text_properties(StreamOptions)),
			process(File, Directory, Options, StreamOptions),
			fail
		;	write_indexes(Options)
		).

	all :-
		all([]).

	% file given in library notation
	locate_file(LibraryNotation, Basename, Directory, StreamOptions) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Basename, Directory, StreamOptions).
	% file given using its name or basename
	locate_file(Source, Basename, Directory, StreamOptions) :-
		add_extension(Source, Basename),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			logtalk::loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		logtalk::loaded_file_property(Path, text_properties(StreamOptions)),
		!.
	% file given using a full path
	locate_file(Source, Basename, Directory, StreamOptions) :-
		add_extension(Source, SourceWithExtension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		atom_concat(Directory, Basename, SourceWithExtension),
		logtalk::loaded_file_property(Path, text_properties(StreamOptions)),
		!.

	add_extension(Source, SourceWithExtension) :-
		% ensure that Source is not specified using library notation
		atom(Source),
		os::decompose_file_name(Source, _, _, SourceExtension),
		(	logtalk::file_type_extension(source, SourceExtension) ->
			% source file extension present
			SourceWithExtension = Source
		;	% try possible source extensions
			logtalk::file_type_extension(source, Extension),
			atom_concat(Source, Extension, SourceWithExtension)
		).

	process(File, Path, Options, StreamOptions) :-
		memberchk(exclude_entities(ExcludedEntities), Options),
		memberchk(omit_path_prefixes(Prefixes), Options),
		entity_property(Entity, file(File, Path)),
		\+ member(Entity, ExcludedEntities),
		functor(Entity, Functor, _),
		(	(	logtalk_library_path(Library, _),
				Library \== startup
			;	logtalk_library_path(Library, _)
			),
			logtalk::expand_library_path(Library, Path) ->
			assertz(library_entity_(Library, Library, Functor, Entity))
		;	true
		),
		(	member(Prefix, Prefixes),
			atom_concat(Prefix, Relative, Path) ->
			assertz(directory_entity_(Relative, Relative, Functor, Entity))
		;	assertz(directory_entity_(Path, Path, Functor, Entity))
		),
		entity_type(Entity, Type),
		assertz(type_entity_(Type, Type, Functor, Entity)),
		write_entity_doc(Entity, Type, Options, StreamOptions),
		fail.
	process(_, _, _, _).

	% write_entity_doc(@entity_identifier, +atom, +list, +list)
	%
	% writes to disk the entity documentation in XML format

	write_entity_doc(Entity, Type, Options, StreamOptions) :-
		memberchk(xml_docs_directory(XMLDirectory), Options),
		entity_doc_file_name(Entity, File),
		atom_concat(XMLDirectory, File, XMLFile),
		convert_stream_options(StreamOptions, ConvertedStreamOptions),
		open(XMLFile, write, Stream, ConvertedStreamOptions),
		write_entity_xml_file(File, Stream, Entity, Type, Options, StreamOptions),
		close(Stream).

	% entity_doc_file_name(@nonvar, -atom)
	%
	% generates the XML file name for an entity using the format <functor>_<arity>

	entity_doc_file_name(Entity, File) :-
		functor(Entity, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		atom_concat(Functor, '_', Aux),
		atom_concat(Aux, Atom, Name),
		atom_concat(Name, '.xml', File).

	convert_stream_options([], []).
	convert_stream_options([StreamOption| StreamOptions], [ConvertedStreamOption| ConvertedStreamOptions]) :-
		convert_stream_option(StreamOption, ConvertedStreamOption),
		convert_stream_options(StreamOptions, ConvertedStreamOptions).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).
		convert_stream_option(bom(Boolean), bom(Boolean)).
		convert_stream_option(encoding(LogtalkEncoding), encoding(PrologEncoding)) :-
			(	convert_encoding(LogtalkEncoding, PrologEncoding) ->
				true
			;	PrologEncoding = utf8
			).

		convert_encoding('US-ASCII', ascii).
		convert_encoding('ISO-8859-1', iso_latin_1).
		convert_encoding('UTF-8', utf8).
		convert_encoding('UCS-2BE', unicode_be).
		convert_encoding('UCS-2LE', unicode_le).
		convert_encoding('UTF-16BE', unicode_be).
		convert_encoding('UTF-16LE', unicode_le).
		convert_encoding('UTF-16', Encoding) :-
			os::operating_system_type(Type),
			(	Type == windows ->
				Encoding = unicode_le
			;	% other operating-systems can be either big-endian or little-endian
				% but most Prolog backend compilers don't provide the reflection
				% support to query about the operating-system and the architecture
				Encoding = unicode_be
			).
	:- else.
		convert_stream_option(StreamOption, StreamOption).
	:- endif.

	% write_entity_xml_file(@atom, @stream, @entity_identifier, +atom, @list, @list)
	%
	% writes an entity XML file containing the documentation of a compiled entity

	write_entity_xml_file(File, Stream, Entity, Type, Options, StreamOptions) :-
		write_entity_xml_header(Stream, Options, StreamOptions),
		write_entity_xml_entity(File, Stream, Entity),
		write_entity_xml_relations(Stream, Entity),
		write_entity_xml_predicates(Stream, Entity, Type, Options),
		write_entity_xml_operators(Stream, Entity),
		write_entity_xml_remarks(Stream, Entity),
		write_entity_xml_see_also(Stream, Entity),
		write_entity_xml_footer(Stream).

	write_entity_xml_header(Stream, Options, StreamOptions) :-
		memberchk(xml_spec_reference(XMLSRef), Options),
		(	member(encoding(Encoding), StreamOptions) ->
			true
		;	Encoding = 'UTF-8'
		),
		memberchk(xml_spec(XMLSpec), Options),
		memberchk(entity_xsl_file(XSL), Options),
		once(kind_ref_doctype_xsd(entity, XMLSRef, DocTypeURL, XSDURL)),
		write_xml_header(XMLSRef, Encoding, XMLSpec, DocTypeURL, XSL, XSDURL, Stream).

	write_xml_header(standalone, Encoding, _, DocType-_, XSL, _, Stream) :-
		!,
		xml_header_text('1.0', Encoding, yes, Text),
		write_xml_open_tag(Stream, Text, []),
		write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
		write(Stream, XSL), write(Stream, '"?>'), nl(Stream),
		write_xml_open_tag(Stream, DocType, []).

	write_xml_header(_, Encoding, XMLSpec, DocType-DocTypeURL, XSL, XSDURL, Stream) :-
		xml_header_text('1.0', Encoding, no, Text),
		write_xml_open_tag(Stream, Text, []),
		(	XMLSpec == dtd ->
			write(Stream, '<!DOCTYPE '), write(Stream, DocType), write(Stream, ' SYSTEM "'),
			write(Stream, DocTypeURL), write(Stream, '">'), nl(Stream)
		;	true
		),
		write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
		write(Stream, XSL),
		write(Stream, '"?>'), nl(Stream),
		(	XMLSpec == dtd ->
			write_xml_open_tag(Stream, DocType, [])
		;	write_xml_open_tag(Stream, DocType,
				['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
				 'xsi:noNamespaceSchemaLocation'-XSDURL])
		).

	xml_header_text(Version, Encoding, Standalone, Text) :-
		atom_concat('?xml version="', Version, Aux1),
		atom_concat(Aux1, '" encoding="', Aux2),
		atom_concat(Aux2, Encoding, Aux3),
		atom_concat(Aux3, '" standalone="', Aux4),
		atom_concat(Aux4, Standalone, Aux5),
		atom_concat(Aux5, '"?', Text).

	write_entity_xml_footer(Stream) :-
		write_xml_close_tag(Stream, logtalk_entity).

	write_entity_xml_entity(File, Stream, Entity) :-
		entity_type(Entity, Type),
		write_xml_open_tag(Stream, entity, []),
		entity_to_xml_term(Entity),
		write_xml_cdata_element(Stream, name, [], Entity),
		functor(Entity, Name, Arity),
		write_xml_cdata_element(Stream, functor, [], Name/Arity),
		os::decompose_file_name(File, _, Basename, _),
		write_xml_cdata_element(Stream, file, [], Basename),
		write_xml_element(Stream, type, [], Type),
		xml_entity_compilation_text(Type, Entity, Compilation),
		write_xml_element(Stream, compilation, [], Compilation),
		(	entity_property(Entity, info(Info)) ->
			write_xml_entity_info(Stream, Info)
		;	true
		),
		write_xml_close_tag(Stream, entity).

	% xml_entity_compilation_text(+atom, @entity_identifier, -atom)

	xml_entity_compilation_text(object, Entity, Flags) :-
		(	object_property(Entity, static) ->
			Mode = static
		;	Mode = (dynamic)
		),
		(	object_property(Entity, built_in) ->
			atom_concat(Mode, ', built_in', Flags0)
		;	Flags0 = Mode
		),
		(	object_property(Entity, context_switching_calls) ->
			atom_concat(Flags0, ', context_switching_calls', Flags1)
		;	Flags1 = Flags0
		),
		(	object_property(Entity, dynamic_declarations) ->
			atom_concat(Flags1, ', dynamic_declarations', Flags2)
		;	Flags2 = Flags1
		),
		(	object_property(Entity, complements(allow)) ->
			atom_concat(Flags2, ', complements(allow)', Flags3)
		;	object_property(Entity, complements(restrict)) ->
			atom_concat(Flags2, ', complements(restrict)', Flags3)
		;	Flags3 = Flags2
		),
		(	object_property(Entity, events) ->
			atom_concat(Flags3, ', events', Flags4)
		;	Flags4 = Flags3
		),
		(	object_property(Entity, threaded) ->
			atom_concat(Flags4, ', threaded', Flags)
		;	Flags = Flags4
		).
	xml_entity_compilation_text(category, Entity, Flags) :-
		(	category_property(Entity, static) ->
			Mode = static
		;	Mode = (dynamic)
		),
		(	object_property(Entity, built_in) ->
			atom_concat(Mode, ', built_in', Flags0)
		;	Flags0 = Mode
		),
		(	category_property(Entity, events) ->
			atom_concat(Flags0, ', events', Flags)
		;	Flags = Flags0
		).
	xml_entity_compilation_text(protocol, Entity, Flags) :-
		(	protocol_property(Entity, static) ->
			Mode = static
		;	Mode = (dynamic)
		),
		(	protocol_property(Entity, built_in) ->
			atom_concat(Mode, ', built_in', Flags)
		;	Flags = Mode
		).

	% write_xml_entity_info(+stream, +list)
	%
	% outputs the contents of entity info/1 directive
	% in the order specified in the Logtalk DTD file

	write_xml_entity_info(Stream, Info) :-
		(	member(comment(Comment), Info) ->
			write_xml_cdata_element(Stream, comment, [], Comment)
		;	true
		),
		(	member(parameters(Parameters), Info) ->
			write_xml_open_tag(Stream, parameters, []),
			forall(
				member(Parname-Description, Parameters),
				(	write_xml_open_tag(Stream, parameter, []),
					write_xml_cdata_element(Stream, name, [], Parname),
					write_xml_cdata_element(Stream, description, [], Description),
					write_xml_close_tag(Stream, parameter)
				)
			),
			write_xml_close_tag(Stream, parameters)
		;	true
		),
		(	member(author(Author), Info) ->
			(	Author = {EntityName} ->
				entity_name_to_xml_entity(EntityName, XMLEntity),
				write_xml_element(Stream, author, [], XMLEntity)
			;	write_xml_cdata_element(Stream, author, [], Author)
			)
		;	true
		),
		(	member(version(Version), Info) ->
			(	number(Version) ->
				number_codes(Version, VersionCodes),
				atom_codes(VersionAtom, VersionCodes),
				write_xml_element(Stream, version, [], VersionAtom)
			;	write_xml_element(Stream, version, [], Version)
			)
		;	true
		),
		(	member(date(Date), Info) ->
			date_to_padded_atom(Date, DateAtom),
			write_xml_element(Stream, date, [], DateAtom)
		;	true
		),
		(	member(copyright(Copyright), Info) ->
			(	Copyright = {EntityName} ->
				entity_name_to_xml_entity(EntityName, XMLEntity),
				write_xml_element(Stream, copyright, [], XMLEntity)
			;	write_xml_element(Stream, copyright, [], Copyright)
			)
		;	true
		),
		(	member(license(License), Info) ->
			(	License = {EntityName} ->
				entity_name_to_xml_entity(EntityName, XMLEntity),
				write_xml_element(Stream, license, [], XMLEntity)
			;	write_xml_element(Stream, license, [], License)
			)
		;	true
		),
		% user-defined keys
		forall(
			(	member(KeyValue, Info),
				KeyValue =.. [Key, Value],
				\+ member(Key, [comment, author, version, date, parameters, parnames, copyright, license, remarks, see_also])
			),
			(	write_xml_open_tag(Stream, info, []),
				write_xml_element(Stream, key, [], Key),
				write_xml_cdata_element(Stream, value, [], Value),
				write_xml_close_tag(Stream, info)
			)
		).

	% entity_name_to_xml_entity(+nonvar, -atom)
	%
	% converts and entity name reference into an atom
	% representing the corresponding XML entity

	entity_name_to_xml_entity({EntityName}, XMLEntity) :-
		atom_concat('&', EntityName, Aux),
		atom_concat(Aux, ';', XMLEntity).

	% entity_to_xml_term(+entity)
	%
	% instantiates the parameters in a parametric object
	% to either user-defined names or to '$VAR(N) terms

	entity_to_xml_term(Entity) :-
		entity_property(Entity, info(List)),
		(	member(parnames(Names), List) ->
			true
		;	member(parameters(Parameters), List),
			findall(Name, member(Name - _, Parameters), Names)
		),
		!,
		Entity =.. [_| Args],
		vars_to_atoms(Args, Args, Names).

	entity_to_xml_term(Entity) :-
		numbervars(Entity, 0, _).

	% relation_to_xml_term(+entity, +entity)
	%
	% instantiates the parameters in a related entity taking
	% in account the parameter sharing with the original entity

	relation_to_xml_term(Entity, Relation) :-
		entity_to_xml_term(Entity),
		Relation =.. [_| Args],
		vars_to_underscore(Args).

	% pred_call_to_xml_term(+entity_identifier, +atom, +integer, +nonvar, +nonvar, -nonvar, -nonvar)
	%
	% instantiates the arguments in a predicate call to user defined names or to the atom '_'

	pred_call_to_xml_term(Entity, Functor, Arity, Call, Bindings, QCall, QBindings) :-
		double_quote_atoms(Call, QCall),
		double_quote_atoms(Bindings, QBindings),
		pred_qcall_to_xml_term(Entity, Functor, Arity, QCall, QBindings).

	pred_qcall_to_xml_term(Entity, Functor, Arity, Call, Bindings) :-
		(	entity_property(Entity, declares(Functor/Arity, Properties)),
			member(info(List), Properties) ->
			true
		;	fail
		),
		(	member(argnames(Names), List) ->
			true
		;	member(arguments(Arguments), List),
			findall(Name, member(Name - _, Arguments), Names)
		),
		!,
		Call =.. [Functor| Args],
		binding_vars(Bindings, Vars),
		vars_to_atoms(Args, Vars, Names).

	pred_qcall_to_xml_term(_, Functor, _, Call, _) :-
		Call =.. [Functor| Args],
		vars_to_underscore(Args).

	double_quote_atoms(Var, Var) :-
		var(Var),
		!.

	double_quote_atoms((Call1, Call2), (QCall1, QCall2)) :-
		!,
		double_quote_atoms(Call1, QCall1),
		double_quote_atoms(Call2, QCall2).

	double_quote_atoms((Call1; Call2), (QCall1; QCall2)) :-
		!,
		double_quote_atoms(Call1, QCall1),
		double_quote_atoms(Call2, QCall2).

	double_quote_atoms((Call1 -> Call2), (QCall1 -> QCall2)) :-
		!,
		double_quote_atoms(Call1, QCall1),
		double_quote_atoms(Call2, QCall2).

	double_quote_atoms(\+ Call, \+ QCall) :-
		!,
		double_quote_atoms(Call, QCall).

	double_quote_atoms([], []) :-
		!.

	double_quote_atoms([Arg| Args], [QArg| QArgs]) :-
		!,
		double_quote_atoms(Arg, QArg),
		double_quote_atoms(Args, QArgs).

	double_quote_atoms(Atom, QAtom) :-
		atom(Atom),
		!,
		(	atom_requires_quotes(Atom) ->
			atom_concat('''', Atom, Aux),
			atom_concat(Aux, '''', QAtom)
		;	Atom = QAtom
		).

	double_quote_atoms(Number, Number) :-
		number(Number),
		!.

	double_quote_atoms(Term, QTerm) :-
		Term =.. [Functor| Args],
		(	predicate_property(Term, built_in) ->
			QFunctor = Functor
		;	double_quote_atoms(Functor, QFunctor)
		),
		double_quote_atoms(Args, QArgs),
		QTerm =.. [QFunctor| QArgs].

	atom_requires_quotes(Atom) :-
		atom_chars(Atom, [First| Rest]),
		(	First @< a
		;	First @> z
		;	member(Char, Rest),
			\+ alpha_numeric_char(Char)
		),
		!.

	alpha_numeric_char('_').
	alpha_numeric_char(Char) :-
		Char @>= a, Char @=< z.
	alpha_numeric_char(Char) :-
		Char @>= 'A', Char @=< 'Z'.
	alpha_numeric_char(Char) :-
		Char @>= '0', Char @=< '9'.

	% binding_vars(@nonvar, -list)
	%
	% returns a list of all binding variables

	binding_vars(Bindings, Vars) :-
		(	atom(Bindings) ->
			% no bindings, just "no", "yes", or equivalent answers
			Vars = []
		;	binding_vars_list(Bindings, Vars)
		).

	binding_vars_list((Var = _), [Var]).
	binding_vars_list(((Var = _), Bindings), [Var| Vars]) :-
		binding_vars_list(Bindings, Vars).

	% vars_to_atoms(+list, +list, +list)
	%
	% instantiates the variables in the input list to either a name or the atom '_'

	vars_to_atoms([], _, []).
	vars_to_atoms([Arg| Args], Vars, [Name| Names]) :-
		(	var(Arg) ->
			(	member_var(Arg, Vars) ->
				Arg = Name
			;	Arg = '_'
			)
		;	true
		),
		vars_to_atoms(Args, Vars, Names).

	% vars_to_underscore(+list)
	%
	% instantiates the variables in the input list to the atom '_'

	vars_to_underscore([]).
	vars_to_underscore([Arg| Args]) :-
		(	var(Arg) ->
			Arg = '_'
		;	true
		),
		vars_to_underscore(Args).

	% relation_to_xml_filename(+entity, -atom)
	%
	% required to build filenames in links to parametric objects/categories

	relation_to_xml_filename(Relation, File) :-
		functor(Relation, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		atom_concat(Functor, '_', Aux),
		atom_concat(Aux, Atom, File).

	% write_entity_xml_predicates(@stream, @entity_identifier, +atom, @list)
	%
	% writes the predicate documentation

	write_entity_xml_predicates(Stream, Entity, Type, Options) :-
		write_xml_open_tag(Stream, predicates, []),
		write_xml_inherited_predicates(Stream, Entity, Type, Options),
		write_xml_public_predicates(Stream, Entity, Options),
		write_xml_protected_predicates(Stream, Entity, Options),
		write_xml_private_predicates(Stream, Entity, Options),
		write_xml_close_tag(Stream, predicates).

	% write_xml_inherited_predicates(@stream, @entity_identifier, +atom, @list)
	%
	% writes the list of inherited public predicates

	write_xml_inherited_predicates(Stream, Entity, Type, Options) :-
		write_xml_open_tag(Stream, inherited, []),
		inherited_predicates(Type, Entity, Predicates0),
		(	member(sort_predicates(true), Options) ->
			sort(1, @=<, Predicates0, Predicates)
		;	Predicates = Predicates0
		),
		member(Predicate, Predicates),
		write_xml_inherited_predicate(Stream, Predicate),
		fail.
	write_xml_inherited_predicates(Stream, _, _, _) :-
		write_xml_close_tag(Stream, inherited).

	% write_xml_public_predicates(@stream, @entity_identifier, @list)
	%
	% writes the documentation of public predicates

	write_xml_public_predicates(Stream, Entity, Options) :-
		write_xml_open_tag(Stream, (public), []),
		entity_property(Entity, public(Predicates0)),
		(	member(sort_predicates(true), Options) ->
			sort(1, @=<, Predicates0, Predicates)
		;	Predicates = Predicates0
		),
		member(Functor/Arity, Predicates),
		entity_property(Entity, declares(Functor/Arity, Properties)),
		functor(Entity, EntityFunctor, _),
		(	member(non_terminal(Functor//Args), Properties) ->
			assertz(predicate_entity_(Functor//Args, Functor, EntityFunctor, Entity)),
			write_xml_predicate(Stream, Entity, Functor//Args, Functor, Arity, (public))
		;	assertz(predicate_entity_(Functor/Arity, Functor, EntityFunctor, Entity)),
			write_xml_predicate(Stream, Entity, Functor/Arity, Functor, Arity, (public))
		),
		fail.
	write_xml_public_predicates(Stream, _, _) :-
		write_xml_close_tag(Stream, (public)).

	% write_xml_protected_predicates(@stream, @entity_identifier, @list)
	%
	% writes the documentation protected predicates

	write_xml_protected_predicates(Stream, Entity, Options) :-
		write_xml_open_tag(Stream, protected, []),
		entity_property(Entity, protected(Predicates0)),
		(	member(sort_predicates(true), Options) ->
			sort(1, @=<, Predicates0, Predicates)
		;	Predicates = Predicates0
		),
		member(Functor/Arity, Predicates),
		entity_property(Entity, declares(Functor/Arity, Properties)),
		(	member(non_terminal(Functor//Args), Properties) ->
			write_xml_predicate(Stream, Entity, Functor//Args, Functor, Arity, protected)
		;	write_xml_predicate(Stream, Entity, Functor/Arity, Functor, Arity, protected)
		),
		fail.
	write_xml_protected_predicates(Stream, _, _) :-
		write_xml_close_tag(Stream, protected).

	% write_xml_private_predicates(@stream, @entity_identifier, @list)
	%
	% writes the documentation of private predicates

	write_xml_private_predicates(Stream, Entity, Options) :-
		write_xml_open_tag(Stream, private, []),
		entity_property(Entity, private(Predicates0)),
		(	member(sort_predicates(true), Options) ->
			sort(1, @=<, Predicates0, Predicates)
		;	Predicates = Predicates0
		),
		member(Functor/Arity, Predicates),
		entity_property(Entity, declares(Functor/Arity, Properties)),
		(	member(non_terminal(Functor//Args), Properties) ->
			write_xml_predicate(Stream, Entity, Functor//Args, Functor, Arity, private)
		;	write_xml_predicate(Stream, Entity, Functor/Arity, Functor, Arity, private)
		),
		fail.
	write_xml_private_predicates(Stream, _, _) :-
		write_xml_close_tag(Stream, private).

	% write_xml_inherited_predicate(@stream, @compound)
	%
	% writes the documentation of an inherited predicate

	write_xml_inherited_predicate(Stream, predicate(Name, From, Functor, File)) :-
		write_xml_open_tag(Stream, inherited_predicate, []),
		write_xml_cdata_element(Stream, name, [], Name),
		write_xml_cdata_element(Stream, from, [], From),
		write_xml_cdata_element(Stream, functor, [], Functor),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, inherited_predicate).

	% write_xml_predicate(@stream, @entity_identifier, +predicate_indicator, +atom, +integer, +term)
	% write_xml_predicate(@stream, @entity_identifier, +non_terminal_indicator, +atom, +integer, +term)
	%
	% writes the documentation of a predicate

	write_xml_predicate(Stream, Entity, Name, Functor, Arity, Scope) :-
		entity_property(Entity, declares(Functor/Arity, Properties)),
		write_xml_open_tag(Stream, predicate, []),
		write_xml_cdata_element(Stream, name, [], Name),
		write_xml_element(Stream, scope, [], Scope),
		(	(entity_property(Entity, (dynamic)); member((dynamic), Properties)) ->
			Compilation0 = (dynamic)
		;	Compilation0 = static
		),
		(	member((multifile), Properties) ->
			atom_concat(Compilation0, ', multifile', Compilation1)
		;	Compilation1 = Compilation0
		),
		(	member(synchronized, Properties) ->
			atom_concat(Compilation1, ', synchronized', Compilation)
		;	Compilation = Compilation1
		),
		write_xml_element(Stream, compilation, [], Compilation),
		functor(Meta, Functor, Arity),
		(	member(meta_predicate(Meta), Properties) ->
			(	Name = _/_ ->
				write_xml_cdata_element(Stream, meta, [], Meta)
			;	convert_meta_predicate_to_meta_non_terminal(Meta, NonTerminalMeta),
				write_xml_cdata_element(Stream, meta, [], NonTerminalMeta)
			)
		;	true
		),
		(	member(coinductive(Coinductive), Properties) ->
			(	Name = _/_ ->
				write_xml_cdata_element(Stream, coinductive, [], Coinductive)
			;	convert_coinductive_to_coinductive_non_terminal(Coinductive, NonTerminalCoinductive),
				write_xml_cdata_element(Stream, coinductive, [], NonTerminalCoinductive)
			)
		;	true
		),
		(	Name = Functor//Args ->
			functor(Template, Functor, Args)
		;	functor(Template, Functor, Arity)
		),
		forall(
			member(mode(Template, Proofs), Properties),
			(	write_xml_open_tag(Stream, (mode), []),
				write_xml_cdata_element(Stream, template, [], Template),
				write_xml_element(Stream, proofs, [], Proofs),
				write_xml_close_tag(Stream, (mode))
			)
		),
		(	member(info(Info), Properties) ->
			write_xml_predicate_info(Stream, Entity, Functor, Arity, Info)
		;	true
		),
		write_xml_close_tag(Stream, predicate).

	convert_meta_predicate_to_meta_non_terminal(Meta, NonTerminalMeta) :-
		Meta =.. [Functor| MetaArgs],
		convert_meta_predicate_to_meta_non_terminal_args(MetaArgs, NonTerminalArgs),
		NonTerminalMeta =.. [Functor| NonTerminalArgs].

	convert_meta_predicate_to_meta_non_terminal_args([_, _], []) :-
		!.
	convert_meta_predicate_to_meta_non_terminal_args([MetaArg| MetaArgs], [NonTerminalArg| NonTerminalArgs]) :-
		(	integer(MetaArg) ->
			NonTerminalArg is MetaArg - 2
		;	NonTerminalArg = MetaArg
		),
		convert_meta_predicate_to_meta_non_terminal_args(MetaArgs, NonTerminalArgs).

	convert_coinductive_to_coinductive_non_terminal(Coinductive, NonTerminalCoinductive) :-
		Coinductive =.. [Functor| Args],
		convert_coinductive_to_coinductive_non_terminal_args(Args, NonTerminalArgs),
		NonTerminalCoinductive =.. [Functor| NonTerminalArgs].

	convert_coinductive_to_coinductive_non_terminal_args([_, _], []) :-
		!.
	convert_coinductive_to_coinductive_non_terminal_args([Arg| Args], [Arg| NonTerminalArgs]) :-
		convert_coinductive_to_coinductive_non_terminal_args(Args, NonTerminalArgs).

	write_xml_predicate_info(Stream, Entity, Functor, Arity, Info) :-
		(	member(comment(Comment), Info) ->
			write_xml_cdata_element(Stream, comment, [], Comment)
		;	true
		),
		(	member(arguments(Arguments), Info) ->
			findall(Name, member(Name - _, Arguments), Names),
			Template =.. [Functor| Names],
			write_xml_cdata_element(Stream, template, [], Template),
			write_xml_open_tag(Stream, arguments, []),
			forall(
				member(Name-Description, Arguments),
				(	write_xml_open_tag(Stream, argument, []),
					write_xml_cdata_element(Stream, name, [], Name),
					write_xml_cdata_element(Stream, description, [], Description),
					write_xml_close_tag(Stream, argument)
				)
			),
			write_xml_close_tag(Stream, arguments)
		;	true
		),
		(	member(argnames(Names), Info) ->
			Template =.. [Functor| Names],
			write_xml_cdata_element(Stream, template, [], Template)
		;	true
		),
		(	member(exceptions(Exceptions), Info) ->
			write_xml_open_tag(Stream, exceptions, []),
			forall(
				member(Cond-Term, Exceptions),
				(	write_xml_open_tag(Stream, exception, []),
					write_xml_cdata_element(Stream, condition, [], Cond),
					write_xml_cdata_element(Stream, term, [], Term),
					write_xml_close_tag(Stream, exception)
				)
			),
			write_xml_close_tag(Stream, exceptions)
		;	true
		),
		(	member(remarks(Remarks), Info) ->
			write_xml_open_tag(Stream, remarks, []),
			forall(
				member((Topic-Text), Remarks),
				(	write_xml_open_tag(Stream, remark, []),
					write_xml_cdata_element(Stream, topic, [], Topic),
					write_xml_cdata_element(Stream, text, [], Text),
					write_xml_close_tag(Stream, remark)
				)
			),
			write_xml_close_tag(Stream, remarks)
		;	true
		),
		(	member(see_also(SeeAlso), Info) ->
			write_xml_open_tag(Stream, see_also, []),
			relation_to_xml_filename(Entity, File),
			forall(
				member(Reference, SeeAlso),
				(	write_xml_open_tag(Stream, reference, []),
					write_xml_cdata_element(Stream, name, [], Reference),
					write_xml_cdata_element(Stream, file, [], File),
					write_xml_close_tag(Stream, reference)
				)
			),
			write_xml_close_tag(Stream, see_also)
		;	true
		),
		% user-defined keys
		forall(
			(	member(KeyValue, Info),
				KeyValue =.. [Key, Value],
				\+ member(Key, [comment, arguments, argnames, exceptions, examples, remarks, see_also])
			),
			(	write_xml_open_tag(Stream, info, []),
				write_xml_element(Stream, key, [], Key),
				write_xml_cdata_element(Stream, value, [], Value),
				write_xml_close_tag(Stream, info)
			)
		),
		(	member(examples(Examples), Info) ->
			write_xml_open_tag(Stream, examples, []),
			forall(
				member((Description - Call - {Bindings}), Examples),
				(	pred_call_to_xml_term(Entity, Functor, Arity, Call, Bindings, QCall, QBindings),
					write_xml_open_tag(Stream, example, []),
					write_xml_cdata_element(Stream, description, [], Description),
					write_xml_cdata_element(Stream, call, [], QCall),
					write_xml_cdata_element(Stream, bindings, [], QBindings),
					write_xml_close_tag(Stream, example)
				)
			),
			write_xml_close_tag(Stream, examples)
		;	true
		).

	write_entity_xml_relations(Stream, Entity) :-
		write_xml_open_tag(Stream, relations, []),
		(	current_object(Entity) ->
			write_xml_object_relations(Stream, Entity)
		;	atom(Entity), current_protocol(Entity) ->
			write_xml_protocol_relations(Stream, Entity)
		;	current_category(Entity) ->
			write_xml_category_relations(Stream, Entity)
		;	fail
		),
		fail.
	write_entity_xml_relations(Stream, Entity) :-
		entity_property(Entity, alias(AFunctor/AArity, Properties)),
		memberchk(from(From), Properties),
		memberchk(for(PFunctor/PArity), Properties),
			Entity =.. [_| Args],				% take care of parametric entities
			vars_to_underscore(Args),
			write_xml_open_tag(Stream, alias, []),
			write_xml_cdata_element(Stream, name, [], From),
			write_xml_cdata_element(Stream, original, [], PFunctor/PArity),
			write_xml_cdata_element(Stream, alternative, [], AFunctor/AArity),
			write_xml_close_tag(Stream, alias),
		fail.
	write_entity_xml_relations(Stream, _) :-
		write_xml_close_tag(Stream, relations).

	write_xml_object_relations(Stream, Entity) :-
		implements_protocol(Entity, Protocol, Scope),
			write_xml_entity_relation(Stream, Entity, Protocol, implements, Scope),
		fail.
	write_xml_object_relations(Stream, Entity) :-
		imports_category(Entity, Category, Scope),
			write_xml_entity_relation(Stream, Entity, Category, imports, Scope),
		fail.
	write_xml_object_relations(Stream, Entity) :-
		extends_object(Entity, Parent, Scope),
			write_xml_entity_relation(Stream, Entity, Parent, extends, Scope),
		fail.
	write_xml_object_relations(Stream, Entity) :-
		instantiates_class(Entity, Class, Scope),
			write_xml_entity_relation(Stream, Entity, Class, instantiates, Scope),
		fail.
	write_xml_object_relations(Stream, Entity) :-
		specializes_class(Entity, Superclass, Scope),
			write_xml_entity_relation(Stream, Entity, Superclass, specializes, Scope),
		fail.
	write_xml_object_relations(Stream, Entity) :-
		object_property(Entity, provides(Functor/Arity, To, _)),
			entity_property(To, declares(Functor/Arity, Properties)),
			(	member(non_terminal(Functor//Args), Properties) ->
				write_xml_provides_relation(Stream, Entity, To, Functor//Args)
			;	write_xml_provides_relation(Stream, Entity, To, Functor/Arity)
			),
		fail.
	write_xml_object_relations(Stream, Entity) :-
		findall(
			Name/Arity,
			(	object_property(Entity, calls(Other::_, _)),
				nonvar(Other),
				functor(Other, Name, Arity)
			),
			Functors
		),
		sort(Functors, UniqueFunctors),
			member(Name/Arity, UniqueFunctors),
			functor(Object, Name, Arity),
			write_xml_uses_relation(Stream, Object),
		fail.
	write_xml_object_relations(_, _).

	write_xml_protocol_relations(Stream, Entity) :-
		extends_protocol(Entity, Protocol, Scope),
			write_xml_entity_relation(Stream, Entity, Protocol, extends, Scope),
		fail.
	write_xml_protocol_relations(_, _).

	write_xml_category_relations(Stream, Entity) :-
		implements_protocol(Entity, Protocol, Scope),
			write_xml_entity_relation(Stream, Entity, Protocol, implements, Scope),
		fail.
	write_xml_category_relations(Stream, Entity) :-
		extends_category(Entity, Category, Scope),
			write_xml_entity_relation(Stream, Entity, Category, extends, Scope),
		fail.
	write_xml_category_relations(Stream, Entity) :-
		complements_object(Entity, Object),
			write_xml_complements_relation(Stream, Entity, Object),
		fail.
	write_xml_category_relations(Stream, Entity) :-
		category_property(Entity, provides(Functor/Arity, To, _)),
			entity_property(To, declares(Functor/Arity, Properties)),
			(	member(non_terminal(Functor//Args), Properties) ->
				write_xml_provides_relation(Stream, Entity, To, Functor//Args)
			;	write_xml_provides_relation(Stream, Entity, To, Functor/Arity)
			),
		fail.
	write_xml_category_relations(Stream, Entity) :-
		findall(
			Name/Arity,
			(	category_property(Entity, calls(Other::_, _)),
				nonvar(Other),
				functor(Other, Name, Arity)
			),
			Functors
		),
		sort(Functors, UniqueFunctors),
			member(Name/Arity, UniqueFunctors),
			functor(Object, Name, Arity),
			write_xml_uses_relation(Stream, Object),
		fail.
	write_xml_category_relations(_, _).

	write_xml_entity_relation(Stream, Entity, Relation, Tag, Scope) :-
		relation_to_xml_term(Entity, Relation),
		relation_to_xml_filename(Relation, File),
		write_xml_open_tag(Stream, Tag, []),
		write_xml_cdata_element(Stream, name, [], Relation),
		functor(Relation, Name, Arity),
		write_xml_cdata_element(Stream, functor, [], Name/Arity),
		write_xml_element(Stream, scope, [], Scope),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, Tag).

	write_xml_complements_relation(Stream, Entity, Relation) :-
		relation_to_xml_term(Entity, Relation),
		relation_to_xml_filename(Relation, File),
		write_xml_open_tag(Stream, complements, []),
		write_xml_cdata_element(Stream, name, [], Relation),
		functor(Relation, Name, Arity),
		write_xml_cdata_element(Stream, functor, [], Name/Arity),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, complements).

	write_xml_provides_relation(Stream, Entity, To, Resource) :-
		relation_to_xml_term(Entity, To),
		relation_to_xml_filename(To, File),
		write_xml_open_tag(Stream, provides, []),
		write_xml_cdata_element(Stream, to, [], To),
		functor(To, Name, Arity),
		write_xml_cdata_element(Stream, functor, [], Name/Arity::Resource),
		write_xml_cdata_element(Stream, resource, [], Resource),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, provides).

	write_xml_uses_relation(Stream, Entity) :-
		entity_to_xml_term(Entity),
		relation_to_xml_filename(Entity, File),
		write_xml_open_tag(Stream, uses, []),
		write_xml_cdata_element(Stream, name, [], Entity),
		functor(Entity, Name, Arity),
		write_xml_cdata_element(Stream, functor, [], Name/Arity),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, uses).

	write_entity_xml_operators(Stream, Entity) :-
		write_xml_open_tag(Stream, operators, []),
		entity_property(Entity, public(PublicResources)),
		forall(
			member(op(Priority, Specifier, Operator), PublicResources),
			(	write_xml_open_tag(Stream, operator, []),
				write_xml_cdata_element(Stream, term, [], op(Priority, Specifier, Operator)),
				write_xml_cdata_element(Stream, scope, [], (public)),
				write_xml_close_tag(Stream, operator)
			)
		),
		entity_property(Entity, protected(ProtectedResources)),
		forall(
			member(op(Priority, Specifier, Operator), ProtectedResources),
			(	write_xml_open_tag(Stream, operator, []),
				write_xml_cdata_element(Stream, term, [], op(Priority, Specifier, Operator)),
				write_xml_cdata_element(Stream, scope, [], protected),
				write_xml_close_tag(Stream, operator)
			)
		),
		entity_property(Entity, private(PrivateResources)),
		forall(
			member(op(Priority, Specifier, Operator), PrivateResources),
			(	write_xml_open_tag(Stream, operator, []),
				write_xml_cdata_element(Stream, term, [], op(Priority, Specifier, Operator)),
				write_xml_cdata_element(Stream, scope, [], (private)),
				write_xml_close_tag(Stream, operator)
			)
		),
		write_xml_close_tag(Stream, operators).

	write_entity_xml_remarks(Stream, Entity) :-
		write_xml_open_tag(Stream, remarks, []),
		(	entity_property(Entity, info(Info)), member(remarks(Remarks), Info) ->
			forall(
				member((Topic - Text), Remarks),
				(	write_xml_open_tag(Stream, remark, []),
					write_xml_cdata_element(Stream, topic, [], Topic),
					write_xml_cdata_element(Stream, text, [], Text),
					write_xml_close_tag(Stream, remark)
				)
			)
		;	true
		),
		write_xml_close_tag(Stream, remarks).

	write_entity_xml_see_also(Stream, Entity) :-
		write_xml_open_tag(Stream, see_also, []),
		(	entity_property(Entity, info(Info)), member(see_also(SeeAlso), Info) ->
			forall(
				member(Reference, SeeAlso),
				(	entity_to_xml_term(Reference),
					relation_to_xml_filename(Reference, File),
					write_xml_open_tag(Stream, reference, []),
					write_xml_cdata_element(Stream, name, [], Reference),
					functor(Reference, Name, Arity),
					write_xml_cdata_element(Stream, functor, [], Name/Arity),
					write_xml_cdata_element(Stream, file, [], File),
					write_xml_close_tag(Stream, reference)
				)
			)
		;	true
		),
		write_xml_close_tag(Stream, see_also).

	% write_xml_open_tag(@stream, @atom, @list)
	%
	% writes <Tag Att1="V1" Att2="V2" ...>

	write_xml_open_tag(Stream, Tag, Atts) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '>'), nl(Stream).

	% write_xml_element(@stream, @atom, @list, @term)
	%
	% writes <Tag Att1="V1" Att2="V2" ...>Text</Tag>

	write_xml_element(Stream, Tag, Atts, Text) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '>'),
		write(Stream, Text),
		write(Stream, '</'),
		write(Stream, Tag),
		write(Stream, '>'), nl(Stream).

	% writeq_xml_cdata_element(@stream, @atom, @list, @term)
	%
	% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag> (quoted)

	%writeq_xml_cdata_element(Stream, Tag, Atts, Text) :-
	%	write(Stream, '<'),
	%	write(Stream, Tag),
	%	write_xml_tag_attributes(Stream, Atts),
	%	write(Stream, '><![CDATA['),
	%	pretty_print_vars_quoted(Stream, Text),
	%	write(Stream, ']]></'),
	%	write(Stream, Tag),
	%	write(Stream, '>'), nl(Stream).

	% write_xml_cdata_element(@stream, @atom, @list, @term)
	%
	% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag>

	write_xml_cdata_element(Stream, Tag, Atts, Text) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '><![CDATA['),
		pretty_print_vars(Stream, Text),
		write(Stream, ']]></'),
		write(Stream, Tag),
		write(Stream, '>'), nl(Stream).

	% write_xml_tag_attributes(@stream, @list)

	write_xml_tag_attributes(_, []) :-
		!.
	write_xml_tag_attributes(Stream, [Attribute-Value| Rest]) :-
		write(Stream, ' '),
		write(Stream, Attribute),
		write(Stream, '="'),
		write(Stream, Value),
		write(Stream, '"'),
		write_xml_tag_attributes(Stream, Rest).

	% write_xml_close_tag(@stream, @atom)
	%
	% writes </Tag>

	write_xml_close_tag(Stream, Tag) :-
		write(Stream, '</'),
		write(Stream, Tag),
		write(Stream, '>'),
		nl(Stream).

	entity_type(Entity, Type) :-
		(	current_object(Entity) ->
			Type = object
		;	current_category(Entity) ->
			Type = category
		;	atom(Entity), current_protocol(Entity) ->
			Type = protocol
		;	fail
		).

	entity_property(Entity, Property) :-
		nonvar(Entity),
		!,
		(	current_object(Entity) ->
			object_property(Entity, Property)
		;	atom(Entity), current_protocol(Entity) ->
			protocol_property(Entity, Property)
		;	current_category(Entity) ->
			category_property(Entity, Property)
		;	fail
		).
	entity_property(Entity, Property) :-
		current_object(Entity),
		object_property(Entity, Property).
	entity_property(Entity, Property) :-
		current_protocol(Entity),
		protocol_property(Entity, Property).
	entity_property(Entity, Property) :-
		current_category(Entity),
		category_property(Entity, Property).

	inherited_predicates(protocol, Entity, Predicates) :-
		create_object(Object, [implements(Entity)], [], []),
		find_inherited_predicates(Object, Entity, Predicates),
		abolish_object(Object).
	inherited_predicates(category, Entity, Predicates) :-
		create_object(Object, [imports(Entity)], [], []),
		find_inherited_predicates(Object, Entity, Predicates),
		abolish_object(Object).
	inherited_predicates(object, Entity, Predicates) :-
		(	\+ instantiates_class(Entity, _),
			\+ specializes_class(Entity, _) ->
			% prototype
			create_object(Object, [extends(Entity)], [], []),
			find_inherited_predicates(Object, Entity, Predicates),
			abolish_object(Object)
		;	\+ specializes_class(Entity, _) ->
			% instance
			find_inherited_predicates(Entity, Entity, Predicates)
		;	% class
			create_object(Object, [instantiates(Entity)], [], []),
			find_inherited_predicates(Object, Entity, Predicates),
			abolish_object(Object)
		).

	find_inherited_predicates(Object, Entity, Predicates) :-
		findall(
			predicate(Predicate, From, EntityName/EntityArity, File),
			(	Object::current_predicate(Name/Arity),
				functor(Template, Name, Arity),
				Object::predicate_property(Template, declared_in(From)),
				From \= Entity,
				\+ Object::predicate_property(Template, alias_of(_)),
				(	Object::predicate_property(Template, non_terminal(Predicate)) ->
					true
				;	Predicate = Name/Arity
				),
				functor(From, EntityName, EntityArity),
				relation_to_xml_filename(From, File)
			),
			Predicates
		).

	write_indexes(Options) :-
		write_library_index(Options),
		write_directory_index(Options),
		write_entity_index(Options),
		write_predicate_index(Options).

	write_library_index(Options) :-
		write_index(library, library_entity_, 'library_index.xml', Options).

	write_directory_index(Options) :-
		write_index(directory, directory_entity_, 'directory_index.xml', Options).

	write_entity_index(Options) :-
		write_index(entity, type_entity_, 'entity_index.xml', Options).

	write_predicate_index(Options) :-
		write_index(predicate, predicate_entity_, 'predicate_index.xml', Options).

	:- meta_predicate(write_index(*, 4, *, *)).

	write_index(Type, Functor, File, Options) :-
		memberchk(xml_docs_directory(XMLDirectory), Options),
		atom_concat(XMLDirectory, File, XMLFile),
		open(XMLFile, write, Stream),
		memberchk(xml_spec_reference(XMLSRef), Options),
		(	member(encoding(Encoding), Options) ->
			true
		;	Encoding = 'UTF-8'
		),
		memberchk(xml_spec(XMLSpec), Options),
		memberchk(index_xsl_file(XSL), Options),
		once(kind_ref_doctype_xsd(index, XMLSRef, DocTypeURL, XSDURL)),
		write_xml_header(XMLSRef, Encoding, XMLSpec, DocTypeURL, XSL, XSDURL, Stream),
		write_xml_element(Stream, type, [], Type),
		(	setof(
				PrimarySortKey-Key,
				SecondarySortKey^Entity^call(Functor, Key, PrimarySortKey, SecondarySortKey, Entity),
				SortedKeys
			) ->
			true
		;	SortedKeys = []
		),
		sorted_keys_to_keys(SortedKeys, Keys),
		write_xml_open_tag(Stream, entries, []),
		write_index_keys(Keys, Functor, Stream),
		write_xml_close_tag(Stream, entries),
		write_xml_close_tag(Stream, logtalk_index),
		close(Stream).

	kind_ref_doctype_xsd(entity, local, logtalk_entity-'logtalk_entity.dtd', 'logtalk_entity.xsd').
	kind_ref_doctype_xsd(entity, web, logtalk_entity-'https://logtalk.org/xml/5.0/logtalk_entity.dtd', 'https://logtalk.org/xml/5.0/logtalk_entity.xsd').
	kind_ref_doctype_xsd(entity, standalone, logtalk_entity-none, none).

	kind_ref_doctype_xsd(index, local, logtalk_index-'logtalk_index.dtd', 'logtalk_index.xsd').
	kind_ref_doctype_xsd(index, web, logtalk_index-'https://logtalk.org/xml/5.0/logtalk_index.dtd', 'https://logtalk.org/xml/5.0/logtalk_index.xsd').
	kind_ref_doctype_xsd(index, standalone, logtalk_index-none, none).

	sorted_keys_to_keys([], []).
	sorted_keys_to_keys([_-Key, _-Key| SortedKeys], Keys) :-
		!,
		sorted_keys_to_keys([_-Key| SortedKeys], Keys).
	sorted_keys_to_keys([_-Key| SortedKeys], [Key| Keys]) :-
		sorted_keys_to_keys(SortedKeys, Keys).

	write_index_keys([], _, _).
	write_index_keys([Key| Keys], Functor, Stream) :-
		write_index_key(Key, Functor, Stream),
		write_index_keys(Keys, Functor, Stream).

	:- meta_predicate(write_index_key(*, 4, *)).

	write_index_key(Key, Functor, Stream) :-
		setof(
			SecondarySortKey-Entity,
			PrimarySortKey^call(Functor, Key, PrimarySortKey, SecondarySortKey, Entity),
			SortedEntities
		),
		sorted_keys_to_keys(SortedEntities, Entities),
		write_xml_open_tag(Stream, entry, []),
		write_xml_cdata_element(Stream, key, [], Key),
		write_xml_open_tag(Stream, entities, []),
		write_index_key_entities(Entities, Stream),
		write_xml_close_tag(Stream, entities),
		write_xml_close_tag(Stream, entry).

	write_index_key_entities([], _).
	write_index_key_entities([Entity| Entities], Stream) :-
		relation_to_xml_filename(Entity, File),
		entity_to_xml_term(Entity),
		write_xml_open_tag(Stream, entity, []),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_cdata_element(Stream, name, [], Entity),
		functor(Entity, Name, Arity),
		write_xml_cdata_element(Stream, functor, [], Name/Arity),
		write_xml_close_tag(Stream, entity),
		write_index_key_entities(Entities, Stream).

	default_option(entity_xsl_file('logtalk_entity_to_xml.xsl')).
	default_option(index_xsl_file('logtalk_index_to_xml.xsl')).
	default_option(xml_spec(dtd)).
	default_option(xml_spec_reference(local)).
	default_option(xml_docs_directory('./xml_docs/')).
	default_option(bom(true)).
	default_option(encoding('UTF-8')).
	default_option(omit_path_prefixes(Prefixes)) :-
		(	logtalk::expand_library_path(home, Home) ->
			Prefixes = [Home]
		;	Prefixes = []
		).
	default_option(exclude_files([])).
	default_option(exclude_paths([])).
	default_option(exclude_prefixes([])).
	default_option(exclude_entities([])).
	default_option(sort_predicates(false)).

	valid_option(entity_xsl_file, File) :-
		atom(File).
	valid_option(index_xsl_file, File) :-
		atom(File).
	valid_option(xml_spec_reference(Reference)) :-
		(Reference == standalone -> true; Reference == (local) -> true; Reference == web).
	valid_option(xml_spec(Spec)) :-
		(Spec == dtd -> true; Spec == xsd).
	valid_option(xml_docs_directory(Directory)) :-
		atom(Directory).
	valid_option(omit_path_prefixes(Prefixes)) :-
		valid(list(atom), Prefixes).
	valid_option(bom(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(encoding(Encoding)) :-
		atom(Encoding).
	valid_option(exclude_files(Files)) :-
		valid(list(atom), Files).
	valid_option(exclude_paths(Paths)) :-
		valid(list(atom), Paths).
	valid_option(exclude_prefixes(Paths)) :-
		valid(list(atom), Paths).
	valid_option(exclude_entities(Entities)) :-
		valid(list(atom), Entities).
	valid_option(sort_predicates(Boolean)) :-
		valid(boolean, Boolean).

	fix_option(xml_docs_directory(Directory0), xml_docs_directory(Directory)) :-
		normalize_directory_paths([Directory0], [Directory]).
	fix_option(omit_path_prefixes(Prefixes0), omit_path_prefixes(Prefixes)) :-
		normalize_directory_paths(Prefixes0, Prefixes).
	fix_option(exclude_prefixes(ExcludedPrefixes0), exclude_prefixes(ExcludedPrefixes)) :-
		normalize_file_paths(ExcludedPrefixes0, ExcludedPrefixes).

	normalize_directory_paths([], []).
	normalize_directory_paths([Directory| Directories], [NormalizedDirectory| NormalizedDirectories]) :-
		os::absolute_file_name(Directory, NormalizedDirectory0),
		(	sub_atom(NormalizedDirectory0, _, _, 0, '/') ->
			NormalizedDirectory = NormalizedDirectory0
		;	atom_concat(NormalizedDirectory0, '/', NormalizedDirectory)
		),
		normalize_directory_paths(Directories, NormalizedDirectories).

	normalize_file_paths([], []).
	normalize_file_paths([File| Files], [NormalizedFile| NormalizedFiles]) :-
		os::absolute_file_name(File, NormalizedFile),
		normalize_file_paths(Files, NormalizedFiles).

	reset :-
		retractall(library_entity_(_, _, _, _)),
		retractall(directory_entity_(_, _, _, _)),
		retractall(type_entity_(_, _, _, _)),
		retractall(predicate_entity_(_, _, _, _)).

	% auxiliary predicates

	date_to_padded_atom(Year-Month-Day, DateAtom) :-
		integer_to_padded_atom(Month, MonthAtom),
		integer_to_padded_atom(Day, DayAtom),
		number_codes(Year, YearCodes),
		atom_codes(YearAtom, YearCodes),
		atom_concat(YearAtom, '-', DateAtom0),
		atom_concat(DateAtom0, MonthAtom, DateAtom1),
		atom_concat(DateAtom1, '-', DateAtom2),
		atom_concat(DateAtom2, DayAtom, DateAtom).
	date_to_padded_atom(Year/Month/Day, DateAtom) :-
		% old, deprecated date format
		date_to_padded_atom(Year-Month-Day, DateAtom).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			char_code('0', ZeroCode),
			atom_codes(Atom, [ZeroCode| Codes])
		;	atom_codes(Atom, Codes)
		).

	pretty_print_vars(Stream, Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Stream, Term, [numbervars(true)])).

	%pretty_print_vars_quoted(Stream, Term) :-
	%	\+ \+ (
	%		numbervars(Term, 0, _),
	%		write_term(Stream, Term, [numbervars(true), quoted(true)])).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
:- endif.
