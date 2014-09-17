%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(lgtdoc,
	implements(lgtdocp)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/09/17,
		comment is 'Documenting tool.',
		remarks is [
			'Compiling files for generating XML documentation' - 'All source files must be compiled with the "source_data" compiler flag turned on.',
			'xmlspec(Specification) option' - 'XML documenting files specification format. Possible option values are "dtd" (for the DTD specification; the usual default) and "xsd" (for the XML Schema specification). Most XSL processors support DTDs but only some of them support XML Schemas.',
			'xmlsref(Reference) option' - 'Reference to the XML specification file in the generated XML documenting files. The default value is "local", i.e. the reference points to a local DTD or XSD file (respectively, "logtalk.dtd" or "logtalk.xsd"), residing in the same directory as the XML file. Other possible values are "web" (the reference points to an web location, either "http://logtalk.org/xml/3.0/logtalk.dtd" or "http://logtalk.org/xml/3.0/logtalk.xsd"), and "standalone" (no reference to specification files in the XML documenting files). The most appropriated option value depends on the XSL processor you intend to use. Some of them are buggy an may not work with the default option value.',
			'xslfile(File) option' - 'XSLT file to be used with the generated XML documenting files. The default value is "lgtxml.xsl", which allows the XML files to be viewed by simply opening them with recent versions of web navigators which support XSLT transformations (after copying the "lgtxml.xsl" and of the "logtalk.css" files to the directory containing the XML files).',
			'xmldir(Directory) option' - 'Directory where the XML documenting files will be generated. The default value is "xml_docs", a sub-directory of the source files directory.',
			'bom(Boolean) option' - 'Defines if a BOM should be added to the generated XML documenting files.',
			'encoding(Encoding) option' - 'Encoding to be used for the generated XML documenting files.',
			'exclude_files(List) option' - 'List of files to exclude when generating the XML documenting files.',
			'exclude_paths(List) option' - 'List of (relative) library paths to exclude when generating the XML documenting files.',
			'exclude_entities(List) option' - 'List of entities to exclude when generating the XML documenting files.'
		]
	]).

	:- private(option_/2).
	:- dynamic(option_/2).
	:- mode(option_(?atom, ?nonvar), zero_or_more).
	:- info(option_/2, [
		comment is 'Table of option values.',
		argnames is ['Option', 'Value']
	]).

	rlibrary(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, TopPath),
		member(xmldir(XMLDirectory), Options), !,
		os::working_directory(Current),
		os::change_directory(TopPath),
		os::make_directory(XMLDirectory),
		os::change_directory(XMLDirectory),
		output_rlibrary(TopPath, Options),
		os::change_directory(Current).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopPath, Options) :-
		member(exclude_paths(ExcludedPaths), Options), !,
		forall(
			sub_library(TopPath, ExcludedPaths, LibraryPath),
			output_library_files(LibraryPath, Options)
		).

	sub_library(TopPath, ExcludedPaths, LibraryPath) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, RelativePath, LibraryPath),
		\+ member(RelativePath, ExcludedPaths).

	library(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		member(xmldir(XMLDirectory), Options), !,
		os::working_directory(Current),
		os::change_directory(Path),
		os::make_directory(XMLDirectory),
		os::change_directory(XMLDirectory),
		output_library_files(Path, Options),
		os::change_directory(Current).

	library(Library) :-
		library(Library, []).

	output_library_files(Directory, Options) :-
		once(member(exclude_files(ExcludedFiles), Options)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(File)),
		logtalk::loaded_file_property(Path, text_properties(StreamOptions)),
		\+ member(File, ExcludedFiles),
		(	atom_concat(Source1, '.lgt', File) ->
			\+ member(Source1, ExcludedFiles)
		;	atom_concat(Source2, '.logtalk', File) ->
			\+ member(Source2, ExcludedFiles)
		;	true
		),
		process(File, Directory, Options, StreamOptions),
		fail.
	output_library_files(_, _).

	file(Source, UserOptions) :-
		locate_file(Source, Basename, Directory, StreamOptions),
		merge_options(UserOptions, Options),
		member(xmldir(XMLDirectory), Options), !,
		os::working_directory(Current),
		os::change_directory(Directory),
		os::make_directory(XMLDirectory),
		os::change_directory(XMLDirectory),
		process(Basename, Directory, Options, StreamOptions),
		os::change_directory(Current).

	file(Source) :-
		file(Source, []).

	all(UserOptions) :-
		merge_options(UserOptions, Options),
		member(xmldir(XMLDirectory), Options), !,
		os::working_directory(Current),
		os::make_directory(XMLDirectory),
		os::change_directory(XMLDirectory),
		(	logtalk::loaded_file_property(Path, directory(Directory)),
			logtalk::loaded_file_property(Path, basename(File)),
			logtalk::loaded_file_property(Path, text_properties(StreamOptions)),
			process(File, Directory, Options, StreamOptions),
			fail
		;	os::change_directory(Current)
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
		atom(Source),
		(	sub_atom(Source, _, 4, 0, '.lgt') ->
			SourceWithExtension = Source
		;	sub_atom(Source, _, 8, 0, '.logtalk') ->
			SourceWithExtension = Source
		;	(	atom_concat(Source, '.lgt', SourceWithExtension)
			;	atom_concat(Source, '.logtalk', SourceWithExtension)
			)
		).

	process(File, Path, Options, StreamOptions) :-
		once(member(exclude_entities(ExcludedEntities), Options)),
		entity_property(Entity, file(File, Path)),
		\+ member(Entity, ExcludedEntities),
		write_entity_doc(Entity, Options, StreamOptions),
		fail.
	process(_, _, _, _).

	% write_entity_doc(@entity_identifier)
	%
	% writes to disk the entity documentation in XML format

	write_entity_doc(Entity, Options, StreamOptions) :-
		entity_doc_file_name(Entity, File),
		convert_stream_options(StreamOptions, ConvertedStreamOptions),
		open(File, write, Stream, ConvertedStreamOptions),
		write_xml_file(Stream, Entity, Options, StreamOptions),
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
				% but most Prolog back-end compilers don't provide the reflection
				% support to query about the operating-system and the architecture
				Encoding = unicode_be
			).
	:- else.
		convert_stream_option(StreamOption, StreamOption).
	:- endif.

	% write_xml_file(@stream)
	%
	% writes a XML file containing the documentation of a compiled entity

	write_xml_file(Stream, Entity, Options, StreamOptions) :-
		write_xml_header(Stream, Options, StreamOptions),
		write_xml_entity(Stream, Entity),
		write_xml_relations(Stream, Entity),
		write_xml_predicates(Stream, Entity),
		write_xml_operators(Stream, Entity),
		write_xml_remarks(Stream, Entity),
		write_xml_footer(Stream).

	write_xml_header(Stream, Options, StreamOptions) :-
		member(xmlsref(XMLSRef), Options),
		(	member(encoding(Encoding), StreamOptions) ->
			true
		;	Encoding = 'UTF-8'
		),
		member(xmlspec(XMLSpec), Options),
		member(xslfile(XSL), Options), !,
		write_xml_header(XMLSRef, Encoding, XMLSpec, XSL, Stream).

	write_xml_header(local, Encoding, XMLSpec, XSL, Stream) :-
		xml_header_text('1.0', Encoding, no, Text),
		write_xml_open_tag(Stream, Text, []),
		(	XMLSpec == dtd ->
			write(Stream, '<!DOCTYPE logtalk SYSTEM "logtalk.dtd">'), nl(Stream)
		;	true
		),
		write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
		write(Stream, XSL),
		write(Stream, '"?>'), nl(Stream),
		(	XMLSpec == dtd ->
			write_xml_open_tag(Stream, logtalk, [])
		;	write_xml_open_tag(Stream, logtalk,
				['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
				 'xsi:noNamespaceSchemaLocation'-'logtalk.xsd'])
		).

	write_xml_header(web, Encoding, XMLSpec, XSL, Stream) :-
		xml_header_text('1.0', Encoding, no, Text),
		write_xml_open_tag(Stream, Text, []),
		(	XMLSpec == dtd ->
			write(Stream, '<!DOCTYPE logtalk SYSTEM "http://logtalk.org/xml/3.0/logtalk.dtd">'), nl(Stream)
		;	true
		),
		write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
		write(Stream, XSL),
		write(Stream, '"?>'), nl(Stream),
		(	XMLSpec == dtd ->
			write_xml_open_tag(Stream, logtalk, [])
		;	write_xml_open_tag(Stream, logtalk,
				['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
				 'xsi:noNamespaceSchemaLocation'-'http://logtalk.org/xml/3.0/logtalk.xsd'])
		).

	write_xml_header(standalone, Encoding, _, XSL, Stream) :-
		xml_header_text('1.0', Encoding, yes, Text),
		write_xml_open_tag(Stream, Text, []),
		write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
		write(Stream, XSL),
		write(Stream, '"?>'), nl(Stream),
		write_xml_open_tag(Stream, logtalk, []).

	xml_header_text(Version, Encoding, Standalone, Text) :-
		atom_concat('?xml version="', Version, Aux1),
		atom_concat(Aux1, '" encoding="', Aux2),
		atom_concat(Aux2, Encoding, Aux3),
		atom_concat(Aux3, '" standalone="', Aux4),
		atom_concat(Aux4, Standalone, Aux5),
		atom_concat(Aux5, '"?', Text).

	write_xml_footer(Stream) :-
		write_xml_close_tag(Stream, logtalk).

	write_xml_entity(Stream, Entity) :-
		entity_type(Entity, Type),
		write_xml_open_tag(Stream, entity, []),
		entity_to_xml_term(Entity),
		write_xml_cdata_element(Stream, name, [], Entity),
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
				(write_xml_open_tag(Stream, parameter, []),
				 write_xml_cdata_element(Stream, name, [], Parname),
				 write_xml_cdata_element(Stream, description, [], Description),
				 write_xml_close_tag(Stream, parameter))),
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
			write_xml_element(Stream, date, [], Date)
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
		forall(
			(member(KeyValue, Info),
			 KeyValue =.. [Key, Value],
			 \+ member(Key, [comment, author, version, date, parameters, parnames, copyright, license, remarks])),
			(write_xml_open_tag(Stream, info, []),
			 write_xml_element(Stream, key, [], Key),
			 write_xml_cdata_element(Stream, value, [], Value),
			 write_xml_close_tag(Stream, info))).

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
	% required to build filenames in links to parametric objects

	relation_to_xml_filename(Relation, File) :-
		functor(Relation, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		atom_concat(Functor, '_', Aux),
		atom_concat(Aux, Atom, File).

	% write_xml_predicates(@stream)
	%
	% writes the predicate documentation

	write_xml_predicates(Stream, Entity) :-
		write_xml_open_tag(Stream, predicates, []),
		write_xml_public_predicates(Stream, Entity),
		write_xml_protected_predicates(Stream, Entity),
		write_xml_private_predicates(Stream, Entity),
		write_xml_close_tag(Stream, predicates).

	% write_xml_public_predicates(@stream)
	%
	% writes the documentation of public predicates

	write_xml_public_predicates(Stream, Entity) :-
		write_xml_open_tag(Stream, (public), []),
		entity_property(Entity, public(Predicates)),
		member(Functor/Arity, Predicates),
		entity_property(Entity, declares(Functor/Arity, Properties)),
		(	member(non_terminal(Functor//Args), Properties) ->
			write_xml_predicate(Stream, Entity, Functor//Args, Functor, Arity, (public))
		;	write_xml_predicate(Stream, Entity, Functor/Arity, Functor, Arity, (public))
		),
		fail.
	write_xml_public_predicates(Stream, _) :-
		write_xml_close_tag(Stream, (public)).

	% write_xml_protected_predicates(@stream)
	%
	% writes the documentation protected predicates

	write_xml_protected_predicates(Stream, Entity) :-
		write_xml_open_tag(Stream, protected, []),
		entity_property(Entity, protected(Predicates)),
		member(Functor/Arity, Predicates),
		entity_property(Entity, declares(Functor/Arity, Properties)),
		(	member(non_terminal(Functor//Args), Properties) ->
			write_xml_predicate(Stream, Entity, Functor//Args, Functor, Arity, protected)
		;	write_xml_predicate(Stream, Entity, Functor/Arity, Functor, Arity, protected)
		),
		fail.
	write_xml_protected_predicates(Stream, _) :-
		write_xml_close_tag(Stream, protected).

	% write_xml_private_predicates(@stream)
	%
	% writes the documentation of private predicates

	write_xml_private_predicates(Stream, Entity) :-
		write_xml_open_tag(Stream, private, []),
		entity_property(Entity, private(Predicates)),
		member(Functor/Arity, Predicates),
		entity_property(Entity, declares(Functor/Arity, Properties)),
		(	member(non_terminal(Functor//Args), Properties) ->
			write_xml_predicate(Stream, Entity, Functor//Args, Functor, Arity, private)
		;	write_xml_predicate(Stream, Entity, Functor/Arity, Functor, Arity, private)
		),
		fail.
	write_xml_private_predicates(Stream, _) :-
		write_xml_close_tag(Stream, private).

	% write_xml_predicate(@stream, +atom, +integer, +term)
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
		functor(Template, Functor, Arity),
		forall(
			member(mode(Template, Solutions), Properties),
			(write_xml_open_tag(Stream, (mode), []),
			 write_xml_cdata_element(Stream, template, [], Template),
			 write_xml_element(Stream, solutions, [], Solutions),
			 write_xml_close_tag(Stream, (mode)))
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
				(write_xml_open_tag(Stream, argument, []),
				 write_xml_cdata_element(Stream, name, [], Name),
				 write_xml_cdata_element(Stream, description, [], Description),
				 write_xml_close_tag(Stream, argument))),
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
				(write_xml_open_tag(Stream, exception, []),
				 write_xml_cdata_element(Stream, condition, [], Cond),
				 write_xml_cdata_element(Stream, term, [], Term),
				 write_xml_close_tag(Stream, exception))),
			write_xml_close_tag(Stream, exceptions)
		;	true
		),
		forall(
			(member(KeyValue, Info),
			 KeyValue =.. [Key, Value],
			 \+ member(Key, [comment, arguments, argnames, exceptions, examples])),
			(write_xml_open_tag(Stream, info, []),
			 write_xml_element(Stream, key, [], Key),
			 write_xml_cdata_element(Stream, value, [], Value),
			 write_xml_close_tag(Stream, info))),
		(	member(examples(Examples), Info) ->
			write_xml_open_tag(Stream, examples, []),
			forall(
				member((Description - Call - {Bindings}), Examples),
				(pred_call_to_xml_term(Entity, Functor, Arity, Call, Bindings, QCall, QBindings),
				 write_xml_open_tag(Stream, example, []),
				 write_xml_cdata_element(Stream, description, [], Description),
				 write_xml_cdata_element(Stream, call, [], QCall),
				 write_xml_cdata_element(Stream, bindings, [], QBindings),
				 write_xml_close_tag(Stream, example))),
			write_xml_close_tag(Stream, examples)
		;	true
		).

	write_xml_relations(Stream, Entity) :-
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
	write_xml_relations(Stream, Entity) :-
		entity_property(Entity, declares(AFunctor/AArity, Properties)),
		member(alias_of(PFunctor/PArity), Properties),
			Entity =.. [_| Args],				% take care of parametric entities
			vars_to_underscore(Args),
			write_xml_open_tag(Stream, alias, []),
			write_xml_cdata_element(Stream, name, [], Entity),
			write_xml_cdata_element(Stream, original, [], PFunctor/PArity),
			write_xml_cdata_element(Stream, alternative, [], AFunctor/AArity),
			write_xml_close_tag(Stream, alias),
		fail.
	write_xml_relations(Stream, _) :-
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
		entity_property(Entity, provides(Functor/Arity, To, _)),
			entity_property(To, declares(Functor/Arity, Properties)),
			(	member(non_terminal(Functor//Args), Properties) ->
				write_xml_provides_relation(Stream, Entity, To, Functor//Args)
			;	write_xml_provides_relation(Stream, Entity, To, Functor/Arity)
			),
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
		entity_property(Entity, provides(Functor/Arity, To, _)),
			entity_property(To, declares(Functor/Arity, Properties)),
			(	member(non_terminal(Functor//Args), Properties) ->
				write_xml_provides_relation(Stream, Entity, To, Functor//Args)
			;	write_xml_provides_relation(Stream, Entity, To, Functor/Arity)
			),
		fail.
	write_xml_category_relations(_, _).

	write_xml_entity_relation(Stream, Entity, Relation, Tag, Scope) :-
		relation_to_xml_term(Entity, Relation),
		relation_to_xml_filename(Relation, File),
		write_xml_open_tag(Stream, Tag, []),
		write_xml_cdata_element(Stream, name, [], Relation),
		write_xml_element(Stream, scope, [], Scope),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, Tag).

	write_xml_complements_relation(Stream, Entity, Relation) :-
		relation_to_xml_term(Entity, Relation),
		relation_to_xml_filename(Relation, File),
		write_xml_open_tag(Stream, complements, []),
		write_xml_cdata_element(Stream, name, [], Relation),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, complements).

	write_xml_provides_relation(Stream, Entity, To, Resource) :-
		relation_to_xml_term(Entity, To),
		relation_to_xml_filename(To, File),
		write_xml_open_tag(Stream, provides, []),
		write_xml_cdata_element(Stream, to, [], To),
		write_xml_cdata_element(Stream, resource, [], Resource),
		write_xml_cdata_element(Stream, file, [], File),
		write_xml_close_tag(Stream, provides).

	write_xml_operators(Stream, Entity) :-
		write_xml_open_tag(Stream, operators, []),
		entity_property(Entity, public(PublicResources)),
		forall(
			member(op(Priority, Specifier, Operator), PublicResources),
			(write_xml_open_tag(Stream, operator, []),
			 write_xml_cdata_element(Stream, term, [], op(Priority, Specifier, Operator)),
			 write_xml_cdata_element(Stream, scope, [], (public)),
			 write_xml_close_tag(Stream, operator))),
		entity_property(Entity, protected(ProtectedResources)),
		forall(
			member(op(Priority, Specifier, Operator), ProtectedResources),
			(write_xml_open_tag(Stream, operator, []),
			 write_xml_cdata_element(Stream, term, [], op(Priority, Specifier, Operator)),
			 write_xml_cdata_element(Stream, scope, [], protected),
			 write_xml_close_tag(Stream, operator))),
		entity_property(Entity, private(PrivateResources)),
		forall(
			member(op(Priority, Specifier, Operator), PrivateResources),
			(write_xml_open_tag(Stream, operator, []),
			 write_xml_cdata_element(Stream, term, [], op(Priority, Specifier, Operator)),
			 write_xml_cdata_element(Stream, scope, [], (private)),
			 write_xml_close_tag(Stream, operator))),
		write_xml_close_tag(Stream, operators).

	write_xml_remarks(Stream, Entity) :-
		write_xml_open_tag(Stream, remarks, []),
		(	entity_property(Entity, info(Info)), member(remarks(Remarks), Info) ->
			forall(
				member((Topic - Text), Remarks),
				(write_xml_open_tag(Stream, remark, []),
				 write_xml_cdata_element(Stream, topic, [], Topic),
				 write_xml_cdata_element(Stream, text, [], Text),
				 write_xml_close_tag(Stream, remark)))
		;	true
		),
		write_xml_close_tag(Stream, remarks).

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

	writeq_xml_cdata_element(Stream, Tag, Atts, Text) :-
		write(Stream, '<'),
		write(Stream, Tag),
		write_xml_tag_attributes(Stream, Atts),
		write(Stream, '><![CDATA['),
		pretty_print_vars_quoted(Stream, Text),
		write(Stream, ']]></'),
		write(Stream, Tag),
		write(Stream, '>'), nl(Stream).

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

	default_option(xslfile, 'lgtxml.xsl').
	default_option(xmlspec, dtd).
	default_option(xmlsref, local).
	default_option(xmldir, './xml_docs/').
	default_option(bom, true).
	default_option(encoding, 'UTF-8').
	default_option(exclude_files, []).
	default_option(exclude_paths, []).
	default_option(exclude_entities, []).

	valid_option(xslfile).
	valid_option(xmlsref).
	valid_option(xmlspec).
	valid_option(xmldir).
	valid_option(bom).
	valid_option(encoding).
	valid_option(exclude_files).
	valid_option(exclude_paths).
	valid_option(exclude_entities).

	valid_option(xslfile, File) :-
		atom(File).
	valid_option(xmlsref, standalone) :- !.
	valid_option(xmlsref, (local)) :- !.
	valid_option(xmlsref, web) :- !.
	valid_option(xmlspec, dtd) :- !.
	valid_option(xmlspec, xsd) :- !.
	valid_option(xmldir, Directory) :-
		atom(Directory).
	valid_option(bom, true) :- !.
	valid_option(bom, false) :- !.
	valid_option(encoding, Encoding) :-
		atom(Encoding).
	valid_option(exclude_files, List) :-
		is_atom_list(List).
	valid_option(exclude_paths, List) :-
		is_atom_list(List).
	valid_option(exclude_entities, List) :-
		is_atom_list(List).

	option(Option, Value) :-
		valid_option(Option),
		(	option_(Option, Value2) ->
			Value = Value2
		;	default_option(Option, Value2) ->
			Value = Value2
		).
	set_option(Option, Value) :-
		atom(Option),
		ground(Value),
		valid_option(Option, Value),
		retractall(option_(Option, _)),
		assertz(option_(Option, Value)).

	merge_options(UserOptions, Options) :-
		(member(xmldir(Directory), UserOptions) -> true; option(xmldir, Directory)),
		(member(xmlsref(XMLSRef), UserOptions) -> true; option(xmlsref, XMLSRef)),
		(member(xmlspec(XMLSpec), UserOptions) -> true; option(xmlspec, XMLSpec)),
		(member(xslfile(XSL), UserOptions) -> true; option(xslfile, XSL)),
		(member(encoding(Encoding), UserOptions) -> true; option(encoding, Encoding)),
		(member(bom(BOM), UserOptions) -> true; option(bom, BOM)),
		% by default, don't exclude any source files:
		(member(exclude_files(ExcludedFiles), UserOptions) -> true; option(exclude_files, ExcludedFiles)),
		% by default, don't exclude any library sub-directories:
		(member(exclude_paths(ExcludedPaths), UserOptions) -> true; option(exclude_paths, ExcludedPaths)),
		% by default, don't exclude any entities:
		(member(exclude_entities(ExcludedEntities), UserOptions) -> true; option(exclude_entities, ExcludedEntities)),
		Options = [
			xmldir(Directory), xmlsref(XMLSRef), xmlspec(XMLSpec), xslfile(XSL),
			encoding(Encoding), bom(BOM),
			exclude_files(ExcludedFiles), exclude_paths(ExcludedPaths), exclude_entities(ExcludedEntities)
		].

	% we want to minimize any dependencies on other entities, including library objects

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	member_var(V, [H| _]) :-
		V == H.
	member_var(V, [_| T]) :-
		member_var(V, T).

	is_atom_list((-)) :-
		!,
		fail.
	is_atom_list([]).
	is_atom_list([Atom| Atoms]) :-
		atom(Atom),
		is_atom_list(Atoms).

	pretty_print_vars(Stream, Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Stream, Term, [numbervars(true)])).

	pretty_print_vars_quoted(Stream, Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Stream, Term, [numbervars(true), quoted(true)])).

:- end_object.
