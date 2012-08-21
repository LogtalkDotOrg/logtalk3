/* xml.pl : Contains parse/[2,3] a bi-directional XML parser written in
 * Prolog.
 *
 * Copyright (C) 2001-2005 Binding Time Limited
 * Copyright (C) 2005, 2006 John Fletcher
 *
 * Current Release: $Revision: 2.0 $
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction,
 * but entirely at your own risk.
 *
 */

:- object(xml).

	:- info([
		version is 2.0,
		author is 'John Fletcher',
		date is 2006/11/2,
		copyright is 'Copyright (C) 2001-2005 Binding Time Limited, Copyright (C) 2005, 2006 John Fletcher',
		license is 'The code has been placed into the public domain, to encourage the use of Prolog with XML. This program is offered free of charge, as unsupported source code. You may use it, copy it, distribute it, modify it or sell it without restriction, but entirely at your own risk.',
		comment is 'Bi-directional XML parser.',
		remarks is [
			'On-line documentation:' - 'http://www.zen37763.zen.co.uk/xml.pl.html',
			'Compliance:' - 'This XML parser supports a subset of XML suitable for XML Data and Worldwide Web applications. It is neither as strict nor as comprehensive as the XML 1.0 Specification mandates.',
			'Compliance-strictness:' - 'It is not as strict, because, while the specification must eliminate ambiguities, not all errors need to be regarded as faults, and some reasonable examples of real XML usage would have to be rejected if they were.',
			'Compliance-comprehensive:' - 'It is not as comprehensive, because, where the XML specification makes provision for more or less complete DTDs to be provided as part of a document, xml.pl actions the local definition of ENTITIES only. Other DTD extensions are treated as commentary.',
			'Bi-directional conversions:' - 'The conversions are not completely symmetrical, in that weaker XML is accepted than can be generated. Specifically, in-bound (Chars -> Document) parsing does not require strictly well-formed XML. If Chars does not represent well-formed XML, Document is instantiated to the term malformed(<attributes>, <content>).'
		]]).

	:- public(parse/2).
	:- mode(parse(+nonvar, ?nonvar), zero_or_one).
	:- mode(parse(?nonvar, +nonvar), zero_or_one).
	:- info(parse/2, [
		comment is 'Parses Chars to/from a data structure of the form xml(<atts>, <content>).',
		argnames is ['Chars', 'Document']]).

	:- public(parse/3).
	:- mode(parse(+nonvar, +nonvar, ?nonvar), zero_or_one).
	:- mode(parse(+nonvar, ?nonvar, +nonvar), zero_or_one).
	:- info(parse/3, [
		comment is 'Parses Chars to/from a data structure of the form xml(<atts>, <content>).',
		argnames is ['Controls', 'Chars', 'Document']]).

	:- public(subterm/2).
	:- mode(subterm(+nonvar, ?nonvar), zero_or_one).
	:- info(subterm/2, [
		comment is 'Unifies Subterm with a sub-term of XMLTerm. Note that XMLTerm is a sub-term of itself.',
		argnames is ['XMLTerm', 'Subterm']]).

	:- public(pp/1).
	:- mode(pp(+nonvar), zero_or_one).
	:- info(pp/1, [
		comment is 'Pretty prints a XML document on the current output stream.',
		argnames is ['XMLDocument']]).

	:- uses(list, [append/3, member/2, select/3, valid/1::is_list/1]).
	:- uses(term, [ground/1]).

	/* parse( {+Controls}, +?Chars, ?+Document ) parses Chars to/from a data
	 * structure of the form xml(<atts>, <content>). <atts> is a list of
	 * <atom>=<string> attributes from the (possibly implicit) XML signature of the
	 * document. <content> is a (possibly empty) list comprising occurrences of :
	 *
	 *    pcdata(<string>)                    :  Text
	 *    comment(<string>)                   :  An xml comment;
	 *    element(<tag>,<atts>,<content>)     :  <tag>..</tag> encloses <content>
	 *                                        :  <tag /> if empty
	 *    instructions(<atom>, <string>)      :  Processing <? <atom> <params> ?>"
	 *     cdata( <string> )                  :  <![CDATA[ <string> ]]>
	 *    doctype(<atom>, <doctype id>)       :  DTD <!DOCTYPE .. >
	 *
	 * The conversions are not completely symmetrical, in that weaker XML is
	 * accepted than can be generated. Specifically, in-bound (Chars -> Document)
	 * does not  require strictly well-formed XML. Document is instantiated to the
	 * term malformed(Attributes, Content) if Chars does not represent well-formed
	 * XML. The Content of a malformed/2 structure can contain:
	 *
	 *    unparsed( <string> )                :  Text which has not been parsed
	 *    out_of_context( <tag> )             :  <tag> is not closed
	 *
	 * in addition to the standard term types.
	 *
	 * Out-bound (Document -> Chars) parsing _does_ require that Document defines
	 * strictly well-formed XML. If an error is detected a 'domain' exception is
	 * raised.
	 *
	 * The domain exception will attempt to identify the particular sub-term in
	 * error and the message will show a list of its ancestor elements in the form
	 * <tag>{(id)}* where <id> is the value of any attribute _named_ id.
	 *
	 * At this release, the Controls applying to in-bound (Chars -> Document)
	 * parsing are:
	 *
	 *    extended_characters(<bool>)         :  Use the extended character
	 *                                        :  entities for XHTML (default true)
	 *
	 *    format(<bool>)                      :  Strip layouts when no character data
	 *                                        :  appears between elements.
	 *                                        :  (default true)
	 *
	 *    remove_attribute_prefixes(<bool>)   :  Remove namespace prefixes from
	 *                                        :  attributes when it's the same as the
	 *                                        :  prefix of the parent element
	 *                                        :  (default false).
	 *
	 *    allow_ampersand(<bool>)             :  Allow unescaped ampersand
	 *                                        :  characters (&) to occur in PCDATA.
	 *                                        :  (default false).
	 *
	 *    [<bool> is one of 'true' or 'false']
	 *
	 * For out-bound (Document -> Chars) parsing, the only available option is:
	 *
	 *    format(<Bool>)                      :  Indent the element content
	 *                                        :  (default true)
	 *
	 * Different DCGs for input and output are used because input parsing is
	 * more flexible than output parsing. Errors in input are recorded as part
	 * of the data structure. Output parsing throws an exception if the document
	 * is not well-formed, diagnosis tries to identify the specific culprit term.
	 */
	parse( Chars, Document ) :-
		parse( [], Chars, Document ).

	parse( Controls, Chars, Document ) :-
		(	ground( Chars ) ->
			xml_to_document( Controls, Chars, Document )
		;	document_to_xml( Controls, Document, Chars )
		).

	document_to_xml( Controls, Document, Chars ) :-
		(	member( format(false), Controls ) ->
			Format = false
		;	Format = true
		),
		(	ground( Document ),
			document_generation(Format, Document, Chars0, [] ) ->
			Chars = Chars0
		;	fault( Document, [], Culprit, Path, Message ),
			throw(
				application_error('XML Parse: ~s in ~q~nCulprit: ~q~nPath: ~s', 
					[Message,Document,Culprit,Path] )
				)
		).

	/* subterm( +XMLTerm, ?Subterm ) unifies Subterm with a sub-term of Term.
	 * Note that XMLTerm is a sub-term of itself. 
	 */
	subterm( Term, Term ).
	subterm( xml(_Attributes, Content), Term ) :-
		subterm( Content, Term ).	
	subterm( [H|T], Term ) :-
		(	subterm( H, Term )
		;	subterm( T, Term )
		).
	subterm( element(_Name,_Attributes,Content), Term ) :-
		subterm( Content, Term ).
	subterm( namespace(_URI,_Prefix,Content), Term ) :-
		subterm( Content, Term ).


	/* xml_to_document( +Controls, +XML, ?Document ) translates the list of
	 * character codes XML into the Prolog term Document. Controls is a list
	 * of terms controlling the treatment of layout characters and character
	 * entities.
	 */

	:- private(xml_to_document/3).
	:- mode(xml_to_document(+nonvar, +nonvar, ?nonvar), zero_or_one).
	:- info(xml_to_document/3, [
		comment is 'Translates the list of character codes XML into the Prolog term Document. Controls is a list of terms controlling the treatment of layout characters and character entities.',
		argnames is ['Controls', 'XML', 'Document']]).

	xml_to_document( Controls, XML, Document ) :-
		initial_context( Controls, Context ),
		(	xml_declaration( Attributes0, XML, XML1 ) ->
			Attributes = Attributes0
		;	otherwise ->
			XML1 = XML,
			Attributes = []
		),
		xml_to_document( XML1, Context, Terms, [], WellFormed ),
		xml_to_document1( WellFormed, Attributes, Terms, Document ).

	xml_to_document1( true,  Attributes, Terms, xml(Attributes, Terms) ).
	xml_to_document1( false, Attributes, Terms, malformed(Attributes, Terms) ).

	% unparsed( +Unparsed, +Context, ?Terms, ?Residue, ?WellFormed )
	unparsed( Unparsed, _Context, [unparsed(Unparsed)], [], false ).

	xml_declaration( Attributes ) -->
		spaces,
		"<?",
		nmtoken( xml ),
		xml_declaration_attributes( Attributes ),
		spaces,
		"?>".

	xml_to_document( [], Context, Terms, [], WF ) :-
		close_context( Context, Terms, WF ).
	xml_to_document( [Char|Chars], Context, Terms, Residue, WF ) :-
		(	Char =:= 0'< ->
			markup_structure( Chars, Context, Terms, Residue, WF )
		;	Char =:= 0'& ->
			entity_reference( Chars, Context, Terms, Residue, WF )
		;	Char =< 0' ,
			\+ space_preserve( Context ) ->		
			layouts( Chars, Context, [Char|T], T, Terms, Residue, WF )
		;	void_context( Context ) ->
			unparsed( [Char|Chars], Context, Terms, Residue, WF )
		;	otherwise ->
			Terms = [pcdata([Char|Chars1])|Terms1],
			acquire_pcdata( Chars, Context, Chars1, Terms1, Residue, WF )
		).

	layouts( [], Context, _Plus, _Minus, Terms, [], WF ) :-
		close_context( Context, Terms, WF ).
	layouts( [Char|Chars], Context, Plus, Minus, Terms, Residue, WF ) :-
		(	Char =:= 0'< ->
			markup_structure( Chars, Context, Terms, Residue, WF )
		;	Char =:= 0'& ->
			reference_in_layout( Chars, Context, Plus, Minus, Terms, Residue, WF )
		;	Char =< 0'  ->
			Minus = [Char|Minus1],
			layouts( Chars, Context, Plus, Minus1, Terms, Residue, WF )
		;	void_context( Context ) ->
			unparsed( [Char|Chars], Context, Terms, Residue, WF )
		;	otherwise ->
			Terms = [pcdata(Plus)|Terms1],
			Minus = [Char|Chars1],
			context_update( space_preserve, Context, true, Context1 ),
			acquire_pcdata( Chars, Context1, Chars1, Terms1, Residue, WF )
		).

	acquire_pcdata( [], Context, [], Terms, [], WF ) :-
		close_context( Context, Terms, WF ).
	acquire_pcdata( [Char|Chars], Context, Chars1, Terms, Residue, WF ) :-
		(	Char =:= 0'< ->
			Chars1 = [],
			markup_structure( Chars, Context, Terms, Residue, WF )
		;	Char =:= 0'& ->
			reference_in_pcdata( Chars, Context, Chars1, Terms, Residue, WF )
		;	otherwise ->
			Chars1 = [Char|Chars2],
			acquire_pcdata( Chars, Context, Chars2, Terms, Residue, WF )
		).

	markup_structure( [], Context, Terms, Residue, WF ) :-
		unparsed( "<", Context, Terms, Residue, WF ).
	markup_structure( Chars, Context, Terms, Residue, WF ) :-
		Chars = [Char|Chars1],
		(	Char =:= 0'/ ->
			closing_tag( Context, Chars1, Terms, Residue, WF )
		;	Char =:= 0'? ->
			pi_acquisition( Chars1, Context, Terms, Residue, WF )
		;	Char =:= 0'! ->
			declaration_acquisition( Chars1, Context, Terms, Residue, WF )
		;	open_tag(Tag,Context,Attributes,Type, Chars, Chars2 ) ->
			push_tag( Tag, Chars2, Context, Attributes, Type, Terms, Residue, WF )
		;	otherwise ->
			unparsed( [0'<|Chars], Context, Terms, Residue, WF ) %'
		).

	push_tag( Tag, Chars, Context, Attributes, Type, Terms, Residue, WF ) :-
		new_element(Tag, Chars, Context, Attributes, Type, Term, Rest, WF0),
		push_tag1( WF0, Context, Term, Rest, Terms, Residue, WF ).

	push_tag1( true, Context, Term, Chars, [Term|Terms], Residue, WF ) :-
		xml_to_document( Chars, Context, Terms, Residue, WF ).
	push_tag1( false, _Context, Term, Chars, [Term], Chars, false ).

	new_element( TagChars, Chars, Context, Attributes0, Type, Term, Residue, WF ) :-
		namespace_attributes( Attributes0, Context, Context1, Attributes1 ),
		(	append( NSChars, [0':|TagChars1], TagChars ), %'
			specific_namespace( NSChars, Context1, SpecificNamespace ) ->
			Namespace0 = SpecificNamespace
		;	otherwise ->
			NSChars = "",
			TagChars1 = TagChars,
			default_namespace( Context1, Namespace0 )
		),
		current_namespace( Context1, CurrentNamespace ),
		(	Namespace0 == CurrentNamespace ->
			Term = element(Tag, Attributes, Contents),
			Context2 = Context1
		;	otherwise ->
			Term = namespace( Namespace0, NSChars,
						element(Tag, Attributes, Contents)
						),
			context_update( current_namespace, Context1, Namespace0, Context2 )
		),
		input_attributes( Attributes1, Context2, Attributes ),
		atom_codes( Tag, TagChars1 ),
		close_tag( Type, Chars, Context2, Contents, Residue, WF ).

	close_tag( empty, Residue, _Context, [], Residue, true ).
	close_tag( push(Tag), Chars, Context0, Contents, Residue, WF ) :-
		context_update( element, Context0, Tag, Context1 ),
		xml_to_document( Chars, Context1, Contents, Residue, WF ).

	pi_acquisition( Chars, Context, Terms, Residue, WellFormed ) :-
		(	inline_instruction(Target, Processing, Chars, Rest ),
			Target \== xml ->
			Terms = [instructions(Target, Processing)|Terms1],
			xml_to_document( Rest, Context, Terms1, Residue, WellFormed )
		;	otherwise ->
			unparsed( [0'<,0'?|Chars], Context, Terms, Residue, WellFormed )
		).

	declaration_acquisition( Chars, Context, Terms, Residue, WF ) :-
		(	declaration_type( Chars, Type, Chars1 ),
			declaration_parse( Type, Context, Term, Context1, Chars1, Rest ) ->
			Terms = [Term|Terms1],
			xml_to_document( Rest, Context1, Terms1, Residue, WF )
		;	otherwise ->
			unparsed( [0'<,0'!|Chars], Context, Terms, Residue, WF )
		).

	open_tag( Tag, Namespaces, Attributes, Termination ) -->
		nmtoken_chars( Tag ),
		attributes( Attributes, [], Namespaces ),
		spaces,
		open_tag_terminator( Tag, Termination ).

	open_tag_terminator( Tag, push(Tag) ) -->
		">".
	open_tag_terminator( _Tag, empty ) -->
		"/>".

	declaration_parse( comment, Namespaces, comment(Comment), Namespaces ) -->
		comment(Comment).
	declaration_parse( cdata, Namespaces, cdata(CData), Namespaces ) -->
		cdata( CData ).
	declaration_parse( doctype, Namespaces0, doctype(Name, Names), Namespaces ) -->
		doctype( Name, Names, Namespaces0, Namespaces ),
		spaces,
		">".

	inline_instruction( Target, Processing, Plus, Minus  ) :-
		nmtoken(Target, Plus, Mid0 ),
		spaces( Mid0, Mid1 ),
		append( Processing, [0'?,0'>|Minus], Mid1 ),
		!.

	entity_reference_name( Reference ) -->
		nmtoken_chars( Reference ),
		";".

	declaration_type( [Char1,Char2|Chars1], Class, Rest ) :-
		Chars = [Char1,Char2|Chars1],
		(	declaration_type1( Char1, Char2, Chars1, Class0, Residue ) ->
			Class = Class0,
			Rest = Residue
		;	otherwise ->
			Class = generic,
			Rest = Chars
		).

	declaration_type1( 0'-, 0'-, Chars, comment, Chars ).
	declaration_type1( 0'[, 0'C, Chars, cdata, Residue ) :-
		append( "DATA[", Residue, Chars ).
	declaration_type1( 0'D, 0'O, Chars, doctype, Residue ) :-
		append( "CTYPE", Residue, Chars ).

	closing_tag( Context, Chars, Terms, Residue, WellFormed ) :-
		(	closing_tag_name( Tag, Chars, Rest ),
			current_tag( Context, Tag ) ->
			Terms = [],
			Residue = Rest,
			WellFormed = true
		;	otherwise ->
			unparsed( [0'<,0'/|Chars], Context, Terms, Residue, WellFormed )
		).

	closing_tag_name( Tag ) -->
		nmtoken_chars( Tag ),
		spaces,
		">".

	entity_reference( Chars, Context, Terms, Residue, WF ) :-
		reference_in_layout( Chars, Context, L, L, Terms, Residue, WF ).

	reference_in_layout( Chars, Context, Plus, Minus, Terms, Residue, WF ) :-
		(	standard_character_entity( Char, Chars, Rest ) ->
			Minus = [Char|Chars1],
			Terms = [pcdata(Plus)|Terms1],
			acquire_pcdata( Rest, Context, Chars1, Terms1, Residue, WF )
		;	entity_reference_name( Reference, Chars, Rest ),
			defined_entity( Reference, Context, String ) ->
			append( String, Rest, Full ),
			xml_to_document( Full, Context, Terms, Residue, WF )
		;	allow_ampersand( Context ) ->
			Minus = [0'&|Chars1], %'
			Terms = [pcdata(Plus)|Terms1],
			acquire_pcdata( Chars, Context, Chars1, Terms1, Residue, WF )
		;	otherwise ->
			unparsed( [0'&|Chars], Context, Terms, Residue, WF ) %'
		).

	reference_in_pcdata( Chars0, Context, Chars1, Terms, Residue, WF ) :-
		(	standard_character_entity( Char, Chars0, Rest ) ->
			Chars1 = [Char|Chars2],
			acquire_pcdata( Rest, Context, Chars2, Terms, Residue, WF )
		;	entity_reference_name( Reference, Chars0, Rest ),
			defined_entity( Reference, Context, String ) ->
			append( String, Rest, Full ),
			acquire_pcdata( Full, Context, Chars1, Terms, Residue, WF )
		;	allow_ampersand( Context ) ->
			Chars1 = [0'&|Chars2],
			acquire_pcdata( Chars0, Context, Chars2, Terms, Residue, WF )
		;	otherwise ->
			Chars1 = [],
			unparsed( [0'&|Chars0], Context, Terms, Residue, WF )
		).

	namespace_attributes( [], Context, Context, [] ).
	namespace_attributes( Attributes0, Context0, Context, Attributes ) :-
		Attributes0 = [_|_],
		append( "xmlns:", Unqualified, QualifiedNameChars ),
		(	select( "xmlns"=Value, Attributes0, Attributes1 ) ->
			atom_codes( URI, Value ),
			context_update( default_namespace, Context0, URI, Context1 ),
			namespace_attributes( Attributes1, Context1, Context, Attributes )
		;	select( QualifiedNameChars=Value, Attributes0, Attributes1 ) ->
			Attributes = [QualifiedNameChars=Value|Attributes2],
			atom_codes( URI, Value ),
			context_update( ns_prefix(Unqualified), Context0, URI, Context1 ),
			namespace_attributes( Attributes1, Context1, Context, Attributes2 )
		;	member( "xml:space"="preserve", Attributes0 ) ->
			Attributes = Attributes0,
			context_update( space_preserve, Context0, true, Context )
		;	otherwise ->
			Context = Context0,
			Attributes = Attributes0
		).

	input_attributes( [], _Context, [] ).
	input_attributes( [NameChars=Value|Attributes0], Context,
			[Name=Value|Attributes] ) :-
		(	remove_attribute_prefixes( Context ),
			append( NSChars, [0':|NameChars1], NameChars ), %'
			NSChars \== "xmlns",
			specific_namespace( NSChars, Context, Namespace ),
			current_namespace( Context, Namespace ) ->
			atom_codes( Name, NameChars1 )
		;	otherwise ->
			atom_codes( Name, NameChars )
		),
		input_attributes( Attributes0, Context, Attributes ).

	attributes( [Name=Value|Attributes], Seen, Namespaces ) -->
		spaces,
		nmtoken_chars( Name ),
		{\+ member(Name, Seen)},
		spaces,
		"=",
		spaces,
		attribute_value( Value, Namespaces ),
		attributes( Attributes, [Name|Seen], Namespaces ).
	attributes( [], _Seen, _Namespaces ) --> "".

	xml_declaration_attributes( [] ) --> "".
	xml_declaration_attributes( [Name=Value|Attributes] ) -->
		spaces,
		nmtoken( Name ),
		spaces,
		"=",
		spaces,
		xml_string( Value ),
		{xml_declaration_attribute_valid(Name, Value)},
		xml_declaration_attributes( Attributes ),
		spaces.

	doctype( Name, External, Namespaces0, Namespaces1 ) -->
		spaces,
		nmtoken( Name ),
		spaces,
		doctype_id( External0 ),
		spaces,
		doctype1( Namespaces0, Literals, Namespaces1 ),
		{doctype_extension(Literals, External0, External)}.

	doctype_extension( [], External, External ).
	doctype_extension( [Literal|Literals], External0, External ) :-
		extended_doctype( External0, [Literal|Literals], External ).

	extended_doctype( system(URL), Literals, system(URL,Literals) ).
	extended_doctype( public(URN,URL), Literals, public(URN,URL,Literals) ).
	extended_doctype( local, Literals, local(Literals) ).

	doctype1( Namespaces0, Literals, Namespaces1 ) -->
		"[",
		!,
		dtd( Namespaces0, Literals, Namespaces1 ),
		"]".
	doctype1( Namespaces, [], Namespaces ) --> "".

	doctype_id( system(URL) ) -->
		"SYSTEM",
		spaces,
		uri( URL ).
	doctype_id( public(URN,URL) ) -->
		"PUBLIC",
		spaces,
		uri( URN ),
		spaces,
		uri( URL ).
	doctype_id( local ) --> "".

	dtd( Namespaces0, Literals, Namespaces1 ) -->
		spaces,
		"<!ENTITY",
		!,
		spaces,
		nmtoken_chars( Name ),
		spaces,
		quote( Quote ),
		entity_value( Quote, Namespaces0, String ),
		spaces,
		">",
		{\+ character_entity( Name, _StandardChar ), 
		 % Don't allow &lt; &quote; etc. to be updated
		 context_update( entity(Name), Namespaces0, String, Namespaces2 )
		 },
		dtd( Namespaces2, Literals, Namespaces1 ).
	dtd( Namespaces0, Literals, Namespaces1 ) -->
		spaces,
		"<!--",
		!,
		dtd_comment,
		">",
		dtd( Namespaces0, Literals, Namespaces1 ).
	dtd( Namespaces0, [dtd_literal(Literal)|Literals], Namespaces1 ) -->
		spaces,
		"<!",
		!,
		dtd_literal( Literal ),
		dtd( Namespaces0, Literals, Namespaces1 ).
	dtd( Namespaces, [], Namespaces ) --> spaces.

	dtd_literal( [] ) --> ">", !.
	dtd_literal( Chars ) -->
		"--",
		!,
		dtd_comment,
		dtd_literal( Chars ).
	dtd_literal( [Char|Chars] ) -->
		[Char],
		dtd_literal( Chars ).

	dtd_comment( Plus, Minus ) :-
		append( _Chars, [0'-,0'-|Minus], Plus ),
		!.

	nmtokens( [Name|Names] ) -->
		spaces,
		nmtoken( Name ),
		nmtokens( Names ).
	nmtokens( [] ) --> [].

	entity_value( Quote, Namespaces, String, [Char|Plus], Minus ) :-
		(	Char == Quote ->
			String = [],
			Minus = Plus
		;	Char =:= 0'& ->
			reference_in_entity( Namespaces, Quote, String, Plus, Minus )
		;	otherwise ->
			String = [Char|String1],
			entity_value( Quote, Namespaces, String1, Plus, Minus )
		).

	attribute_value( String, Namespaces ) -->
		quote( Quote ),
		attribute_leading_layouts( Quote, Namespaces, String ).

	attribute_leading_layouts( _Quote, _Namespace, [], [], [] ).
	attribute_leading_layouts( Quote, Namespaces, String, [Char|Plus], Minus ) :-
		(	Char == Quote ->
			String = [],
			Minus = Plus
		;	Char =:= 0'& ->
			ref_in_attribute_layout( Namespaces, Quote, String, Plus, Minus )
		;	Char > 32, Char \== 160 ->
			String = [Char|String1],
			attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
		;	otherwise ->
			attribute_leading_layouts( Quote, Namespaces, String, Plus, Minus )
		).

	attribute_layouts( _Quote, _Namespaces, _Layout, [], [], [] ).
	attribute_layouts( Quote, Namespaces, Layout, String, [Char|Plus], Minus ) :-
		(	Char == Quote ->
			String = [],
			Minus = Plus
		;	Char =:= 0'& ->
			reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus )
		;	Char > 32, Char \== 160 ->
			(	Layout == true ->
				String = [0' ,Char|String1] %'
			;	otherwise ->
				String = [Char|String1]
			),
			attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
		;	otherwise ->
			attribute_layouts( Quote, Namespaces, true, String, Plus, Minus )
		).

	ref_in_attribute_layout( NS, Quote, String, Plus, Minus ) :-
		(	standard_character_entity( Char, Plus, Mid ) ->
			String = [Char|String1],
			attribute_layouts( Quote, NS, false,  String1, Mid, Minus )
		;	entity_reference_name( Name, Plus, Suffix ),
			defined_entity( Name, NS, Text ) ->
			append( Text, Suffix, Mid ),
			attribute_leading_layouts( Quote, NS, String, Mid, Minus )
		;	otherwise -> % Just & is okay in a value
			String = [0'&|String1], %'
			attribute_layouts( Quote, NS, false, String1, Plus, Minus )
		).

	reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus ) :-
		(	standard_character_entity( Char, Plus, Mid ) ->
			(	Layout == true ->
				String = [0' ,Char|String1] %'
			;	otherwise ->
				String = [Char|String1]
			),
			Layout1 = false
		;	entity_reference_name( Name, Plus, Suffix ),
			defined_entity( Name, Namespaces, Text ) ->
			String = String1,
			append( Text, Suffix, Mid ),
			Layout1 = Layout
		;	otherwise -> % Just & is okay in a value
			Mid = Plus,
			String = [0'&|String1], %'
			Layout1 = false
		),
		attribute_layouts( Quote, Namespaces, Layout1, String1, Mid, Minus ).

	/* References are resolved backwards in Entity defintions so that
	 * circularity is avoided.
	 */
	reference_in_entity( Namespaces, Quote, String, Plus, Minus ) :-
		(	standard_character_entity( _SomeChar, Plus, _Rest ) ->
			String = [0'&|String1], % ' Character entities are unparsed
			Mid = Plus
		;	entity_reference_name( Name, Plus, Suffix ), 
			defined_entity( Name, Namespaces, Text ) -> 
			String = String1,
			append( Text, Suffix, Mid )
		),
		entity_value( Quote, Namespaces, String1, Mid, Minus ).

	standard_character_entity( Char ) -->
		"#x", hex_character_reference( Char ), ";".
	standard_character_entity( Char ) -->
		"#", digit( Digit ), digits( Digits ), ";",
		{number_chars( Char, [Digit|Digits])}.
	standard_character_entity( C ) -->
		chars( String ),
		";",
		!,
		{character_entity(String, C)}.

	uri( URI ) -->
		quote( Quote ),
		uri1( Quote, URI ).

	uri1( Quote, [] ) -->
		quote( Quote ),
		!.
	uri1( Quote, [Char|Chars] ) -->
		[Char],
		uri1( Quote, Chars ).

	comment( Chars, Plus, Minus ) :-
		append( Chars, [0'-,0'-,0'>|Minus], Plus ), %'
		!.

	cdata( Chars, Plus, Minus ) :-
		append( Chars, [0'],0'],0'>|Minus], Plus ), %'
		!.
	% Syntax Components

	hex_character_reference( Code ) -->
		hex_character_reference1( 0, Code ).

	hex_character_reference1( Current, Code ) -->
		hex_digit_char( Value ),
		!,
		{New is (Current << 4) + Value},
		hex_character_reference1( New, Code ).
	hex_character_reference1( Code, Code ) --> "".

	hex_digit_char( 0 ) --> "0".
	hex_digit_char( 1 ) --> "1".
	hex_digit_char( 2 ) --> "2".
	hex_digit_char( 3 ) --> "3".
	hex_digit_char( 4 ) --> "4".
	hex_digit_char( 5 ) --> "5".
	hex_digit_char( 6 ) --> "6".
	hex_digit_char( 7 ) --> "7".
	hex_digit_char( 8 ) --> "8".
	hex_digit_char( 9 ) --> "9".
	hex_digit_char( 10 ) --> "A".
	hex_digit_char( 11 ) --> "B".
	hex_digit_char( 12 ) --> "C".
	hex_digit_char( 13 ) --> "D".
	hex_digit_char( 14 ) --> "E".
	hex_digit_char( 15 ) --> "F".
	hex_digit_char( 10 ) --> "a".
	hex_digit_char( 11 ) --> "b".
	hex_digit_char( 12 ) --> "c".
	hex_digit_char( 13 ) --> "d".
	hex_digit_char( 14 ) --> "e".
	hex_digit_char( 15 ) --> "f".

	quote( 0'" ) --> %'
		"""".
%	quote( 0'' ) -->
%		"'".
	quote( QuoteCode ) -->
		"'", {atom_codes('''', [QuoteCode])}.

	spaces( [], [] ).
	spaces( [Char|Chars0], Chars1 ) :-
		(	Char =< 32 ->
			spaces( Chars0, Chars1 )
		;	otherwise ->
			Chars1 = [Char|Chars0]
		).

	nmtoken( Name ) -->
		nmtoken_chars( Chars ),
		{atom_codes(Name, Chars)}.

	nmtoken_chars( [Char|Chars] ) -->
		[Char],
		{nmtoken_first( Char )},
		nmtoken_chars_tail( Chars ).

	nmtoken_chars_tail( [Char|Chars] ) -->
		[Char],
		{nmtoken_char(Char)},
		!,
		nmtoken_chars_tail( Chars ).
	nmtoken_chars_tail([]) --> "".

	nmtoken_first( 0': ).
	nmtoken_first( 0'_ ).
	nmtoken_first( Char ) :-
		alphabet( Char ).

	nmtoken_char( 0'a ).
	nmtoken_char( 0'b ).
	nmtoken_char( 0'c ).
	nmtoken_char( 0'd ).
	nmtoken_char( 0'e ).
	nmtoken_char( 0'f ).
	nmtoken_char( 0'g ).
	nmtoken_char( 0'h ).
	nmtoken_char( 0'i ).
	nmtoken_char( 0'j ).
	nmtoken_char( 0'k ).
	nmtoken_char( 0'l ).
	nmtoken_char( 0'm ).
	nmtoken_char( 0'n ).
	nmtoken_char( 0'o ).
	nmtoken_char( 0'p ).
	nmtoken_char( 0'q ).
	nmtoken_char( 0'r ).
	nmtoken_char( 0's ).
	nmtoken_char( 0't ).
	nmtoken_char( 0'u ).
	nmtoken_char( 0'v ).
	nmtoken_char( 0'w ).
	nmtoken_char( 0'x ).
	nmtoken_char( 0'y ).
	nmtoken_char( 0'z ).
	nmtoken_char( 0'A ).
	nmtoken_char( 0'B ).
	nmtoken_char( 0'C ).
	nmtoken_char( 0'D ).
	nmtoken_char( 0'E ).
	nmtoken_char( 0'F ).
	nmtoken_char( 0'G ).
	nmtoken_char( 0'H ).
	nmtoken_char( 0'I ).
	nmtoken_char( 0'J ).
	nmtoken_char( 0'K ).
	nmtoken_char( 0'L ).
	nmtoken_char( 0'M ).
	nmtoken_char( 0'N ).
	nmtoken_char( 0'O ).
	nmtoken_char( 0'P ).
	nmtoken_char( 0'Q ).
	nmtoken_char( 0'R ).
	nmtoken_char( 0'S ).
	nmtoken_char( 0'T ).
	nmtoken_char( 0'U ).
	nmtoken_char( 0'V ).
	nmtoken_char( 0'W ).
	nmtoken_char( 0'X ).
	nmtoken_char( 0'Y ).
	nmtoken_char( 0'Z ).
	nmtoken_char( 0'0 ).
	nmtoken_char( 0'1 ).
	nmtoken_char( 0'2 ).
	nmtoken_char( 0'3 ).
	nmtoken_char( 0'4 ).
	nmtoken_char( 0'5 ).
	nmtoken_char( 0'6 ).
	nmtoken_char( 0'7 ).
	nmtoken_char( 0'8 ).
	nmtoken_char( 0'9 ).
	nmtoken_char( 0'. ).
	nmtoken_char( 0'- ).
	nmtoken_char( 0'_ ).
	nmtoken_char( 0': ).

	xml_string( String ) -->
		quote( Quote ),
		xml_string1( Quote, String ).

	xml_string1( Quote, [] ) -->
		quote( Quote ),
		!.
	xml_string1( Quote, [Char|Chars] ) -->
		[Char],
		xml_string1( Quote, Chars ).

	alphabet( 0'a ).
	alphabet( 0'b ).
	alphabet( 0'c ).
	alphabet( 0'd ).
	alphabet( 0'e ).
	alphabet( 0'f ).
	alphabet( 0'g ).
	alphabet( 0'h ).
	alphabet( 0'i ).
	alphabet( 0'j ).
	alphabet( 0'k ).
	alphabet( 0'l ).
	alphabet( 0'm ).
	alphabet( 0'n ).
	alphabet( 0'o ).
	alphabet( 0'p ).
	alphabet( 0'q ).
	alphabet( 0'r ).
	alphabet( 0's ).
	alphabet( 0't ).
	alphabet( 0'u ).
	alphabet( 0'v ).
	alphabet( 0'w ).
	alphabet( 0'x ).
	alphabet( 0'y ).
	alphabet( 0'z ).
	alphabet( 0'A ).
	alphabet( 0'B ).
	alphabet( 0'C ).
	alphabet( 0'D ).
	alphabet( 0'E ).
	alphabet( 0'F ).
	alphabet( 0'G ).
	alphabet( 0'H ).
	alphabet( 0'I ).
	alphabet( 0'J ).
	alphabet( 0'K ).
	alphabet( 0'L ).
	alphabet( 0'M ).
	alphabet( 0'N ).
	alphabet( 0'O ).
	alphabet( 0'P ).
	alphabet( 0'Q ).
	alphabet( 0'R ).
	alphabet( 0'S ).
	alphabet( 0'T ).
	alphabet( 0'U ).
	alphabet( 0'V ).
	alphabet( 0'W ).
	alphabet( 0'X ).
	alphabet( 0'Y ).
	alphabet( 0'Z ).

	digit( C ) --> [C], {digit_table( C )}.

	digit_table( 0'0 ).
	digit_table( 0'1 ).
	digit_table( 0'2 ).
	digit_table( 0'3 ).
	digit_table( 0'4 ).
	digit_table( 0'5 ).
	digit_table( 0'6 ).
	digit_table( 0'7 ).
	digit_table( 0'8 ).
	digit_table( 0'9 ).

	digits( [Digit|Digits] ) -->
		digit( Digit ),
		digits( Digits ).
	digits( [] ) --> [].

	character_entity( "quot", 0'" ). %'
	character_entity( "amp", 0'&  ). %'
	character_entity( "lt", 0'< ). %'
	character_entity( "gt", 0'> ). %'
%	character_entity( "apos", 0'' ).
	character_entity( "apos", QuoteCode ) :-
		atom_codes('''', [QuoteCode]).

	/* pp( +XMLDocument ) "pretty prints" XMLDocument on the current
	 * output stream.
	 */
	pp( xml(Attributes, Document) ) :-
		write( 'xml( ' ), pp_attributes( Attributes, 0 ), pp_comma, nl,
		pp_list( Document, s(0) ),
		write( ' ).' ), nl.
	pp( malformed(Attributes, Document) ) :-
		write( 'malformed( ' ), pp_attributes( Attributes, 0 ), pp_comma, nl,
		pp_list( Document, s(0) ),
		write( ' ).' ), nl.

	pp_indented( [], Indent ) :-
		pp_indent( Indent), write( '[]' ).
	pp_indented( List, Indent ) :-
		List = [_|_],
		pp_indent( Indent ),
		pp_list( List, Indent ).
	pp_indented( comment(Text), Indent ) :-
		pp_indent( Indent ), write( 'comment(' ), pp_string(Text), write( ')' ).
	pp_indented( namespace(URI,Prefix,Element), Indent ) :-
		pp_indent( Indent ),
		write( 'namespace( ' ), writeq( URI ), pp_comma_sp,
		pp_string( Prefix ), pp_comma, nl,
		pp_indented( Element, s(Indent) ), nl,
		pp_indent( s(Indent) ), write( ')' ).
	pp_indented( element(Tag,Attributes,Contents), Indent ) :-
		pp_indent( Indent ), write( 'element( ' ), writeq( Tag ), pp_comma, nl,
		pp_attributes( Attributes, s(Indent) ), pp_comma, nl,
		pp_list( Contents, s(Indent) ), write( ' )' ).
	pp_indented( instructions(Target, Processing), Indent ) :-
		pp_indent( Indent ), write( 'instructions( ' ), writeq( Target ), pp_comma_sp,
		pp_string(Processing), write( ')' ).
	pp_indented( doctype(Name, DoctypeId), Indent ) :-
		pp_indent( Indent ), write( 'doctype( ' ), writeq( Name ), pp_comma_sp,
		pp_indented( DoctypeId, s(Indent) ), %'
		write( ' )' ).
	pp_indented( cdata(CData), Indent ) :-
		pp_indent( Indent ), write( 'cdata(' ), pp_string(CData), write( ')' ).
	pp_indented( pcdata(PCData), Indent ) :-
		pp_indent( Indent ), write( 'pcdata(' ), pp_string(PCData), write( ')' ).
	pp_indented( public(URN,URL), _Indent ) :-
		write( 'public(' ), pp_string(URN), pp_comma_sp,
		pp_string(URL), write( ')' ).
	pp_indented( public(URN,URL,Literals), Indent ) :-
		write( 'public(' ), pp_string(URN), pp_comma_sp,
		pp_string(URL), pp_list( Literals, s(Indent) ), write( ')' ).
	pp_indented( system(URL), _Indent ) :-
		write( 'system(' ), pp_string(URL), write( ')' ).
	pp_indented( system(URL,Literals), Indent ) :-
		write( 'system(' ), pp_string(URL), pp_comma_sp,
		pp_list( Literals, s(Indent) ), write( ')' ).
	pp_indented( local, _Indent ) :-
		write( local ).
	pp_indented( local(Literals), Indent ) :-
		write( 'local(' ), nl,
		pp_list( Literals, s(Indent) ), write( ')' ).
	pp_indented( dtd_literal(String), Indent ) :-
		pp_indent( Indent ), write( 'dtd_literal(' ), pp_string(String), write( ')' ).
	pp_indented( out_of_context(Tag), Indent ) :-
		pp_indent( Indent ), write( '/* SYNTAX ERROR */ out_of_context( ' ),
		writeq( Tag ), write( ' )' ).
	pp_indented( unparsed(String), Indent ) :-
		pp_indent( Indent ), write( '/* SYNTAX ERROR */ unparsed( ' ),
		pp_string(String), write( ' )' ).

	pp_list( [], Indent ) :-
		pp_indent( Indent ), write( [] ).
	pp_list( [H|T], Indent ) :-
		pp_indent( Indent ), write( '[' ), nl,
		pp_indented( H, Indent ),
		pp_list1( T, Indent ),
		pp_indent( Indent ), write( ']' ).

	pp_list1( [], _Indent ) :-
		nl.
	pp_list1( [H|T], Indent ) :-
		pp_comma, nl,
		pp_indented( H, Indent ),
		pp_list1( T, Indent ).

	pp_attributes( [], Indent ) :-
		pp_indent( Indent ), write( [] ).
	pp_attributes( [Attribute|Attributes], Indent ) :-
		pp_indent( Indent ), write( '[' ),
		pp_attributes1( Attributes, Attribute ),
		write( ']' ).

	pp_attributes1( [], Name=Value ) :-
		pp_name( Name ), pp_string( Value ).
	pp_attributes1( [H|T], Name=Value ) :-
		pp_name( Name ), pp_string( Value ), pp_comma_sp,
		pp_attributes1( T, H ).


	pp_name( Name ) :-
		(	possible_operator( Name ) ->
			write( '(' ), write( Name ), write( ')=' )
		;	otherwise ->
			writeq( Name ), write( '=' )
		).

	possible_operator( (abolish) ).
	possible_operator( (attribute) ).
	possible_operator( (check_advice) ).
	possible_operator( (compile_command) ).
	possible_operator( (delay) ).
	possible_operator( (demon) ).
	possible_operator( (discontiguous) ).
	possible_operator( (div) ).
	possible_operator( (do) ).
	possible_operator( (document_export) ).
	possible_operator( (document_import) ).
	possible_operator( (dy) ).
	possible_operator( (dynamic) ).
	possible_operator( (edb) ).
	possible_operator( (eexport) ).
	possible_operator( (else) ).
	possible_operator( (except) ).
	possible_operator( (export) ).
	possible_operator( (foreign_pred) ).
	possible_operator( (from) ).
	possible_operator( (from_chars) ).
	possible_operator( (from_file) ).
	possible_operator( (from_stream) ).
	possible_operator( (global) ).
	possible_operator( (help) ).
	possible_operator( (hilog) ).
	possible_operator( (if) ).
	possible_operator( (import) ).
	possible_operator( (index) ).
	possible_operator( (initialization) ).
	possible_operator( (is) ).
	possible_operator( (listing) ).
	possible_operator( (local) ).
	possible_operator( (locked) ).
	possible_operator( (meta_predicate) ).
	possible_operator( (mod) ).
	possible_operator( (mode) ).
	possible_operator( (module_transparent) ).
	possible_operator( (multifile) ).
	possible_operator( (namic) ).
	possible_operator( (nocheck_advice) ).
	possible_operator( (nospy) ).
	possible_operator( (not) ).
	possible_operator( (of) ).
	possible_operator( (once) ).
	possible_operator( (onto_chars) ).
	possible_operator( (onto_file) ).
	possible_operator( (onto_stream) ).
	possible_operator( (parallel) ).
	possible_operator( (public) ).
	possible_operator( (r) ).
	possible_operator( (rem) ).
	possible_operator( (skipped) ).
	possible_operator( (spy) ).
	possible_operator( (table) ).
	possible_operator( (then) ).
	possible_operator( (thread_local) ).
	possible_operator( (ti) ).
	possible_operator( (ti_off) ).
	possible_operator( (traceable) ).
	possible_operator( (unskipped) ).
	possible_operator( (untraceable) ).
	possible_operator( (use_subsumptive_tabling) ).
	possible_operator( (use_variant_tabling) ).
	possible_operator( (volatile) ).
	possible_operator( (with) ).
	possible_operator( (with_input_from_chars) ).
	possible_operator( (with_output_to_chars) ).
	possible_operator( (xor) ).

	pp_indent( 0 ).
	pp_indent( s(N) ) :-
		write( '\t' ),
		pp_indent( N ).

	pp_comma :-
		write( ',' ).

	pp_comma_sp :-
		write( ', ' ).


	% Entity and Namespace map operations: these maps are usually quite small, so
	% a linear list lookup is okay. They could be substituted by a logarithmic
	% data structure - in extremis.


	/* empty_map( ?Map ) is true if Map is a null map.
	 */
	:- private(empty_map/1).
	:- mode(empty_map(?nonvar), zero_or_one).
	:- info(empty_map/1, [
		comment is 'True if Map is a null map.',
		argnames is ['Map']]).

	empty_map( [] ).

	/* map_member( +Key, +Map, ?Data ) is true if Map is a ordered map structure
	 * which records the pair Key-Data. Key must be ground.
	 */
	:- private(map_member/3).
	:- mode(map_member(+nonvar, +nonvar, ?nonvar), zero_or_one).
	:- info(map_member/3, [
		comment is 'True if Map is a ordered map structure which records the pair Key-Data. Key must be ground.',
		argnames is ['Key', 'Map', 'Data']]).

	map_member( Key0, [Key1-Data1|Rest], Data0 ) :-
		(	Key0 == Key1 ->
			Data0 = Data1
		;	Key0 @> Key1 ->
			map_member( Key0, Rest, Data0 )
		).

	/* map_store( +Map0, +Key, +Data, ?Map1 ) is true if Map0 is an ordered map
	 * structure, Key must be ground, and Map1 is identical to Map0 except that
	 * the pair Key-Data is recorded by Map1.
	 */
	:- private(map_store/4).
	:- mode(map_store(+nonvar, +nonvar, +nonvar, ?nonvar), zero_or_one).
	:- info(map_store/4, [
		comment is 'True if Map0 is an ordered map structure, Key must be ground, and Map1 is identical to Map0 except that the pair Key-Data is recorded by Map1.',
		argnames is ['Map0', 'Key', 'Data', 'Map1']]).

	map_store( [], Key, Data, [Key-Data] ).
	map_store( [Key0-Data0|Map0], Key, Data, Map ) :-
		(	Key == Key0 ->
			Map = [Key-Data|Map0]
		;	Key @< Key0 ->
			Map = [Key-Data,Key0-Data0|Map0]
		;	otherwise -> % >
			Map = [Key0-Data0|Map1],
			map_store( Map0, Key, Data, Map1 )
		).

	/* context(?Element, ?PreserveSpace, ?CurrentNS, ?DefaultNS, ?Entities, ?Namespaces )
	 * is an ADT hiding the "state" arguments for XML Acquisition
	 */
	initial_context(
			Controls,
			context(void,PreserveSpace,'','',Entities,Empty,
				RemoveAttributePrefixes,AllowAmpersand)
			) :-
		empty_map( Empty ),
		(	member( extended_characters(false), Controls ) ->
			Entities = Empty
		;	otherwise ->
			extended_character_entities(Entities)
		),
		(	member( format(false), Controls ) ->
			PreserveSpace = true
		;	otherwise ->
			PreserveSpace = false
		),
		(	member( remove_attribute_prefixes(true), Controls ) ->
			RemoveAttributePrefixes = true
		;	otherwise ->
			RemoveAttributePrefixes = false
		),
		(	member( allow_ampersand(true), Controls ) ->
			AllowAmpersand = true
		;	otherwise ->
			AllowAmpersand = false
		).

	context_update( current_namespace, Context0, URI, Context1 ) :-
		Context0 = context(Element,Preserve,_Current,Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp),
		Context1 = context(Element,Preserve,URI,Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp).
	context_update( element, Context0, Tag, Context1 ) :-
		Context0 = context(_Element,Preserve,Current,Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp),
		Context1 = context(tag(Tag),Preserve,Current,Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp).
	context_update( default_namespace, Context0, URI, Context1 ):-
		Context0 = context(Element,Preserve,Current,_Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp),
		Context1 = context(Element,Preserve,Current,URI,Entities,
			Namespaces,RemoveAttributePrefixes,Amp).
	context_update( space_preserve, Context0, Boolean, Context1 ):-
		Context0 = context(Element,_Preserve,Current,Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp),
		Context1 = context(Element,Boolean,Current,Default,Entities,
			Namespaces,RemoveAttributePrefixes,Amp).
	context_update( ns_prefix(Prefix), Context0, URI, Context1 ) :-
		Context0 = context(Element,Preserve,Current,Default,Entities,
			Namespaces0,RemoveAttributePrefixes,Amp),
		Context1 = context(Element,Preserve,Current,Default,Entities,
			Namespaces1,RemoveAttributePrefixes,Amp),
		map_store( Namespaces0, Prefix, URI, Namespaces1 ).
	context_update( entity(Name), Context0, String, Context1 ) :-
		Context0 = context(Element,Preserve,Current,Default,Entities0,
			Namespaces,RemoveAttributePrefixes,Amp),
		Context1 = context(Element,Preserve,Current,Default,Entities1,
			Namespaces,RemoveAttributePrefixes,Amp),
		map_store( Entities0, Name, String, Entities1 ).

	remove_attribute_prefixes( Context ) :-
		Context = context(_Element,_Preserve,_Current,_Default,_Entities,
			_Namespaces,true,_Amp).

	current_tag( Context, Tag ) :-
		Context = context(tag(Tag),_Preserve,_Current,_Default,_Entities,
			_Namespaces,_RPFA,_Amp).

	current_namespace( Context, Current ) :-
		Context = context(_Element,_Preserve,Current,_Default,_Entities,
			_Namespaces,_RPFA,_Amp).

	default_namespace( Context, Default ) :-
		Context = context(_Element,_Preserve,_Current,Default,_Entities,
			_Namespaces,_RPFA,_Amp).

	space_preserve( Context ) :-
		Context = context(tag(_Tag),true,_Current,_Default,_Entities,
			_Namespaces,_RPFA,_Amp).

	specific_namespace( Prefix, Context, URI ) :-
		Context = context(_Element,_Preserve,_Current,_Default,_Entities,
			Namespaces,_RPFA,_Amp),
		map_member( Prefix, Namespaces, URI ).

	defined_entity( Reference, Context, String ) :-
		Context = context(_Element,_Preserve,_Current,_Default,Entities,
			_Namespaces,_RPFA,_Amp),
		map_member( Reference, Entities, String ).
	
	close_context( Context, Terms, WellFormed ) :-
		Context = context(Element,_Preserve,_Current,_Default,_Entities,
			_Namespaces,_RPFA,_Amp),
		close_context1( Element, Terms, WellFormed ).

	close_context1( void, [], true ).
	close_context1( tag(TagChars), [out_of_context(Tag)], false ) :-
		atom_chars( Tag, TagChars ).

	void_context(
		context(void,_Preserve,_Current,_Default,_Entities,_Names,_RPFA,_Amp)
		).

	allow_ampersand(
		context(_Void,_Preserve,_Current,_Default,_Entities,_Names,_RPFA,true)
		).

	/* pp_string( +String ) prints String onto the current output stream.
	 * If String contains only 7-bit chars it is printed in shorthand quoted
	 * format, otherwise it is written as a list.
	 * If your Prolog uses " to delimit a special string type, just use write/1.
	 */
	:- private(pp_string/1).
	:- mode(pp_string(+nonvar), zero_or_one).
	:- info(pp_string/1, [
		comment is 'Prints String onto the current output stream. If String contains only 7-bit chars it is printed in shorthand quoted format, otherwise it is written as a list.',
		argnames is ['String']]).

	pp_string( Chars ) :-
		(	member( Char, Chars ),
			(Char > 255 ; Char < 9) ->
			write( Chars )
		;	otherwise ->
			put_quote,
			pp_string1( Chars ),
			put_quote
		).

	put_quote :-
		put_code( 0'" ). % '

	pp_string1( [] ).
	pp_string1( [Char|Chars] ) :-
		(	Char =:= 0'"  -> % Meta-quote
			put_code( Char ),
			put_code( Char ),
			pp_string1( Chars )
		;	Char =:= 13,	% Handle Windows border-settings
			Chars = [10|Chars1] ->
			put_code( 10 ),
			pp_string1( Chars1 )
		;	otherwise ->
			put_code( Char ),
			pp_string1( Chars )
		).

	xml_declaration_attributes_valid( [] ).
	xml_declaration_attributes_valid( [Name=Value|Attributes] ) :-
		xml_declaration_attribute_valid( Name, Value ),
		xml_declaration_attributes_valid( Attributes ).

	xml_declaration_attribute_valid( Name, Value ) :-
		lowercase( Value, Lowercase ),
		canonical_xml_declaration_attribute( Name, Lowercase ).

	canonical_xml_declaration_attribute( version, "1.0" ).
	canonical_xml_declaration_attribute( standalone, "yes" ).
	canonical_xml_declaration_attribute( standalone, "no" ).
	% The encodings here are all valid for the output produced.
	canonical_xml_declaration_attribute( encoding, "utf-8" ).
	% canonical_xml_declaration_attribute( encoding, "utf-16" ).
	% This is erroneous for the output of this library
	canonical_xml_declaration_attribute( encoding, "us-ascii" ).
	canonical_xml_declaration_attribute( encoding, "ascii" ).
	canonical_xml_declaration_attribute( encoding, "iso-8859-1" ).
	canonical_xml_declaration_attribute( encoding, "iso-8859-2" ).
	canonical_xml_declaration_attribute( encoding, "iso-8859-15" ).
	canonical_xml_declaration_attribute( encoding, "windows-1252" ).
	% In general, it's better not to specify an encoding.

	/* lowercase( +MixedCase, ?Lowercase ) holds when Lowercase and MixedCase are
	 * lists of character codes, and Lowercase is identical to MixedCase with
	 * every uppercase character replaced by its lowercase equivalent.
	 */
	lowercase( [], [] ).
	lowercase( [Char|Chars], [Lower|LowerCase] ) :-
		(	Char >= 0'A, Char =< 0'Z ->
			Lower is Char + 0'a - 0'A
		;	otherwise ->
			Lower = Char
		),
		lowercase( Chars, LowerCase ).

	extended_character_entities( [
		"Aacute"-[193],		% latin capital letter A with acute,
		"aacute"-[225],		% latin small letter a with acute,
		"Acirc"-[194],		% latin capital letter A with circumflex,
		"acirc"-[226],		% latin small letter a with circumflex,
		"acute"-[180],		% acute accent = spacing acute,
		"AElig"-[198],		% latin capital letter AE
		"aelig"-[230],		% latin small letter ae
		"Agrave"-[192],		% latin capital letter A with grave
		"agrave"-[224],		% latin small letter a with grave
		"alefsym"-[8501],	% alef symbol = first transfinite cardinal,
		"Alpha"-[913],		% greek capital letter alpha, U+0391
		"alpha"-[945],		% greek small letter alpha,
		"and"-[8743],		% logical and = wedge, U+2227 ISOtech
		"ang"-[8736],		% angle, U+2220 ISOamso
		"Aring"-[197],		% latin capital letter A with ring above
		"aring"-[229],		% latin small letter a with ring above
		"asymp"-[8776],		% almost equal to = asymptotic to,
		"Atilde"-[195],		% latin capital letter A with tilde,
		"atilde"-[227],		% latin small letter a with tilde,
		"Auml"-[196],		% latin capital letter A with diaeresis,
		"auml"-[228],		% latin small letter a with diaeresis,
		"bdquo"-[8222],		% double low-9 quotation mark, U+201E NEW
		"Beta"-[914],		% greek capital letter beta, U+0392
		"beta"-[946],		% greek small letter beta, U+03B2 ISOgrk3
		"brvbar"-[166],		% broken bar = broken vertical bar,
		"bull"-[8226],		% bullet = black small circle,
		"cap"-[8745],		% intersection = cap, U+2229 ISOtech
		"Ccedil"-[199],		% latin capital letter C with cedilla,
		"ccedil"-[231],		% latin small letter c with cedilla,
		"cedil"-[184],		% cedilla = spacing cedilla, U+00B8 ISOdia>
		"cent"-[162],		% cent sign, U+00A2 ISOnum>
		"Chi"-[935],		% greek capital letter chi, U+03A7
		"chi"-[967],		% greek small letter chi, U+03C7 ISOgrk3
		"circ"-[710],		% modifier letter circumflex accent,
		"clubs"-[9827],		% black club suit = shamrock,
		"cong"-[8773],		% approximately equal to, U+2245 ISOtech
		"copy"-[169],		% copyright sign, U+00A9 ISOnum>
		"crarr"-[8629],		% downwards arrow with corner leftwards
		"cup"-[8746],		% union = cup, U+222A ISOtech
		"curren"-[164],		% currency sign, U+00A4 ISOnum>
		"dagger"-[8224],	% dagger, U+2020 ISOpub
		"Dagger"-[8225],	% double dagger, U+2021 ISOpub
		"darr"-[8595],		% downwards arrow, U+2193 ISOnum
		"dArr"-[8659],		% downwards double arrow, U+21D3 ISOamsa
		"deg"-[176],		% degree sign, U+00B0 ISOnum>
		"Delta"-[916],		% greek capital letter delta,
		"delta"-[948],		% greek small letter delta,
		"diams"-[9830],		% black diamond suit, U+2666 ISOpub
		"divide"-[247],		% division sign, U+00F7 ISOnum>
		"Eacute"-[201],		% latin capital letter E with acute,
		"eacute"-[233],		% latin small letter e with acute,
		"Ecirc"-[202],		% latin capital letter E with circumflex,
		"ecirc"-[234],		% latin small letter e with circumflex,
		"Egrave"-[200],		% latin capital letter E with grave,
		"egrave"-[232],		% latin small letter e with grave,
		"empty"-[8709],		% empty set = null set = diameter,
		"emsp"-[8195],		% em space, U+2003 ISOpub
		"ensp"-[8194],		% en space, U+2002 ISOpub
		"Epsilon"-[917],	% greek capital letter epsilon, U+0395
		"epsilon"-[949],	% greek small letter epsilon,
		"equiv"-[8801],		% identical to, U+2261 ISOtech
		"Eta"-[919],		% greek capital letter eta, U+0397
		"eta"-[951],		% greek small letter eta, U+03B7 ISOgrk3
		"ETH"-[208],		% latin capital letter ETH, U+00D0 ISOlat1>
		"eth"-[240],		% latin small letter eth, U+00F0 ISOlat1>
		"Euml"-[203],		% latin capital letter E with diaeresis,
		"euml"-[235],		% latin small letter e with diaeresis,
		"euro"-[8364],		% euro sign, U+20AC NEW
		"exist"-[8707],		% there exists, U+2203 ISOtech
		"fnof"-[402],		% latin small f with hook = function
		"forall"-[8704],	% for all, U+2200 ISOtech
		"frac12"-[189],		% vulgar fraction one half
		"frac14"-[188],		% vulgar fraction one quarter
		"frac34"-[190],		% vulgar fraction three quarters
		"frasl"-[8260],		% fraction slash, U+2044 NEW
		"Gamma"-[915],		% greek capital letter gamma,
		"gamma"-[947],		% greek small letter gamma,
		"ge"-[8805],		% greater-than or equal to,
		"harr"-[8596],		% left right arrow, U+2194 ISOamsa
		"hArr"-[8660],		% left right double arrow,
		"hearts"-[9829],	% black heart suit = valentine,
		"hellip"-[8230],	% horizontal ellipsis = three dot leader,
		"Iacute"-[205],		% latin capital letter I with acute,
		"iacute"-[237],		% latin small letter i with acute,
		"Icirc"-[206],		% latin capital letter I with circumflex,
		"icirc"-[238],		% latin small letter i with circumflex,
		"iexcl"-[161],		% inverted exclamation mark, U+00A1 ISOnum>
		"Igrave"-[204],		% latin capital letter I with grave,
		"igrave"-[236],		% latin small letter i with grave,
		"image"-[8465],		% blackletter capital I = imaginary part,
		"infin"-[8734],		% infinity, U+221E ISOtech
		"int"-[8747],		% integral, U+222B ISOtech
		"Iota"-[921],		% greek capital letter iota, U+0399
		"iota"-[953],		% greek small letter iota, U+03B9 ISOgrk3
		"iquest"-[191],		% inverted question mark
		"isin"-[8712],		% element of, U+2208 ISOtech
		"Iuml"-[207],		% latin capital letter I with diaeresis,
		"iuml"-[239],		% latin small letter i with diaeresis,
		"Kappa"-[922],		% greek capital letter kappa, U+039A
		"kappa"-[954],		% greek small letter kappa,
		"Lambda"-[923],		% greek capital letter lambda,
		"lambda"-[955],		% greek small letter lambda,
		"lang"-[9001],		% left-pointing angle bracket = bra,
		"laquo"-[171],		% left-pointing double angle quotation mark
		"larr"-[8592],		% leftwards arrow, U+2190 ISOnum
		"lArr"-[8656],		% leftwards double arrow, U+21D0 ISOtech
		"lceil"-[8968],		% left ceiling = apl upstile,
		"ldquo"-[8220],		% left double quotation mark,
		"le"-[8804],		% less-than or equal to, U+2264 ISOtech
		"lfloor"-[8970],	% left floor = apl downstile,
		"lowast"-[8727],	% asterisk operator, U+2217 ISOtech
		"loz"-[9674],		% lozenge, U+25CA ISOpub
		"lrm"-[8206],		% left-to-right mark, U+200E NEW RFC 2070
		"lsaquo"-[8249],	% single left-pointing angle quotation mark,
		"lsquo"-[8216],		% left single quotation mark,
		"macr"-[175],		% macron = spacing macron = overline
		"mdash"-[8212],		% em dash, U+2014 ISOpub
		"micro"-[181],		% micro sign, U+00B5 ISOnum>
		"middot"-[183],		% middle dot = Georgian comma
		"minus"-[8722],		% minus sign, U+2212 ISOtech
		"Mu"-[924],			% greek capital letter mu, U+039C
		"mu"-[956],			% greek small letter mu, U+03BC ISOgrk3
		"nabla"-[8711],		% nabla = backward difference,
		"nbsp"-[160],		% no-break space = non-breaking space,
		"ndash"-[8211],		% en dash, U+2013 ISOpub
		"ne"-[8800],		% not equal to, U+2260 ISOtech
		"ni"-[8715],		% contains as member, U+220B ISOtech
		"not"-[172],		% not sign, U+00AC ISOnum>
		"notin"-[8713],		% not an element of, U+2209 ISOtech
		"nsub"-[8836],		% not a subset of, U+2284 ISOamsn
		"Ntilde"-[209],		% latin capital letter N with tilde,
		"ntilde"-[241],		% latin small letter n with tilde,
		"Nu"-[925],			% greek capital letter nu, U+039D
		"nu"-[957],			% greek small letter nu, U+03BD ISOgrk3
		"Oacute"-[211],		% latin capital letter O with acute,
		"oacute"-[243],		% latin small letter o with acute,
		"Ocirc"-[212],		% latin capital letter O with circumflex,
		"ocirc"-[244],		% latin small letter o with circumflex,
		"OElig"-[338],		% latin capital ligature OE,
		"oelig"-[339],		% latin small ligature oe, U+0153 ISOlat2
		"Ograve"-[210],		% latin capital letter O with grave,
		"ograve"-[242],		% latin small letter o with grave,
		"oline"-[8254],		% overline = spacing overscore,
		"Omega"-[937],		% greek capital letter omega,
		"omega"-[969],		% greek small letter omega,
		"Omicron"-[927],	% greek capital letter omicron, U+039F
		"omicron"-[959],	% greek small letter omicron, U+03BF NEW
		"oplus"-[8853],		% circled plus = direct sum,
		"or"-[8744],		% logical or = vee, U+2228 ISOtech
		"ordf"-[170],		% feminine ordinal indicator, U+00AA ISOnum>
		"ordm"-[186],		% masculine ordinal indicator,
		"Oslash"-[216],		% latin capital letter O with stroke
		"oslash"-[248],		% latin small letter o with stroke,
		"Otilde"-[213],		% latin capital letter O with tilde,
		"otilde"-[245],		% latin small letter o with tilde,
		"otimes"-[8855],	% circled times = vector product,
		"Ouml"-[214],		% latin capital letter O with diaeresis,
		"ouml"-[246],		% latin small letter o with diaeresis,
		"para"-[182],		% pilcrow sign = paragraph sign,
		"part"-[8706],		% partial differential, U+2202 ISOtech
		"permil"-[8240],	% per mille sign, U+2030 ISOtech
		"perp"-[8869],		% up tack = orthogonal to = perpendicular,
		"Phi"-[934],		% greek capital letter phi,
		"phi"-[966],		% greek small letter phi, U+03C6 ISOgrk3
		"Pi"-[928],			% greek capital letter pi, U+03A0 ISOgrk3
		"pi"-[960],			% greek small letter pi, U+03C0 ISOgrk3
		"piv"-[982],		% greek pi symbol, U+03D6 ISOgrk3
		"plusmn"-[177],		% plus-minus sign = plus-or-minus sign,
		"pound"-[163],		% pound sign, U+00A3 ISOnum>
		"prime"-[8242],		% prime = minutes = feet, U+2032 ISOtech
		"Prime"-[8243],		% double prime = seconds = inches,
		"prod"-[8719],		% n-ary product = product sign,
		"prop"-[8733],		% proportional to, U+221D ISOtech
		"Psi"-[936],		% greek capital letter psi,
		"psi"-[968],		% greek small letter psi, U+03C8 ISOgrk3
		"radic"-[8730],		% square root = radical sign,
		"rang"-[9002],		% right-pointing angle bracket = ket,
		"raquo"-[187],		% right-pointing double angle quotation mark
		"rarr"-[8594],		% rightwards arrow, U+2192 ISOnum
		"rArr"-[8658],		% rightwards double arrow,
		"rceil"-[8969],		% right ceiling, U+2309 ISOamsc
		"rdquo"-[8221],		% right double quotation mark,
		"real"-[8476],		% blackletter capital R = real part symbol,
		"reg"-[174],		% registered sign = registered trade mark sign,
		"rfloor"-[8971],	% right floor, U+230B ISOamsc
		"Rho"-[929],		% greek capital letter rho, U+03A1
		"rho"-[961],		% greek small letter rho, U+03C1 ISOgrk3
		"rlm"-[8207],		% right-to-left mark, U+200F NEW RFC 2070
		"rsaquo"-[8250],	% single right-pointing angle quotation mark,
		"rsquo"-[8217],		% right single quotation mark,
		"sbquo"-[8218],		% single low-9 quotation mark, U+201A NEW
		"Scaron"-[352],		% latin capital letter S with caron,
		"scaron"-[353],		% latin small letter s with caron,
		"sdot"-[8901],		% dot operator, U+22C5 ISOamsb
		"sect"-[167],		% section sign, U+00A7 ISOnum>
		"shy"-[173],		% soft hyphen = discretionary hyphen,
		"Sigma"-[931],		% greek capital letter sigma,
		"sigma"-[963],		% greek small letter sigma,
		"sigmaf"-[962],		% greek small letter final sigma,
		"sim"-[8764],		% tilde operator = varies with = similar to,
		"spades"-[9824],	% black spade suit, U+2660 ISOpub
		"sub"-[8834],		% subset of, U+2282 ISOtech
		"sube"-[8838],		% subset of or equal to, U+2286 ISOtech
		"sum"-[8721],		% n-ary sumation, U+2211 ISOamsb
		"sup"-[8835],		% superset of, U+2283 ISOtech
		"sup1"-[185],		% superscript one = superscript digit one,
		"sup2"-[178],		% superscript two = superscript digit two
		"sup3"-[179],		% superscript three = superscript digit three
		"supe"-[8839],		% superset of or equal to,
		"szlig"-[223],		% latin small letter sharp s = ess-zed,
		"Tau"-[932],		% greek capital letter tau, U+03A4
		"tau"-[964],		% greek small letter tau, U+03C4 ISOgrk3
		"there4"-[8756],	% therefore, U+2234 ISOtech
		"Theta"-[920],		% greek capital letter theta,
		"theta"-[952],		% greek small letter theta,
		"thetasym"-[977],	% greek small letter theta symbol,
		"thinsp"-[8201],	% thin space, U+2009 ISOpub
		"THORN"-[222],		% latin capital letter THORN,
		"thorn"-[254],		% latin small letter thorn with,
		"tilde"-[732],		% small tilde, U+02DC ISOdia
		"times"-[215],		% multiplication sign, U+00D7 ISOnum>
		"trade"-[8482],		% trade mark sign, U+2122 ISOnum
		"Uacute"-[218],		% latin capital letter U with acute,
		"uacute"-[250],		% latin small letter u with acute,
		"uarr"-[8593],		% upwards arrow, U+2191 ISOnum
		"uArr"-[8657],		% upwards double arrow, U+21D1 ISOamsa
		"Ucirc"-[219],		% latin capital letter U with circumflex,
		"ucirc"-[251],		% latin small letter u with circumflex,
		"Ugrave"-[217],		% latin capital letter U with grave,
		"ugrave"-[249],		% latin small letter u with grave,
		"uml"-[168],		% diaeresis = spacing diaeresis,
		"upsih"-[978],		% greek upsilon with hook symbol,
		"Upsilon"-[933],	% greek capital letter upsilon,
		"upsilon"-[965],	% greek small letter upsilon,
		"Uuml"-[220],		% latin capital letter U with diaeresis,
		"uuml"-[252],		% latin small letter u with diaeresis,
		"weierp"-[8472],	% script capital P = power set
		"Xi"-[926],			% greek capital letter xi, U+039E ISOgrk3
		"xi"-[958],			% greek small letter xi, U+03BE ISOgrk3
		"Yacute"-[221],		% latin capital letter Y with acute,
		"yacute"-[253],		% latin small letter y with acute,
		"yen"-[165],		% yen sign = yuan sign, U+00A5 ISOnum>
		"yuml"-[255],		% latin small letter y with diaeresis,
		"Yuml"-[376],		% latin capital letter Y with diaeresis,
		"Zeta"-[918],		% greek capital letter zeta, U+0396
		"zeta"-[950],		% greek small letter zeta, U+03B6 ISOgrk3
		"zwj"-[8205],		% zero width joiner, U+200D NEW RFC 2070
		"zwnj"-[8204]		% zero width non-joiner,
		] ).


	/* chars( ?Chars, ?Plus, ?Minus ) used as chars( ?Chars ) in a DCG to
	 * copy the list Chars inline.
	 *
	 * This is best expressed in terms of append/3 where append/3 is built-in.
	 * For other Prologs, a straightforward specification can be used:
	 *
	 *	chars( [] ) --> "".
	 *	chars( [Char|Chars] ) -->
	 *		[Char],
	 *		chars( Chars ).
	 */

	chars(Chars, Plus, Minus) :-
		append(Chars, Minus, Plus).


	/* fault( +Term, +Indentation, ?SubTerm, ?Path, ?Message ) identifies SubTerm
	 * as a sub-term of Term which cannot be serialized after Indentation.
	 * Message is an atom naming the type of error; Path is a string encoding a
	 * list of SubTerm's ancestor elements in the form <tag>{(id)}* where <tag> is the
	 * element tag and <id> is the value of any attribute _named_ id.
	 */
	:- private(fault/5).
	:- mode(fault(+nonvar, +nonvar, ?nonvar, ?nonvar, ?nonvar), zero_or_one).
	:- info(fault/5, [
		comment is 'Identifies SubTerm as a sub-term of Term which cannot be serialized after Indentation. Message is an atom naming the type of error; Path is a string encoding a list of SubTerm''s ancestor elements in the form <tag>{(id)}* where <tag> is the element tag and <id> is the value of any attribute _named_ id.',
		argnames is ['Term', 'Indentation', 'SubTerm', 'Path', 'Message']]).

	fault( Term, _Indent, Term, [], "Illegal Variable" ) :-
		var( Term ).
	fault( xml(Attributes,_Content), _Indent, Term, [], Message ) :-
		member( Attribute, Attributes ),
		attribute_fault( Attribute, Term, Message ).
	fault( xml(_Attributes,Content), Indent, Culprit, Path, Message ) :-
		content_fault( Content, Indent, Culprit, Path, Message ).
	fault( Term, _Indent, Term, [], "Illegal Term" ).

	content_fault( Term, _Indent, Term, [], "Illegal Variable" ) :-
		var( Term ).
	content_fault( pcdata(Chars), _Indent, Chars, [], "Invalid Character Data" ) :-
		\+ is_chars( Chars ).
	content_fault( cdata(Chars), _Indent, Chars, [], "Invalid Character Data" ) :-
		\+ is_chars( Chars ).
	content_fault( [H|_T], Indent, Culprit, Path, Message ) :-
		content_fault( H, Indent, Culprit, Path, Message ).
	content_fault( [_H|T], Indent, Culprit, Path, Message ) :-
		content_fault( T, Indent, Culprit, Path, Message ).
	content_fault( namespace(_URI,_Prefix,Element), Indent, Culprit, Path, Message ) :-
		element_fault( Element, [0' |Indent], Culprit, Path, Message ).
	content_fault( Element, Indent, Culprit, Path, Message ) :-
		element_fault( Element, [0' |Indent], Culprit, Path, Message ).
	content_fault( Term, Indent, Term, [], "Illegal Term" ) :-
		\+ generation(Term, "", false, Indent, _Format, _Plus, _Minus ).

	element_fault( element(Tag, _Attributes, _Contents), _Indent, Tag, [], "Tag must be an atom" ) :-
		\+ atom( Tag ).
	element_fault( element(Tag, Attributes, _Contents), _Indent, Tag, [], "Attributes must be instantiated" ) :-
		var( Attributes ).
	element_fault( element(Tag, Attributes, _Contents), _Indent, Faulty, Path, Message ) :-
		fault_path( Tag, Attributes, Path, [] ),
		member( Attribute, Attributes ),
		attribute_fault( Attribute, Faulty, Message ).
	element_fault( element(Tag, Attributes, Contents), Indent, Culprit, Path, Message ) :-
		fault_path( Tag, Attributes, Path, Path1 ),
		content_fault( Contents, Indent, Culprit, Path1, Message ).

	attribute_fault( Attribute, Attribute, "Illegal Variable" ) :-
		var( Attribute ).
	attribute_fault( Name=Value, Name=Value, "Attribute Name must be atom" ) :-
		\+ atom(Name).
	attribute_fault( Name=Value, Name=Value, "Attribute Value must be chars" ) :-
		\+ is_chars( Value ).
	attribute_fault( Attribute, Attribute, "Malformed Attribute" ) :-
		\+ Attribute = (_Name=_Value).

	is_chars( Chars ) :-
		is_list( Chars ),
		\+ (member( Char, Chars ), \+ (integer(Char), Char >=0, Char =< 255)).

	fault_path( Tag, Attributes ) -->
		{atom_codes( Tag, Chars )},
		chars( Chars ),
		fault_id( Attributes ),
		" ".

	fault_id( Attributes ) -->
		{member( id=Chars, Attributes ), is_chars( Chars )},
		!,
		"(", chars(Chars), ")".
	fault_id( _Attributes ) --> "".


	/* document_generation( +Format, +Document ) is a DCG generating Document
	 * as a list of character codes. Format is true|false defining whether layouts,
	 * to provide indentation, should be added between the element content of
	 * the resultant "string". Note that formatting is disabled for elements that
	 * are interspersed with pcdata/1 terms, such as XHTML's 'inline' elements.
	 * Also, Format is over-ridden, for an individual element, by an explicit
	 * 'xml:space'="preserve" attribute.
	 */
	:- private(document_generation//2).
	:- mode(document_generation(+nonvar, +nonvar), zero_or_one).
	:- info(document_generation//2, [
		comment is 'DCG generating Document as a list of character codes. Format is true|false defining whether layouts, to provide indentation, should be added between the element content of the resultant "string". Note that formatting is disabled for elements that are interspersed with pcdata/1 terms, such as XHTML''s ''inline'' elements. Also, Format is over-ridden, for an individual element, by an explicit ''xml:space''="preserve" attribute.',
		argnames is ['Format', 'Document']]).

	document_generation( Format, xml(Attributes, Document) ) -->
		document_generation_body( Attributes, Format, Document ).

	document_generation_body( [], Format, Document ) -->
		generation( Document, "", Format, [], _Format1 ).
	document_generation_body( Attributes, Format, Document ) -->
		{	Attributes = [_|_],
			xml_declaration_attributes_valid( Attributes )
		},
		"<?xml",
		generated_attributes( Attributes, Format, Format0 ),
		"?>",
		indent( true, [] ),
		generation( Document, "", Format0, [], _Format1 ).

	generation( [], _Prefix, Format, _Indent, Format ) --> [].
	generation( [Term|Terms], Prefix, Format0, Indent, Format ) -->
		generation( Term, Prefix, Format0, Indent, Format1 ),
		generation( Terms, Prefix, Format1, Indent, Format ).
	generation( doctype(Name, External), _Prefix, Format, [], Format ) -->
		"<!DOCTYPE ",
		generated_name( Name ),
		generated_external_id( External ),
		">".
	generation( instructions(Target,Process), _Prefix, Format, Indent, Format ) -->
		indent( Format, Indent ),
		"<?", generated_name(Target), " ", chars( Process ) ,"?>".
	generation( pcdata(Chars), _Prefix, Format0, _Indent, Format1 ) -->
		pcdata_generation( Chars ),
		{pcdata_format( Chars, Format0, Format1 )}.
	generation( comment( Comment ), _Prefix, Format, Indent, Format ) -->
		indent( Format, Indent ),
		"<!--", chars( Comment ), "-->".
	generation( namespace(URI, Prefix, element(Name, Atts, Content)),
			_Prefix0, Format, Indent, Format ) -->
		indent( Format, Indent ),
		"<", generated_prefixed_name( Prefix, Name ),
		generated_prefixed_attributes( Prefix, URI, Atts, Format, Format1 ), 
		generated_content( Content, Format1, Indent, Prefix, Name ).
	generation( element(Name, Atts, Content), Prefix, Format, Indent, Format ) -->
		indent( Format, Indent ),
		"<", generated_prefixed_name( Prefix, Name ),
		generated_attributes( Atts, Format, Format1 ), 
		generated_content( Content, Format1, Indent, Prefix, Name ).
	generation( cdata(CData), _Prefix, Format, Indent, Format ) -->
		indent( Format, Indent ),
		"<![CDATA[", cdata_generation(CData), "]]>".

	generated_attributes( [], Format, Format  ) --> [].
	generated_attributes( [Name=Value|Attributes], Format0, Format  ) -->
		{(	Name == 'xml:space',
			Value="preserve" ->
				Format1 = false
		  ; otherwise ->
				Format1 = Format0
		  )},
		" ",
		generated_name( Name ),
		"=""",
		quoted_string( Value ),
		"""",
		generated_attributes( Attributes, Format1, Format  ).

	generated_prefixed_name( [], Name ) -->
		generated_name( Name ).
	generated_prefixed_name( Prefix, Name ) -->
		{Prefix = [_|_]},
		chars( Prefix ), ":",
		generated_name( Name ).

	generated_content( [], _Format, _Indent, _Prefix, _Namespace ) -->
		" />". % Leave an extra space for XHTML output.
	generated_content( [H|T], Format, Indent, Prefix, Namespace ) -->
		">",
		generation( H, Prefix, Format, [0' |Indent], Format1 ),
		generation( T, Prefix, Format1, [0' |Indent], Format2 ),
		indent( Format2, Indent ),
		"</", generated_prefixed_name( Prefix, Namespace ), ">".

	generated_prefixed_attributes( [_|_Prefix], _URI, Atts, Format0, Format ) -->
		generated_attributes( Atts, Format0, Format  ).
	generated_prefixed_attributes( [], URI, Atts, Format0, Format  ) -->
		{	atom_codes( URI, Namespace ),
			findall( Attr, (member(Attr, Atts), \+ Attr=(xmlns=_Val)), Atts1 )
		},
		generated_attributes( [xmlns=Namespace|Atts1], Format0, Format  ).

	generated_name( Name, Plus, Minus ) :-
		atom_codes( Name, Chars ),
		append( Chars, Minus, Plus ).

	generated_external_id( local ) --> "".
	generated_external_id( local(Literals) ) --> " [",
		generated_doctype_literals( Literals ), "\n\t]".
	generated_external_id( system(URL) ) -->
		" SYSTEM """,
		chars( URL ),
		"""".
	generated_external_id( system(URL,Literals) ) -->
		" SYSTEM """,
		chars( URL ),
		""" [",
		generated_doctype_literals( Literals ), "\n\t]".
	generated_external_id( public(URN,URL) ) -->
		" PUBLIC """,
		chars( URN ),
		""" """,
		chars( URL ),
		"""".
	generated_external_id( public(URN,URL,Literals) ) -->
		" PUBLIC """,
		chars( URN ),
		""" """,
		chars( URL ),
		""" [",
		generated_doctype_literals( Literals ), "\n\t]".

	generated_doctype_literals( [] ) --> "".
	generated_doctype_literals( [dtd_literal(String)|Literals] ) -->
		"\n\t<!", cdata_generation( String ), ">",
		generated_doctype_literals( Literals ).

	/* quoted_string( +Chars ) is a DCG representing Chars, a list of character
	 * codes, as a legal XML attribute string. Any leading or trailing layout
	 * characters are removed. &, " and < characters are replaced by &amp;, &quot;
	 * and &lt; respectively, .
	 */
	quoted_string( Raw, Plus, Minus ) :-
		quoted_string1( Raw, NoLeadingLayouts ),
		quoted_string2( NoLeadingLayouts, Layout, Layout, Plus, Minus ).

	quoted_string1( [], [] ).
	quoted_string1( [Char|Chars], NoLeadingLayouts ) :-
		(	Char > 32 ->
			NoLeadingLayouts = [Char|Chars]
		;	otherwise ->
			quoted_string1( Chars, NoLeadingLayouts )
		).

	quoted_string2( [], _LayoutPlus, _LayoutMinus, List, List ).
	quoted_string2( [Char|Chars], LayoutPlus, LayoutMinus, Plus, Minus ) :-
		(	Char =< 0'  ->
			Plus = Plus1,
			LayoutMinus = [Char|LayoutMinus1],
			LayoutPlus = LayoutPlus1
		;	Char =< 127 ->
			Plus = LayoutPlus,
			pcdata_7bit( Char, LayoutMinus, Plus1 ),
			LayoutPlus1 = LayoutMinus1
		;	legal_xml_unicode( Char ) ->
			Plus = LayoutPlus,
			number_codes( Char, Codes ),
			pcdata_8bits_plus( Codes, LayoutMinus, Plus1 ),
			LayoutPlus1 = LayoutMinus1
		;	otherwise ->
			LayoutPlus = LayoutPlus1,
			LayoutMinus = LayoutMinus1,
			Plus = Plus1
		),
		quoted_string2( Chars, LayoutPlus1, LayoutMinus1, Plus1, Minus ).

	indent( false, _Indent ) --> [].
	indent( true, Indent ) -->
		"\n",	chars( Indent ).

	/* pcdata_generation( +Chars ) is a DCG representing Chars, a list of character
	 * codes as legal XML "Parsed character data" (PCDATA) string. Any codes
	 * which cannot be represented by a 7-bit character are replaced by their
	 * decimal numeric character entity e.g. code 160 (non-breaking space) is
	 * represented as &#160;. Any character codes disallowed by the XML
	 * specification are not encoded.
	 */
	pcdata_generation( [], Plus, Plus ).
	pcdata_generation( [Char|Chars], Plus, Minus ) :-
		(	Char =< 127 ->
			pcdata_7bit( Char, Plus, Mid )
		;	legal_xml_unicode( Char ) ->
			number_codes( Char, Codes ),
			pcdata_8bits_plus( Codes, Plus, Mid )
		;	otherwise ->
			Plus = Mid
		),
		pcdata_generation( Chars, Mid, Minus ).

	/* pcdata_7bit(+Char) represents the ascii character set in its
	 * simplest format, using the character entities &amp; &quot; &lt; and &gt;
	 * which are common to both XML and HTML. The numeric entity &#39; is used in
	 * place of &apos;, because browsers don't recognize it in HTML.
	 */
	:- private(pcdata_7bit//1).
	:- mode(pcdata_7bit(?nonvar), zero_or_one).
	:- info(pcdata_7bit//1, [
		comment is 'Represents the ascii character set in its simplest format, using the character entities &amp; &quot; &lt; and &gt; which are common to both XML and HTML. The numeric entity &#39; is used in place of &apos;, because browsers don''t recognize it in HTML.',
		argnames is ['Char']]).

	pcdata_7bit( 0 ) --> "".
	pcdata_7bit( 1 ) --> "".
	pcdata_7bit( 2 ) --> "".
	pcdata_7bit( 3 ) --> "".
	pcdata_7bit( 4 ) --> "".
	pcdata_7bit( 5 ) --> "".
	pcdata_7bit( 6 ) --> "".
	pcdata_7bit( 7 ) --> "".
	pcdata_7bit( 8 ) --> "".
	pcdata_7bit( 9 ) --> [9].
	pcdata_7bit( 10 ) --> [10].
	pcdata_7bit( 11 ) --> "".
	pcdata_7bit( 12 ) --> "".
	pcdata_7bit( 13 ) --> [13].
	pcdata_7bit( 14 ) --> "".
	pcdata_7bit( 15 ) --> "".
	pcdata_7bit( 16 ) --> "".
	pcdata_7bit( 17 ) --> "".
	pcdata_7bit( 18 ) --> "".
	pcdata_7bit( 19 ) --> "".
	pcdata_7bit( 20 ) --> "".
	pcdata_7bit( 21 ) --> "".
	pcdata_7bit( 22 ) --> "".
	pcdata_7bit( 23 ) --> "".
	pcdata_7bit( 24 ) --> "".
	pcdata_7bit( 25 ) --> "".
	pcdata_7bit( 26 ) --> "".
	pcdata_7bit( 27 ) --> "".
	pcdata_7bit( 28 ) --> "".
	pcdata_7bit( 29 ) --> "".
	pcdata_7bit( 30 ) --> "".
	pcdata_7bit( 31 ) --> "".
	pcdata_7bit( 32 ) --> " ".
	pcdata_7bit( 33 ) --> "!".
	pcdata_7bit( 34 ) --> "&quot;".
	pcdata_7bit( 35 ) --> "#".
	pcdata_7bit( 36 ) --> "$".
	pcdata_7bit( 37 ) --> "%".
	pcdata_7bit( 38 ) --> "&amp;".
	pcdata_7bit( 39 ) --> "&#39;".
	pcdata_7bit( 40 ) --> "(".
	pcdata_7bit( 41 ) --> ")".
	pcdata_7bit( 42 ) --> "*".
	pcdata_7bit( 43 ) --> "+".
	pcdata_7bit( 44 ) --> ",".
	pcdata_7bit( 45 ) --> "-".
	pcdata_7bit( 46 ) --> ".".
	pcdata_7bit( 47 ) --> "/".
	pcdata_7bit( 48 ) --> "0".
	pcdata_7bit( 49 ) --> "1".
	pcdata_7bit( 50 ) --> "2".
	pcdata_7bit( 51 ) --> "3".
	pcdata_7bit( 52 ) --> "4".
	pcdata_7bit( 53 ) --> "5".
	pcdata_7bit( 54 ) --> "6".
	pcdata_7bit( 55 ) --> "7".
	pcdata_7bit( 56 ) --> "8".
	pcdata_7bit( 57 ) --> "9".
	pcdata_7bit( 58 ) --> ":".
	pcdata_7bit( 59 ) --> ";".
	pcdata_7bit( 60 ) --> "&lt;".
	pcdata_7bit( 61 ) --> "=".
	pcdata_7bit( 62 ) --> "&gt;".
	pcdata_7bit( 63 ) --> "?".
	pcdata_7bit( 64 ) --> "@".
	pcdata_7bit( 65 ) --> "A".
	pcdata_7bit( 66 ) --> "B".
	pcdata_7bit( 67 ) --> "C".
	pcdata_7bit( 68 ) --> "D".
	pcdata_7bit( 69 ) --> "E".
	pcdata_7bit( 70 ) --> "F".
	pcdata_7bit( 71 ) --> "G".
	pcdata_7bit( 72 ) --> "H".
	pcdata_7bit( 73 ) --> "I".
	pcdata_7bit( 74 ) --> "J".
	pcdata_7bit( 75 ) --> "K".
	pcdata_7bit( 76 ) --> "L".
	pcdata_7bit( 77 ) --> "M".
	pcdata_7bit( 78 ) --> "N".
	pcdata_7bit( 79 ) --> "O".
	pcdata_7bit( 80 ) --> "P".
	pcdata_7bit( 81 ) --> "Q".
	pcdata_7bit( 82 ) --> "R".
	pcdata_7bit( 83 ) --> "S".
	pcdata_7bit( 84 ) --> "T".
	pcdata_7bit( 85 ) --> "U".
	pcdata_7bit( 86 ) --> "V".
	pcdata_7bit( 87 ) --> "W".
	pcdata_7bit( 88 ) --> "X".
	pcdata_7bit( 89 ) --> "Y".
	pcdata_7bit( 90 ) --> "Z".
	pcdata_7bit( 91 ) --> "[".
	pcdata_7bit( 92 ) --> [92].
	pcdata_7bit( 93 ) --> "]".
	pcdata_7bit( 94 ) --> "^".
	pcdata_7bit( 95 ) --> "_".
	pcdata_7bit( 96 ) --> "&#96;".
	pcdata_7bit( 97 ) --> "a".
	pcdata_7bit( 98 ) --> "b".
	pcdata_7bit( 99 ) --> "c".
	pcdata_7bit( 100 ) --> "d".
	pcdata_7bit( 101 ) --> "e".
	pcdata_7bit( 102 ) --> "f".
	pcdata_7bit( 103 ) --> "g".
	pcdata_7bit( 104 ) --> "h".
	pcdata_7bit( 105 ) --> "i".
	pcdata_7bit( 106 ) --> "j".
	pcdata_7bit( 107 ) --> "k".
	pcdata_7bit( 108 ) --> "l".
	pcdata_7bit( 109 ) --> "m".
	pcdata_7bit( 110 ) --> "n".
	pcdata_7bit( 111 ) --> "o".
	pcdata_7bit( 112 ) --> "p".
	pcdata_7bit( 113 ) --> "q".
	pcdata_7bit( 114 ) --> "r".
	pcdata_7bit( 115 ) --> "s".
	pcdata_7bit( 116 ) --> "t".
	pcdata_7bit( 117 ) --> "u".
	pcdata_7bit( 118 ) --> "v".
	pcdata_7bit( 119 ) --> "w".
	pcdata_7bit( 120 ) --> "x".
	pcdata_7bit( 121 ) --> "y".
	pcdata_7bit( 122 ) --> "z".
	pcdata_7bit( 123 ) --> "{".
	pcdata_7bit( 124 ) --> "|".
	pcdata_7bit( 125 ) --> "}".
	pcdata_7bit( 126 ) --> "~".
	pcdata_7bit( 127 ) --> "&#127;".

	pcdata_8bits_plus( Codes ) -->
		"&#", chars( Codes ), ";".

	/* pcdata_format( +Chars, +Format0, ?Format1 ) holds when Format0 and Format1
	 * are the statuses of XML formatting before and after Chars - which may be
	 * null.
	 */
	:- private(pcdata_format/3).
	:- mode(pcdata_format(+nonvar, +nonvar, ?nonvar), zero_or_one).
	:- info(pcdata_format/3, [
		comment is 'Holds when Format0 and Format1 are the statuses of XML formatting before and after Chars - which may be null.',
		argnames is ['Chars', 'Format0', 'Format1']]).

	pcdata_format( [], Format, Format ).
	pcdata_format( [_Char|_Chars], _Format, false ).

	/* cdata_generation( +Chars ) is a DCG representing Chars, a list of character
	 * codes as a legal XML CDATA string. Any character codes disallowed by the XML
	 * specification are not encoded.
	 */
	:- private(cdata_generation//1).
	:- mode(cdata_generation(+list), zero_or_one).
	:- info(cdata_generation//1, [
		comment is 'Holds when Format0 and Format1 are the statuses of XML formatting before and after Chars - which may be null.',
		argnames is ['Chars']]).

	cdata_generation( [] ) --> "".
	cdata_generation( [Char|Chars] ) -->
		(	{legal_xml_unicode( Char )}, !, [Char]
		;	""
		),
		cdata_generation( Chars ).

	legal_xml_unicode( 9 ).
	legal_xml_unicode( 10 ).
	legal_xml_unicode( 13 ).
	legal_xml_unicode( Code ) :-
		Code >= 32,
		Code =< 55295.
	legal_xml_unicode( Code ) :-
		Code >= 57344,
		Code =< 65533.
	legal_xml_unicode( Code ) :-
		Code >= 65536,
		Code =< 1114111.

	otherwise.

	/* For reference, this is a comprehensive recognizer for namechar, based on
	 * the definition of in http://www.w3.org/TR/2000/REC-xml-20001006 .

	namechar -->
		( letter
		; unicode_digit
		;  "."
		;  "-"
		;  "_"
		;  ":"
		;  combiningchar
		;  extender
		).

	letter  --> (basechar ; ideographic).

	basechar  --> 
		( range( 16'0041, 16'005A )
		; range( 16'0061, 16'007A )
		; range( 16'00C0, 16'00D6 )
		; range( 16'00D8, 16'00F6 )
		; range( 16'00F8, 16'00FF )
		; range( 16'0100, 16'0131 )
		; range( 16'0134, 16'013E )
		; range( 16'0141, 16'0148 )
		; range( 16'014A, 16'017E )
		; range( 16'0180, 16'01C3 )
		; range( 16'01CD, 16'01F0 )
		; range( 16'01F4, 16'01F5 )
		; range( 16'01FA, 16'0217 )
		; range( 16'0250, 16'02A8 )
		; range( 16'02BB, 16'02C1 )
		; [16'0386]
		; range( 16'0388, 16'038A )
		; [16'038C]
		; range( 16'038E, 16'03A1 )
		; range( 16'03A3, 16'03CE )
		; range( 16'03D0, 16'03D6 )
		; [16'03DA]
		; [16'03DC]
		; [16'03DE]
		; [16'03E0]
		; range( 16'03E2, 16'03F3 )
		; range( 16'0401, 16'040C )
		; range( 16'040E, 16'044F )
		; range( 16'0451, 16'045C )
		; range( 16'045E, 16'0481 )
		; range( 16'0490, 16'04C4 )
		; range( 16'04C7, 16'04C8 )
		; range( 16'04CB, 16'04CC )
		; range( 16'04D0, 16'04EB )
		; range( 16'04EE, 16'04F5 )
		; range( 16'04F8, 16'04F9 )
		; range( 16'0531, 16'0556 )
		; [16'0559]
		; range( 16'0561, 16'0586 )
		; range( 16'05D0, 16'05EA )
		; range( 16'05F0, 16'05F2 )
		; range( 16'0621, 16'063A )
		; range( 16'0641, 16'064A )
		; range( 16'0671, 16'06B7 )
		; range( 16'06BA, 16'06BE )
		; range( 16'06C0, 16'06CE )
		; range( 16'06D0, 16'06D3 )
		; [16'06D5]
		; range( 16'06E5, 16'06E6 )
		; range( 16'0905, 16'0939 )
		; [16'093D]
		; range( 16'0958, 16'0961 )
		; range( 16'0985, 16'098C )
		; range( 16'098F, 16'0990 )
		; range( 16'0993, 16'09A8 )
		; range( 16'09AA, 16'09B0 )
		; [16'09B2]
		; range( 16'09B6, 16'09B9 )
		; range( 16'09DC, 16'09DD )
		; range( 16'09DF, 16'09E1 )
		; range( 16'09F0, 16'09F1 )
		; range( 16'0A05, 16'0A0A )
		; range( 16'0A0F, 16'0A10 )
		; range( 16'0A13, 16'0A28 )
		; range( 16'0A2A, 16'0A30 )
		; range( 16'0A32, 16'0A33 )
		; range( 16'0A35, 16'0A36 )
		; range( 16'0A38, 16'0A39 )
		; range( 16'0A59, 16'0A5C )
		; [16'0A5E]
		; range( 16'0A72, 16'0A74 )
		; range( 16'0A85, 16'0A8B )
		; [16'0A8D]
		; range( 16'0A8F, 16'0A91 )
		; range( 16'0A93, 16'0AA8 )
		; range( 16'0AAA, 16'0AB0 )
		; range( 16'0AB2, 16'0AB3 )
		; range( 16'0AB5, 16'0AB9 )
		; [16'0ABD]
		; [16'0AE0]
		; range( 16'0B05, 16'0B0C )
		; range( 16'0B0F, 16'0B10 )
		; range( 16'0B13, 16'0B28 )
		; range( 16'0B2A, 16'0B30 )
		; range( 16'0B32, 16'0B33 )
		; range( 16'0B36, 16'0B39 )
		; [16'0B3D]
		; range( 16'0B5C, 16'0B5D )
		; range( 16'0B5F, 16'0B61 )
		; range( 16'0B85, 16'0B8A )
		; range( 16'0B8E, 16'0B90 )
		; range( 16'0B92, 16'0B95 )
		; range( 16'0B99, 16'0B9A )
		; [16'0B9C]
		; range( 16'0B9E, 16'0B9F )
		; range( 16'0BA3, 16'0BA4 )
		; range( 16'0BA8, 16'0BAA )
		; range( 16'0BAE, 16'0BB5 )
		; range( 16'0BB7, 16'0BB9 )
		; range( 16'0C05, 16'0C0C )
		; range( 16'0C0E, 16'0C10 )
		; range( 16'0C12, 16'0C28 )
		; range( 16'0C2A, 16'0C33 )
		; range( 16'0C35, 16'0C39 )
		; range( 16'0C60, 16'0C61 )
		; range( 16'0C85, 16'0C8C )
		; range( 16'0C8E, 16'0C90 )
		; range( 16'0C92, 16'0CA8 )
		; range( 16'0CAA, 16'0CB3 )
		; range( 16'0CB5, 16'0CB9 )
		; [16'0CDE]
		; range( 16'0CE0, 16'0CE1 )
		; range( 16'0D05, 16'0D0C )
		; range( 16'0D0E, 16'0D10 )
		; range( 16'0D12, 16'0D28 )
		; range( 16'0D2A, 16'0D39 )
		; range( 16'0D60, 16'0D61 )
		; range( 16'0E01, 16'0E2E )
		; [16'0E30]
		; range( 16'0E32, 16'0E33 )
		; range( 16'0E40, 16'0E45 )
		; range( 16'0E81, 16'0E82 )
		; [16'0E84]
		; range( 16'0E87, 16'0E88 )
		; [16'0E8A]
		; [16'0E8D]
		; range( 16'0E94, 16'0E97 )
		; range( 16'0E99, 16'0E9F )
		; range( 16'0EA1, 16'0EA3 )
		; [16'0EA5]
		; [16'0EA7]
		; range( 16'0EAA, 16'0EAB )
		; range( 16'0EAD, 16'0EAE )
		; [16'0EB0]
		; range( 16'0EB2, 16'0EB3 )
		; [16'0EBD]
		; range( 16'0EC0, 16'0EC4 )
		; range( 16'0F40, 16'0F47 )
		; range( 16'0F49, 16'0F69 )
		; range( 16'10A0, 16'10C5 )
		; range( 16'10D0, 16'10F6 )
		; [16'1100]
		; range( 16'1102, 16'1103 )
		; range( 16'1105, 16'1107 )
		; [16'1109]
		; range( 16'110B, 16'110C )
		; range( 16'110E, 16'1112 )
		; [16'113C]
		; [16'113E]
		; [16'1140]
		; [16'114C]
		; [16'114E]
		; [16'1150]
		; range( 16'1154, 16'1155 )
		; [16'1159]
		; range( 16'115F, 16'1161 )
		; [16'1163]
		; [16'1165]
		; [16'1167]
		; [16'1169]
		; range( 16'116D, 16'116E )
		; range( 16'1172, 16'1173 )
		; [16'1175]
		; [16'119E]
		; [16'11A8]
		; [16'11AB]
		; range( 16'11AE, 16'11AF )
		; range( 16'11B7, 16'11B8 )
		; [16'11BA]
		; range( 16'11BC, 16'11C2 )
		; [16'11EB]
		; [16'11F0]
		; [16'11F9]
		; range( 16'1E00, 16'1E9B )
		; range( 16'1EA0, 16'1EF9 )
		; range( 16'1F00, 16'1F15 )
		; range( 16'1F18, 16'1F1D )
		; range( 16'1F20, 16'1F45 )
		; range( 16'1F48, 16'1F4D )
		; range( 16'1F50, 16'1F57 )
		; [16'1F59]
		; [16'1F5B]
		; [16'1F5D]
		; range( 16'1F5F, 16'1F7D )
		; range( 16'1F80, 16'1FB4 )
		; range( 16'1FB6, 16'1FBC )
		; [16'1FBE]
		; range( 16'1FC2, 16'1FC4 )
		; range( 16'1FC6, 16'1FCC )
		; range( 16'1FD0, 16'1FD3 )
		; range( 16'1FD6, 16'1FDB )
		; range( 16'1FE0, 16'1FEC )
		; range( 16'1FF2, 16'1FF4 )
		; range( 16'1FF6, 16'1FFC )
		; [16'2126]
		; range( 16'212A, 16'212B )
		; [16'212E]
		; range( 16'2180, 16'2182 )
		; range( 16'3041, 16'3094 )
		; range( 16'30A1, 16'30FA )
		; range( 16'3105, 16'312C )
		; range( 16'AC00, 16'D7A3 )
		).
	ideographic  -->
		( range( 16'4E00, 16'9FA5 )
		; [16'3007]
		; range( 16'3021, 16'3029 )
		).
	combiningchar  -->
		( range( 16'0300, 16'0345 )
		; range( 16'0360, 16'0361 )
		; range( 16'0483, 16'0486 )
		; range( 16'0591, 16'05A1 )
		; range( 16'05A3, 16'05B9 )
		; range( 16'05BB, 16'05BD )
		; [16'05BF]
		; range( 16'05C1, 16'05C2 )
		; [16'05C4]
		; range( 16'064B, 16'0652 )
		; [16'0670]
		; range( 16'06D6, 16'06DC )
		; range( 16'06DD, 16'06DF )
		; range( 16'06E0, 16'06E4 )
		; range( 16'06E7, 16'06E8 )
		; range( 16'06EA, 16'06ED )
		; range( 16'0901, 16'0903 )
		; [16'093C]
		; range( 16'093E, 16'094C )
		; [16'094D]
		; range( 16'0951, 16'0954 )
		; range( 16'0962, 16'0963 )
		; range( 16'0981, 16'0983 )
		; [16'09BC]
		; [16'09BE]
		; [16'09BF]
		; range( 16'09C0, 16'09C4 )
		; range( 16'09C7, 16'09C8 )
		; range( 16'09CB, 16'09CD )
		; [16'09D7]
		; range( 16'09E2, 16'09E3 )
		; [16'0A02]
		; [16'0A3C]
		; [16'0A3E]
		; [16'0A3F]
		; range( 16'0A40, 16'0A42 )
		; range( 16'0A47, 16'0A48 )
		; range( 16'0A4B, 16'0A4D )
		; range( 16'0A70, 16'0A71 )
		; range( 16'0A81, 16'0A83 )
		; [16'0ABC]
		; range( 16'0ABE, 16'0AC5 )
		; range( 16'0AC7, 16'0AC9 )
		; range( 16'0ACB, 16'0ACD )
		; range( 16'0B01, 16'0B03 )
		; [16'0B3C]
		; range( 16'0B3E, 16'0B43 )
		; range( 16'0B47, 16'0B48 )
		; range( 16'0B4B, 16'0B4D )
		; range( 16'0B56, 16'0B57 )
		; range( 16'0B82, 16'0B83 )
		; range( 16'0BBE, 16'0BC2 )
		; range( 16'0BC6, 16'0BC8 )
		; range( 16'0BCA, 16'0BCD )
		; [16'0BD7]
		; range( 16'0C01, 16'0C03 )
		; range( 16'0C3E, 16'0C44 )
		; range( 16'0C46, 16'0C48 )
		; range( 16'0C4A, 16'0C4D )
		; range( 16'0C55, 16'0C56 )
		; range( 16'0C82, 16'0C83 )
		; range( 16'0CBE, 16'0CC4 )
		; range( 16'0CC6, 16'0CC8 )
		; range( 16'0CCA, 16'0CCD )
		; range( 16'0CD5, 16'0CD6 )
		; range( 16'0D02, 16'0D03 )
		; range( 16'0D3E, 16'0D43 )
		; range( 16'0D46, 16'0D48 )
		; range( 16'0D4A, 16'0D4D )
		; [16'0D57]
		; [16'0E31]
		; range( 16'0E34, 16'0E3A )
		; range( 16'0E47, 16'0E4E )
		; [16'0EB1]
		; range( 16'0EB4, 16'0EB9 )
		; range( 16'0EBB, 16'0EBC )
		; range( 16'0EC8, 16'0ECD )
		; range( 16'0F18, 16'0F19 )
		; [16'0F35]
		; [16'0F37]
		; [16'0F39]
		; [16'0F3E]
		; [16'0F3F]
		; range( 16'0F71, 16'0F84 )
		; range( 16'0F86, 16'0F8B )
		; range( 16'0F90, 16'0F95 )
		; [16'0F97]
		; range( 16'0F99, 16'0FAD )
		; range( 16'0FB1, 16'0FB7 )
		; [16'0FB9]
		; range( 16'20D0, 16'20DC )
		; [16'20E1]
		; range( 16'302A, 16'302F )
		; [16'3099]
		; [16'309A]
		).

	unicode_digit  -->
		( range( 16'0030, 16'0039 )
		; range( 16'0660, 16'0669 )
		; range( 16'06F0, 16'06F9 )
		; range( 16'0966, 16'096F )
		; range( 16'09E6, 16'09EF )
		; range( 16'0A66, 16'0A6F )
		; range( 16'0AE6, 16'0AEF )
		; range( 16'0B66, 16'0B6F )
		; range( 16'0BE7, 16'0BEF )
		; range( 16'0C66, 16'0C6F )
		; range( 16'0CE6, 16'0CEF )
		; range( 16'0D66, 16'0D6F )
		; range( 16'0E50, 16'0E59 )
		; range( 16'0ED0, 16'0ED9 )
		; range( 16'0F20, 16'0F29 )
		).

	extender  -->
		( [16'00B7]
		; [16'02D0]
		; [16'02D1]
		; [16'0387]
		; [16'0640]
		; [16'0E46]
		; [16'0EC6]
		; [16'3005]
		; range( 16'3031, 16'3035 )
		; range( 16'309D, 16'309E )
		; range( 16'30FC, 16'30FE )
		).

	range( Low, High ) -->
		[Char],
		{Char >= Low, Char =< High}.
*/

:- end_object.
