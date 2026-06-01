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


:- object(http_directory_listing,
	imports([options, http_docroot_paths])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Router-agnostic directory listing helper built on the normalized ``http`` library.'
	]).

	:- public(serve/4).
	:- mode(serve(+atom, +compound, +atom, -compound), one_or_error).
	:- info(serve/4, [
		comment is 'Serves a directory listing for a relative request path from the given document root using the default options and returns a normalized response.',
		argnames is ['Path', 'Request', 'DocumentRoot', 'Response']
	]).

	:- public(serve/5).
	:- mode(serve(+atom, +compound, +atom, -compound, +list(compound)), one_or_error).
	:- info(serve/5, [
		comment is 'Serves a directory listing for a relative request path from the given document root using the given options and returns a normalized response.',
		argnames is ['Path', 'Request', 'DocumentRoot', 'Response', 'Options']
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2
	]).

	:- uses(date, [
		format_date_time/4, unix_to_date_time/2
	]).

	:- uses(term_io, [
		with_output_to/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	serve(Path, Request, DocumentRoot, Response) :-
		serve(Path, Request, DocumentRoot, Response, []).

	serve(Path, Request, DocumentRoot, Response, UserOptions) :-
		^^validate_relative_path(Path),
		^^validate_request(Request),
		^^validate_document_root(DocumentRoot),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	^^supported_method(Request) ->
			(	resolve_directory(Path, DocumentRoot, Directory) ->
				directory_response(Path, Request, Directory, Response, Options)
			;	not_found_response(Request, Response)
			)
		;	method_not_allowed_response(Request, Response)
		),
		!.

	valid_option(dot_files(Boolean)) :-
		valid_boolean_option(Boolean).
	valid_option(directories_first(Boolean)) :-
		valid_boolean_option(Boolean).
	valid_option(sort_by(SortBy)) :-
		valid_sort_by_value(SortBy).
	valid_option(sort_order(SortOrder)) :-
		valid_sort_order_value(SortOrder).
	valid_option(columns(Columns)) :-
		valid_columns_option(Columns).
	valid_option(type_display(TypeDisplay)) :-
		valid_type_display_value(TypeDisplay).
	valid_option(title(Title)) :-
		atom(Title).
	valid_option(theme(Theme)) :-
		atom(Theme),
		Theme \== ''.
	valid_option(stylesheets(Stylesheets)) :-
		valid_stylesheet_list(Stylesheets).

	default_option(dot_files(false)).
	default_option(directories_first(true)).
	default_option(sort_by(name)).
	default_option(sort_order(ascending)).
	default_option(columns([name, type, size, modified])).
	default_option(type_display(simple)).
	default_option(title('Directory listing')).
	default_option(theme(default)).
	default_option(stylesheets([])).

	resolve_directory(Path, DocumentRoot, Directory) :-
		resolved_document_root(DocumentRoot, Root),
		^^resolved_target_path(Path, Root, Candidate),
		os::directory_exists(Candidate),
		Directory = Candidate.

	resolved_document_root(DocumentRoot, Root) :-
		os::absolute_file_name(DocumentRoot, AbsoluteDocumentRoot),
		(	os::directory_exists(AbsoluteDocumentRoot) ->
			os::path_concat(AbsoluteDocumentRoot, '', Root)
		;	domain_error(http_directory_listing_document_root, DocumentRoot)
		).

	directory_response(Path, Request, Directory, Response, Options) :-
		display_path(Path, DisplayPath),
		listing_settings(Request, Options, Settings),
		directory_entries(Directory, Settings, Entries, Options),
		render_directory_listing(DisplayPath, Settings, Entries, HTML, Options),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/html', text(HTML)), [], Response).

	display_path(Path, '/') :-
		strip_leading_slashes(Path, ''),
		!.
	display_path(Path, DisplayPath) :-
		strip_leading_slashes(Path, StrippedPath),
		(	sub_atom(StrippedPath, _, 1, 0, '/') ->
			Suffix = StrippedPath
		;	atom_concat(StrippedPath, '/', Suffix)
		),
		atom_concat('/', Suffix, DisplayPath).

	strip_leading_slashes(Path, StrippedPath) :-
		atom_codes(Path, Codes),
		strip_leading_slash_codes(Codes, StrippedCodes),
		atom_codes(StrippedPath, StrippedCodes).

	strip_leading_slash_codes([0'/| Codes], StrippedCodes) :-
		!,
		strip_leading_slash_codes(Codes, StrippedCodes).
	strip_leading_slash_codes(Codes, Codes).

	listing_settings(Request, Options, settings(DirectoriesFirst, SortBy, SortOrder)) :-
		^^option(directories_first(DirectoriesFirst), Options),
		^^option(columns(Columns), Options),
		^^option(sort_by(DefaultSortBy), Options),
		^^option(sort_order(DefaultSortOrder), Options),
		visible_sort_by(DefaultSortBy, Columns, VisibleDefaultSortBy),
		request_sort_by(Request, Columns, VisibleDefaultSortBy, SortBy),
		request_sort_order(Request, DefaultSortOrder, SortOrder).

	request_sort_by(Request, Columns, DefaultSortBy, SortBy) :-
		(	request_query_pairs(Request, Pairs),
			query_option_atom(Pairs, sort, SortBy0),
			member(SortBy0, Columns) ->
			SortBy = SortBy0
		;	SortBy = DefaultSortBy
		).

	visible_sort_by(SortBy, Columns, SortBy) :-
		member(SortBy, Columns),
		!.
	visible_sort_by(_SortBy, _Columns, name).

	request_sort_order(Request, DefaultSortOrder, SortOrder) :-
		(	request_query_pairs(Request, Pairs),
			query_option_atom(Pairs, order, SortOrderAtom),
			valid_sort_order_value(SortOrderAtom) ->
			SortOrder0 = SortOrderAtom,
			SortOrder = SortOrder0
		;	SortOrder = DefaultSortOrder
		).

	request_query_pairs(Request, Pairs) :-
		http::property(Request, query_pairs(Pairs)),
		!.
	request_query_pairs(Request, Pairs) :-
		http::target(Request, origin(_Path, Query)),
		query_text_pairs(Query, Pairs).

	query_text_pairs('', []) :-
		!.
	query_text_pairs(Query, Pairs) :-
		atom::split(Query, '&', PairTexts),
		query_pair_texts(PairTexts, Pairs).

	query_pair_texts([], []).
	query_pair_texts([PairText| PairTexts], Pairs) :-
		(	PairText == '' ->
			Pairs = RestPairs
		;	query_pair_text(PairText, Pair) ->
			Pairs = [Pair| RestPairs]
		;	Pairs = RestPairs
		),
		query_pair_texts(PairTexts, RestPairs).

	query_pair_text(PairText, Name-Value) :-
		atom::split(PairText, '=', Parts),
		Parts = [Name| ValueParts],
		Name \== '',
		(	ValueParts == [] ->
			Value = ''
		;	ValueParts = [Value] ->
			true
		;	atomic_list_concat(ValueParts, '=', Value)
		).

	query_option_atom(Pairs, Name, Atom) :-
		memberchk(Name-Value, Pairs),
		text_atom(Value, Atom).

	text_atom(Value, Atom) :-
		atom(Value),
		!,
		Atom = Value.
	text_atom([Head| Tail], Atom) :-
		integer(Head),
		!,
		atom_codes(Atom, [Head| Tail]).
	text_atom([Head| Tail], Atom) :-
		atom(Head),
		atom_chars(Atom, [Head| Tail]).

	valid_sort_by_value(SortBy) :-
		once((
			SortBy == name;
			SortBy == type;
			SortBy == size;
			SortBy == modified
		)).

	valid_sort_order_value(SortOrder) :-
		once((
			SortOrder == ascending;
			SortOrder == descending
		)).

	valid_boolean_option(Boolean) :-
		once((
			Boolean == true;
			Boolean == false
		)).

	directory_entries(Directory, Settings, Entries, Options) :-
		^^option(dot_files(DotFiles), Options),
		os::directory_files(Directory, Names0, [dot_files(DotFiles)]),
		filter_special_entries(Names0, Names1),
		entry_terms(Names1, Directory, Entries0),
		sort_entries(Entries0, Settings, Entries).

	filter_special_entries([], []).
	filter_special_entries([Name| Names], FilteredNames) :-
		member(Name, ['.', '..']),
		!,
		filter_special_entries(Names, FilteredNames).
	filter_special_entries([Name| Names], [Name| FilteredNames]) :-
		filter_special_entries(Names, FilteredNames).

	entry_terms([], _Directory, []).
	entry_terms([Name0| Names], Directory, [Entry| Entries]) :-
		strip_trailing_slash(Name0, Name),
		os::path_concat(Directory, Name, EntryPath),
		entry_term(Name, EntryPath, Entry),
		entry_terms(Names, Directory, Entries).

	entry_term(Name, EntryPath, entry(Name, Href, Label, EntryKind, TypeKey, TypeSimpleDisplay, TypeMediaDisplay, SizeValue, SizeDisplay, ModifiedValue, ModifiedDisplay)) :-
		(	os::directory_exists(EntryPath) ->
			atom_concat(Name, '/', Href),
			Label = Href,
			EntryKind = directory,
			TypeKey = directory,
			TypeSimpleDisplay = directory,
			TypeMediaDisplay = directory,
			SizeValue = -1,
			SizeDisplay = ('-'),
			ModifiedValue = -1,
			ModifiedDisplay = ('-')
		;	Href = Name,
			Label = Name,
			EntryKind = file,
			TypeSimpleDisplay = file,
			file_type_metadata(EntryPath, TypeKey, TypeMediaDisplay),
			os::file_size(EntryPath, SizeValue),
			size_display(SizeValue, SizeDisplay),
			os::file_modification_time(EntryPath, ModifiedValue0),
			normalize_modification_time(ModifiedValue0, ModifiedValue),
			modified_display(ModifiedValue, ModifiedDisplay)
		).

	file_type_metadata(EntryPath, type(MediaType, Encoding), Display) :-
		mime_types::guess_file_type(EntryPath, MediaType0, Encoding, false),
		guessed_media_type(MediaType0, MediaType),
		media_type_display(MediaType, Encoding, Display).

	media_type_display(MediaType, '', MediaType) :-
		!.
	media_type_display(MediaType, Encoding, Display) :-
		atomic_list_concat([MediaType, ' (', Encoding, ')'], Display).

	size_display(Size, Display) :-
		number_codes(Size, Codes),
		atom_codes(Display, Codes).

	normalize_modification_time(ModifiedTime, NormalizedTime) :-
		(	integer(ModifiedTime) ->
			NormalizedTime = ModifiedTime
		;	NormalizedTime is floor(ModifiedTime)
		).

	modified_display(ModifiedTime, Display) :-
		unix_to_date_time(ModifiedTime, DateTime),
		format_date_time(DateTime, 0, date_time_medium, Display).

	sort_entries(Entries0, settings(true, SortBy, SortOrder), Entries) :-
		!,
		split_entries_by_type(Entries0, DirectoryEntries0, FileEntries0),
		sort_entries_group(DirectoryEntries0, SortBy, SortOrder, DirectoryEntries),
		sort_entries_group(FileEntries0, SortBy, SortOrder, FileEntries),
		append(DirectoryEntries, FileEntries, Entries).
	sort_entries(Entries0, settings(false, SortBy, SortOrder), Entries) :-
		sort_entries_group(Entries0, SortBy, SortOrder, Entries).

	split_entries_by_type([], [], []).
	split_entries_by_type([entry(Name, Href, Label, directory, TypeKey, TypeSimpleDisplay, TypeMediaDisplay, SizeValue, SizeDisplay, ModifiedValue, ModifiedDisplay)| Entries], [entry(Name, Href, Label, directory, TypeKey, TypeSimpleDisplay, TypeMediaDisplay, SizeValue, SizeDisplay, ModifiedValue, ModifiedDisplay)| DirectoryEntries], FileEntries) :-
		!,
		split_entries_by_type(Entries, DirectoryEntries, FileEntries).
	split_entries_by_type([Entry| Entries], DirectoryEntries, [Entry| FileEntries]) :-
		split_entries_by_type(Entries, DirectoryEntries, FileEntries).

	sort_entries_group([], _SortBy, _SortOrder, []) :-
		!.
	sort_entries_group(Entries0, SortBy, SortOrder, Entries) :-
		keyed_entries(Entries0, SortBy, KeyedEntries0),
		sort(KeyedEntries0, KeyedEntries1),
		keyed_entries_values(KeyedEntries1, Entries1),
		(	SortOrder == descending ->
			reverse(Entries1, Entries)
		;	Entries = Entries1
		).

	keyed_entries([], _SortBy, []).
	keyed_entries([Entry| Entries], SortBy, [Key-Entry| KeyedEntries]) :-
		entry_sort_key(SortBy, Entry, Key),
		keyed_entries(Entries, SortBy, KeyedEntries).

	entry_sort_key(name, entry(Name, _Href, _Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), key(Name)).
	entry_sort_key(type, entry(Name, _Href, _Label, _EntryKind, TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), key(TypeKey, Name)).
	entry_sort_key(size, entry(Name, _Href, _Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), key(SizeValue, Name)).
	entry_sort_key(modified, entry(Name, _Href, _Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, ModifiedValue, _ModifiedDisplay), key(ModifiedValue, Name)).

	keyed_entries_values([], []).
	keyed_entries_values([_Key-Entry| KeyedEntries], [Entry| Entries]) :-
		keyed_entries_values(KeyedEntries, Entries).

	strip_trailing_slash(Name0, Name) :-
		sub_atom(Name0, _, 1, 0, '/'),
		!,
		sub_atom(Name0, 0, _, 1, Name).
	strip_trailing_slash(Name, Name).

	render_directory_listing(DisplayPath, Settings, Entries, HTML, Options) :-
		^^option(title(Title), Options),
		listing_document(Title, DisplayPath, Settings, Entries, Document, Options),
		with_output_to(atom(HTML), (
			current_output(Stream),
			html5::generate(stream(Stream), Document)
		)).

	listing_document(Title, DisplayPath, Settings, Entries, html([lang=en], [head(Head), body([class=BodyClass], Body)]), Options) :-
		listing_head(Title, Head, Options),
		body_class(Options, BodyClass),
		listing_body(Title, DisplayPath, Settings, Entries, Options, Body).

	listing_head(Title, Head, Options) :-
		^^option(stylesheets(Stylesheets), Options),
		stylesheet_links(Stylesheets, StylesheetLinks),
		Head = [meta([charset='utf-8']), title(Title)| StylesheetLinks].

	stylesheet_links([], []).
	stylesheet_links([Href| Hrefs], [link([rel=stylesheet, href=Href])| Links]) :-
		stylesheet_links(Hrefs, Links).

	listing_body(Title, DisplayPath, Settings, Entries, Options, Body) :-
		listing_table(Settings, Entries, Options, Table),
		listing_navigation(DisplayPath, Navigation),
		Body0 = [h1([class='listing-title'], Title), p([class='listing-path'], ['Directory: ', code(DisplayPath)])| Navigation],
		(	Entries == [] ->
			append(Body0, [Table, p('Directory is empty.')], Body)
		;	append(Body0, [Table], Body)
		).

	listing_navigation(DisplayPath, [p([class='breadcrumbs'], ['Breadcrumbs: '| Breadcrumbs])| ParentBlocks]) :-
		path_segments(DisplayPath, Segments),
		breadcrumbs_content(Segments, Breadcrumbs),
		parent_directory_blocks(Segments, ParentBlocks).

	path_segments(Path, Segments) :-
		strip_leading_slashes(Path, LeadingStrippedPath),
		strip_trailing_slash(LeadingStrippedPath, NormalizedPath),
		(	NormalizedPath == '' ->
			Segments = []
		;	atom::split(NormalizedPath, '/', Segments)
		).

	breadcrumbs_content([], [code('/')]) :-
		!.
	breadcrumbs_content(Segments, [a([href=RootHref], '/'), ' / '| Breadcrumbs]) :-
		up_href(Segments, RootHref),
		breadcrumb_segments(Segments, Breadcrumbs).

	breadcrumb_segments([Segment], [code(Segment)]) :-
		!.
	breadcrumb_segments([Segment| RemainingSegments], [a([href=Href], Segment), ' / '| Breadcrumbs]) :-
		up_href(RemainingSegments, Href),
		breadcrumb_segments(RemainingSegments, Breadcrumbs).

	up_href([], '').
	up_href([_| Segments], Href) :-
		up_href(Segments, TailHref),
		atom_concat('../', TailHref, Href).

	parent_directory_blocks([], []).
	parent_directory_blocks([_| _], [p([class='parent-directory'], a([href='../'], 'Parent directory'))]).

	listing_table(Settings, Entries, Options, table([class=TableClass], [thead(HeaderRow), tbody(Rows)])) :-
		^^option(columns(Columns), Options),
		table_class(Options, Columns, TableClass),
		listing_header_row(Settings, Columns, HeaderRow),
		listing_rows(Entries, Columns, Options, Rows).

	listing_header_row(Settings, Columns, tr(Cells)) :-
		listing_header_cells(Columns, Settings, Cells).

	listing_header_cells([], _Settings, []).
	listing_header_cells([Column| Columns], Settings, [Cell| Cells]) :-
		column_label(Column, Label),
		sort_header_cell(Settings, Column, Label, Cell),
		listing_header_cells(Columns, Settings, Cells).

	column_label(name, 'Name').
	column_label(type, 'Type').
	column_label(size, 'Size').
	column_label(modified, 'Modified').

	sort_header_cell(settings(_DirectoriesFirst, CurrentSortBy, CurrentSortOrder), SortBy, Label, th([class=Class], a([href=Href], Content))) :-
		next_sort_order(SortBy, CurrentSortBy, CurrentSortOrder, NextSortOrder),
		sort_query_href(SortBy, NextSortOrder, Href),
		column_class(SortBy, Class),
		sort_header_content(SortBy, Label, CurrentSortBy, CurrentSortOrder, Content).

	next_sort_order(SortBy, SortBy, ascending, descending) :-
		!.
	next_sort_order(_SortBy, _CurrentSortBy, _CurrentSortOrder, ascending).

	sort_query_href(SortBy, SortOrder, Href) :-
		atomic_list_concat(['?sort=', SortBy, '&order=',  SortOrder], Href).

	column_class(Column, Class) :-
		atom_concat('column-', Column, Class).

	sort_header_content(SortBy, Label, SortBy, SortOrder, [Label, ' (', OrderAtom, ')']) :-
		!,
		OrderAtom = SortOrder.
	sort_header_content(_SortBy, Label, _CurrentSortBy, _CurrentSortOrder, Label).

	listing_rows([], _Columns, _Options, []).
	listing_rows([Entry| Entries], Columns, Options, [tr([class=RowClass], Cells)| Rows]) :-
		entry_row_class(Entry, RowClass),
		listing_row_cells(Columns, Entry, Options, Cells),
		listing_rows(Entries, Columns, Options, Rows).

	entry_row_class(entry(_Name, _Href, _Label, EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), RowClass) :-
		atom_concat('entry-', EntryKind, KindClass),
		atomic_list_concat([entry, KindClass], ' ', RowClass).

	listing_row_cells([], _Entry, _Options, []).
	listing_row_cells([Column| Columns], Entry, Options, [Cell| Cells]) :-
		listing_row_cell(Column, Entry, Options, Cell),
		listing_row_cells(Columns, Entry, Options, Cells).

	listing_row_cell(name, entry(_Name, Href, Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), _Options, td(a([href=Href], Label))).
	listing_row_cell(type, Entry, Options, td(TypeDisplay)) :-
		entry_type_display(Entry, Options, TypeDisplay).
	listing_row_cell(size, entry(_Name, _Href, _Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, SizeDisplay, _ModifiedValue, _ModifiedDisplay), _Options, td(SizeDisplay)).
	listing_row_cell(modified, entry(_Name, _Href, _Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, ModifiedDisplay), _Options, td(ModifiedDisplay)).

	entry_type_display(entry(_Name, _Href, _Label, _EntryKind, _TypeKey, _TypeSimpleDisplay, TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), Options, TypeDisplay) :-
		^^option(type_display(media), Options),
		!,
		TypeDisplay = TypeMediaDisplay.
	entry_type_display(entry(_Name, _Href, _Label, _EntryKind, _TypeKey, TypeSimpleDisplay, _TypeMediaDisplay, _SizeValue, _SizeDisplay, _ModifiedValue, _ModifiedDisplay), _Options, TypeDisplay) :-
		TypeDisplay = TypeSimpleDisplay.

	body_class(Options, Class) :-
		^^option(theme(Theme), Options),
		theme_class(Theme, ThemeClass),
		atomic_list_concat(['http-directory-listing', ThemeClass], ' ', Class).

	table_class(Options, Columns, Class) :-
		^^option(theme(Theme), Options),
		theme_class(Theme, ThemeClass),
		columns_class(Columns, ColumnsClass),
		atomic_list_concat(['directory-listing-table', ThemeClass, ColumnsClass], ' ', Class).

	theme_class(Theme, ThemeClass) :-
		atom_concat('theme-', Theme, ThemeClass).

	columns_class(Columns, Class) :-
		atomic_list_concat(Columns, '-', Suffix),
		atom_concat('columns-', Suffix, Class).

	guessed_media_type('', 'application/octet-stream') :-
		!.
	guessed_media_type(Type, Type).

	not_found_response(Request, Response) :-
		http::version(Request, Version),
		http::response(Version, status(404, 'Not Found'), [], content('text/plain', text('Not Found')), [], Response).

	method_not_allowed_response(Request, Response) :-
		http::version(Request, Version),
		http::response(Version, status(405, 'Method Not Allowed'), [allow-'GET, HEAD'], content('text/plain', text('Method Not Allowed')), [], Response).

	valid_columns_option(Columns) :-
		Columns \== [],
		ground(Columns),
		memberchk(name, Columns),
		valid_columns_option(Columns, []).

	valid_columns_option([], _Seen).
	valid_columns_option([Column| Columns], Seen) :-
		valid_column_value(Column),
		\+ member(Column, Seen),
		valid_columns_option(Columns, [Column| Seen]).

	valid_column_value(name).
	valid_column_value(type).
	valid_column_value(size).
	valid_column_value(modified).

	valid_type_display_value(simple).
	valid_type_display_value(media).

	valid_stylesheet_list([]).
	valid_stylesheet_list([Stylesheet| Stylesheets]) :-
		atom(Stylesheet),
		Stylesheet \== '',
		valid_stylesheet_list(Stylesheets).

:- end_object.
