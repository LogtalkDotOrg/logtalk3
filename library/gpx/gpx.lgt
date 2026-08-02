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


:- object(gpx,
	implements(gpx_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'GPX 1.1 parser, generator, and validator.'
	]).

	:- uses(reader, [
		file_to_codes/2, stream_to_codes/2
	]).

	:- uses(list, [
		append/2, append/3, member/2, select/3, valid/1 as is_list/1
	]).

	parse(Source, GPX) :-
		source_codes(Source, Codes),
		xml::parse(Codes, XML),
		xml_to_gpx(XML, GPX),
		!.
	parse(Source, _) :-
		valid_source(Source),
		domain_error(gpx, Source).
	parse(Source, _) :-
		domain_error(gpx_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(_, GPX) :-
		\+ ground(GPX),
		instantiation_error.
	generate(Sink, GPX) :-
		valid_sink(Sink),
		!,
		gpx_to_xml(GPX, XML),
		xml::parse(Codes, XML),
		write_sink(Sink, Codes).
	generate(Sink, _) :-
		domain_error(gpx_sink, Sink).

	validate(GPX) :-
		validate(GPX, Errors),
		Errors == [].

	validate(GPX, _) :-
		var(GPX),
		instantiation_error.
	validate(gpx(Creator, Properties), Errors) :-
		!,
		validate_creator(Creator, CreatorErrors),
		validate_properties(Properties, root, [], PropertyErrors),
		append(CreatorErrors, PropertyErrors, Errors).
	validate(_, [invalid_gpx_term([])]).

	validate_creator(Creator, []) :-
		atom(Creator),
		!.
	validate_creator(_, [invalid_creator([creator])]).

	validate_properties(Properties, Context, Path, Errors) :-
		is_list(Properties),
		!,
		validate_properties(Properties, Context, Path, [], Errors).
	validate_properties(_, _, Path, [invalid_properties(PropertiesPath)]) :-
		path_element(Path, properties, PropertiesPath).

	validate_properties([], _, _, _, []).
	validate_properties([Property| Properties], Context, Path, Seen, Errors) :-
		property_name(Property, Name),
		known_property(Context, Name),
		!,
		path_element(Path, Name, PropertyPath),
		(	member(Name, Seen) ->
			Errors = [duplicate_property(Name, PropertyPath)| RestErrors]
		;	validate_known_property(Context, Property, PropertyPath, PropertyErrors),
			append(PropertyErrors, RestErrors, Errors)
		),
		validate_properties(Properties, Context, Path, [Name| Seen], RestErrors).
	validate_properties([Property| Properties], Context, Path, Seen, [unknown_property(Property, PropertyPath)| Errors]) :-
		property_path(Property, Path, PropertyPath),
		validate_properties(Properties, Context, Path, Seen, Errors).

	property_name(Property, Name) :-
		compound(Property),
		functor(Property, Name, 1).

	property_path(Property, Path, PropertyPath) :-
		property_name(Property, Name),
		!,
		path_element(Path, Name, PropertyPath).
	property_path(_, Path, Path).

	validate_known_property(Context, Property, Path, Errors) :-
		validate_property(Context, Property, Path, Errors),
		!.

	known_property(root, metadata).
	known_property(root, waypoints).
	known_property(root, routes).
	known_property(root, tracks).
	known_property(root, extensions).
	known_property(metadata, name).
	known_property(metadata, description).
	known_property(metadata, author).
	known_property(metadata, copyright).
	known_property(metadata, links).
	known_property(metadata, time).
	known_property(metadata, keywords).
	known_property(metadata, bounds).
	known_property(metadata, extensions).
	known_property(person, name).
	known_property(person, email).
	known_property(person, link).
	known_property(copyright, year).
	known_property(copyright, license).
	known_property(link, text).
	known_property(link, type).
	known_property(point, elevation).
	known_property(point, time).
	known_property(point, magnetic_variation).
	known_property(point, geoid_height).
	known_property(point, name).
	known_property(point, comment).
	known_property(point, description).
	known_property(point, source).
	known_property(point, links).
	known_property(point, symbol).
	known_property(point, type).
	known_property(point, fix).
	known_property(point, satellites).
	known_property(point, hdop).
	known_property(point, vdop).
	known_property(point, pdop).
	known_property(point, age_of_dgps_data).
	known_property(point, dgps_station).
	known_property(point, extensions).
	known_property(route_track, name).
	known_property(route_track, comment).
	known_property(route_track, description).
	known_property(route_track, source).
	known_property(route_track, links).
	known_property(route_track, number).
	known_property(route_track, type).
	known_property(route_track, extensions).
	known_property(track_segment, extensions).

	validate_property(root, metadata(Value), Path, Errors) :-
		validate_metadata(Value, Path, Errors).
	validate_property(root, waypoints(Values), Path, Errors) :-
		validate_list(Values, point, Path, Errors).
	validate_property(root, routes(Values), Path, Errors) :-
		validate_list(Values, route, Path, Errors).
	validate_property(root, tracks(Values), Path, Errors) :-
		validate_list(Values, track, Path, Errors).
	validate_property(root, extensions(Nodes), Path, Errors) :-
		validate_extensions(Nodes, Path, Errors).
	validate_property(metadata, name(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(metadata, description(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(metadata, author(Value), Path, Errors) :-
		validate_person(Value, Path, Errors).
	validate_property(metadata, copyright(Value), Path, Errors) :-
		validate_copyright(Value, Path, Errors).
	validate_property(metadata, links(Values), Path, Errors) :-
		validate_list(Values, link, Path, Errors).
	validate_property(metadata, time(Value), Path, Errors) :-
		validate_scalar(date_time, Value, Path, Errors).
	validate_property(metadata, keywords(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(metadata, bounds(Value), Path, Errors) :-
		validate_bounds(Value, Path, Errors).
	validate_property(metadata, extensions(Nodes), Path, Errors) :-
		validate_extensions(Nodes, Path, Errors).
	validate_property(person, name(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(person, email(Value), Path, Errors) :-
		validate_email(Value, Path, Errors).
	validate_property(person, link(Value), Path, Errors) :-
		validate_link(Value, Path, Errors).
	validate_property(copyright, year(Value), Path, Errors) :-
		validate_scalar(g_year, Value, Path, Errors).
	validate_property(copyright, license(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(link, text(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(link, type(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, elevation(Value), Path, Errors) :-
		validate_scalar(number, Value, Path, Errors).
	validate_property(point, time(Value), Path, Errors) :-
		validate_scalar(date_time, Value, Path, Errors).
	validate_property(point, magnetic_variation(Value), Path, Errors) :-
		validate_scalar(degrees, Value, Path, Errors).
	validate_property(point, geoid_height(Value), Path, Errors) :-
		validate_scalar(number, Value, Path, Errors).
	validate_property(point, name(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, comment(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, description(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, source(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, links(Values), Path, Errors) :-
		validate_list(Values, link, Path, Errors).
	validate_property(point, symbol(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, type(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(point, fix(Value), Path, Errors) :-
		validate_scalar(fix, Value, Path, Errors).
	validate_property(point, satellites(Value), Path, Errors) :-
		validate_scalar(nonnegative_integer, Value, Path, Errors).
	validate_property(point, hdop(Value), Path, Errors) :-
		validate_scalar(number, Value, Path, Errors).
	validate_property(point, vdop(Value), Path, Errors) :-
		validate_scalar(number, Value, Path, Errors).
	validate_property(point, pdop(Value), Path, Errors) :-
		validate_scalar(number, Value, Path, Errors).
	validate_property(point, age_of_dgps_data(Value), Path, Errors) :-
		validate_scalar(number, Value, Path, Errors).
	validate_property(point, dgps_station(Value), Path, Errors) :-
		validate_scalar(dgps_station, Value, Path, Errors).
	validate_property(point, extensions(Nodes), Path, Errors) :-
		validate_extensions(Nodes, Path, Errors).
	validate_property(route_track, name(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(route_track, comment(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(route_track, description(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(route_track, source(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(route_track, links(Values), Path, Errors) :-
		validate_list(Values, link, Path, Errors).
	validate_property(route_track, number(Value), Path, Errors) :-
		validate_scalar(nonnegative_integer, Value, Path, Errors).
	validate_property(route_track, type(Value), Path, Errors) :-
		validate_scalar(atom, Value, Path, Errors).
	validate_property(route_track, extensions(Nodes), Path, Errors) :-
		validate_extensions(Nodes, Path, Errors).
	validate_property(track_segment, extensions(Nodes), Path, Errors) :-
		validate_extensions(Nodes, Path, Errors).

	validate_metadata(metadata(Properties), Path, Errors) :-
		!,
		validate_properties(Properties, metadata, Path, Errors).
	validate_metadata(_, Path, [invalid_gpx_term(Path)]).

	validate_person(person(Properties), Path, Errors) :-
		!,
		validate_properties(Properties, person, Path, Errors).
	validate_person(_, Path, [invalid_gpx_term(Path)]).

	validate_copyright(copyright(Author, Properties), Path, Errors) :-
		!,
		path_element(Path, author, AuthorPath),
		validate_scalar(atom, Author, AuthorPath, AuthorErrors),
		validate_properties(Properties, copyright, Path, PropertyErrors),
		append(AuthorErrors, PropertyErrors, Errors).
	validate_copyright(_, Path, [invalid_gpx_term(Path)]).

	validate_email(email(Id, Domain), Path, Errors) :-
		!,
		path_element(Path, id, IdPath),
		path_element(Path, domain, DomainPath),
		validate_scalar(atom, Id, IdPath, IdErrors),
		validate_scalar(atom, Domain, DomainPath, DomainErrors),
		append(IdErrors, DomainErrors, Errors).
	validate_email(_, Path, [invalid_gpx_term(Path)]).

	validate_link(link(Href, Properties), Path, Errors) :-
		!,
		path_element(Path, href, HrefPath),
		validate_scalar(atom, Href, HrefPath, HrefErrors),
		validate_properties(Properties, link, Path, PropertyErrors),
		append(HrefErrors, PropertyErrors, Errors).
	validate_link(_, Path, [invalid_gpx_term(Path)]).

	validate_bounds(bounds(MinLat, MinLon, MaxLat, MaxLon), Path, Errors) :-
		!,
		validate_bound_value(latitude, MinLat, min_latitude, Path, MinLatErrors),
		validate_bound_value(longitude, MinLon, min_longitude, Path, MinLonErrors),
		validate_bound_value(latitude, MaxLat, max_latitude, Path, MaxLatErrors),
		validate_bound_value(longitude, MaxLon, max_longitude, Path, MaxLonErrors),
		append([MinLatErrors, MinLonErrors, MaxLatErrors, MaxLonErrors], Errors2),
		(	valid_latitude(MinLat),
			valid_latitude(MaxLat),
			MinLat > MaxLat ->
			Errors = [invalid_bounds(Path)| Errors2]
		;	Errors = Errors2
		).
	validate_bounds(_, Path, [invalid_gpx_term(Path)]).

	validate_bound_value(Kind, Value, Name, Path, Errors) :-
		path_element(Path, Name, ValuePath),
		validate_scalar(Kind, Value, ValuePath, Errors).

	validate_list(Values, Kind, Path, Errors) :-
		is_list(Values),
		!,
		validate_list(Values, Kind, Path, 0, Errors).
	validate_list(_, _, Path, [invalid_list(Path)]).

	validate_list([], _, _, _, []).
	validate_list([Value| Values], Kind, Path, Index, Errors) :-
		path_element(Path, Index, ValuePath),
		validate_list_value(Kind, Value, ValuePath, ValueErrors),
		NextIndex is Index + 1,
		validate_list(Values, Kind, Path, NextIndex, RestErrors),
		append(ValueErrors, RestErrors, Errors).

	validate_list_value(point, Value, Path, Errors) :-
		validate_point(Value, Path, Errors).
	validate_list_value(route, Value, Path, Errors) :-
		validate_route(Value, Path, Errors).
	validate_list_value(track, Value, Path, Errors) :-
		validate_track(Value, Path, Errors).
	validate_list_value(track_segment, Value, Path, Errors) :-
		validate_track_segment(Value, Path, Errors).
	validate_list_value(link, Value, Path, Errors) :-
		validate_link(Value, Path, Errors).

	validate_point(waypoint(geographic(Latitude, Longitude), Properties), Path, Errors) :-
		!,
		path_element(Path, latitude, LatitudePath),
		path_element(Path, longitude, LongitudePath),
		validate_scalar(latitude, Latitude, LatitudePath, LatitudeErrors),
		validate_scalar(longitude, Longitude, LongitudePath, LongitudeErrors),
		validate_properties(Properties, point, Path, PropertyErrors),
		append([LatitudeErrors, LongitudeErrors, PropertyErrors], Errors).
	validate_point(_, Path, [invalid_gpx_term(Path)]).

	validate_route(route(Points, Properties), Path, Errors) :-
		!,
		path_element(Path, points, PointsPath),
		validate_list(Points, point, PointsPath, PointErrors),
		validate_properties(Properties, route_track, Path, PropertyErrors),
		append(PointErrors, PropertyErrors, Errors).
	validate_route(_, Path, [invalid_gpx_term(Path)]).

	validate_track(track(Segments, Properties), Path, Errors) :-
		!,
		path_element(Path, segments, SegmentsPath),
		validate_list(Segments, track_segment, SegmentsPath, SegmentErrors),
		validate_properties(Properties, route_track, Path, PropertyErrors),
		append(SegmentErrors, PropertyErrors, Errors).
	validate_track(_, Path, [invalid_gpx_term(Path)]).

	validate_track_segment(track_segment(Points, Properties), Path, Errors) :-
		!,
		path_element(Path, points, PointsPath),
		validate_list(Points, point, PointsPath, PointErrors),
		validate_properties(Properties, track_segment, Path, PropertyErrors),
		append(PointErrors, PropertyErrors, Errors).
	validate_track_segment(_, Path, [invalid_gpx_term(Path)]).

	validate_extensions(Nodes, _, []) :-
		valid_extension_nodes(Nodes),
		!.
	validate_extensions(_, Path, [invalid_extensions(Path)]).

	validate_scalar(Kind, Value, _, []) :-
		valid_scalar(Kind, Value),
		!.
	validate_scalar(date_time, Value, Path, [invalid_date_time(Value, Path)]) :-
		!.
	validate_scalar(g_year, Value, Path, [invalid_year(Value, Path)]) :-
		!.
	validate_scalar(_, Value, Path, [invalid_value(Value, Path)]).

	path_element(Path, Element, ElementPath) :-
		append(Path, [Element], ElementPath).

	xml_to_gpx(XML, _) :-
		\+ ground(XML),
		instantiation_error.
	xml_to_gpx(xml(_Declaration, [namespace('http://www.topografix.com/GPX/1/1', [], element(gpx, Attributes, Content))]), gpx(Creator, Properties)) :-
		gpx_attributes(Attributes, Creator),
		gpx_content(Content, Properties),
		!.
	xml_to_gpx(XML, _) :-
		domain_error(gpx, XML).

	gpx_to_xml(GPX, _) :-
		\+ ground(GPX),
		instantiation_error.
	gpx_to_xml(GPX, XML) :-
		native_gpx_xml(GPX, XML),
		!.
	gpx_to_xml(GPX, _) :-
		domain_error(gpx, GPX).

	native_gpx_xml(gpx(Creator, Properties), xml([version=[0'1, 0'., 0'0], encoding=[0'U, 0'T, 0'F, 0'-, 0'8]], [
		namespace('http://www.topografix.com/GPX/1/1', [], element(gpx, Attributes, Content))
	])) :-
		atom(Creator),
		atom_codes(Creator, CreatorCodes),
		is_list(Properties),
		extension_namespace_attributes(Properties, NamespaceAttributes),
		Attributes = [version=[0'1, 0'., 0'1], creator=CreatorCodes| NamespaceAttributes],
		gpx_properties_content(Properties, Content).

	gpx_attributes(Attributes, Creator) :-
		select(version=[0'1, 0'., 0'1], Attributes, Attributes1),
		select(creator=CreatorCodes, Attributes1, Remaining),
		allowed_root_attributes(Remaining),
		atom_codes(Creator, CreatorCodes).

	allowed_root_attributes([]).
	allowed_root_attributes([Attribute| Attributes]) :-
		allowed_root_attribute(Attribute),
		allowed_root_attributes(Attributes).

	allowed_root_attribute('xsi:schemaLocation'=_).
	allowed_root_attribute('xmlns:xsi'=_).
	allowed_root_attribute(Name=_) :-
		atom(Name),
		atom_codes(Name, [0'x, 0'm, 0'l, 0'n, 0's, 0':| _]).

	extension_namespace_attributes(Term, Attributes) :-
		extension_namespace_attributes(Term, [], Reversed),
		reverse_bindings(Reversed, Attributes).

	extension_namespace_attributes(namespace(URI, Prefix, Element), Attributes0, Attributes) :-
		!,
		(	Prefix = [_|_] ->
			namespace_attribute(URI, Prefix, Attribute),
			add_namespace_attribute(Attribute, Attributes0, Attributes1)
		;	Attributes1 = Attributes0
		),
		extension_namespace_attributes(Element, Attributes1, Attributes).
	extension_namespace_attributes(Term, Attributes0, Attributes) :-
		compound(Term),
		!,
		Term =.. [_| Arguments],
		extension_namespace_arguments(Arguments, Attributes0, Attributes).
	extension_namespace_attributes(_, Attributes, Attributes).

	extension_namespace_arguments([], Attributes, Attributes).
	extension_namespace_arguments([Argument| Arguments], Attributes0, Attributes) :-
		extension_namespace_attributes(Argument, Attributes0, Attributes1),
		extension_namespace_arguments(Arguments, Attributes1, Attributes).

	namespace_attribute(URI, Prefix, Name=Codes) :-
		NameCodes = [0'x, 0'm, 0'l, 0'n, 0's, 0':| Prefix],
		atom_codes(Name, NameCodes),
		atom_codes(URI, Codes).

	add_namespace_attribute(Attribute, Attributes, Attributes) :-
		namespace_attribute_present(Attribute, Attributes),
		!.
	add_namespace_attribute(Attribute, Attributes, [Attribute| Attributes]).

	namespace_attribute_present(Attribute, [Existing| _]) :-
		Attribute == Existing,
		!.
	namespace_attribute_present(Attribute, [_| Attributes]) :-
		namespace_attribute_present(Attribute, Attributes).

	reverse_bindings(List, Reversed) :-
		reverse_bindings(List, [], Reversed).

	reverse_bindings([], Reversed, Reversed).
	reverse_bindings([Head| Tail], Accumulator, Reversed) :-
		reverse_bindings(Tail, [Head| Accumulator], Reversed).

	gpx_content(Content0, Properties) :-
		optional_complex2(metadata, metadata_content, metadata, Content0, Content1, MetadataProperties),
		repeated_complex(wpt, point_element, Content1, Content2, Waypoints),
		repeated_complex(rte, route_element, Content2, Content3, Routes),
		repeated_complex(trk, track_element, Content3, Content4, Tracks),
		optional_extensions(Content4, [], ExtensionProperties),
		optional_list_property(waypoints, Waypoints, WaypointProperties),
		optional_list_property(routes, Routes, RouteProperties),
		optional_list_property(tracks, Tracks, TrackProperties),
		append([MetadataProperties, WaypointProperties, RouteProperties, TrackProperties, ExtensionProperties], Properties).

	gpx_properties_content(Properties0, Content) :-
		take_optional(metadata, Properties0, Properties1, Metadata),
		take_list(waypoints, Properties1, Properties2, Waypoints),
		take_list(routes, Properties2, Properties3, Routes),
		take_list(tracks, Properties3, Properties4, Tracks),
		take_extensions(Properties4, [], ExtensionContent),
		optional_complex_node2(metadata, metadata_properties_content, Metadata, MetadataContent),
		complex_nodes(wpt, point_properties_element, Waypoints, WaypointContent),
		complex_nodes(rte, route_properties_element, Routes, RouteContent),
		complex_nodes(trk, track_properties_element, Tracks, TrackContent),
		append([MetadataContent, WaypointContent, RouteContent, TrackContent, ExtensionContent], Content).

	metadata_content(Content0, metadata(Properties)) :-
		optional_scalar(name, atom, name, Content0, Content1, Name),
		optional_scalar(desc, atom, description, Content1, Content2, Description),
		optional_complex2(author, person_content, author, Content2, Content3, Author),
		optional_complex3(copyright, copyright_content, copyright, Content3, Content4, Copyright),
		repeated_complex(link, link_element, Content4, Content5, Links),
		optional_scalar(time, date_time, time, Content5, Content6, Time),
		optional_scalar(keywords, atom, keywords, Content6, Content7, Keywords),
		optional_complex3(bounds, bounds_element, bounds, Content7, Content8, Bounds),
		optional_extensions(Content8, [], Extensions),
		optional_list_property(links, Links, LinkProperties),
		append([Name, Description, Author, Copyright, LinkProperties, Time, Keywords, Bounds, Extensions], Properties).

	metadata_properties_content(metadata(Properties0), Content) :-
		take_scalar(name, atom, Properties0, Properties1, Name),
		take_scalar(description, atom, Properties1, Properties2, Description),
		take_optional(author, Properties2, Properties3, Author),
		take_optional(copyright, Properties3, Properties4, Copyright),
		take_list(links, Properties4, Properties5, Links),
		take_scalar(time, date_time, Properties5, Properties6, Time),
		take_scalar(keywords, atom, Properties6, Properties7, Keywords),
		take_optional(bounds, Properties7, Properties8, Bounds),
		take_extensions(Properties8, [], Extensions),
		scalar_node(name, Name, NameContent), scalar_node(desc, Description, DescriptionContent),
		optional_complex_node2(author, person_properties_content, Author, AuthorContent),
		optional_complex_node3(copyright, copyright_properties_content, Copyright, CopyrightContent),
		complex_nodes(link, link_properties_element, Links, LinkContent),
		scalar_node(time, Time, TimeContent), scalar_node(keywords, Keywords, KeywordContent),
		optional_complex_node3(bounds, bounds_properties_element, Bounds, BoundsContent),
		append([NameContent, DescriptionContent, AuthorContent, CopyrightContent, LinkContent, TimeContent, KeywordContent, BoundsContent, Extensions], Content).

	person_content(Content0, person(Properties)) :-
		optional_scalar(name, atom, name, Content0, Content1, Name),
		optional_complex3(email, email_element, email, Content1, Content2, Email),
		optional_complex3(link, link_element, link, Content2, [], Link),
		append([Name, Email, Link], Properties).

	person_properties_content(person(Properties0), Content) :-
		take_scalar(name, atom, Properties0, Properties1, Name),
		take_optional(email, Properties1, Properties2, Email),
		take_optional(link, Properties2, [], Link),
		scalar_node(name, Name, NameContent),
		optional_complex_node3(email, email_properties_element, Email, EmailContent),
		optional_complex_node3(link, link_properties_element, Link, LinkContent),
		append([NameContent, EmailContent, LinkContent], Content).

	email_element([], Attributes, email(Id, Domain)) :-
		select(id=IdCodes, Attributes, Attributes1),
		select(domain=DomainCodes, Attributes1, []),
		atom_codes(Id, IdCodes),
		atom_codes(Domain, DomainCodes).

	email_properties_element(email(Id, Domain), [], [id=IdCodes, domain=DomainCodes]) :-
		atom(Id),
		atom(Domain),
		atom_codes(Id, IdCodes),
		atom_codes(Domain, DomainCodes).

	copyright_content(Content0, Attributes, copyright(Author, Properties)) :-
		select(author=AuthorCodes, Attributes, []),
		atom_codes(Author, AuthorCodes),
		optional_scalar(year, g_year, year, Content0, Content1, Year),
		optional_scalar(license, atom, license, Content1, [], License),
		append(Year, License, Properties).

	copyright_properties_content(copyright(Author, Properties0), Content, [author=AuthorCodes]) :-
		atom(Author),
		atom_codes(Author, AuthorCodes),
		take_scalar(year, g_year, Properties0, Properties1, Year),
		take_scalar(license, atom, Properties1, [], License),
		scalar_node(year, Year, YearContent),
		scalar_node(license, License, LicenseContent),
		append(YearContent, LicenseContent, Content).

	link_element(Content0, Attributes, link(Href, Properties)) :-
		select(href=HrefCodes, Attributes, []),
		atom_codes(Href, HrefCodes),
		optional_scalar(text, atom, text, Content0, Content1, Text),
		optional_scalar(type, atom, type, Content1, [], Type),
		append(Text, Type, Properties).

	link_properties_element(link(Href, Properties0), Content, [href=HrefCodes]) :-
		atom(Href),
		atom_codes(Href, HrefCodes),
		take_scalar(text, atom, Properties0, Properties1, Text),
		take_scalar(type, atom, Properties1, [], Type),
		scalar_node(text, Text, TextContent),
		scalar_node(type, Type, TypeContent),
		append(TextContent, TypeContent, Content).

	bounds_element([], Attributes, bounds(MinLat, MinLon, MaxLat, MaxLon)) :-
		select(minlat=MinLatCodes, Attributes, Attributes1),
		select(minlon=MinLonCodes, Attributes1, Attributes2),
		select(maxlat=MaxLatCodes, Attributes2, Attributes3),
		select(maxlon=MaxLonCodes, Attributes3, []),
		number_codes(MinLat, MinLatCodes),
		number_codes(MinLon, MinLonCodes),
		number_codes(MaxLat, MaxLatCodes),
		number_codes(MaxLon, MaxLonCodes),
		valid_latitude(MinLat),
		valid_longitude(MinLon),
		valid_latitude(MaxLat),
		valid_longitude(MaxLon),
		MinLat =< MaxLat.

	bounds_properties_element(bounds(MinLat, MinLon, MaxLat, MaxLon), [], Attributes) :-
		valid_latitude(MinLat),
		valid_longitude(MinLon),
		valid_latitude(MaxLat),
		valid_longitude(MaxLon),
		MinLat =< MaxLat,
		number_codes(MinLat, MinLatCodes),
		number_codes(MinLon, MinLonCodes),
		number_codes(MaxLat, MaxLatCodes),
		number_codes(MaxLon, MaxLonCodes),
		Attributes = [minlat=MinLatCodes, minlon=MinLonCodes, maxlat=MaxLatCodes, maxlon=MaxLonCodes].

	point_element(Content, Attributes, waypoint(geographic(Latitude, Longitude), Properties)) :-
		select(lat=LatitudeCodes, Attributes, Attributes1),
		select(lon=LongitudeCodes, Attributes1, []),
		number_codes(Latitude, LatitudeCodes),
		number_codes(Longitude, LongitudeCodes),
		valid_latitude(Latitude),
		valid_longitude(Longitude),
		point_content(Content, Properties).

	point_properties_element(waypoint(geographic(Latitude, Longitude), Properties), Content, [lat=LatitudeCodes, lon=LongitudeCodes]) :-
		valid_latitude(Latitude),
		valid_longitude(Longitude),
		number_codes(Latitude, LatitudeCodes),
		number_codes(Longitude, LongitudeCodes),
		point_properties_content(Properties, Content).

	point_content(Content0, Properties) :-
		optional_scalar(ele, number, elevation, Content0, Content1, Elevation),
		optional_scalar(time, date_time, time, Content1, Content2, Time),
		optional_scalar(magvar, degrees, magnetic_variation, Content2, Content3, MagneticVariation),
		optional_scalar(geoidheight, number, geoid_height, Content3, Content4, GeoidHeight),
		optional_scalar(name, atom, name, Content4, Content5, Name),
		optional_scalar(cmt, atom, comment, Content5, Content6, Comment),
		optional_scalar(desc, atom, description, Content6, Content7, Description),
		optional_scalar(src, atom, source, Content7, Content8, Source),
		repeated_complex(link, link_element, Content8, Content9, Links),
		optional_scalar(sym, atom, symbol, Content9, Content10, Symbol),
		optional_scalar(type, atom, type, Content10, Content11, Type),
		optional_scalar(fix, fix, fix, Content11, Content12, Fix),
		optional_scalar(sat, nonnegative_integer, satellites, Content12, Content13, Satellites),
		optional_scalar(hdop, number, hdop, Content13, Content14, HDOP),
		optional_scalar(vdop, number, vdop, Content14, Content15, VDOP),
		optional_scalar(pdop, number, pdop, Content15, Content16, PDOP),
		optional_scalar(ageofdgpsdata, number, age_of_dgps_data, Content16, Content17, Age),
		optional_scalar(dgpsid, dgps_station, dgps_station, Content17, Content18, Station),
		optional_extensions(Content18, [], Extensions), optional_list_property(links, Links, LinkProperties),
		append([Elevation, Time, MagneticVariation, GeoidHeight, Name, Comment, Description, Source, LinkProperties, Symbol, Type, Fix, Satellites, HDOP, VDOP, PDOP, Age, Station, Extensions], Properties).

	point_properties_content(Properties0, Content) :-
		point_property_specs(Specs),
		properties_scalars(Specs, Properties0, Properties1, ScalarContent),
		take_list(links, Properties1, Properties2, Links),
		take_extensions(Properties2, [], Extensions),
		complex_nodes(link, link_properties_element, Links, LinkContent),
		insert_links(ScalarContent, LinkContent, OrderedContent),
		append(OrderedContent, Extensions, Content).

	point_property_specs([
		elevation-ele-number, time-time-date_time, magnetic_variation-magvar-degrees, geoid_height-geoidheight-number,
		name-name-atom, comment-cmt-atom, description-desc-atom, source-src-atom,
		symbol-sym-atom, type-type-atom, fix-fix-fix, satellites-sat-nonnegative_integer,
		hdop-hdop-number, vdop-vdop-number, pdop-pdop-number, age_of_dgps_data-ageofdgpsdata-number,
		dgps_station-dgpsid-dgps_station
	]).

	insert_links(Content0, Links, Content) :-
		split_after_source(Content0, Before, After),
		append([Before, Links, After], Content).

	split_after_source([], [], []).
	split_after_source([Element| Elements], [Element| Before], After) :-
		Element = element(Name, _, _),
		Name \== sym,
		Name \== type,
		Name \== fix,
		Name \== sat,
		Name \== hdop,
		Name \== vdop,
		Name \== pdop,
		Name \== ageofdgpsdata,
		Name \== dgpsid,
		!,
		split_after_source(Elements, Before, After).
	split_after_source(Elements, [], Elements).

	route_element(Content0, [], route(Points, Properties)) :-
		common_route_track_content(Content0, Content1, Properties),
		repeated_complex(rtept, point_element, Content1, [], Points).

	route_properties_element(route(Points, Properties), Content, []) :-
		common_route_track_properties(Properties, CommonContent),
		complex_nodes(rtept, point_properties_element, Points, PointContent),
		append(CommonContent, PointContent, Content).

	track_element(Content0, [], track(Segments, Properties)) :-
		common_route_track_content(Content0, Content1, Properties),
		repeated_complex(trkseg, track_segment_element, Content1, [], Segments).

	track_properties_element(track(Segments, Properties), Content, []) :-
		common_route_track_properties(Properties, CommonContent),
		complex_nodes(trkseg, track_segment_properties_element, Segments, SegmentContent),
		append(CommonContent, SegmentContent, Content).

	track_segment_element(Content0, [], track_segment(Points, Properties)) :-
		repeated_complex(trkpt, point_element, Content0, Content1, Points),
		optional_extensions(Content1, [], Properties).

	track_segment_properties_element(track_segment(Points, Properties), Content, []) :-
		complex_nodes(trkpt, point_properties_element, Points, PointContent),
		take_extensions(Properties, [], Extensions),
		append(PointContent, Extensions, Content).

	common_route_track_content(Content0, Remaining, Properties) :-
		optional_scalar(name, atom, name, Content0, Content1, Name),
		optional_scalar(cmt, atom, comment, Content1, Content2, Comment),
		optional_scalar(desc, atom, description, Content2, Content3, Description),
		optional_scalar(src, atom, source, Content3, Content4, Source),
		repeated_complex(link, link_element, Content4, Content5, Links),
		optional_scalar(number, nonnegative_integer, number, Content5, Content6, Number),
		optional_scalar(type, atom, type, Content6, Content7, Type),
		optional_extensions(Content7, Remaining, Extensions),
		optional_list_property(links, Links, LinkProperties),
		append([Name, Comment, Description, Source, LinkProperties, Number, Type, Extensions], Properties).

	common_route_track_properties(Properties0, Content) :-
		take_scalar(name, atom, Properties0, Properties1, Name), take_scalar(comment, atom, Properties1, Properties2, Comment),
		take_scalar(description, atom, Properties2, Properties3, Description), take_scalar(source, atom, Properties3, Properties4, Source),
		take_list(links, Properties4, Properties5, Links), take_scalar(number, nonnegative_integer, Properties5, Properties6, Number),
		take_scalar(type, atom, Properties6, Properties7, Type), take_extensions(Properties7, [], Extensions),
		scalar_node(name, Name, NameContent), scalar_node(cmt, Comment, CommentContent), scalar_node(desc, Description, DescriptionContent),
		scalar_node(src, Source, SourceContent), complex_nodes(link, link_properties_element, Links, LinkContent),
		scalar_node(number, Number, NumberContent), scalar_node(type, Type, TypeContent),
		append([NameContent, CommentContent, DescriptionContent, SourceContent, LinkContent, NumberContent, TypeContent, Extensions], Content).

	optional_scalar(Name, Kind, PropertyName, [element(Name, [], [pcdata(Codes)])| Content], Content, [Property]) :-
		!,
		scalar_value(Kind, Codes, Value),
		Property =.. [PropertyName, Value].
	optional_scalar(_, _, _, Content, Content, []).

	:- meta_predicate(optional_complex2(*, 2, *, *, *, *)).

	optional_complex2(Name, Parser, PropertyName, [element(Name, [], ElementContent)| Content], Content, [Property]) :-
		!,
		call(Parser, ElementContent, Value),
		Property =.. [PropertyName, Value].
	optional_complex2(_, _, _, Content, Content, []).

	:- meta_predicate(optional_complex3(*, 3, *, *, *, *)).

	optional_complex3(Name, Parser, PropertyName, [element(Name, Attributes, ElementContent)| Content], Content, [Property]) :-
		!,
		call(Parser, ElementContent, Attributes, Value),
		Property =.. [PropertyName, Value].
	optional_complex3(_, _, _, Content, Content, []).

	:- meta_predicate(repeated_complex(*, 3, *, *, *)).

	repeated_complex(Name, Parser, [element(Name, Attributes, ElementContent)| Content0], Content, [Value| Values]) :-
		!,
		call(Parser, ElementContent, Attributes, Value),
		repeated_complex(Name, Parser, Content0, Content, Values).
	repeated_complex(_, _, Content, Content, []).

	optional_extensions([element(extensions, [], Nodes)| Content], Content, [extensions(Nodes)]) :-
		valid_extension_nodes(Nodes),
		!.
	optional_extensions(Content, Content, []).

	optional_list_property(_, [], []) :-
		!.
	optional_list_property(Name, Values, [Property]) :-
		Values = [_|_],
		Property =.. [Name, Values].

	take_optional(Name, Properties0, Properties, Value) :-
		select(Property, Properties0, Properties1),
		Property =.. [Name, Value],
		!,
		\+ property_named(Name, Properties1), Properties = Properties1.
	take_optional(_, Properties, Properties, absent(gpx_property)).

	take_list(Name, Properties0, Properties, Values) :-
		take_optional(Name, Properties0, Properties, Value),
		(	Value == absent(gpx_property) ->
			Values = []
		;	is_list(Value),
			Values = Value
		).

	take_scalar(Name, Kind, Properties0, Properties, Value) :-
		take_optional(Name, Properties0, Properties, Optional),
		(	Optional == absent(gpx_property) ->
			Value = absent(gpx_property)
		;	valid_scalar(Kind, Optional), Value = Optional
		).

	take_extensions(Properties0, Properties, Content) :-
		take_optional(extensions, Properties0, Properties, Nodes),
		(	Nodes == absent(gpx_property) ->
			Content = []
		;	valid_extension_nodes(Nodes),
			Content = [element(extensions, [], Nodes)]
		).

	property_named(Name, [Property| _]) :-
		functor(Property, Name, 1),
		!.
	property_named(Name, [_| Properties]) :-
		property_named(Name, Properties).

	:- meta_predicate(optional_complex_node2(*, 2, *, *)).

	optional_complex_node2(_, _, absent(gpx_property), []) :-
		!.
	optional_complex_node2(Name, Generator, Value, [element(Name, [], Content)]) :-
		Value \== absent(gpx_property),
		call(Generator, Value, Content).

	:- meta_predicate(optional_complex_node3(*, 3, *, *)).

	optional_complex_node3(_, _, absent(gpx_property), []) :-
		!.
	optional_complex_node3(Name, Generator, Value, [element(Name, Attributes, Content)]) :-
		Value \== absent(gpx_property),
		call(Generator, Value, Content, Attributes).

	:- meta_predicate(complex_nodes(*, 3, *, *)).

	complex_nodes(_, _, [], []) :-
		!.
	complex_nodes(Name, Generator, [Value| Values], [element(Name, Attributes, Content)| Nodes]) :-
		call(Generator, Value, Content, Attributes),
		complex_nodes(Name, Generator, Values, Nodes).

	scalar_node(_, absent(gpx_property), []) :-
		!.
	scalar_node(Name, Value, [element(Name, [], [pcdata(Codes)])]) :-
		scalar_codes(Value, Codes).

	properties_scalars([], Properties, Properties, []).
	properties_scalars([PropertyName-ElementName-Kind| Specs], Properties0, Properties, Content) :-
		take_scalar(PropertyName, Kind, Properties0, Properties1, Value),
		scalar_node(ElementName, Value, ValueContent),
		properties_scalars(Specs, Properties1, Properties, RemainingContent),
		append(ValueContent, RemainingContent, Content).

	scalar_value(atom, Codes, Value) :-
		atom_codes(Value, Codes).
	scalar_value(date_time, Codes, Value) :-
		atom_codes(Value, Codes), valid_date_time_codes(Codes).
	scalar_value(g_year, Codes, Value) :-
		atom_codes(Value, Codes), valid_g_year_codes(Codes).
	scalar_value(number, Codes, Value) :-
		number_codes(Value, Codes).
	scalar_value(degrees, Codes, Value) :-
		number_codes(Value, Codes), valid_degrees(Value).
	scalar_value(nonnegative_integer, Codes, Value) :-
		number_codes(Value, Codes), valid_nonnegative_integer(Value).
	scalar_value(dgps_station, Codes, Value) :-
		number_codes(Value, Codes), integer(Value), Value >= 0, Value =< 1023.
	scalar_value(fix, Codes, Value) :-
		atom_codes(Value, Codes), valid_fix(Value).

	valid_scalar(atom, Value) :-
		atom(Value).
	valid_scalar(date_time, Value) :-
		atom(Value),
		atom_codes(Value, Codes),
		valid_date_time_codes(Codes).
	valid_scalar(g_year, Value) :-
		atom(Value),
		atom_codes(Value, Codes),
		valid_g_year_codes(Codes).
	valid_scalar(number, Value) :-
		number(Value).
	valid_scalar(latitude, Value) :-
		valid_latitude(Value).
	valid_scalar(longitude, Value) :-
		valid_longitude(Value).
	valid_scalar(degrees, Value) :-
		valid_degrees(Value).
	valid_scalar(nonnegative_integer, Value) :-
		valid_nonnegative_integer(Value).
	valid_scalar(dgps_station, Value) :-
		integer(Value), Value >= 0, Value =< 1023.
	valid_scalar(fix, Value) :-
		valid_fix(Value).

	scalar_codes(Value, Codes) :-
		atom(Value),
		!,
		atom_codes(Value, Codes).
	scalar_codes(Value, Codes) :-
		number(Value),
		number_codes(Value, Codes).

	valid_latitude(Value) :-
		number(Value), Value >= -90, Value =< 90.

	valid_longitude(Value) :-
		number(Value), Value >= -180, Value < 180.

	valid_degrees(Value) :-
		number(Value), Value >= 0, Value < 360.

	valid_nonnegative_integer(Value) :-
		integer(Value), Value >= 0.

	valid_fix(none).
	valid_fix('2d').
	valid_fix('3d').
	valid_fix(dgps).
	valid_fix(pps).

	valid_date_time_codes(Codes) :-
		year_codes(Codes, Year, Rest),
		Rest = [0'-,M1,M2,0'-,D1,D2,0'T,H1,H2,0':,N1,N2,0':,S1,S2| Suffix],
		two_digits(M1, M2, Month),
		two_digits(D1, D2, Day),
		two_digits(H1, H2, Hour),
		two_digits(N1, N2, Minute),
		two_digits(S1, S2, Second),
		date::valid(Year, Month, Day),
		Minute >= 0, Minute =< 59,
		Second >= 0, Second =< 60,
		date_time_suffix(Suffix, Fraction, Zone),
		valid_time(Hour, Minute, Second, Fraction),
		valid_time_zone(Zone).

	valid_g_year_codes(Codes) :-
		year_codes(Codes, _, Zone),
		valid_time_zone(Zone).

	year_codes([0'-| Codes], Year, Rest) :-
		!,
		take_digits(Codes, Digits, Rest),
		valid_year_digits(Digits),
		number_codes(AbsoluteYear, Digits),
		Year is -AbsoluteYear.
	year_codes(Codes, Year, Rest) :-
		take_digits(Codes, Digits, Rest),
		valid_year_digits(Digits),
		number_codes(Year, Digits).

	take_digits([Code| Codes], [Code| Digits], Rest) :-
		digit(Code),
		!,
		take_digits(Codes, Digits, Rest).
	take_digits(Rest, [], Rest).

	valid_year_digits([D1,D2,D3,D4| Digits]) :-
		all_digits(Digits),
		\+ all_zero_digits([D1,D2,D3,D4| Digits]),
		(	Digits == [] ->
			true
		;	D1 =\= 0'0
		).

	all_digits([]).
	all_digits([Code| Codes]) :-
		digit(Code), all_digits(Codes).

	all_zero_digits([]).
	all_zero_digits([0'0| Codes]) :-
		all_zero_digits(Codes).

	digit(Code) :-
		Code >= 0'0, Code =< 0'9.

	two_digits(D1, D2, Value) :-
		digit(D1), digit(D2),
		Value is (D1 - 0'0) * 10 + D2 - 0'0.

	date_time_suffix([0'.| Codes], Fraction, Zone) :-
		!,
		take_digits(Codes, Fraction, Zone),
		Fraction = [_|_].
	date_time_suffix(Zone, [], Zone).

	valid_time(Hour, Minute, Second, Fraction) :-
		Hour >= 0, Hour =< 23,
		!,
		Minute >= 0, Minute =< 59,
		Second >= 0, Second =< 60,
		all_digits(Fraction).
	valid_time(24, 0, 0, Fraction) :-
		all_zero_digits(Fraction).

	valid_time_zone([]).
	valid_time_zone([0'Z]).
	valid_time_zone([Sign,H1,H2,0':,M1,M2]) :-
		(Sign =:= 0'+ -> true; Sign =:= 0'-),
		two_digits(H1, H2, Hour),
		two_digits(M1, M2, Minute),
		Hour >= 0, Hour =< 14,
		Minute >= 0, Minute =< 59,
		(	Hour =:= 14 ->
			Minute =:= 0
		;	true
		).

	valid_extension_nodes([]).
	valid_extension_nodes([namespace(URI, Prefix, Element)| Nodes]) :-
		URI \== 'http://www.topografix.com/GPX/1/1',
		xml::parse(_, xml([], [namespace(URI, Prefix, Element)])),
		valid_extension_nodes(Nodes).

	valid_source(file(_)).
	valid_source(stream(_)).
	valid_source(codes(_)).
	valid_source(chars(_)).
	valid_source(atom(_)).

	valid_sink(file(_)).
	valid_sink(stream(_)).
	valid_sink(codes(_)).
	valid_sink(chars(_)).
	valid_sink(atom(_)).

	source_codes(Source, _) :-
		\+ ground(Source),
		instantiation_error.
	source_codes(file(File), Codes) :-
		!,
		file_to_codes(File, Codes).
	source_codes(stream(Stream), Codes) :-
		!,
		stream_to_codes(Stream, Codes).
	source_codes(codes(Codes), Codes) :-
		!.
	source_codes(chars(Chars), Codes) :-
		!,
		chars_to_codes(Chars, Codes).
	source_codes(atom(Atom), Codes) :-
		!,
		atom_codes(Atom, Codes).
	source_codes(Source, _) :-
		domain_error(gpx_source, Source).

	write_sink(file(File), Codes) :-
		open(File, write, Stream),
		catch(write_codes(Codes, Stream), Error, (close(Stream), throw(Error))),
		close(Stream).
	write_sink(stream(Stream), Codes) :-
		write_codes(Codes, Stream).
	write_sink(codes(Codes), Codes).
	write_sink(chars(Chars), Codes) :-
		codes_to_chars(Codes, Chars).
	write_sink(atom(Atom), Codes) :-
		atom_codes(Atom, Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

:- end_object.
