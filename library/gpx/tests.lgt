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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'Unit tests for the ``gpx`` library.'
	]).

	:- uses(gpx, [
		parse/2, generate/2, validate/1, validate/2, xml_to_gpx/2, gpx_to_xml/2
	]).

	cover(gpx).

	cleanup :-
		^^clean_file('test_output.gpx'),
		^^clean_file('test_stream.gpx').

	test(gpx_parse_minimal_atom_01, deterministic(GPX == gpx('Logtalk', []))) :-
		parse(atom('<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="Logtalk"/>'), GPX).

	test(gpx_generate_minimal_atom_01, deterministic(Parsed == gpx('Logtalk', []))) :-
		generate(atom(Atom), gpx('Logtalk', [])),
		parse(atom(Atom), Parsed).

	test(gpx_generate_variable_term_01, error(instantiation_error)) :-
		generate(atom(_), _).

	test(gpx_generate_nonground_term_01, error(instantiation_error)) :-
		generate(atom(_), gpx('Logtalk', [_])).

	test(gpx_parse_variable_source_01, error(instantiation_error)) :-
		parse(_, _).

	test(gpx_parse_variable_wrapped_source_01, error(instantiation_error)) :-
		parse(codes(_), _).

	test(gpx_parse_invalid_source_01, error(domain_error(gpx_source, foo))) :-
		parse(foo, _).

	test(gpx_parse_invalid_document_01, error(domain_error(gpx, _))) :-
		parse(atom('<foo/>'), _).

	test(gpx_generate_invalid_sink_01, error(domain_error(gpx_sink, foo))) :-
		generate(foo, gpx('Logtalk', [])).

	test(gpx_generate_invalid_term_01, error(domain_error(gpx, foo))) :-
		generate(atom(_), foo).

	test(gpx_xml_to_gpx_variable_01, error(instantiation_error)) :-
		xml_to_gpx(_, _).

	test(gpx_xml_to_gpx_invalid_01, error(domain_error(gpx, foo))) :-
		xml_to_gpx(foo, _).

	test(gpx_gpx_to_xml_variable_01, error(instantiation_error)) :-
		gpx_to_xml(_, _).

	test(gpx_gpx_to_xml_invalid_01, error(domain_error(gpx, foo))) :-
		gpx_to_xml(foo, _).

	test(gpx_sources_codes_chars_01, deterministic(CodesGPX-CharsGPX == Expected-Expected)) :-
		Atom = '<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="Logtalk"/>',
		atom_codes(Atom, Codes),
		atom_chars(Atom, Chars),
		parse(codes(Codes), CodesGPX),
		parse(chars(Chars), CharsGPX),
		Expected = gpx('Logtalk', []).

	test(gpx_sinks_codes_chars_01, deterministic(CodesGPX-CharsGPX == Expected-Expected)) :-
		Expected = gpx('Logtalk', []),
		generate(codes(Codes), Expected),
		generate(chars(Chars), Expected),
		parse(codes(Codes), CodesGPX),
		parse(chars(Chars), CharsGPX).

	test(gpx_file_source_and_sink_01, deterministic(Parsed == GPX)) :-
		GPX = gpx('Logtalk', []),
		^^file_path('test_output.gpx', Path),
		generate(file(Path), GPX),
		parse(file(Path), Parsed).

	test(gpx_stream_source_and_sink_01, deterministic(Parsed == GPX)) :-
		GPX = gpx('Logtalk', []),
		^^file_path('test_stream.gpx', Path),
		open(Path, write, Output),
		generate(stream(Output), GPX),
		close(Output),
		open(Path, read, Input),
		parse(stream(Input), Parsed),
		close(Input).

	test(gpx_validate_minimal_01, deterministic) :-
		validate(gpx('Logtalk', [])).

	test(gpx_validate_variable_01, error(instantiation_error)) :-
		validate(_, _).

	test(gpx_validate_invalid_term_01, deterministic(Errors == [invalid_gpx_term([])])) :-
		validate(foo, Errors).

	test(gpx_validate_invalid_properties_01, deterministic(Errors == [invalid_properties([properties])])) :-
		validate(gpx('Logtalk', foo), Errors).

	test(gpx_validate_invalid_creator_01, deterministic(Errors == [invalid_creator([creator])])) :-
		validate(gpx(42, []), Errors).

	test(gpx_xml_round_trip_01, deterministic(Parsed == GPX)) :-
		GPX = gpx('Logtalk', []),
		gpx_to_xml(GPX, XML),
		xml_to_gpx(XML, Parsed).

	test(gpx_parse_metadata_01, deterministic(GPX == gpx('Logtalk', [metadata(metadata([
		name('Morning ride'),
		description('A short route'),
		author(person([name('Paulo'), email(email(pmoura, 'logtalk.org'))])),
		copyright(copyright('Paulo Moura', [year('2026'), license('https://example.com/license')])),
		links([link('https://logtalk.org', [text('Logtalk'), type('text/html')])]),
		time('2026-08-02T12:30:00Z'),
		keywords('cycling,gpx'),
		bounds(bounds(38.0, -9.0, 39.0, -8.0))
	]))]))) :-
		parse(atom('<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="Logtalk"><metadata><name>Morning ride</name><desc>A short route</desc><author><name>Paulo</name><email id="pmoura" domain="logtalk.org"/></author><copyright author="Paulo Moura"><year>2026</year><license>https://example.com/license</license></copyright><link href="https://logtalk.org"><text>Logtalk</text><type>text/html</type></link><time>2026-08-02T12:30:00Z</time><keywords>cycling,gpx</keywords><bounds minlat="38.0" minlon="-9.0" maxlat="39.0" maxlon="-8.0"/></metadata></gpx>'), GPX).

	test(gpx_parse_waypoint_01, deterministic(GPX == gpx('Logtalk', [waypoints([
		waypoint(geographic(38.7, -9.1), [elevation(42.5), time('2026-08-02T12:30:00Z'), name('Start'), fix('3d'), satellites(8), hdop(0.9), dgps_station(12)])
	])]))) :-
		parse(atom('<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="Logtalk"><wpt lat="38.7" lon="-9.1"><ele>42.5</ele><time>2026-08-02T12:30:00Z</time><name>Start</name><fix>3d</fix><sat>8</sat><hdop>0.9</hdop><dgpsid>12</dgpsid></wpt></gpx>'), GPX).

	test(gpx_parse_route_track_01, deterministic(GPX == gpx('Logtalk', [
		routes([route([waypoint(geographic(38.0, -9.0), [])], [name('Route 1'), number(1)])]),
		tracks([track([track_segment([waypoint(geographic(38.1, -9.1), []), waypoint(geographic(38.2, -9.2), [])], [])], [name('Track 1')])])
	]))) :-
		parse(atom('<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="Logtalk"><rte><name>Route 1</name><number>1</number><rtept lat="38.0" lon="-9.0"/></rte><trk><name>Track 1</name><trkseg><trkpt lat="38.1" lon="-9.1"/><trkpt lat="38.2" lon="-9.2"/></trkseg></trk></gpx>'), GPX).

	test(gpx_generate_full_round_trip_01, deterministic(Parsed == gpx('Logtalk', [
		metadata(metadata([name('Sample'), keywords(gpx)])),
		waypoints([waypoint(geographic(38.0, -9.0), [name('Point'), links([link('https://example.com', [])])])]),
		tracks([track([track_segment([waypoint(geographic(38.1, -9.1), [elevation(10)])], [])], [name('Track'), type(hike)])])
	]))) :-
		GPX = gpx('Logtalk', [
			tracks([track([track_segment([waypoint(geographic(38.1, -9.1), [elevation(10)])], [])], [type(hike), name('Track')])]),
			waypoints([waypoint(geographic(38.0, -9.0), [name('Point'), links([link('https://example.com', [])])])]),
			metadata(metadata([keywords(gpx), name('Sample')]))
		]),
		generate(atom(Atom), GPX),
		parse(atom(Atom), Parsed).

	test(gpx_generate_schema_complete_round_trip_01, deterministic(Parsed == GPX)) :-
		Extension = namespace('https://example.com/ext', [0'x], element(data, [], [pcdata([0'4,0'2])])),
		PointProperties = [
			elevation(10.5), time('2026-08-02T12:30:00Z'), magnetic_variation(359.5), geoid_height(2.5),
			name('Point'), comment('Comment'), description('Description'), source('Survey'),
			links([link('https://example.com/point', [text('Point link'), type('text/html')])]),
			symbol('Flag'), type('Waypoint'), fix(none), satellites(8), hdop(0.9), vdop(1.0), pdop(1.2),
			age_of_dgps_data(0.5), dgps_station(1023), extensions([Extension])
		],
		RouteTrackProperties = [
			name('Path'), comment('Comment'), description('Description'), source('Survey'),
			links([link('https://example.com/path', [text('Path link'), type('text/html')])]),
			number(1), type('Hike'), extensions([Extension])
		],
		GPX = gpx('Logtalk', [
			metadata(metadata([
				name('Sample'), description('Complete GPX'),
				author(person([name('Paulo'), email(email(pmoura, 'logtalk.org')), link(link('https://logtalk.org', []))])),
				copyright(copyright('Paulo Moura', [year('2026'), license('https://example.com/license')])),
				links([link('https://example.com', [text('Example'), type('text/html')])]),
				time('2026-08-02T12:30:00Z'), keywords('gpx,complete'), bounds(bounds(-90, -180, 90, 179.5)),
				extensions([Extension])
			])),
			waypoints([
				waypoint(geographic(38.0, -9.0), PointProperties),
				waypoint(geographic(38.1, -9.1), [fix('2d')]),
				waypoint(geographic(38.2, -9.2), [fix('3d')]),
				waypoint(geographic(38.3, -9.3), [fix(dgps)]),
				waypoint(geographic(38.4, -9.4), [fix(pps)])
			]),
			routes([route([waypoint(geographic(38.5, -9.5), [])], RouteTrackProperties)]),
			tracks([track([track_segment([waypoint(geographic(38.6, -9.6), [])], [extensions([Extension])])], RouteTrackProperties)]),
			extensions([Extension])
		]),
		validate(GPX),
		generate(atom(Atom), GPX),
		parse(atom(Atom), Parsed).

	test(gpx_validate_longitude_upper_bound_01, false) :-
		validate(gpx('Logtalk', [waypoints([waypoint(geographic(0, 180), [])])])).

	test(gpx_validate_empty_route_and_segment_01, deterministic) :-
		validate(gpx('Logtalk', [routes([route([], [])]), tracks([track([track_segment([], [])], [])])])).

	test(gpx_validate_nested_time_path_01, deterministic(Errors == [invalid_date_time('2026-02-29T12:00:00Z', [tracks,0,segments,0,points,0,time])])) :-
		validate(gpx('Logtalk', [tracks([track([track_segment([
			waypoint(geographic(0, 0), [time('2026-02-29T12:00:00Z')])
		], [])], [])])]), Errors).

	test(gpx_validate_nested_year_path_01, deterministic(Errors == [invalid_year('0000', [metadata,copyright,year])])) :-
		validate(gpx('Logtalk', [metadata(metadata([
			copyright(copyright('Logtalk', [year('0000')]))
		]))]), Errors).

	test(gpx_validate_accumulated_errors_01, deterministic(Errors == [
		invalid_creator([creator]),
		invalid_value(91, [waypoints,0,latitude]),
		invalid_value(180, [waypoints,0,longitude])
	])) :-
		validate(gpx(42, [waypoints([waypoint(geographic(91, 180), [])])]), Errors).

	test(gpx_validate_duplicate_property_path_01, deterministic(Errors == [duplicate_property(name, [metadata,name])])) :-
		validate(gpx('Logtalk', [metadata(metadata([name(one), name(two)]))]), Errors).

	test(gpx_validate_unknown_property_path_01, deterministic(Errors == [unknown_property(foo(bar), [routes,0,foo])])) :-
		validate(gpx('Logtalk', [routes([route([], [foo(bar)])])]), Errors).

	test(gpx_validate_unknown_noncompound_property_01, deterministic(Errors == [unknown_property(foo, [])])) :-
		validate(gpx('Logtalk', [foo]), Errors).

	test(gpx_validate_invalid_nested_terms_01, deterministic(Errors == [
		invalid_gpx_term([metadata,author]),
		invalid_gpx_term([metadata,copyright]),
		invalid_gpx_term([metadata,links,0]),
		invalid_gpx_term([metadata,bounds]),
		invalid_gpx_term([waypoints,0]),
		invalid_gpx_term([routes,0]),
		invalid_gpx_term([tracks,0])
	])) :-
		validate(gpx('Logtalk', [
			metadata(metadata([author(foo), copyright(foo), links([foo]), bounds(foo)])),
			waypoints([foo]), routes([foo]), tracks([foo])
		]), Errors).

	test(gpx_validate_invalid_nested_lists_01, deterministic(Errors == [
		invalid_list([waypoints]),
		invalid_list([routes,0,points]),
		invalid_list([tracks,0,segments])
	])) :-
		validate(gpx('Logtalk', [
			waypoints(foo), routes([route(foo, [])]), tracks([track(foo, [])])
		]), Errors).

	test(gpx_validate_invalid_track_segment_01, deterministic(Errors == [invalid_gpx_term([tracks,0,segments,0])])) :-
		validate(gpx('Logtalk', [tracks([track([foo], [])])]), Errors).

	test(gpx_validate_invalid_complex_values_01, deterministic(Errors == [
		invalid_value(42, [metadata,author,name]),
		invalid_value(42, [metadata,author,email,id]),
		invalid_value(42, [metadata,author,email,domain]),
		invalid_value(42, [metadata,author,link,href]),
		invalid_value(42, [metadata,copyright,author]),
		invalid_value(91, [metadata,bounds,min_latitude]),
		invalid_value(180, [metadata,bounds,min_longitude])
	])) :-
		validate(gpx('Logtalk', [metadata(metadata([
			author(person([name(42), email(email(42, 42)), link(link(42, []))])),
			copyright(copyright(42, [])),
			bounds(bounds(91, 180, 0, 0))
		]))]), Errors).

	test(gpx_validate_inverted_bounds_01, deterministic(Errors == [invalid_bounds([metadata,bounds])])) :-
		validate(gpx('Logtalk', [metadata(metadata([
			bounds(bounds(10, 0, -10, 0))
		]))]), Errors).

	test(gpx_validate_invalid_extensions_01, deterministic(Errors == [
		invalid_extensions([metadata,extensions]),
		invalid_extensions([extensions])
	])) :-
		validate(gpx('Logtalk', [
			metadata(metadata([extensions(foo)])),
			extensions([namespace('http://www.topografix.com/GPX/1/1', [], element(foo, [], []))])
		]), Errors).

	test(gpx_validate_invalid_point_scalars_01, deterministic(Errors == [
		invalid_value(360, [waypoints,0,magnetic_variation]),
		invalid_value(foo, [waypoints,0,fix]),
		invalid_value(-1, [waypoints,0,satellites]),
		invalid_value(1024, [waypoints,0,dgps_station])
	])) :-
		validate(gpx('Logtalk', [waypoints([waypoint(geographic(0, 0), [
			magnetic_variation(360), fix(foo), satellites(-1), dgps_station(1024)
		])])]), Errors).

	test(gpx_validate_date_time_lexical_valid_01, deterministic) :-
		validate(gpx('Logtalk', [waypoints([
			waypoint(geographic(0, 0), [time('2000-02-29T24:00:00.0+14:00')]),
			waypoint(geographic(0, 0), [time('-0001-12-31T23:59:60Z')]),
			waypoint(geographic(0, 0), [time('10000-01-01T00:00:00')])
		])])).

	test(gpx_validate_date_time_timezone_01, deterministic(Errors == [invalid_date_time('2026-08-02T12:30:00+14:01', [metadata,time])])) :-
		validate(gpx('Logtalk', [metadata(metadata([time('2026-08-02T12:30:00+14:01')]))]), Errors).

	test(gpx_validate_date_time_midnight_01, deterministic(Errors == [invalid_date_time('2026-08-02T24:00:00.1Z', [metadata,time])])) :-
		validate(gpx('Logtalk', [metadata(metadata([time('2026-08-02T24:00:00.1Z')]))]), Errors).

	test(gpx_validate_date_time_expanded_year_01, deterministic(Errors == [invalid_date_time('02026-08-02T12:30:00Z', [metadata,time])])) :-
		validate(gpx('Logtalk', [metadata(metadata([time('02026-08-02T12:30:00Z')]))]), Errors).

	test(gpx_validate_year_lexical_valid_01, deterministic) :-
		validate(gpx('Logtalk', [metadata(metadata([
			copyright(copyright('Logtalk', [year('-0001+05:30')]))
		]))])).

	test(gpx_validate_year_leading_zero_01, deterministic(Errors == [invalid_year('02026', [metadata,copyright,year])])) :-
		validate(gpx('Logtalk', [metadata(metadata([
			copyright(copyright('Logtalk', [year('02026')]))
		]))]), Errors).

	test(gpx_parse_wrong_root_order_01, error(domain_error(gpx, _))) :-
		parse(atom('<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1" creator="Logtalk"><trk/><wpt lat="0" lon="0"/></gpx>'), _).

	test(gpx_extension_round_trip_01, deterministic(Parsed == GPX)) :-
		parse(atom('<gpx xmlns="http://www.topografix.com/GPX/1/1" xmlns:x="https://example.com/ext" version="1.1" creator="Logtalk"><extensions><x:data unit="m">42</x:data></extensions></gpx>'), GPX),
		generate(atom(Atom), GPX),
		parse(atom(Atom), Parsed).

:- end_object.
