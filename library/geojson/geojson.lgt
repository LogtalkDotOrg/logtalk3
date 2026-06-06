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
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.geo
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(geojson(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_),
	implements(geojson_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'GeoJSON (RFC 7946) parser, generator, and validator built on top of the ``json`` library.',
		parameters is [
			'ObjectRepresentation' - 'Object representation to be used when decoding embedded JSON objects. Possible values are ``curly`` (default) and ``list``.',
			'PairRepresentation' - 'Pair representation to be used when decoding embedded JSON objects. Possible values are ``dash`` (default), ``equal``, and ``colon``.',
			'StringRepresentation' - 'Text representation to be used when decoding JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		],
		see_also is [json, geospatial]
	]).

	:- uses(json(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_), [
		parse/2 as json_parse/2, generate/2 as json_generate/2
	]).

	:- uses(list, [
		append/2, append/3, last/2, length/2, member/2, memberchk/2, reverse/2, valid/1 as is_list/1
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(Source, GeoJSON) :-
		catch(parse_source_json(Source, JSON), error(domain_error(json_source, _), _), fail),
		!,
		json_to_geojson(JSON, GeoJSON).
	parse(Source, _) :-
		valid_source(Source),
		!,
		domain_error(geojson, Source).
	parse(Source, _) :-
		domain_error(geojson_source, Source).

	parse_source_json(file(File), JSON) :-
		json_parse(file(File), JSON),
		!.
	parse_source_json(stream(Stream), JSON) :-
		json_parse(stream(Stream), JSON),
		!.
	parse_source_json(codes(Codes), JSON) :-
		json_parse(codes(Codes), JSON),
		!.
	parse_source_json(chars(Chars), JSON) :-
		json_parse(chars(Chars), JSON),
		!.
	parse_source_json(atom(Atom), JSON) :-
		json_parse(atom(Atom), JSON),
		!.

	valid_source(file(_)).
	valid_source(stream(_)).
	valid_source(codes(_)).
	valid_source(chars(_)).
	valid_source(atom(_)).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(Sink, GeoJSON) :-
		generate_sink_json(Sink, GeoJSON),
		!.
	generate(Sink, GeoJSON) :-
		valid_sink(Sink),
		!,
		domain_error(geojson, GeoJSON).
	generate(Sink, _) :-
		domain_error(geojson_sink, Sink).

	generate_sink_json(file(File), GeoJSON) :-
		geojson_to_json(GeoJSON, JSON),
		json_generate(file(File), JSON),
		!.
	generate_sink_json(stream(Stream), GeoJSON) :-
		geojson_to_json(GeoJSON, JSON),
		json_generate(stream(Stream), JSON),
		!.
	generate_sink_json(codes(Codes), GeoJSON) :-
		geojson_to_json(GeoJSON, JSON),
		json_generate(codes(Codes), JSON),
		!.
	generate_sink_json(chars(Chars), GeoJSON) :-
		geojson_to_json(GeoJSON, JSON),
		json_generate(chars(Chars), JSON),
		!.
	generate_sink_json(atom(Atom), GeoJSON) :-
		geojson_to_json(GeoJSON, JSON),
		json_generate(atom(Atom), JSON),
		!.

	valid_sink(file(_)).
	valid_sink(stream(_)).
	valid_sink(codes(_)).
	valid_sink(chars(_)).
	valid_sink(atom(_)).

	validate(Term) :-
		validate(Term, Errors),
		Errors == [].

	validate(Term, _) :-
		var(Term),
		instantiation_error.
	validate(Term, Errors) :-
		validate_geojson(Term, [], Errors).

	json_to_geojson(JSON, _) :-
		var(JSON),
		instantiation_error.
	json_to_geojson(JSON, GeoJSON) :-
		json_object_pairs(JSON, Pairs),
		convert_geojson_object(Pairs, GeoJSON),
		validate(GeoJSON),
		!.
	json_to_geojson(JSON, _) :-
		domain_error(geojson, JSON).

	geojson_to_json(GeoJSON, _) :-
		var(GeoJSON),
		instantiation_error.
	geojson_to_json(GeoJSON, JSON) :-
		validate(GeoJSON),
		geojson_term_json(GeoJSON, JSON),
		!.
	geojson_to_json(GeoJSON, _) :-
		domain_error(geojson, GeoJSON).

	convert_geojson_object(Pairs, GeoJSON) :-
		get_pair_value(type, Pairs, TypeValue),
		string_term_to_atom(TypeValue, Type),
		convert_geojson_object(Type, Pairs, GeoJSON).
	convert_geojson_object('Point', Pairs, GeoJSON) :-
		get_pair_value(coordinates, Pairs, Coordinates),
		geometry_options_from_pairs(Pairs, [type, coordinates], Options),
		term_with_options(point(Coordinates), Options, GeoJSON).
	convert_geojson_object('MultiPoint', Pairs, GeoJSON) :-
		get_pair_value(coordinates, Pairs, Coordinates),
		geometry_options_from_pairs(Pairs, [type, coordinates], Options),
		term_with_options(multi_point(Coordinates), Options, GeoJSON).
	convert_geojson_object('LineString', Pairs, GeoJSON) :-
		get_pair_value(coordinates, Pairs, Coordinates),
		geometry_options_from_pairs(Pairs, [type, coordinates], Options),
		term_with_options(line_string(Coordinates), Options, GeoJSON).
	convert_geojson_object('MultiLineString', Pairs, GeoJSON) :-
		get_pair_value(coordinates, Pairs, Coordinates),
		geometry_options_from_pairs(Pairs, [type, coordinates], Options),
		term_with_options(multi_line_string(Coordinates), Options, GeoJSON).
	convert_geojson_object('Polygon', Pairs, GeoJSON) :-
		get_pair_value(coordinates, Pairs, Coordinates),
		geometry_options_from_pairs(Pairs, [type, coordinates], Options),
		term_with_options(polygon(Coordinates), Options, GeoJSON).
	convert_geojson_object('MultiPolygon', Pairs, GeoJSON) :-
		get_pair_value(coordinates, Pairs, Coordinates),
		geometry_options_from_pairs(Pairs, [type, coordinates], Options),
		term_with_options(multi_polygon(Coordinates), Options, GeoJSON).
	convert_geojson_object('GeometryCollection', Pairs, GeoJSON) :-
		get_pair_value(geometries, Pairs, GeometryJSON),
		convert_geometry_list(GeometryJSON, Geometries),
		geometry_options_from_pairs(Pairs, [type, geometries], Options),
		term_with_options(geometry_collection(Geometries), Options, GeoJSON).
	convert_geojson_object('Feature', Pairs, GeoJSON) :-
		get_pair_value(geometry, Pairs, GeometryJSON),
		(	GeometryJSON == @null ->
			Geometry = @null
		;	json_object_pairs(GeometryJSON, GeometryPairs),
			convert_geojson_object(GeometryPairs, Geometry),
			is_geometry_term(Geometry)
		),
		get_pair_value(properties, Pairs, Properties),
		feature_options_from_pairs(Pairs, Options),
		term_with_options(feature(Geometry, Properties), Options, GeoJSON).
	convert_geojson_object('FeatureCollection', Pairs, GeoJSON) :-
		get_pair_value(features, Pairs, FeaturesJSON),
		convert_feature_list(FeaturesJSON, Features),
		collection_options_from_pairs(Pairs, [type, features], Options),
		term_with_options(feature_collection(Features), Options, GeoJSON).

	convert_geometry_list([], []).
	convert_geometry_list([GeometryJSON| GeometryJSONs], [Geometry| Geometries]) :-
		json_object_pairs(GeometryJSON, GeometryPairs),
		convert_geojson_object(GeometryPairs, Geometry),
		is_geometry_term(Geometry),
		convert_geometry_list(GeometryJSONs, Geometries).

	convert_feature_list([], []).
	convert_feature_list([FeatureJSON| FeatureJSONs], [Feature| Features]) :-
		json_object_pairs(FeatureJSON, FeaturePairs),
		convert_geojson_object(FeaturePairs, Feature),
		is_feature_term(Feature),
		convert_feature_list(FeatureJSONs, Features).

	geometry_options_from_pairs(Pairs, Reserved, Options) :-
		common_options_from_pairs(Pairs, [bbox| Reserved], Options).

	collection_options_from_pairs(Pairs, Reserved, Options) :-
		common_options_from_pairs(Pairs, [bbox| Reserved], Options).

	feature_options_from_pairs(Pairs, Options) :-
		common_options_from_pairs(Pairs, [bbox, id, type, geometry, properties], CommonOptions),
		(	get_pair_value(id, Pairs, Id) ->
			Options = [id(Id)| CommonOptions]
		;	Options = CommonOptions
		).

	common_options_from_pairs(Pairs, Reserved, Options) :-
		reserved_keys_unique(Pairs, Reserved),
		(	get_pair_value(bbox, Pairs, BBox) ->
			Options0 = [bbox(BBox)]
		;	Options0 = []
		),
		foreign_member_pairs(Pairs, Reserved, ForeignPairs),
		(	ForeignPairs == [] ->
			Options = Options0
		;	append(Options0, [foreign_members(ForeignPairs)], Options)
		).

	foreign_member_pairs([], _, []).
	foreign_member_pairs([Pair| Pairs], Reserved, ForeignPairs) :-
		(	pair_key_atom(Pair, Key),
			memberchk(Key, Reserved) ->
			foreign_member_pairs(Pairs, Reserved, ForeignPairs)
		;	ForeignPairs = [Pair| Rest],
			foreign_member_pairs(Pairs, Reserved, Rest)
		).

	reserved_keys_unique(Pairs, Reserved) :-
		\+ duplicate_reserved_key(Reserved, Pairs).

	duplicate_reserved_key([Key| _], Pairs) :-
		get_pair_values(Pairs, Key, [_First, _Second| _]),
		!.
	duplicate_reserved_key([_| Keys], Pairs) :-
		duplicate_reserved_key(Keys, Pairs).

	term_with_options(Term, [], Term) :-
		!.
	term_with_options(Term, Options, TermWithOptions) :-
		Term =.. [Functor| Arguments],
		append(Arguments, [Options], ArgumentsWithOptions),
		TermWithOptions =.. [Functor| ArgumentsWithOptions].

	geojson_term_json(point(Position), JSON) :-
		!,
		pairs_to_object([type-'Point', coordinates-Position], JSON).
	geojson_term_json(point(Position, Options), JSON) :-
		!,
		options_pairs(Options, [type-'Point', coordinates-Position], JSON).
	geojson_term_json(multi_point(Positions), JSON) :-
		!,
		pairs_to_object([type-'MultiPoint', coordinates-Positions], JSON).
	geojson_term_json(multi_point(Positions, Options), JSON) :-
		!,
		options_pairs(Options, [type-'MultiPoint', coordinates-Positions], JSON).
	geojson_term_json(line_string(Positions), JSON) :-
		!,
		pairs_to_object([type-'LineString', coordinates-Positions], JSON).
	geojson_term_json(line_string(Positions, Options), JSON) :-
		!,
		options_pairs(Options, [type-'LineString', coordinates-Positions], JSON).
	geojson_term_json(multi_line_string(LineStrings), JSON) :-
		!,
		pairs_to_object([type-'MultiLineString', coordinates-LineStrings], JSON).
	geojson_term_json(multi_line_string(LineStrings, Options), JSON) :-
		!,
		options_pairs(Options, [type-'MultiLineString', coordinates-LineStrings], JSON).
	geojson_term_json(polygon(Rings), JSON) :-
		!,
		pairs_to_object([type-'Polygon', coordinates-Rings], JSON).
	geojson_term_json(polygon(Rings, Options), JSON) :-
		!,
		options_pairs(Options, [type-'Polygon', coordinates-Rings], JSON).
	geojson_term_json(multi_polygon(Polygons), JSON) :-
		!,
		pairs_to_object([type-'MultiPolygon', coordinates-Polygons], JSON).
	geojson_term_json(multi_polygon(Polygons, Options), JSON) :-
		!,
		options_pairs(Options, [type-'MultiPolygon', coordinates-Polygons], JSON).
	geojson_term_json(geometry_collection(Geometries), JSON) :-
		!,
		geometry_terms_json(Geometries, GeometryJSON),
		pairs_to_object([type-'GeometryCollection', geometries-GeometryJSON], JSON).
	geojson_term_json(geometry_collection(Geometries, Options), JSON) :-
		!,
		geometry_terms_json(Geometries, GeometryJSON),
		options_pairs(Options, [type-'GeometryCollection', geometries-GeometryJSON], JSON).
	geojson_term_json(feature(Geometry, Properties), JSON) :-
		!,
		feature_geometry_json(Geometry, GeometryJSON),
		pairs_to_object([type-'Feature', geometry-GeometryJSON, properties-Properties], JSON).
	geojson_term_json(feature(Geometry, Properties, Options), JSON) :-
		!,
		feature_geometry_json(Geometry, GeometryJSON),
		options_pairs(Options, [type-'Feature', geometry-GeometryJSON, properties-Properties], JSON).
	geojson_term_json(feature_collection(Features), JSON) :-
		!,
		feature_terms_json(Features, FeaturesJSON),
		pairs_to_object([type-'FeatureCollection', features-FeaturesJSON], JSON).
	geojson_term_json(feature_collection(Features, Options), JSON) :-
		!,
		feature_terms_json(Features, FeaturesJSON),
		options_pairs(Options, [type-'FeatureCollection', features-FeaturesJSON], JSON).

	feature_geometry_json(@null, @null) :-
		!.
	feature_geometry_json(Geometry, GeometryJSON) :-
		geojson_term_json(Geometry, GeometryJSON).

	geometry_terms_json([], []).
	geometry_terms_json([Geometry| Geometries], [GeometryJSON| GeometryJSONs]) :-
		geojson_term_json(Geometry, GeometryJSON),
		geometry_terms_json(Geometries, GeometryJSONs).

	feature_terms_json([], []).
	feature_terms_json([Feature| Features], [FeatureJSON| FeatureJSONs]) :-
		geojson_term_json(Feature, FeatureJSON),
		feature_terms_json(Features, FeatureJSONs).

	options_pairs(Options, BasePairs, JSON) :-
		option_pairs(Options, OptionPairs, ForeignPairs),
		append(BasePairs, OptionPairs, Pairs0),
		append(Pairs0, ForeignPairs, Pairs),
		pairs_to_object(Pairs, JSON).

	option_pairs([], [], []).
	option_pairs([bbox(BBox)| Options], [bbox-BBox| Pairs], ForeignPairs) :-
		!,
		option_pairs(Options, Pairs, ForeignPairs).
	option_pairs([id(Id)| Options], [id-Id| Pairs], ForeignPairs) :-
		!,
		option_pairs(Options, Pairs, ForeignPairs).
	option_pairs([foreign_members(ForeignMembers)| Options], Pairs, AllForeignPairs) :-
		!,
		foreign_pairs(ForeignMembers, ForeignPairs),
		append(ForeignPairs, RestForeignPairs, AllForeignPairs),
		option_pairs(Options, Pairs, RestForeignPairs).
	option_pairs([_| Options], Pairs, ForeignPairs) :-
		option_pairs(Options, Pairs, ForeignPairs).

	validate_geojson(point(Position), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_position(Position, CoordinatesPath, Errors).
	validate_geojson(point(Position, Options), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_position(Position, CoordinatesPath, PositionErrors),
		bbox_length_context(point(Position), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(PositionErrors, OptionErrors, Errors).
	validate_geojson(multi_point(Positions), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_position_array(Positions, 1, CoordinatesPath, Errors).
	validate_geojson(multi_point(Positions, Options), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_position_array(Positions, 1, CoordinatesPath, PositionErrors),
		bbox_length_context(multi_point(Positions), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(PositionErrors, OptionErrors, Errors).
	validate_geojson(line_string(Positions), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_position_array(Positions, 2, CoordinatesPath, Errors).
	validate_geojson(line_string(Positions, Options), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_position_array(Positions, 2, CoordinatesPath, PositionErrors),
		bbox_length_context(line_string(Positions), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(PositionErrors, OptionErrors, Errors).
	validate_geojson(multi_line_string(LineStrings), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_line_strings(LineStrings, CoordinatesPath, Errors).
	validate_geojson(multi_line_string(LineStrings, Options), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_line_strings(LineStrings, CoordinatesPath, LineErrors),
		bbox_length_context(multi_line_string(LineStrings), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(LineErrors, OptionErrors, Errors).
	validate_geojson(polygon(Rings), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_polygon_rings(Rings, CoordinatesPath, Errors).
	validate_geojson(polygon(Rings, Options), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_polygon_rings(Rings, CoordinatesPath, RingErrors),
		bbox_length_context(polygon(Rings), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(RingErrors, OptionErrors, Errors).
	validate_geojson(multi_polygon(Polygons), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_multi_polygon(Polygons, CoordinatesPath, Errors).
	validate_geojson(multi_polygon(Polygons, Options), Path, Errors) :-
		!,
		path_push(coordinates, Path, CoordinatesPath),
		validate_multi_polygon(Polygons, CoordinatesPath, PolygonErrors),
		bbox_length_context(multi_polygon(Polygons), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(PolygonErrors, OptionErrors, Errors).
	validate_geojson(geometry_collection(Geometries), Path, Errors) :-
		!,
		path_push(geometries, Path, GeometriesPath),
		validate_geometry_list(Geometries, GeometriesPath, Errors).
	validate_geojson(geometry_collection(Geometries, Options), Path, Errors) :-
		!,
		path_push(geometries, Path, GeometriesPath),
		validate_geometry_list(Geometries, GeometriesPath, GeometryErrors),
		bbox_length_context(geometry_collection(Geometries), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(GeometryErrors, OptionErrors, Errors).
	validate_geojson(feature(Geometry, Properties), Path, Errors) :-
		!,
		path_push(geometry, Path, GeometryPath),
		validate_nullable_geometry(Geometry, GeometryPath, GeometryErrors),
		path_push(properties, Path, PropertiesPath),
		validate_properties(Properties, PropertiesPath, PropertyErrors),
		append(GeometryErrors, PropertyErrors, Errors).
	validate_geojson(feature(Geometry, Properties, Options), Path, Errors) :-
		!,
		path_push(geometry, Path, GeometryPath),
		validate_nullable_geometry(Geometry, GeometryPath, GeometryErrors),
		path_push(properties, Path, PropertiesPath),
		validate_properties(Properties, PropertiesPath, PropertyErrors),
		bbox_length_context(feature(Geometry, Properties), BBoxLength),
		validate_options(Options, Path, [bbox, id, foreign_members], BBoxLength, OptionErrors),
		append(GeometryErrors, PropertyErrors, Errors0),
		append(Errors0, OptionErrors, Errors).
	validate_geojson(feature_collection(Features), Path, Errors) :-
		!,
		path_push(features, Path, FeaturesPath),
		validate_feature_list(Features, FeaturesPath, Errors).
	validate_geojson(feature_collection(Features, Options), Path, Errors) :-
		!,
		path_push(features, Path, FeaturesPath),
		validate_feature_list(Features, FeaturesPath, FeatureErrors),
		bbox_length_context(feature_collection(Features), BBoxLength),
		validate_options(Options, Path, [bbox, foreign_members], BBoxLength, OptionErrors),
		append(FeatureErrors, OptionErrors, Errors).
	validate_geojson(_, Path, [Error]) :-
		path_error(Path, invalid_geojson_term, Error).

	validate_nullable_geometry(@null, _, []) :-
		!.
	validate_nullable_geometry(Geometry, Path, Errors) :-
		(	is_geometry_term(Geometry) ->
			validate_geojson(Geometry, Path, Errors)
		;	path_error(Path, invalid_geometry, Error),
			Errors = [Error]
		).

	validate_properties(@null, _, []) :-
		!.
	validate_properties(Properties, Path, Errors) :-
		(	json_object_term_pairs(Properties, Pairs) ->
			validate_json_pairs(Pairs, Path, [], Errors)
		;	path_error(Path, invalid_properties, Error),
			Errors = [Error]
		).

	validate_options(Options, Path, Allowed, BBoxLength, Errors) :-
		(	is_list(Options) ->
			validate_options(Options, Path, Allowed, BBoxLength, [], Errors)
		;	path_error(Path, invalid_options, Error),
			Errors = [Error]
		).

	validate_options([], _, _, _, _, []).
	validate_options([Option| Options], Path, Allowed, BBoxLength, Seen, Errors) :-
		option_name(Option, Name),
		!,
		(	\+ member(Name, Allowed) ->
			path_error(Path, option_not_allowed(Name), Error),
			Errors = [Error| RestErrors],
			validate_options(Options, Path, Allowed, BBoxLength, Seen, RestErrors)
		;	member(Name, Seen) ->
			path_error(Path, duplicate_option(Name), Error),
			Errors = [Error| RestErrors],
			validate_options(Options, Path, Allowed, BBoxLength, Seen, RestErrors)
		;	validate_option(Option, Path, BBoxLength, OptionErrors),
			append(OptionErrors, RestErrors, Errors),
			validate_options(Options, Path, Allowed, BBoxLength, [Name| Seen], RestErrors)
		).
	validate_options([Option| Options], Path, Allowed, BBoxLength, Seen, [Error| Errors]) :-
		path_error(Path, unknown_option(Option), Error),
		validate_options(Options, Path, Allowed, BBoxLength, Seen, Errors).

	option_name(bbox(_), bbox).
	option_name(id(_), id).
	option_name(foreign_members(_), foreign_members).

	validate_option(bbox(BBox), Path, BBoxLength, Errors) :-
		path_push(bbox, Path, BBoxPath),
		validate_bbox(BBox, BBoxLength, BBoxPath, Errors).
	validate_option(id(Id), Path, _, Errors) :-
		path_push(id, Path, IdPath),
		validate_feature_id(Id, IdPath, Errors).
	validate_option(foreign_members(ForeignMembers), Path, _, Errors) :-
		path_push(foreign_members, Path, ForeignPath),
		validate_foreign_members(ForeignMembers, ForeignPath, Errors).

	validate_bbox(BBox, BBoxLength, Path, Errors) :-
		is_list(BBox),
		length(BBox, Length),
		( Length =:= 4 ; Length =:= 6 ),
		all_numbers(BBox),
		!,
		validate_bbox_dimension(Length, BBoxLength, Path, DimensionErrors),
		validate_bbox_values(BBox, Path, ValueErrors),
		append(DimensionErrors, ValueErrors, Errors).
	validate_bbox(_, _, Path, [Error]) :-
		path_error(Path, invalid_bbox, Error).

	validate_bbox_values([West, South, East, North], Path, Errors) :-
		!,
		path_push(0, Path, WestPath),
		validate_bbox_longitude(West, WestPath, WestErrors),
		path_push(1, Path, SouthPath),
		validate_bbox_latitude(South, SouthPath, SouthErrors),
		path_push(2, Path, EastPath),
		validate_bbox_longitude(East, EastPath, EastErrors),
		path_push(3, Path, NorthPath),
		validate_bbox_latitude(North, NorthPath, NorthErrors),
		(	South =< North ->
			OrderErrors = []
		;	path_error(Path, bbox_latitude_order, OrderError),
			OrderErrors = [OrderError]
		),
		append([WestErrors, SouthErrors, EastErrors, NorthErrors, OrderErrors], Errors).
	validate_bbox_values([West, South, Min, East, North, Max], Path, Errors) :-
		path_push(0, Path, WestPath),
		validate_bbox_longitude(West, WestPath, WestErrors),
		path_push(1, Path, SouthPath),
		validate_bbox_latitude(South, SouthPath, SouthErrors),
		path_push(2, Path, MinPath),
		validate_bbox_altitude(Min, MinPath, MinErrors),
		path_push(3, Path, EastPath),
		validate_bbox_longitude(East, EastPath, EastErrors),
		path_push(4, Path, NorthPath),
		validate_bbox_latitude(North, NorthPath, NorthErrors),
		path_push(5, Path, MaxPath),
		validate_bbox_altitude(Max, MaxPath, MaxErrors),
		(	South =< North ->
			LatitudeOrderErrors = []
		;	path_error(Path, bbox_latitude_order, LatitudeOrderError),
			LatitudeOrderErrors = [LatitudeOrderError]
		),
		(	Min =< Max ->
			AltitudeOrderErrors = []
		;	path_error(Path, bbox_altitude_order, AltitudeOrderError),
			AltitudeOrderErrors = [AltitudeOrderError]
		),
		append([WestErrors, SouthErrors, MinErrors, EastErrors, NorthErrors, MaxErrors, LatitudeOrderErrors, AltitudeOrderErrors], Errors).

	validate_bbox_longitude(Longitude, _, []) :-
		Longitude >= -180.0,
		Longitude =< 180.0,
		!.
	validate_bbox_longitude(_, Path, [Error]) :-
		path_error(Path, bbox_longitude_out_of_range, Error).

	validate_bbox_latitude(Latitude, _, []) :-
		Latitude >= -90.0,
		Latitude =< 90.0,
		!.
	validate_bbox_latitude(_, Path, [Error]) :-
		path_error(Path, bbox_latitude_out_of_range, Error).

	validate_bbox_altitude(_, _, []).

	validate_bbox_dimension(_, unspecified, _, []) :-
		!.
	validate_bbox_dimension(Expected, Expected, _, []) :-
		!.
	validate_bbox_dimension(Actual, Expected, Path, [Error]) :-
		path_error(Path, bbox_dimension_mismatch(Actual, Expected), Error).

	bbox_length_context(Term, Context) :-
		(	bbox_length_context_(Term, Context0) ->
			Context = Context0
		;	Context = unspecified
		).

	bbox_length_context_(point(Position), Length) :-
		bbox_length_for_position(Position, Length).
	bbox_length_context_(point(Position, _), Length) :-
		bbox_length_for_position(Position, Length).
	bbox_length_context_(multi_point(Positions), Length) :-
		bbox_length_for_positions(Positions, Length).
	bbox_length_context_(multi_point(Positions, _), Length) :-
		bbox_length_for_positions(Positions, Length).
	bbox_length_context_(line_string(Positions), Length) :-
		bbox_length_for_positions(Positions, Length).
	bbox_length_context_(line_string(Positions, _), Length) :-
		bbox_length_for_positions(Positions, Length).
	bbox_length_context_(multi_line_string(LineStrings), Length) :-
		bbox_length_for_line_strings(LineStrings, Length).
	bbox_length_context_(multi_line_string(LineStrings, _), Length) :-
		bbox_length_for_line_strings(LineStrings, Length).
	bbox_length_context_(polygon(Rings), Length) :-
		bbox_length_for_line_strings(Rings, Length).
	bbox_length_context_(polygon(Rings, _), Length) :-
		bbox_length_for_line_strings(Rings, Length).
	bbox_length_context_(multi_polygon(Polygons), Length) :-
		bbox_length_for_polygons(Polygons, Length).
	bbox_length_context_(multi_polygon(Polygons, _), Length) :-
		bbox_length_for_polygons(Polygons, Length).
	bbox_length_context_(geometry_collection(Geometries), Length) :-
		bbox_length_for_geometries(Geometries, Length).
	bbox_length_context_(geometry_collection(Geometries, _), Length) :-
		bbox_length_for_geometries(Geometries, Length).
	bbox_length_context_(feature(Geometry, _), Length) :-
		bbox_length_for_nullable_geometry(Geometry, Length).
	bbox_length_context_(feature(Geometry, _, _), Length) :-
		bbox_length_for_nullable_geometry(Geometry, Length).
	bbox_length_context_(feature_collection(Features), Length) :-
		bbox_length_for_features(Features, Length).
	bbox_length_context_(feature_collection(Features, _), Length) :-
		bbox_length_for_features(Features, Length).

	bbox_length_for_position(Position, Length) :-
		position_shape(Position, Dimension),
		(	Dimension =:= 2 ->
			Length = 4
		;	Length = 6
		).

	bbox_length_for_positions([], unspecified).
	bbox_length_for_positions([Position| Positions], Length) :-
		bbox_length_for_position(Position, PositionLength),
		bbox_length_for_positions(Positions, PositionLength, Length).

	bbox_length_for_positions([], Length, Length).
	bbox_length_for_positions([Position| Positions], Length0, Length) :-
		bbox_length_for_position(Position, PositionLength),
		combine_bbox_lengths(Length0, PositionLength, Length1),
		bbox_length_for_positions(Positions, Length1, Length).

	bbox_length_for_line_strings([], unspecified).
	bbox_length_for_line_strings([Positions| LineStrings], Length) :-
		bbox_length_for_positions(Positions, LineLength),
		bbox_length_for_line_strings(LineStrings, LineLength, Length).

	bbox_length_for_line_strings([], Length, Length).
	bbox_length_for_line_strings([Positions| LineStrings], Length0, Length) :-
		bbox_length_for_positions(Positions, LineLength),
		combine_bbox_lengths(Length0, LineLength, Length1),
		bbox_length_for_line_strings(LineStrings, Length1, Length).

	bbox_length_for_polygons([], unspecified).
	bbox_length_for_polygons([Polygon| Polygons], Length) :-
		bbox_length_for_line_strings(Polygon, PolygonLength),
		bbox_length_for_polygons(Polygons, PolygonLength, Length).

	bbox_length_for_polygons([], Length, Length).
	bbox_length_for_polygons([Polygon| Polygons], Length0, Length) :-
		bbox_length_for_line_strings(Polygon, PolygonLength),
		combine_bbox_lengths(Length0, PolygonLength, Length1),
		bbox_length_for_polygons(Polygons, Length1, Length).

	bbox_length_for_nullable_geometry(@null, unspecified) :-
		!.
	bbox_length_for_nullable_geometry(Geometry, Length) :-
		bbox_length_context_(Geometry, Length).

	bbox_length_for_geometries([], unspecified).
	bbox_length_for_geometries([Geometry| Geometries], Length) :-
		bbox_length_for_nullable_geometry(Geometry, GeometryLength),
		bbox_length_for_geometries(Geometries, GeometryLength, Length).

	bbox_length_for_geometries([], Length, Length).
	bbox_length_for_geometries([Geometry| Geometries], Length0, Length) :-
		bbox_length_for_nullable_geometry(Geometry, GeometryLength),
		combine_bbox_lengths(Length0, GeometryLength, Length1),
		bbox_length_for_geometries(Geometries, Length1, Length).

	bbox_length_for_features([], unspecified).
	bbox_length_for_features([Feature| Features], Length) :-
		bbox_length_context_(Feature, FeatureLength),
		bbox_length_for_features(Features, FeatureLength, Length).

	bbox_length_for_features([], Length, Length).
	bbox_length_for_features([Feature| Features], Length0, Length) :-
		bbox_length_context_(Feature, FeatureLength),
		combine_bbox_lengths(Length0, FeatureLength, Length1),
		bbox_length_for_features(Features, Length1, Length).

	combine_bbox_lengths(unspecified, Length, Length) :-
		!.
	combine_bbox_lengths(Length, unspecified, Length) :-
		!.
	combine_bbox_lengths(6, _, 6) :-
		!.
	combine_bbox_lengths(_, 6, 6) :-
		!.
	combine_bbox_lengths(4, 4, 4) :-
		!.

	validate_feature_id(Id, _Path, []) :-
		number(Id),
		!.
	validate_feature_id(Id, _Path, []) :-
		valid_string_term(Id),
		!.
	validate_feature_id(_, Path, [Error]) :-
		path_error(Path, invalid_id, Error).

	validate_foreign_members(ForeignMembers, Path, Errors) :-
		foreign_pairs(ForeignMembers, Pairs),
		!,
		validate_foreign_member_pairs(Pairs, Path, [], Errors).
	validate_foreign_members(_, Path, [Error]) :-
		path_error(Path, invalid_foreign_members, Error).

	validate_foreign_member_pairs([], _, _, []).
	validate_foreign_member_pairs([Pair| Pairs], Path, Seen, Errors) :-
		validate_foreign_member_pair(Pair, Path, Seen, NextSeen, PairErrors),
		append(PairErrors, RestErrors, Errors),
		validate_foreign_member_pairs(Pairs, Path, NextSeen, RestErrors).

	validate_foreign_member_pair(Pair, Path, Seen, Seen, [Error]) :-
		\+ pair_key_value(Pair, _, _),
		path_error(Path, invalid_foreign_members, Error),
		!.
	validate_foreign_member_pair(Pair, Path, Seen, NextSeen, Errors) :-
		pair_key_value(Pair, Key, Value),
		json_key_path(Key, Path, KeyAtom, KeyPath),
		validate_json_key(Key, Path, KeyErrors),
		(	KeyErrors == [] ->
			validate_unique_json_key(KeyAtom, Seen, KeyPath, DuplicateErrors),
			validate_foreign_member_key(KeyAtom, KeyPath, ForeignKeyErrors),
			validate_json_value(Value, KeyPath, ValueErrors),
			(	DuplicateErrors == [] ->
				NextSeen = [KeyAtom| Seen]
			;	NextSeen = Seen
			),
			append(DuplicateErrors, ForeignKeyErrors, Errors0),
			append(Errors0, ValueErrors, Errors)
		;	NextSeen = Seen,
			Errors = KeyErrors
		).

	validate_foreign_member_key(crs, Path, [Error]) :-
		path_error(Path, prohibited_member(crs), Error),
		!.
	validate_foreign_member_key(Key, Path, [Error]) :-
		reserved_member_name(Key),
		path_error(Path, reserved_foreign_member(Key), Error),
		!.
	validate_foreign_member_key(_, _, []).

	validate_position(Position, Path, Errors) :-
		position_shape(Position, _),
		!,
		Position = [Longitude, Latitude| _],
		path_push(0, Path, LongitudePath),
		validate_position_longitude(Longitude, LongitudePath, LongitudeErrors),
		path_push(1, Path, LatitudePath),
		validate_position_latitude(Latitude, LatitudePath, LatitudeErrors),
		append(LongitudeErrors, LatitudeErrors, Errors).
	validate_position(_, Path, [Error]) :-
		path_error(Path, invalid_position, Error).

	validate_position_longitude(Longitude, _, []) :-
		Longitude >= -180.0,
		Longitude =< 180.0,
		!.
	validate_position_longitude(_, Path, [Error]) :-
		path_error(Path, position_longitude_out_of_range, Error).

	validate_position_latitude(Latitude, _, []) :-
		Latitude >= -90.0,
		Latitude =< 90.0,
		!.
	validate_position_latitude(_, Path, [Error]) :-
		path_error(Path, position_latitude_out_of_range, Error).

	validate_json_pairs([], _, _, []).
	validate_json_pairs([Pair| Pairs], Path, Seen, Errors) :-
		validate_json_pair(Pair, Path, Seen, NextSeen, PairErrors),
		append(PairErrors, RestErrors, Errors),
		validate_json_pairs(Pairs, Path, NextSeen, RestErrors).

	validate_json_pair(Pair, Path, Seen, Seen, [Error]) :-
		\+ pair_key_value(Pair, _, _),
		path_error(Path, invalid_json_object_pair, Error),
		!.
	validate_json_pair(Pair, Path, Seen, NextSeen, Errors) :-
		pair_key_value(Pair, Key, Value),
		json_key_path(Key, Path, KeyAtom, KeyPath),
		validate_json_key(Key, Path, KeyErrors),
		(	KeyErrors == [] ->
			validate_unique_json_key(KeyAtom, Seen, KeyPath, DuplicateErrors),
			validate_json_value(Value, KeyPath, ValueErrors),
			(	DuplicateErrors == [] ->
				NextSeen = [KeyAtom| Seen]
			;	NextSeen = Seen
			),
			append(DuplicateErrors, ValueErrors, Errors)
		;	NextSeen = Seen,
			Errors = KeyErrors
		).

	validate_json_key(Key, _, []) :-
		valid_string_term(Key),
		!.
	validate_json_key(_, Path, [Error]) :-
		path_error(Path, invalid_json_key, Error).

	validate_unique_json_key(Key, Seen, Path, [Error]) :-
		memberchk(Key, Seen),
		path_error(Path, duplicate_json_object_key(Key), Error),
		!.
	validate_unique_json_key(_, _, _, []).

	validate_json_value(Value, _, []) :-
		number(Value),
		!.
	validate_json_value(@true, _, []) :-
		!.
	validate_json_value(@false, _, []) :-
		!.
	validate_json_value(@null, _, []) :-
		!.
	validate_json_value(Value, _, []) :-
		valid_string_term(Value),
		!.
	validate_json_value(Value, Path, Errors) :-
		json_object_term_pairs(Value, Pairs),
		!,
		validate_json_pairs(Pairs, Path, [], Errors).
	validate_json_value(Value, Path, Errors) :-
		is_list(Value),
		!,
		validate_json_array(Value, 0, Path, Errors).
	validate_json_value(_, Path, [Error]) :-
		path_error(Path, invalid_json_value, Error).

	validate_json_array([], _, _, []).
	validate_json_array([Value| Values], Index, Path, Errors) :-
		path_push(Index, Path, ValuePath),
		validate_json_value(Value, ValuePath, ValueErrors),
		NextIndex is Index + 1,
		append(ValueErrors, RestErrors, Errors),
		validate_json_array(Values, NextIndex, Path, RestErrors).

	validate_position_array(Positions, MinimumLength, Path, Errors) :-
		(	is_list(Positions) ->
			length(Positions, Length),
			(	Length >= MinimumLength ->
				validate_positions_same_dimension(Positions, Path, Errors)
			;	path_error(Path, insufficient_positions(MinimumLength), Error),
				Errors = [Error]
			)
		;	path_error(Path, invalid_position_array, Error),
			Errors = [Error]
		).

	validate_positions_same_dimension([], _, []).
	validate_positions_same_dimension([Position| Positions], Path, Errors) :-
		position_dimension(Position, Dimension),
		!,
		validate_positions_same_dimension([Position| Positions], 0, Dimension, Path, Errors).
	validate_positions_same_dimension(_, Path, [Error]) :-
		path_error(Path, invalid_position_array, Error).

	validate_positions_same_dimension([], _, _, _, []).
	validate_positions_same_dimension([Position| Positions], Index, Dimension, Path, Errors) :-
		path_push(Index, Path, PositionPath),
		validate_position(Position, PositionPath, PositionErrors),
		(	position_dimension(Position, Dimension) ->
			DimensionErrors = []
		;	path_error(PositionPath, inconsistent_position_dimension, Error),
			DimensionErrors = [Error]
		),
		NextIndex is Index + 1,
		append(PositionErrors, DimensionErrors, Errors0),
		append(Errors0, RestErrors, Errors),
		validate_positions_same_dimension(Positions, NextIndex, Dimension, Path, RestErrors).

	validate_line_strings(LineStrings, Path, Errors) :-
		(	is_list(LineStrings) ->
			validate_line_strings(LineStrings, 0, Path, Errors)
		;	path_error(Path, invalid_line_string_array, Error),
			Errors = [Error]
		).

	validate_line_strings([], _, _, []).
	validate_line_strings([LineString| LineStrings], Index, Path, Errors) :-
		path_push(Index, Path, LinePath),
		validate_position_array(LineString, 2, LinePath, LineErrors),
		NextIndex is Index + 1,
		append(LineErrors, RestErrors, Errors),
		validate_line_strings(LineStrings, NextIndex, Path, RestErrors).

	validate_polygon_rings(Rings, Path, Errors) :-
		(	is_list(Rings) ->
			(	Rings == [] ->
				path_error(Path, invalid_polygon, Error),
				Errors = [Error]
			;	validate_polygon_rings(Rings, 0, Path, Errors)
			)
		;	path_error(Path, invalid_polygon, Error),
			Errors = [Error]
		).

	validate_polygon_rings([], _, _, []).
	validate_polygon_rings([Ring| Rings], Index, Path, Errors) :-
		path_push(Index, Path, RingPath),
		validate_linear_ring(Ring, RingPath, RingErrors),
		NextIndex is Index + 1,
		append(RingErrors, RestErrors, Errors),
		validate_polygon_rings(Rings, NextIndex, Path, RestErrors).

	validate_linear_ring(Ring, Path, Errors) :-
		validate_position_array(Ring, 4, Path, PositionErrors),
		(	Ring = [First| _], last(Ring, Last), positions_equal(First, Last) ->
			RingClosureErrors = []
		;	path_error(Path, ring_not_closed, Error),
			RingClosureErrors = [Error]
		),
		append(PositionErrors, RingClosureErrors, Errors).

	validate_multi_polygon(Polygons, Path, Errors) :-
		(	is_list(Polygons) ->
			validate_multi_polygon(Polygons, 0, Path, Errors)
		;	path_error(Path, invalid_multi_polygon, Error),
			Errors = [Error]
		).

	validate_multi_polygon([], _, _, []).
	validate_multi_polygon([Polygon| Polygons], Index, Path, Errors) :-
		path_push(Index, Path, PolygonPath),
		validate_polygon_rings(Polygon, PolygonPath, PolygonErrors),
		NextIndex is Index + 1,
		append(PolygonErrors, RestErrors, Errors),
		validate_multi_polygon(Polygons, NextIndex, Path, RestErrors).

	validate_geometry_list(Geometries, Path, Errors) :-
		(	is_list(Geometries) ->
			validate_geometry_list(Geometries, 0, Path, Errors)
		;	path_error(Path, invalid_geometry_collection, Error),
			Errors = [Error]
		).

	validate_geometry_list([], _, _, []).
	validate_geometry_list([Geometry| Geometries], Index, Path, Errors) :-
		path_push(Index, Path, GeometryPath),
		validate_nullable_geometry(Geometry, GeometryPath, GeometryErrors0),
		(	Geometry == @null ->
			path_error(GeometryPath, invalid_geometry, Error),
			GeometryErrors = [Error]
		;	GeometryErrors = GeometryErrors0
		),
		NextIndex is Index + 1,
		append(GeometryErrors, RestErrors, Errors),
		validate_geometry_list(Geometries, NextIndex, Path, RestErrors).

	validate_feature_list(Features, Path, Errors) :-
		(	is_list(Features) ->
			validate_feature_list(Features, 0, Path, Errors)
		;	path_error(Path, invalid_feature_collection, Error),
			Errors = [Error]
		).

	validate_feature_list([], _, _, []).
	validate_feature_list([Feature| Features], Index, Path, Errors) :-
		path_push(Index, Path, FeaturePath),
		(	is_feature_term(Feature) ->
			validate_geojson(Feature, FeaturePath, FeatureErrors)
		;	path_error(FeaturePath, invalid_feature, Error),
			FeatureErrors = [Error]
		),
		NextIndex is Index + 1,
		append(FeatureErrors, RestErrors, Errors),
		validate_feature_list(Features, NextIndex, Path, RestErrors).

	path_push(Segment, Path, [Segment| Path]).

	path_error(ReversePath, Reason0, Reason) :-
		reverse(ReversePath, Path),
		(	atom(Reason0) ->
			Reason =.. [Reason0, Path]
		;	Reason0 =.. [Functor| Arguments],
			append(Arguments, [Path], ArgumentsWithPath),
			Reason =.. [Functor| ArgumentsWithPath]
		).

	position_dimension(Position, Dimension) :-
		position_shape(Position, Dimension).

	position_shape(Position, Dimension) :-
		is_list(Position),
		length(Position, Dimension),
		Dimension >= 2,
		all_numbers(Position).

	positions_equal(Position1, Position2) :-
		is_list(Position1),
		is_list(Position2),
		positions_equal_list(Position1, Position2).

	positions_equal_list([], []).
	positions_equal_list([Number1| Numbers1], [Number2| Numbers2]) :-
		Number1 =:= Number2,
		positions_equal_list(Numbers1, Numbers2).

	all_numbers([]).
	all_numbers([Number| Numbers]) :-
		number(Number),
		all_numbers(Numbers).

	is_geometry_term(point(_)).
	is_geometry_term(point(_, _)).
	is_geometry_term(multi_point(_)).
	is_geometry_term(multi_point(_, _)).
	is_geometry_term(line_string(_)).
	is_geometry_term(line_string(_, _)).
	is_geometry_term(multi_line_string(_)).
	is_geometry_term(multi_line_string(_, _)).
	is_geometry_term(polygon(_)).
	is_geometry_term(polygon(_, _)).
	is_geometry_term(multi_polygon(_)).
	is_geometry_term(multi_polygon(_, _)).
	is_geometry_term(geometry_collection(_)).
	is_geometry_term(geometry_collection(_, _)).

	is_feature_term(feature(_, _)).
	is_feature_term(feature(_, _, _)).

	json_object_pairs({}, []) :-
		!.
	json_object_pairs({Pairs}, PairsList) :-
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_pairs(json(Pairs), Pairs) :-
		!.

	json_object_term_pairs({}, []) :-
		_ObjectRepresentation_ == curly,
		!.
	json_object_term_pairs({Pairs}, PairsList) :-
		_ObjectRepresentation_ == curly,
		!,
		curly_pairs_to_list(Pairs, PairsList).
	json_object_term_pairs(json(Pairs), Pairs) :-
		_ObjectRepresentation_ == list.

	foreign_pairs(ForeignMembers, ForeignMembers) :-
		is_list(ForeignMembers),
		!.
	foreign_pairs(ForeignMembers, Pairs) :-
		json_object_term_pairs(ForeignMembers, Pairs).

	pairs_to_object([], Object) :-
		(	_ObjectRepresentation_ == list ->
			Object = json([])
		;	Object = {}
		),
		!.
	pairs_to_object(Pairs, json(Pairs)) :-
		_ObjectRepresentation_ == list,
		!.
	pairs_to_object(Pairs, {CurlyPairs}) :-
		list_to_curly_pairs(Pairs, CurlyPairs).

	curly_pairs_to_list((Pair, Rest), [Pair| Pairs]) :-
		!,
		curly_pairs_to_list(Rest, Pairs).
	curly_pairs_to_list(Pair, [Pair]).

	list_to_curly_pairs([Pair], Pair) :-
		!.
	list_to_curly_pairs([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly_pairs(Pairs, Rest).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

	pair_key_atom(Pair, Key) :-
		pair_key_value(Pair, RawKey, _),
		string_term_to_atom(RawKey, Key).

	get_pair_value(Key, Pairs, Value) :-
		get_pair_values(Pairs, Key, [Value]).

	get_pair_values([], _, []).
	get_pair_values([Pair| Pairs], Key, Values) :-
		(	pair_key_value(Pair, PairKey, Value),
			string_term_to_atom(PairKey, PairAtom),
			PairAtom == Key ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		get_pair_values(Pairs, Key, Rest).

	json_key_path(Key, Path, KeyAtom, KeyPath) :-
		(	string_term_to_atom(Key, KeyAtom) ->
			path_push(KeyAtom, Path, KeyPath)
		;	KeyAtom = Key,
			path_push(Key, Path, KeyPath)
		).

	string_term_to_atom(Atom, Atom) :-
		atom(Atom),
		!.
	string_term_to_atom(chars(Chars), Atom) :-
		valid_chars_list(Chars),
		atom_chars(Atom, Chars),
		!.
	string_term_to_atom(codes(Codes), Atom) :-
		valid_codes_list(Codes),
		atom_codes(Atom, Codes),
		!.

	valid_string_term(Term) :-
		atom(Term),
		!.
	valid_string_term(chars(Chars)) :-
		valid_chars_list(Chars),
		!.
	valid_string_term(codes(Codes)) :-
		valid_codes_list(Codes).

	valid_chars_list(Chars) :-
		is_list(Chars),
		valid_chars(Chars).

	valid_chars([]).
	valid_chars([Char| Chars]) :-
		atom(Char),
		atom_length(Char, 1),
		valid_chars(Chars).

	valid_codes_list(Codes) :-
		is_list(Codes),
		valid_codes(Codes).

	valid_codes([]).
	valid_codes([Code| Codes]) :-
		integer(Code),
		catch(char_code(_, Code), _, fail),
		valid_codes(Codes).

	reserved_member_name(type).
	reserved_member_name(coordinates).
	reserved_member_name(geometries).
	reserved_member_name(geometry).
	reserved_member_name(properties).
	reserved_member_name(features).
	reserved_member_name(id).
	reserved_member_name(bbox).

:- end_object.


:- object(geojson,
	extends(geojson(curly, dash, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'GeoJSON parser, generator, and validator using default JSON term representations.'
	]).

:- end_object.
