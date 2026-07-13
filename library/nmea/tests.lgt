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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-07-13,
		comment is 'Unit tests for the "nmea" library.'
	]).

	:- uses(nmea, [
		parse/2, parse/3, talker/2, sentence_type/2, fields/2, checksum/2, data/2
	]).

	cover(nmea).

	cleanup :-
		^^clean_file('test_nmea_input.txt'),
		^^clean_file('test_nmea_stream.txt').

	test(nmea_parse_empty_01, deterministic(Sentences == [])) :-
		parse(atom(''), Sentences).

	test(nmea_parse_blank_lines_01, deterministic(Sentences == [])) :-
		parse(codes([13, 10, 10]), Sentences).

	test(nmea_parse_chars_source_01, deterministic(Talker-Type == gp-gga)) :-
		atom_chars('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47', Chars),
		parse(chars(Chars), [Sentence]),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type).

	test(nmea_parse_codes_source_01, deterministic(Talker-Type == gp-gll)) :-
		atom_codes('$GPGLL,4916.45,N,12311.12,W,225444,A,A*5C', Codes),
		parse(codes(Codes), [Sentence]),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type).

	test(nmea_parse_file_source_01, deterministic(Talker-Type == gp-rmc)) :-
		^^file_path('test_nmea_input.txt', Path),
		^^create_text_file(Path, '$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,003.1,W,A*07'),
		parse(file(Path), [Sentence]),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type).

	test(nmea_parse_stream_source_01, deterministic(Talker-Type == gp-gsa)) :-
		^^file_path('test_nmea_stream.txt', Path),
		^^create_text_file(Path, '$GPGSA,A,3,04,05,09,12,,,,,,,,,1.8,1.0,1.5*35'),
		open(Path, read, Input),
		parse(stream(Input), [Sentence]),
		close(Input),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type).

	test(nmea_parse_var_source_01, error(instantiation_error)) :-
		parse(_, _).

	test(nmea_parse_invalid_source_01, error(domain_error(nmea_source, foo))) :-
		parse(foo, _).

	test(nmea_parse_invalid_identifier_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GP1LL,4916.45,N,12311.12,W,225444,A,A*2A'), _).

	test(nmea_parse_gga_01, deterministic(Talker-Type-Provided-Computed == gp-gga-'47'-'47')) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47'), [Sentence]),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type),
		checksum(Sentence, checksum(Provided, Computed)).

	test(nmea_fields_gga_01, deterministic(Fields == ['123519', '4807.038', 'N', '01131.000', 'E', '1', '08', '0.9', '545.4', 'M', '46.9', 'M', '', ''])) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47'), [Sentence]),
		fields(Sentence, Fields).

	test(nmea_data_gga_01, deterministic((
		Data = gga(utc_time(12, 35, 19, fraction(0, 1)), geographic(Latitude, Longitude), fix(gps, 8, 0.9), altitude(545.4, 46.9), dgps(missing, missing)),
		abs(Latitude - 48.1173) < 1.0e-6,
		abs(Longitude - 11.516666666666667) < 1.0e-6
	))) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_rmc_01, deterministic((
		Data = rmc(utc_time(12, 35, 19, fraction(0, 1)), active, geographic(Latitude, Longitude), movement(22.4, 84.4), date(1994, 3, 23), magnetic_variation(3.1, west), autonomous),
		abs(Latitude - 48.1173) < 1.0e-6,
		abs(Longitude - 11.516666666666667) < 1.0e-6
	))) :-
		parse(atom('$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,003.1,W,A*07'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gsa_01, deterministic(Data == gsa(automatic, fix_3d, [4, 5, 9, 12], dop(1.8, 1.0, 1.5), missing))) :-
		parse(atom('$GPGSA,A,3,04,05,09,12,,,,,,,,,1.8,1.0,1.5*35'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gsv_01, deterministic(Data == gsv(2, 1, 8, [satellite(1, 40, 83, 41), satellite(2, 17, 273, 43), satellite(3, 13, 172, 42), satellite(4, 9, 312, 39)]))) :-
		parse(atom('$GPGSV,2,1,08,01,40,083,41,02,17,273,43,03,13,172,42,04,09,312,39*78'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_vtg_01, deterministic(Data == vtg(track(54.7, 34.4), speed(5.5, 10.2), autonomous))) :-
		parse(atom('$GPVTG,054.7,T,034.4,M,005.5,N,010.2,K,A*25'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gll_01, deterministic((
		Data = gll(geographic(Latitude, Longitude), utc_time(22, 54, 44, fraction(0, 1)), active, autonomous),
		abs(Latitude - 49.274166666666666) < 1.0e-6,
		abs(Longitude + 123.18533333333333) < 1.0e-6
	))) :-
		parse(atom('$GPGLL,4916.45,N,12311.12,W,225444,A,A*5C'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gga_fractional_invalid_01, deterministic((
		Data = gga(utc_time(12, 35, 19, fraction(1, 4)), geographic(Latitude, Longitude), fix(invalid, 0, 99.9), altitude(missing, missing), dgps(missing, missing)),
		abs(Latitude - 48.1173) < 1.0e-6,
		abs(Longitude - 11.516666666666667) < 1.0e-6
	))) :-
		parse(atom('$GPGGA,123519.25,4807.038,N,01131.000,E,0,00,99.9,,,,,,*6C'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gga_unknown_fix_station_01, deterministic((
		Data = gga(utc_time(12, 35, 20, fraction(0, 1)), geographic(Latitude, Longitude), fix(unknown(9), 5, 1.2), altitude(10.0, 5.0), dgps(1.5, '0001')),
		abs(Latitude - 48.1173) < 1.0e-6,
		abs(Longitude - 11.516666666666667) < 1.0e-6
	))) :-
		parse(atom('$GPGGA,123520,4807.038,N,01131.000,E,9,05,1.2,10.0,M,5.0,M,1.5,0001*66'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_rmc_void_missing_date_01, deterministic((
		Data = rmc(utc_time(12, 35, 19, fraction(1, 2)), void, geographic(Latitude, Longitude), movement(0.0, 0.0), missing, missing, data_not_valid),
		abs(Latitude - 48.1173) < 1.0e-6,
		abs(Longitude - 11.516666666666667) < 1.0e-6
	))) :-
		parse(atom('$GPRMC,123519.5,V,4807.038,N,01131.000,E,000.0,000.0,,,,N*7C'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gsa_manual_system_01, deterministic(Data == gsa(manual, no_fix, [], dop(2.5, 1.3, 2.1), galileo))) :-
		parse(atom('$GPGSA,M,1,,,,,,,,,,,,,2.5,1.3,2.1,3*25'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gsa_missing_selection_01, deterministic(Data == gsa(missing, missing, [], dop(2.5, 1.3, 2.1), navic))) :-
		parse(atom('$GPGSA,,,,,,,,,,,,,,,2.5,1.3,2.1,6*5C'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gsv_missing_snr_01, deterministic(Data == gsv(1, 1, 1, [satellite(1, 40, 83, missing)]))) :-
		parse(atom('$GPGSV,1,1,01,01,40,083,*46'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_vtg_missing_courses_01, deterministic(Data == vtg(track(missing, missing), speed(0.0, 0.0), data_not_valid))) :-
		parse(atom('$GPVTG,,,,,0.0,N,0.0,K,N*35'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_vtg_differential_01, deterministic(Data == vtg(track(54.7, 34.4), speed(5.5, 10.2), differential))) :-
		parse(atom('$GPVTG,054.7,T,034.4,M,005.5,N,010.2,K,D*20'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gll_fractional_missing_mode_01, deterministic((
		Data = gll(geographic(Latitude, Longitude), utc_time(22, 54, 44, fraction(3, 4)), void, missing),
		abs(Latitude - 49.274166666666666) < 1.0e-6,
		abs(Longitude + 123.18533333333333) < 1.0e-6
	))) :-
		parse(atom('$GPGLL,4916.45,N,12311.12,W,225444.75,V,*26'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gll_missing_coordinate_01, deterministic(Data == gll(missing, utc_time(22, 54, 44, fraction(1, 2)), void, data_not_valid))) :-
		parse(atom('$GPGLL,,,,,225444.50,V,N*4E'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gll_unknown_mode_01, deterministic((
		Data = gll(geographic(Latitude, Longitude), utc_time(22, 54, 44, fraction(3, 4)), void, unknown(x)),
		abs(Latitude - 49.274166666666666) < 1.0e-6,
		abs(Longitude + 123.18533333333333) < 1.0e-6
	))) :-
		parse(atom('$GPGLL,4916.45,N,12311.12,W,225444.75,V,X*7E'), [Sentence]),
		data(Sentence, Data).

	test(nmea_data_gll_numeric_unknown_mode_01, deterministic((
		Data = gll(geographic(Latitude, Longitude), utc_time(22, 54, 44, fraction(0, 1)), void, unknown('1')),
		abs(Latitude - 49.274166666666666) < 1.0e-6,
		abs(Longitude + 123.18533333333333) < 1.0e-6
	))) :-
		parse(atom('$GPGLL,4916.45,N,12311.12,W,225444,V,1*3B'), [Sentence]),
		data(Sentence, Data).

	test(nmea_parse_missing_checksum_optional_01, deterministic(Provided-Computed == missing-'5C')) :-
		parse(atom('$GPGLL,4916.45,N,12311.12,W,225444,A,A'), [Sentence], [checksum(optional)]),
		checksum(Sentence, checksum(Provided, Computed)).

	test(nmea_parse_present_checksum_optional_01, deterministic(Provided-Computed == '47'-'47')) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47'), [Sentence], [checksum(optional)]),
		checksum(Sentence, checksum(Provided, Computed)).

	test(nmea_parse_bad_checksum_ignore_01, deterministic(Provided-Computed == '48'-'47')) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*48'), [Sentence], [checksum(ignore)]),
		checksum(Sentence, checksum(Provided, Computed)).

	test(nmea_parse_missing_checksum_required_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGLL,4916.45,N,12311.12,W,225444,A,A'), _).

	test(nmea_parse_bad_checksum_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*48'), _).

	test(nmea_parse_invalid_coordinate_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGLL,4960.00,N,12311.12,W,225444,A,A*5C'), _).

	test(nmea_parse_invalid_date_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,310299,003.1,W,A*08'), _).

	test(nmea_parse_invalid_proprietary_identifier_short_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$PX,1*15'), _).

	test(nmea_parse_invalid_proprietary_identifier_manufacturer_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$P1ABC,1*3C'), _).

	test(nmea_parse_invalid_gga_satellites_used_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,-1,0.9,545.4,M,46.9,M,,*53'), _).

	test(nmea_parse_invalid_gsa_zero_satellite_id_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGSA,A,3,00,05,09,12,,,,,,,,,1.8,1.0,1.5*31'), _).

	test(nmea_parse_invalid_gsa_negative_satellite_id_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGSA,A,3,-1,05,09,12,,,,,,,,,1.8,1.0,1.5*2D'), _).

	test(nmea_parse_invalid_gsv_message_count_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGSV,0,1,08,01,40,083,41,02,17,273,43,03,13,172,42,04,09,312,39*7A'), _).

	test(nmea_parse_invalid_gsv_satellites_in_view_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGSV,1,1,-1,01,40,083,41*5E'), _).

	test(nmea_parse_invalid_gsv_elevation_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGSV,1,1,08,01,91,083,41*46'), _).

	test(nmea_parse_invalid_gsv_impossible_cardinality_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGSV,1,1,01,01,40,083,41,02,17,273,43*76'), _).

	test(nmea_parse_invalid_gga_scientific_notation_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,1e0,545.4,M,46.9,M,,*04'), _).

	test(nmea_parse_unknown_type_keep_01, deterministic(Talker-Type == gp-hdt)) :-
		parse(atom('$GPHDT,123.4,T*31'), [Sentence]),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type).

	test(nmea_parse_unknown_type_error_01, error(domain_error(nmea_sentence, _))) :-
		parse(atom('$GPHDT,123.4,T*31'), _, [unknown_type(error)]).

	test(nmea_parse_supported_types_with_error_policy_01, deterministic(length(Sentences, 6))) :-
		parse(atom('$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47\r\n$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,003.1,W,A*07\r\n$GPGSA,A,3,04,05,09,12,,,,,,,,,1.8,1.0,1.5*35\r\n$GPGSV,2,1,08,01,40,083,41,02,17,273,43,03,13,172,42,04,09,312,39*78\r\n$GPVTG,054.7,T,034.4,M,005.5,N,010.2,K,A*25\r\n$GPGLL,4916.45,N,12311.12,W,225444,A,A*5C'), Sentences, [unknown_type(error)]).

	test(nmea_parse_proprietary_01, deterministic(Talker-Type-Fields == proprietary-grmz-['93', 'f', '3'])) :-
		parse(atom('$PGRMZ,93,f,3*21'), [Sentence]),
		talker(Sentence, Talker),
		sentence_type(Sentence, Type),
		fields(Sentence, Fields).

	test(nmea_data_proprietary_01, fail) :-
		parse(atom('$PGRMZ,93,f,3*21'), [Sentence]),
		data(Sentence, _).

:- end_object.
