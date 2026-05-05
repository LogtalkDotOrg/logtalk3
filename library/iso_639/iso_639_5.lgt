:- encoding('UTF-8').

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


:- object(iso_639_5).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 639-5 fact table.',
		remarks is [
			'Source update' - '2026-04-15',
			'Generated entries' - '115',
			'Source' - 'Library of Congress ISO 639-5 identifier list'
		]
	]).

	:- public(language_group/2).
	:- mode(language_group(?atom, ?atom), zero_or_more).
	:- info(language_group/2, [
		comment is 'Generated ISO 639-5 fact table.',
		argnames is ['Code', 'Name']
	]).

	language_group('aav', 'Austro-Asiatic languages').
	language_group('afa', 'Afro-Asiatic languages').
	language_group('alg', 'Algonquian languages').
	language_group('alv', 'Atlantic-Congo languages').
	language_group('apa', 'Apache languages').
	language_group('aqa', 'Alacalufan languages').
	language_group('aql', 'Algic languages').
	language_group('art', 'Artificial languages').
	language_group('ath', 'Athapascan languages').
	language_group('auf', 'Arauan languages').
	language_group('aus', 'Australian languages').
	language_group('awd', 'Arawakan languages').
	language_group('azc', 'Uto-Aztecan languages').
	language_group('bad', 'Banda languages').
	language_group('bai', 'Bamileke languages').
	language_group('bat', 'Baltic languages').
	language_group('ber', 'Berber languages').
	language_group('bih', 'Bihari languages').
	language_group('bnt', 'Bantu languages').
	language_group('btk', 'Batak languages').
	language_group('cai', 'Central American Indian languages').
	language_group('cau', 'Caucasian languages').
	language_group('cba', 'Chibchan languages').
	language_group('ccn', 'North Caucasian languages').
	language_group('ccs', 'South Caucasian languages').
	language_group('cdc', 'Chadic languages').
	language_group('cdd', 'Caddoan languages').
	language_group('cel', 'Celtic languages').
	language_group('cmc', 'Chamic languages').
	language_group('cpe', 'Creoles and pidgins, English‑based').
	language_group('cpf', 'Creoles and pidgins, French‑based').
	language_group('cpp', 'Creoles and pidgins, Portuguese-based').
	language_group('crp', 'Creoles and pidgins').
	language_group('csu', 'Central Sudanic languages').
	language_group('cus', 'Cushitic languages').
	language_group('day', 'Land Dayak languages').
	language_group('dmn', 'Mande languages').
	language_group('dra', 'Dravidian languages').
	language_group('egx', 'Egyptian languages').
	language_group('esx', 'Eskimo-Aleut languages').
	language_group('euq', 'Basque (family)').
	language_group('fiu', 'Finno-Ugrian languages').
	language_group('fox', 'Formosan languages').
	language_group('gem', 'Germanic languages').
	language_group('gme', 'East Germanic languages').
	language_group('gmq', 'North Germanic languages').
	language_group('gmw', 'West Germanic languages').
	language_group('grk', 'Greek languages').
	language_group('hmx', 'Hmong-Mien languages').
	language_group('hok', 'Hokan languages').
	language_group('hyx', 'Armenian (family)').
	language_group('iir', 'Indo-Iranian languages').
	language_group('ijo', 'Ijo languages').
	language_group('inc', 'Indic languages').
	language_group('ine', 'Indo-European languages').
	language_group('ira', 'Iranian languages').
	language_group('iro', 'Iroquoian languages').
	language_group('itc', 'Italic languages').
	language_group('jpx', 'Japanese (family)').
	language_group('kar', 'Karen languages').
	language_group('kdo', 'Kordofanian languages').
	language_group('khi', 'Khoisan languages').
	language_group('kro', 'Kru languages').
	language_group('map', 'Austronesian languages').
	language_group('mkh', 'Mon-Khmer languages').
	language_group('mno', 'Manobo languages').
	language_group('mun', 'Munda languages').
	language_group('myn', 'Mayan languages').
	language_group('nah', 'Nahuatl languages').
	language_group('nai', 'North American Indian languages').
	language_group('ngf', 'Trans-New Guinea languages').
	language_group('nic', 'Niger-Kordofanian languages').
	language_group('nub', 'Nubian languages').
	language_group('omq', 'Oto-Manguean languages').
	language_group('omv', 'Omotic languages').
	language_group('oto', 'Otomian languages').
	language_group('paa', 'Papuan languages').
	language_group('phi', 'Philippine languages').
	language_group('plf', 'Central Malayo-Polynesian languages').
	language_group('poz', 'Malayo-Polynesian languages').
	language_group('pqe', 'Eastern Malayo-Polynesian languages').
	language_group('pqw', 'Western Malayo-Polynesian languages').
	language_group('pra', 'Prakrit languages').
	language_group('qwe', 'Quechuan (family)').
	language_group('roa', 'Romance languages').
	language_group('sai', 'South American Indian languages').
	language_group('sal', 'Salishan languages').
	language_group('sdv', 'Eastern Sudanic languages').
	language_group('sem', 'Semitic languages').
	language_group('sgn', 'sign languages').
	language_group('sio', 'Siouan languages').
	language_group('sit', 'Sino-Tibetan languages').
	language_group('sla', 'Slavic languages').
	language_group('smi', 'Sami languages').
	language_group('son', 'Songhai languages').
	language_group('sqj', 'Albanian languages').
	language_group('ssa', 'Nilo-Saharan languages').
	language_group('syd', 'Samoyedic languages').
	language_group('tai', 'Tai languages').
	language_group('tbq', 'Tibeto-Burman languages').
	language_group('trk', 'Turkic languages').
	language_group('tup', 'Tupi languages').
	language_group('tut', 'Altaic languages').
	language_group('tuw', 'Tungus languages').
	language_group('urj', 'Uralic languages').
	language_group('wak', 'Wakashan languages').
	language_group('wen', 'Sorbian languages').
	language_group('xgn', 'Mongolian languages').
	language_group('xnd', 'Na-Dene languages').
	language_group('ypk', 'Yupik languages').
	language_group('zhx', 'Chinese (family)').
	language_group('zle', 'East Slavic languages').
	language_group('zls', 'South Slavic languages').
	language_group('zlw', 'West Slavic languages').
	language_group('znd', 'Zande languages').

:- end_object.
