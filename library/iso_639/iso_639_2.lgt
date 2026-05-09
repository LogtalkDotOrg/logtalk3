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


:- object(iso_639_2).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 639-2 fact table.',
		remarks is [
			'Source update' - '2026-01-15',
			'Generated entries' - '487',
			'Source' - 'Library of Congress ISO 639-2 code list.'
		]
	]).

	:- public(language_code/5).
	:- mode(language_code(?atom, ?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(language_code/5, [
		comment is 'Generated ISO 639-2 fact table.',
		argnames is ['Bibliographic', 'Terminologic', 'Alpha2', 'Name', 'Class']
	]).

	language_code('aar', 'aar', 'aa', 'Afar', individual).
	language_code('abk', 'abk', 'ab', 'Abkhazian', individual).
	language_code('ace', 'ace', '', 'Achinese', individual).
	language_code('ach', 'ach', '', 'Acoli', individual).
	language_code('ada', 'ada', '', 'Adangme', individual).
	language_code('ady', 'ady', '', 'Adyghe; Adygei', individual).
	language_code('afa', 'afa', '', 'Afro-Asiatic languages', collective).
	language_code('afh', 'afh', '', 'Afrihili', individual).
	language_code('afr', 'afr', 'af', 'Afrikaans', individual).
	language_code('ain', 'ain', '', 'Ainu', individual).
	language_code('aka', 'aka', 'ak', 'Akan', macrolanguage).
	language_code('akk', 'akk', '', 'Akkadian', individual).
	language_code('alb', 'sqi', 'sq', 'Albanian', macrolanguage).
	language_code('ale', 'ale', '', 'Aleut', individual).
	language_code('alg', 'alg', '', 'Algonquian languages', collective).
	language_code('alt', 'alt', '', 'Southern Altai', individual).
	language_code('amh', 'amh', 'am', 'Amharic', individual).
	language_code('ang', 'ang', '', 'English, Old (ca.450-1100)', individual).
	language_code('anp', 'anp', '', 'Angika', individual).
	language_code('apa', 'apa', '', 'Apache languages', collective).
	language_code('ara', 'ara', 'ar', 'Arabic', macrolanguage).
	language_code('arc', 'arc', '', 'Official Aramaic (700-300 BCE); Imperial Aramaic (700-300 BCE)', individual).
	language_code('arg', 'arg', 'an', 'Aragonese', individual).
	language_code('arm', 'hye', 'hy', 'Armenian', individual).
	language_code('arn', 'arn', '', 'Mapudungun; Mapuche', individual).
	language_code('arp', 'arp', '', 'Arapaho', individual).
	language_code('art', 'art', '', 'Artificial languages', collective).
	language_code('arw', 'arw', '', 'Arawak', individual).
	language_code('asm', 'asm', 'as', 'Assamese', individual).
	language_code('ast', 'ast', '', 'Asturian; Bable; Leonese; Asturleonese', individual).
	language_code('ath', 'ath', '', 'Athapascan languages', collective).
	language_code('aus', 'aus', '', 'Australian languages', collective).
	language_code('ava', 'ava', 'av', 'Avaric', individual).
	language_code('ave', 'ave', 'ae', 'Avestan', individual).
	language_code('awa', 'awa', '', 'Awadhi', individual).
	language_code('aym', 'aym', 'ay', 'Aymara', macrolanguage).
	language_code('aze', 'aze', 'az', 'Azerbaijani', macrolanguage).
	language_code('bad', 'bad', '', 'Banda languages', collective).
	language_code('bai', 'bai', '', 'Bamileke languages', collective).
	language_code('bak', 'bak', 'ba', 'Bashkir', individual).
	language_code('bal', 'bal', '', 'Baluchi', macrolanguage).
	language_code('bam', 'bam', 'bm', 'Bambara', individual).
	language_code('ban', 'ban', '', 'Balinese', individual).
	language_code('baq', 'eus', 'eu', 'Basque', individual).
	language_code('bas', 'bas', '', 'Basa', individual).
	language_code('bat', 'bat', '', 'Baltic languages', collective).
	language_code('bej', 'bej', '', 'Beja; Bedawiyet', individual).
	language_code('bel', 'bel', 'be', 'Belarusian', individual).
	language_code('bem', 'bem', '', 'Bemba', individual).
	language_code('ben', 'ben', 'bn', 'Bengali', individual).
	language_code('ber', 'ber', '', 'Berber languages', collective).
	language_code('bho', 'bho', '', 'Bhojpuri', individual).
	language_code('bih', 'bih', '', 'Bihari languages', collective).
	language_code('bik', 'bik', '', 'Bikol', macrolanguage).
	language_code('bin', 'bin', '', 'Bini; Edo', individual).
	language_code('bis', 'bis', 'bi', 'Bislama', individual).
	language_code('bla', 'bla', '', 'Siksika', individual).
	language_code('bnt', 'bnt', '', 'Bantu languages', collective).
	language_code('tib', 'bod', 'bo', 'Tibetan', individual).
	language_code('bos', 'bos', 'bs', 'Bosnian', individual).
	language_code('bra', 'bra', '', 'Braj', individual).
	language_code('bre', 'bre', 'br', 'Breton', individual).
	language_code('btk', 'btk', '', 'Batak languages', collective).
	language_code('bua', 'bua', '', 'Buriat', macrolanguage).
	language_code('bug', 'bug', '', 'Buginese', individual).
	language_code('bul', 'bul', 'bg', 'Bulgarian', individual).
	language_code('bur', 'mya', 'my', 'Burmese', individual).
	language_code('byn', 'byn', '', 'Blin; Bilin', individual).
	language_code('cad', 'cad', '', 'Caddo', individual).
	language_code('cai', 'cai', '', 'Central American Indian languages', collective).
	language_code('car', 'car', '', 'Galibi Carib', individual).
	language_code('cat', 'cat', 'ca', 'Catalan; Valencian', individual).
	language_code('cau', 'cau', '', 'Caucasian languages', collective).
	language_code('ceb', 'ceb', '', 'Cebuano', individual).
	language_code('cel', 'cel', '', 'Celtic languages', collective).
	language_code('cze', 'ces', 'cs', 'Czech', individual).
	language_code('cha', 'cha', 'ch', 'Chamorro', individual).
	language_code('chb', 'chb', '', 'Chibcha', individual).
	language_code('che', 'che', 'ce', 'Chechen', individual).
	language_code('chg', 'chg', '', 'Chagatai', individual).
	language_code('chi', 'zho', 'zh', 'Chinese', macrolanguage).
	language_code('chk', 'chk', '', 'Chuukese', individual).
	language_code('chm', 'chm', '', 'Mari', macrolanguage).
	language_code('chn', 'chn', '', 'Chinook jargon', individual).
	language_code('cho', 'cho', '', 'Choctaw', individual).
	language_code('chp', 'chp', '', 'Chipewyan; Dene Suline', individual).
	language_code('chr', 'chr', '', 'Cherokee', individual).
	language_code('chu', 'chu', 'cu', 'Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic', individual).
	language_code('chv', 'chv', 'cv', 'Chuvash', individual).
	language_code('chy', 'chy', '', 'Cheyenne', individual).
	language_code('cmc', 'cmc', '', 'Chamic languages', collective).
	language_code('cnr', 'cnr', '', 'Montenegrin', individual).
	language_code('cop', 'cop', '', 'Coptic', individual).
	language_code('cor', 'cor', 'kw', 'Cornish', individual).
	language_code('cos', 'cos', 'co', 'Corsican', individual).
	language_code('cpe', 'cpe', '', 'Creoles and pidgins, English based', collective).
	language_code('cpf', 'cpf', '', 'Creoles and pidgins, French-based', collective).
	language_code('cpp', 'cpp', '', 'Creoles and pidgins, Portuguese-based', collective).
	language_code('cre', 'cre', 'cr', 'Cree', macrolanguage).
	language_code('crh', 'crh', '', 'Crimean Tatar; Crimean Turkish', individual).
	language_code('crp', 'crp', '', 'Creoles and pidgins', collective).
	language_code('csb', 'csb', '', 'Kashubian', individual).
	language_code('cus', 'cus', '', 'Cushitic languages', collective).
	language_code('wel', 'cym', 'cy', 'Welsh', individual).
	language_code('dak', 'dak', '', 'Dakota', individual).
	language_code('dan', 'dan', 'da', 'Danish', individual).
	language_code('dar', 'dar', '', 'Dargwa', individual).
	language_code('day', 'day', '', 'Land Dayak languages', collective).
	language_code('del', 'del', '', 'Delaware', macrolanguage).
	language_code('den', 'den', '', 'Slave (Athapascan)', macrolanguage).
	language_code('ger', 'deu', 'de', 'German', individual).
	language_code('dgr', 'dgr', '', 'Tlicho; Dogrib', individual).
	language_code('din', 'din', '', 'Dinka', macrolanguage).
	language_code('div', 'div', 'dv', 'Divehi; Dhivehi; Maldivian', individual).
	language_code('doi', 'doi', '', 'Dogri (macrolanguage)', macrolanguage).
	language_code('dra', 'dra', '', 'Dravidian languages', collective).
	language_code('dsb', 'dsb', '', 'Lower Sorbian', individual).
	language_code('dua', 'dua', '', 'Duala', individual).
	language_code('dum', 'dum', '', 'Dutch, Middle (ca.1050-1350)', individual).
	language_code('dut', 'nld', 'nl', 'Dutch; Flemish', individual).
	language_code('dyu', 'dyu', '', 'Dyula', individual).
	language_code('dzo', 'dzo', 'dz', 'Dzongkha', individual).
	language_code('efi', 'efi', '', 'Efik', individual).
	language_code('egy', 'egy', '', 'Egyptian (Ancient)', individual).
	language_code('eka', 'eka', '', 'Ekajuk', individual).
	language_code('gre', 'ell', 'el', 'Modern Greek (1453-)', individual).
	language_code('elx', 'elx', '', 'Elamite', individual).
	language_code('eng', 'eng', 'en', 'English', individual).
	language_code('enm', 'enm', '', 'English, Middle (1100-1500)', individual).
	language_code('epo', 'epo', 'eo', 'Esperanto', individual).
	language_code('est', 'est', 'et', 'Estonian', macrolanguage).
	language_code('ewe', 'ewe', 'ee', 'Ewe', individual).
	language_code('ewo', 'ewo', '', 'Ewondo', individual).
	language_code('fan', 'fan', '', 'Fang', individual).
	language_code('fao', 'fao', 'fo', 'Faroese', individual).
	language_code('per', 'fas', 'fa', 'Persian', macrolanguage).
	language_code('fat', 'fat', '', 'Fanti', individual).
	language_code('fij', 'fij', 'fj', 'Fijian', individual).
	language_code('fil', 'fil', '', 'Filipino; Pilipino', individual).
	language_code('fin', 'fin', 'fi', 'Finnish', individual).
	language_code('fiu', 'fiu', '', 'Finno-Ugrian languages', collective).
	language_code('fon', 'fon', '', 'Fon', individual).
	language_code('fre', 'fra', 'fr', 'French', individual).
	language_code('frm', 'frm', '', 'French, Middle (ca.1400-1600)', individual).
	language_code('fro', 'fro', '', 'French, Old (842-ca.1400)', individual).
	language_code('frr', 'frr', '', 'Northern Frisian', individual).
	language_code('frs', 'frs', '', 'Eastern Frisian', individual).
	language_code('fry', 'fry', 'fy', 'Western Frisian', individual).
	language_code('ful', 'ful', 'ff', 'Fulah', macrolanguage).
	language_code('fur', 'fur', '', 'Friulian', individual).
	language_code('gaa', 'gaa', '', 'Ga', individual).
	language_code('gay', 'gay', '', 'Gayo', individual).
	language_code('gba', 'gba', '', 'Gbaya', macrolanguage).
	language_code('gem', 'gem', '', 'Germanic languages', collective).
	language_code('geo', 'kat', 'ka', 'Georgian', individual).
	language_code('gez', 'gez', '', 'Geez', individual).
	language_code('gil', 'gil', '', 'Gilbertese', individual).
	language_code('gla', 'gla', 'gd', 'Gaelic; Scottish Gaelic', individual).
	language_code('gle', 'gle', 'ga', 'Irish', individual).
	language_code('glg', 'glg', 'gl', 'Galician', individual).
	language_code('glv', 'glv', 'gv', 'Manx', individual).
	language_code('gmh', 'gmh', '', 'German, Middle High (ca.1050-1500)', individual).
	language_code('goh', 'goh', '', 'German, Old High (ca.750-1050)', individual).
	language_code('gon', 'gon', '', 'Gondi', macrolanguage).
	language_code('gor', 'gor', '', 'Gorontalo', individual).
	language_code('got', 'got', '', 'Gothic', individual).
	language_code('grb', 'grb', '', 'Grebo', macrolanguage).
	language_code('grc', 'grc', '', 'Greek, Ancient (to 1453)', individual).
	language_code('grn', 'grn', 'gn', 'Guarani', macrolanguage).
	language_code('gsw', 'gsw', '', 'Swiss German; Alemannic; Alsatian', individual).
	language_code('guj', 'guj', 'gu', 'Gujarati', individual).
	language_code('gwi', 'gwi', '', 'Gwich''in', individual).
	language_code('hai', 'hai', '', 'Haida', macrolanguage).
	language_code('hat', 'hat', 'ht', 'Haitian; Haitian Creole', individual).
	language_code('hau', 'hau', 'ha', 'Hausa', individual).
	language_code('haw', 'haw', '', 'Hawaiian', individual).
	language_code('heb', 'heb', 'he', 'Hebrew', individual).
	language_code('her', 'her', 'hz', 'Herero', individual).
	language_code('hil', 'hil', '', 'Hiligaynon', individual).
	language_code('him', 'him', '', 'Himachali languages; Western Pahari languages', collective).
	language_code('hin', 'hin', 'hi', 'Hindi', individual).
	language_code('hit', 'hit', '', 'Hittite', individual).
	language_code('hmn', 'hmn', '', 'Hmong; Mong', macrolanguage).
	language_code('hmo', 'hmo', 'ho', 'Hiri Motu', individual).
	language_code('hrv', 'hrv', 'hr', 'Croatian', individual).
	language_code('hsb', 'hsb', '', 'Upper Sorbian', individual).
	language_code('hun', 'hun', 'hu', 'Hungarian', individual).
	language_code('hup', 'hup', '', 'Hupa', individual).
	language_code('iba', 'iba', '', 'Iban', individual).
	language_code('ibo', 'ibo', 'ig', 'Igbo', individual).
	language_code('ice', 'isl', 'is', 'Icelandic', individual).
	language_code('ido', 'ido', 'io', 'Ido', individual).
	language_code('iii', 'iii', 'ii', 'Sichuan Yi; Nuosu', individual).
	language_code('ijo', 'ijo', '', 'Ijo languages', collective).
	language_code('iku', 'iku', 'iu', 'Inuktitut', macrolanguage).
	language_code('ile', 'ile', 'ie', 'Interlingue; Occidental', individual).
	language_code('ilo', 'ilo', '', 'Iloko', individual).
	language_code('ina', 'ina', 'ia', 'Interlingua (International Auxiliary Language Association)', individual).
	language_code('inc', 'inc', '', 'Indic languages', collective).
	language_code('ind', 'ind', 'id', 'Indonesian', individual).
	language_code('ine', 'ine', '', 'Indo-European languages', collective).
	language_code('inh', 'inh', '', 'Ingush', individual).
	language_code('ipk', 'ipk', 'ik', 'Inupiaq', macrolanguage).
	language_code('ira', 'ira', '', 'Iranian languages', collective).
	language_code('iro', 'iro', '', 'Iroquoian languages', collective).
	language_code('ita', 'ita', 'it', 'Italian', individual).
	language_code('jav', 'jav', 'jv', 'Javanese', individual).
	language_code('jbo', 'jbo', '', 'Lojban', individual).
	language_code('jpn', 'jpn', 'ja', 'Japanese', individual).
	language_code('jpr', 'jpr', '', 'Judeo-Persian', individual).
	language_code('jrb', 'jrb', '', 'Judeo-Arabic', macrolanguage).
	language_code('kaa', 'kaa', '', 'Kara-Kalpak', individual).
	language_code('kab', 'kab', '', 'Kabyle', individual).
	language_code('kac', 'kac', '', 'Kachin; Jingpho', individual).
	language_code('kal', 'kal', 'kl', 'Kalaallisut; Greenlandic', individual).
	language_code('kam', 'kam', '', 'Kamba', individual).
	language_code('kan', 'kan', 'kn', 'Kannada', individual).
	language_code('kar', 'kar', '', 'Karen languages', collective).
	language_code('kas', 'kas', 'ks', 'Kashmiri', individual).
	language_code('kau', 'kau', 'kr', 'Kanuri', macrolanguage).
	language_code('kaw', 'kaw', '', 'Kawi', individual).
	language_code('kaz', 'kaz', 'kk', 'Kazakh', individual).
	language_code('kbd', 'kbd', '', 'Kabardian', individual).
	language_code('kha', 'kha', '', 'Khasi', individual).
	language_code('khi', 'khi', '', 'Khoisan languages', collective).
	language_code('khm', 'khm', 'km', 'Central Khmer', individual).
	language_code('kho', 'kho', '', 'Khotanese; Sakan', individual).
	language_code('kik', 'kik', 'ki', 'Kikuyu; Gikuyu', individual).
	language_code('kin', 'kin', 'rw', 'Kinyarwanda', individual).
	language_code('kir', 'kir', 'ky', 'Kirghiz; Kyrgyz', individual).
	language_code('kmb', 'kmb', '', 'Kimbundu', individual).
	language_code('kok', 'kok', '', 'Konkani (macrolanguage)', macrolanguage).
	language_code('kom', 'kom', 'kv', 'Komi', macrolanguage).
	language_code('kon', 'kon', 'kg', 'Kongo', macrolanguage).
	language_code('kor', 'kor', 'ko', 'Korean', individual).
	language_code('kos', 'kos', '', 'Kosraean', individual).
	language_code('kpe', 'kpe', '', 'Kpelle', macrolanguage).
	language_code('krc', 'krc', '', 'Karachay-Balkar', individual).
	language_code('krl', 'krl', '', 'Karelian', individual).
	language_code('kro', 'kro', '', 'Kru languages', collective).
	language_code('kru', 'kru', '', 'Kurukh', individual).
	language_code('kua', 'kua', 'kj', 'Kuanyama; Kwanyama', individual).
	language_code('kum', 'kum', '', 'Kumyk', individual).
	language_code('kur', 'kur', 'ku', 'Kurdish', macrolanguage).
	language_code('kut', 'kut', '', 'Kutenai', individual).
	language_code('lad', 'lad', '', 'Ladino', individual).
	language_code('lah', 'lah', '', 'Lahnda', macrolanguage).
	language_code('lam', 'lam', '', 'Lamba', individual).
	language_code('lao', 'lao', 'lo', 'Lao', individual).
	language_code('lat', 'lat', 'la', 'Latin', individual).
	language_code('lav', 'lav', 'lv', 'Latvian', macrolanguage).
	language_code('lez', 'lez', '', 'Lezghian', individual).
	language_code('lim', 'lim', 'li', 'Limburgan; Limburger; Limburgish', individual).
	language_code('lin', 'lin', 'ln', 'Lingala', individual).
	language_code('lit', 'lit', 'lt', 'Lithuanian', individual).
	language_code('lol', 'lol', '', 'Mongo', individual).
	language_code('loz', 'loz', '', 'Lozi', individual).
	language_code('ltz', 'ltz', 'lb', 'Luxembourgish; Letzeburgesch', individual).
	language_code('lua', 'lua', '', 'Luba-Lulua', individual).
	language_code('lub', 'lub', 'lu', 'Luba-Katanga', individual).
	language_code('lug', 'lug', 'lg', 'Ganda', individual).
	language_code('lui', 'lui', '', 'Luiseño', individual).
	language_code('lun', 'lun', '', 'Lunda', individual).
	language_code('luo', 'luo', '', 'Luo (Kenya and Tanzania)', individual).
	language_code('lus', 'lus', '', 'Lushai', individual).
	language_code('mac', 'mkd', 'mk', 'Macedonian', individual).
	language_code('mad', 'mad', '', 'Madurese', individual).
	language_code('mag', 'mag', '', 'Magahi', individual).
	language_code('mah', 'mah', 'mh', 'Marshallese', individual).
	language_code('mai', 'mai', '', 'Maithili', individual).
	language_code('mak', 'mak', '', 'Makasar', individual).
	language_code('mal', 'mal', 'ml', 'Malayalam', individual).
	language_code('man', 'man', '', 'Mandingo', macrolanguage).
	language_code('mao', 'mri', 'mi', 'Maori', individual).
	language_code('map', 'map', '', 'Austronesian languages', collective).
	language_code('mar', 'mar', 'mr', 'Marathi', individual).
	language_code('mas', 'mas', '', 'Masai', individual).
	language_code('may', 'msa', 'ms', 'Malay (macrolanguage)', macrolanguage).
	language_code('mdf', 'mdf', '', 'Moksha', individual).
	language_code('mdr', 'mdr', '', 'Mandar', individual).
	language_code('men', 'men', '', 'Mende', individual).
	language_code('mga', 'mga', '', 'Irish, Middle (900-1200)', individual).
	language_code('mic', 'mic', '', 'Mi''kmaq; Micmac', individual).
	language_code('min', 'min', '', 'Minangkabau', individual).
	language_code('mis', 'mis', '', 'Uncoded languages', special).
	language_code('mkh', 'mkh', '', 'Mon-Khmer languages', collective).
	language_code('mlg', 'mlg', 'mg', 'Malagasy', macrolanguage).
	language_code('mlt', 'mlt', 'mt', 'Maltese', individual).
	language_code('mnc', 'mnc', '', 'Manchu', individual).
	language_code('mni', 'mni', '', 'Manipuri', individual).
	language_code('mno', 'mno', '', 'Manobo languages', collective).
	language_code('moh', 'moh', '', 'Mohawk', individual).
	language_code('mon', 'mon', 'mn', 'Mongolian', macrolanguage).
	language_code('mos', 'mos', '', 'Mossi', individual).
	language_code('mul', 'mul', '', 'Multiple languages', special).
	language_code('mun', 'mun', '', 'Munda languages', collective).
	language_code('mus', 'mus', '', 'Creek', individual).
	language_code('mwl', 'mwl', '', 'Mirandese', individual).
	language_code('mwr', 'mwr', '', 'Marwari', macrolanguage).
	language_code('myn', 'myn', '', 'Mayan languages', collective).
	language_code('myv', 'myv', '', 'Erzya', individual).
	language_code('nah', 'nah', '', 'Nahuatl languages', collective).
	language_code('nai', 'nai', '', 'North American Indian languages', collective).
	language_code('nap', 'nap', '', 'Neapolitan', individual).
	language_code('nau', 'nau', 'na', 'Nauru', individual).
	language_code('nav', 'nav', 'nv', 'Navajo; Navaho', individual).
	language_code('nbl', 'nbl', 'nr', 'South Ndebele', individual).
	language_code('nde', 'nde', 'nd', 'North Ndebele', individual).
	language_code('ndo', 'ndo', 'ng', 'Ndonga', individual).
	language_code('nds', 'nds', '', 'Low German; Low Saxon; German, Low; Saxon, Low', individual).
	language_code('nep', 'nep', 'ne', 'Nepali (macrolanguage)', macrolanguage).
	language_code('new', 'new', '', 'Nepal Bhasa; Newar; Newari', individual).
	language_code('nia', 'nia', '', 'Nias', individual).
	language_code('nic', 'nic', '', 'Niger-Kordofanian languages', collective).
	language_code('niu', 'niu', '', 'Niuean', individual).
	language_code('nno', 'nno', 'nn', 'Norwegian Nynorsk', individual).
	language_code('nob', 'nob', 'nb', 'Norwegian Bokmål', individual).
	language_code('nog', 'nog', '', 'Nogai', individual).
	language_code('non', 'non', '', 'Norse, Old', individual).
	language_code('nor', 'nor', 'no', 'Norwegian', macrolanguage).
	language_code('nqo', 'nqo', '', 'N''Ko', individual).
	language_code('nso', 'nso', '', 'Pedi; Sepedi; Northern Sotho', individual).
	language_code('nub', 'nub', '', 'Nubian languages', collective).
	language_code('nwc', 'nwc', '', 'Classical Newari; Old Newari; Classical Nepal Bhasa', individual).
	language_code('nya', 'nya', 'ny', 'Chichewa; Chewa; Nyanja', individual).
	language_code('nym', 'nym', '', 'Nyamwezi', individual).
	language_code('nyn', 'nyn', '', 'Nyankole', individual).
	language_code('nyo', 'nyo', '', 'Nyoro', individual).
	language_code('nzi', 'nzi', '', 'Nzima', individual).
	language_code('oci', 'oci', 'oc', 'Occitan (post 1500)', individual).
	language_code('oji', 'oji', 'oj', 'Ojibwa', macrolanguage).
	language_code('ori', 'ori', 'or', 'Oriya (macrolanguage)', macrolanguage).
	language_code('orm', 'orm', 'om', 'Oromo', macrolanguage).
	language_code('osa', 'osa', '', 'Osage', individual).
	language_code('oss', 'oss', 'os', 'Ossetian; Ossetic', individual).
	language_code('ota', 'ota', '', 'Turkish, Ottoman (1500-1928)', individual).
	language_code('oto', 'oto', '', 'Otomian languages', collective).
	language_code('paa', 'paa', '', 'Papuan languages', collective).
	language_code('pag', 'pag', '', 'Pangasinan', individual).
	language_code('pal', 'pal', '', 'Pahlavi', individual).
	language_code('pam', 'pam', '', 'Pampanga; Kapampangan', individual).
	language_code('pan', 'pan', 'pa', 'Panjabi; Punjabi', individual).
	language_code('pap', 'pap', '', 'Papiamento', individual).
	language_code('pau', 'pau', '', 'Palauan', individual).
	language_code('peo', 'peo', '', 'Persian, Old (ca.600-400 B.C.)', individual).
	language_code('phi', 'phi', '', 'Philippine languages', collective).
	language_code('phn', 'phn', '', 'Phoenician', individual).
	language_code('pli', 'pli', 'pi', 'Pali', individual).
	language_code('pol', 'pol', 'pl', 'Polish', individual).
	language_code('pon', 'pon', '', 'Pohnpeian', individual).
	language_code('por', 'por', 'pt', 'Portuguese', individual).
	language_code('pra', 'pra', '', 'Prakrit languages', collective).
	language_code('pro', 'pro', '', 'Provençal, Old (to 1500);Occitan, Old (to 1500)', individual).
	language_code('pus', 'pus', 'ps', 'Pushto; Pashto', macrolanguage).
	language_code('qaa-qtz', 'qaa-qtz', '', 'Reserved for local use', local_use).
	language_code('que', 'que', 'qu', 'Quechua', macrolanguage).
	language_code('raj', 'raj', '', 'Rajasthani', macrolanguage).
	language_code('rap', 'rap', '', 'Rapanui', individual).
	language_code('rar', 'rar', '', 'Rarotongan; Cook Islands Maori', individual).
	language_code('roa', 'roa', '', 'Romance languages', collective).
	language_code('roh', 'roh', 'rm', 'Romansh', individual).
	language_code('rom', 'rom', '', 'Romany', macrolanguage).
	language_code('rum', 'ron', 'ro', 'Romanian; Moldavian; Moldovan', individual).
	language_code('run', 'run', 'rn', 'Rundi', individual).
	language_code('rup', 'rup', '', 'Aromanian; Arumanian; Macedo-Romanian', individual).
	language_code('rus', 'rus', 'ru', 'Russian', individual).
	language_code('sad', 'sad', '', 'Sandawe', individual).
	language_code('sag', 'sag', 'sg', 'Sango', individual).
	language_code('sah', 'sah', '', 'Yakut', individual).
	language_code('sai', 'sai', '', 'South American Indian languages', collective).
	language_code('sal', 'sal', '', 'Salishan languages', collective).
	language_code('sam', 'sam', '', 'Samaritan Aramaic', individual).
	language_code('san', 'san', 'sa', 'Sanskrit', macrolanguage).
	language_code('sas', 'sas', '', 'Sasak', individual).
	language_code('sat', 'sat', '', 'Santali', individual).
	language_code('scn', 'scn', '', 'Sicilian', individual).
	language_code('sco', 'sco', '', 'Scots', individual).
	language_code('sel', 'sel', '', 'Selkup', individual).
	language_code('sem', 'sem', '', 'Semitic languages', collective).
	language_code('sga', 'sga', '', 'Irish, Old (to 900)', individual).
	language_code('sgn', 'sgn', '', 'Sign Languages', collective).
	language_code('shn', 'shn', '', 'Shan', individual).
	language_code('sid', 'sid', '', 'Sidamo', individual).
	language_code('sin', 'sin', 'si', 'Sinhala; Sinhalese', individual).
	language_code('sio', 'sio', '', 'Siouan languages', collective).
	language_code('sit', 'sit', '', 'Sino-Tibetan languages', collective).
	language_code('sla', 'sla', '', 'Slavic languages', collective).
	language_code('slo', 'slk', 'sk', 'Slovak', individual).
	language_code('slv', 'slv', 'sl', 'Slovenian', individual).
	language_code('sma', 'sma', '', 'Southern Sami', individual).
	language_code('sme', 'sme', 'se', 'Northern Sami', individual).
	language_code('smi', 'smi', '', 'Sami languages', collective).
	language_code('smj', 'smj', '', 'Lule Sami', individual).
	language_code('smn', 'smn', '', 'Inari Sami', individual).
	language_code('smo', 'smo', 'sm', 'Samoan', individual).
	language_code('sms', 'sms', '', 'Skolt Sami', individual).
	language_code('sna', 'sna', 'sn', 'Shona', individual).
	language_code('snd', 'snd', 'sd', 'Sindhi', individual).
	language_code('snk', 'snk', '', 'Soninke', individual).
	language_code('sog', 'sog', '', 'Sogdian', individual).
	language_code('som', 'som', 'so', 'Somali', individual).
	language_code('son', 'son', '', 'Songhai languages', collective).
	language_code('sot', 'sot', 'st', 'Sotho, Southern', individual).
	language_code('spa', 'spa', 'es', 'Spanish; Castilian', individual).
	language_code('srd', 'srd', 'sc', 'Sardinian', macrolanguage).
	language_code('srn', 'srn', '', 'Sranan Tongo', individual).
	language_code('srp', 'srp', 'sr', 'Serbian', individual).
	language_code('srr', 'srr', '', 'Serer', individual).
	language_code('ssa', 'ssa', '', 'Nilo-Saharan languages', collective).
	language_code('ssw', 'ssw', 'ss', 'Swati', individual).
	language_code('suk', 'suk', '', 'Sukuma', individual).
	language_code('sun', 'sun', 'su', 'Sundanese', individual).
	language_code('sus', 'sus', '', 'Susu', individual).
	language_code('sux', 'sux', '', 'Sumerian', individual).
	language_code('swa', 'swa', 'sw', 'Swahili (macrolanguage)', macrolanguage).
	language_code('swe', 'swe', 'sv', 'Swedish', individual).
	language_code('syc', 'syc', '', 'Classical Syriac', individual).
	language_code('syr', 'syr', '', 'Syriac', macrolanguage).
	language_code('tah', 'tah', 'ty', 'Tahitian', individual).
	language_code('tai', 'tai', '', 'Tai languages', collective).
	language_code('tam', 'tam', 'ta', 'Tamil', individual).
	language_code('tat', 'tat', 'tt', 'Tatar', individual).
	language_code('tel', 'tel', 'te', 'Telugu', individual).
	language_code('tem', 'tem', '', 'Timne', individual).
	language_code('ter', 'ter', '', 'Tereno', individual).
	language_code('tet', 'tet', '', 'Tetum', individual).
	language_code('tgk', 'tgk', 'tg', 'Tajik', individual).
	language_code('tgl', 'tgl', 'tl', 'Tagalog', individual).
	language_code('tha', 'tha', 'th', 'Thai', individual).
	language_code('tig', 'tig', '', 'Tigre', individual).
	language_code('tir', 'tir', 'ti', 'Tigrinya', individual).
	language_code('tiv', 'tiv', '', 'Tiv', individual).
	language_code('tkl', 'tkl', '', 'Tokelau', individual).
	language_code('tlh', 'tlh', '', 'Klingon; tlhIngan-Hol', individual).
	language_code('tli', 'tli', '', 'Tlingit', individual).
	language_code('tmh', 'tmh', '', 'Tamashek', macrolanguage).
	language_code('tog', 'tog', '', 'Tonga (Nyasa)', individual).
	language_code('ton', 'ton', 'to', 'Tonga (Tonga Islands)', individual).
	language_code('tpi', 'tpi', '', 'Tok Pisin', individual).
	language_code('tsi', 'tsi', '', 'Tsimshian', individual).
	language_code('tsn', 'tsn', 'tn', 'Tswana', individual).
	language_code('tso', 'tso', 'ts', 'Tsonga', individual).
	language_code('tuk', 'tuk', 'tk', 'Turkmen', individual).
	language_code('tum', 'tum', '', 'Tumbuka', individual).
	language_code('tup', 'tup', '', 'Tupi languages', collective).
	language_code('tur', 'tur', 'tr', 'Turkish', individual).
	language_code('tut', 'tut', '', 'Altaic languages', collective).
	language_code('tvl', 'tvl', '', 'Tuvalu', individual).
	language_code('twi', 'twi', 'tw', 'Twi', individual).
	language_code('tyv', 'tyv', '', 'Tuvinian', individual).
	language_code('udm', 'udm', '', 'Udmurt', individual).
	language_code('uga', 'uga', '', 'Ugaritic', individual).
	language_code('uig', 'uig', 'ug', 'Uighur; Uyghur', individual).
	language_code('ukr', 'ukr', 'uk', 'Ukrainian', individual).
	language_code('umb', 'umb', '', 'Umbundu', individual).
	language_code('und', 'und', '', 'Undetermined', special).
	language_code('urd', 'urd', 'ur', 'Urdu', individual).
	language_code('uzb', 'uzb', 'uz', 'Uzbek', macrolanguage).
	language_code('vai', 'vai', '', 'Vai', individual).
	language_code('ven', 'ven', 've', 'Venda', individual).
	language_code('vie', 'vie', 'vi', 'Vietnamese', individual).
	language_code('vol', 'vol', 'vo', 'Volapük', individual).
	language_code('vot', 'vot', '', 'Votic', individual).
	language_code('wak', 'wak', '', 'Wakashan languages', collective).
	language_code('wal', 'wal', '', 'Wolaitta; Wolaytta', individual).
	language_code('war', 'war', '', 'Waray', individual).
	language_code('was', 'was', '', 'Washo', individual).
	language_code('wen', 'wen', '', 'Sorbian languages', collective).
	language_code('wln', 'wln', 'wa', 'Walloon', individual).
	language_code('wol', 'wol', 'wo', 'Wolof', individual).
	language_code('xal', 'xal', '', 'Kalmyk; Oirat', individual).
	language_code('xho', 'xho', 'xh', 'Xhosa', individual).
	language_code('yao', 'yao', '', 'Yao', individual).
	language_code('yap', 'yap', '', 'Yapese', individual).
	language_code('yid', 'yid', 'yi', 'Yiddish', macrolanguage).
	language_code('yor', 'yor', 'yo', 'Yoruba', individual).
	language_code('ypk', 'ypk', '', 'Yupik languages', collective).
	language_code('zap', 'zap', '', 'Zapotec', macrolanguage).
	language_code('zbl', 'zbl', '', 'Blissymbols; Blissymbolics; Bliss', individual).
	language_code('zen', 'zen', '', 'Zenaga', individual).
	language_code('zgh', 'zgh', '', 'Standard Moroccan Tamazight', individual).
	language_code('zha', 'zha', 'za', 'Zhuang; Chuang', macrolanguage).
	language_code('znd', 'znd', '', 'Zande languages', collective).
	language_code('zul', 'zul', 'zu', 'Zulu', individual).
	language_code('zun', 'zun', '', 'Zuni', individual).
	language_code('zxx', 'zxx', '', 'No linguistic content; Not applicable', special).
	language_code('zza', 'zza', '', 'Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki', macrolanguage).

:- end_object.
