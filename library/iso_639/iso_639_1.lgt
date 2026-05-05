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


:- object(iso_639_1).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 639-1 fact table.',
		remarks is [
			'Source update' - '2026-01-15',
			'Generated entries' - '183',
			'Source' - 'Library of Congress ISO 639-2 code list'
		]
	]).

	:- public(language/3).
	:- mode(language(?atom, ?atom, ?atom), zero_or_more).
	:- info(language/3, [
		comment is 'Generated ISO 639-1 fact table.',
		argnames is ['Alpha2', 'Alpha3', 'Name']
	]).

	language('aa', 'aar', 'Afar').
	language('ab', 'abk', 'Abkhazian').
	language('af', 'afr', 'Afrikaans').
	language('ak', 'aka', 'Akan').
	language('sq', 'sqi', 'Albanian').
	language('am', 'amh', 'Amharic').
	language('ar', 'ara', 'Arabic').
	language('an', 'arg', 'Aragonese').
	language('hy', 'hye', 'Armenian').
	language('as', 'asm', 'Assamese').
	language('av', 'ava', 'Avaric').
	language('ae', 'ave', 'Avestan').
	language('ay', 'aym', 'Aymara').
	language('az', 'aze', 'Azerbaijani').
	language('ba', 'bak', 'Bashkir').
	language('bm', 'bam', 'Bambara').
	language('eu', 'eus', 'Basque').
	language('be', 'bel', 'Belarusian').
	language('bn', 'ben', 'Bengali').
	language('bi', 'bis', 'Bislama').
	language('bo', 'bod', 'Tibetan').
	language('bs', 'bos', 'Bosnian').
	language('br', 'bre', 'Breton').
	language('bg', 'bul', 'Bulgarian').
	language('my', 'mya', 'Burmese').
	language('ca', 'cat', 'Catalan; Valencian').
	language('cs', 'ces', 'Czech').
	language('ch', 'cha', 'Chamorro').
	language('ce', 'che', 'Chechen').
	language('zh', 'zho', 'Chinese').
	language('cu', 'chu', 'Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic').
	language('cv', 'chv', 'Chuvash').
	language('kw', 'cor', 'Cornish').
	language('co', 'cos', 'Corsican').
	language('cr', 'cre', 'Cree').
	language('cy', 'cym', 'Welsh').
	language('da', 'dan', 'Danish').
	language('de', 'deu', 'German').
	language('dv', 'div', 'Divehi; Dhivehi; Maldivian').
	language('nl', 'nld', 'Dutch; Flemish').
	language('dz', 'dzo', 'Dzongkha').
	language('el', 'ell', 'Modern Greek (1453-)').
	language('en', 'eng', 'English').
	language('eo', 'epo', 'Esperanto').
	language('et', 'est', 'Estonian').
	language('ee', 'ewe', 'Ewe').
	language('fo', 'fao', 'Faroese').
	language('fa', 'fas', 'Persian').
	language('fj', 'fij', 'Fijian').
	language('fi', 'fin', 'Finnish').
	language('fr', 'fra', 'French').
	language('fy', 'fry', 'Western Frisian').
	language('ff', 'ful', 'Fulah').
	language('ka', 'kat', 'Georgian').
	language('gd', 'gla', 'Gaelic; Scottish Gaelic').
	language('ga', 'gle', 'Irish').
	language('gl', 'glg', 'Galician').
	language('gv', 'glv', 'Manx').
	language('gn', 'grn', 'Guarani').
	language('gu', 'guj', 'Gujarati').
	language('ht', 'hat', 'Haitian; Haitian Creole').
	language('ha', 'hau', 'Hausa').
	language('he', 'heb', 'Hebrew').
	language('hz', 'her', 'Herero').
	language('hi', 'hin', 'Hindi').
	language('ho', 'hmo', 'Hiri Motu').
	language('hr', 'hrv', 'Croatian').
	language('hu', 'hun', 'Hungarian').
	language('ig', 'ibo', 'Igbo').
	language('is', 'isl', 'Icelandic').
	language('io', 'ido', 'Ido').
	language('ii', 'iii', 'Sichuan Yi; Nuosu').
	language('iu', 'iku', 'Inuktitut').
	language('ie', 'ile', 'Interlingue; Occidental').
	language('ia', 'ina', 'Interlingua (International Auxiliary Language Association)').
	language('id', 'ind', 'Indonesian').
	language('ik', 'ipk', 'Inupiaq').
	language('it', 'ita', 'Italian').
	language('jv', 'jav', 'Javanese').
	language('ja', 'jpn', 'Japanese').
	language('kl', 'kal', 'Kalaallisut; Greenlandic').
	language('kn', 'kan', 'Kannada').
	language('ks', 'kas', 'Kashmiri').
	language('kr', 'kau', 'Kanuri').
	language('kk', 'kaz', 'Kazakh').
	language('km', 'khm', 'Central Khmer').
	language('ki', 'kik', 'Kikuyu; Gikuyu').
	language('rw', 'kin', 'Kinyarwanda').
	language('ky', 'kir', 'Kirghiz; Kyrgyz').
	language('kv', 'kom', 'Komi').
	language('kg', 'kon', 'Kongo').
	language('ko', 'kor', 'Korean').
	language('kj', 'kua', 'Kuanyama; Kwanyama').
	language('ku', 'kur', 'Kurdish').
	language('lo', 'lao', 'Lao').
	language('la', 'lat', 'Latin').
	language('lv', 'lav', 'Latvian').
	language('li', 'lim', 'Limburgan; Limburger; Limburgish').
	language('ln', 'lin', 'Lingala').
	language('lt', 'lit', 'Lithuanian').
	language('lb', 'ltz', 'Luxembourgish; Letzeburgesch').
	language('lu', 'lub', 'Luba-Katanga').
	language('lg', 'lug', 'Ganda').
	language('mk', 'mkd', 'Macedonian').
	language('mh', 'mah', 'Marshallese').
	language('ml', 'mal', 'Malayalam').
	language('mi', 'mri', 'Maori').
	language('mr', 'mar', 'Marathi').
	language('ms', 'msa', 'Malay (macrolanguage)').
	language('mg', 'mlg', 'Malagasy').
	language('mt', 'mlt', 'Maltese').
	language('mn', 'mon', 'Mongolian').
	language('na', 'nau', 'Nauru').
	language('nv', 'nav', 'Navajo; Navaho').
	language('nr', 'nbl', 'South Ndebele').
	language('nd', 'nde', 'North Ndebele').
	language('ng', 'ndo', 'Ndonga').
	language('ne', 'nep', 'Nepali (macrolanguage)').
	language('nn', 'nno', 'Norwegian Nynorsk').
	language('nb', 'nob', 'Norwegian Bokmål').
	language('no', 'nor', 'Norwegian').
	language('ny', 'nya', 'Chichewa; Chewa; Nyanja').
	language('oc', 'oci', 'Occitan (post 1500)').
	language('oj', 'oji', 'Ojibwa').
	language('or', 'ori', 'Oriya (macrolanguage)').
	language('om', 'orm', 'Oromo').
	language('os', 'oss', 'Ossetian; Ossetic').
	language('pa', 'pan', 'Panjabi; Punjabi').
	language('pi', 'pli', 'Pali').
	language('pl', 'pol', 'Polish').
	language('pt', 'por', 'Portuguese').
	language('ps', 'pus', 'Pushto; Pashto').
	language('qu', 'que', 'Quechua').
	language('rm', 'roh', 'Romansh').
	language('ro', 'ron', 'Romanian; Moldavian; Moldovan').
	language('rn', 'run', 'Rundi').
	language('ru', 'rus', 'Russian').
	language('sg', 'sag', 'Sango').
	language('sa', 'san', 'Sanskrit').
	language('si', 'sin', 'Sinhala; Sinhalese').
	language('sk', 'slk', 'Slovak').
	language('sl', 'slv', 'Slovenian').
	language('se', 'sme', 'Northern Sami').
	language('sm', 'smo', 'Samoan').
	language('sn', 'sna', 'Shona').
	language('sd', 'snd', 'Sindhi').
	language('so', 'som', 'Somali').
	language('st', 'sot', 'Sotho, Southern').
	language('es', 'spa', 'Spanish; Castilian').
	language('sc', 'srd', 'Sardinian').
	language('sr', 'srp', 'Serbian').
	language('ss', 'ssw', 'Swati').
	language('su', 'sun', 'Sundanese').
	language('sw', 'swa', 'Swahili (macrolanguage)').
	language('sv', 'swe', 'Swedish').
	language('ty', 'tah', 'Tahitian').
	language('ta', 'tam', 'Tamil').
	language('tt', 'tat', 'Tatar').
	language('te', 'tel', 'Telugu').
	language('tg', 'tgk', 'Tajik').
	language('tl', 'tgl', 'Tagalog').
	language('th', 'tha', 'Thai').
	language('ti', 'tir', 'Tigrinya').
	language('to', 'ton', 'Tonga (Tonga Islands)').
	language('tn', 'tsn', 'Tswana').
	language('ts', 'tso', 'Tsonga').
	language('tk', 'tuk', 'Turkmen').
	language('tr', 'tur', 'Turkish').
	language('tw', 'twi', 'Twi').
	language('ug', 'uig', 'Uighur; Uyghur').
	language('uk', 'ukr', 'Ukrainian').
	language('ur', 'urd', 'Urdu').
	language('uz', 'uzb', 'Uzbek').
	language('ve', 'ven', 'Venda').
	language('vi', 'vie', 'Vietnamese').
	language('vo', 'vol', 'Volapük').
	language('wa', 'wln', 'Walloon').
	language('wo', 'wol', 'Wolof').
	language('xh', 'xho', 'Xhosa').
	language('yi', 'yid', 'Yiddish').
	language('yo', 'yor', 'Yoruba').
	language('za', 'zha', 'Zhuang; Chuang').
	language('zu', 'zul', 'Zulu').

:- end_object.
