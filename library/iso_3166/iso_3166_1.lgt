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


:- object(iso_3166_1).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 3166-1 country facts extracted from the UN M49 overview page, which republishes ISO alpha-2 and alpha-3 codes together with M49 numeric codes.',
		remarks is [
			'Source URL' - 'https://unstats.un.org/unsd/methodology/m49/overview/',
			'Generated entries' - '248'
		]
	]).

	:- public(country/4).
	:- mode(country(?atom, ?atom, ?integer, ?atom), zero_or_more).
	:- info(country/4, [
		comment is 'Generated ISO 3166-1 country fact table.',
		argnames is ['Alpha2', 'Alpha3', 'Numeric', 'Name']
	]).

	country('dz', 'dza', 12, 'Algeria').
	country('eg', 'egy', 818, 'Egypt').
	country('ly', 'lby', 434, 'Libya').
	country('ma', 'mar', 504, 'Morocco').
	country('sd', 'sdn', 729, 'Sudan').
	country('tn', 'tun', 788, 'Tunisia').
	country('eh', 'esh', 732, 'Western Sahara').
	country('io', 'iot', 86, 'British Indian Ocean Territory').
	country('bi', 'bdi', 108, 'Burundi').
	country('km', 'com', 174, 'Comoros').
	country('dj', 'dji', 262, 'Djibouti').
	country('er', 'eri', 232, 'Eritrea').
	country('et', 'eth', 231, 'Ethiopia').
	country('tf', 'atf', 260, 'French Southern Territories').
	country('ke', 'ken', 404, 'Kenya').
	country('mg', 'mdg', 450, 'Madagascar').
	country('mw', 'mwi', 454, 'Malawi').
	country('mu', 'mus', 480, 'Mauritius').
	country('yt', 'myt', 175, 'Mayotte').
	country('mz', 'moz', 508, 'Mozambique').
	country('re', 'reu', 638, 'Réunion').
	country('rw', 'rwa', 646, 'Rwanda').
	country('sc', 'syc', 690, 'Seychelles').
	country('so', 'som', 706, 'Somalia').
	country('ss', 'ssd', 728, 'South Sudan').
	country('ug', 'uga', 800, 'Uganda').
	country('tz', 'tza', 834, 'United Republic of Tanzania').
	country('zm', 'zmb', 894, 'Zambia').
	country('zw', 'zwe', 716, 'Zimbabwe').
	country('ao', 'ago', 24, 'Angola').
	country('cm', 'cmr', 120, 'Cameroon').
	country('cf', 'caf', 140, 'Central African Republic').
	country('td', 'tcd', 148, 'Chad').
	country('cg', 'cog', 178, 'Congo').
	country('cd', 'cod', 180, 'Democratic Republic of the Congo').
	country('gq', 'gnq', 226, 'Equatorial Guinea').
	country('ga', 'gab', 266, 'Gabon').
	country('st', 'stp', 678, 'Sao Tome and Principe').
	country('bw', 'bwa', 72, 'Botswana').
	country('sz', 'swz', 748, 'Eswatini').
	country('ls', 'lso', 426, 'Lesotho').
	country('na', 'nam', 516, 'Namibia').
	country('za', 'zaf', 710, 'South Africa').
	country('bj', 'ben', 204, 'Benin').
	country('bf', 'bfa', 854, 'Burkina Faso').
	country('cv', 'cpv', 132, 'Cabo Verde').
	country('ci', 'civ', 384, 'Côte d’Ivoire').
	country('gm', 'gmb', 270, 'Gambia').
	country('gh', 'gha', 288, 'Ghana').
	country('gn', 'gin', 324, 'Guinea').
	country('gw', 'gnb', 624, 'Guinea-Bissau').
	country('lr', 'lbr', 430, 'Liberia').
	country('ml', 'mli', 466, 'Mali').
	country('mr', 'mrt', 478, 'Mauritania').
	country('ne', 'ner', 562, 'Niger').
	country('ng', 'nga', 566, 'Nigeria').
	country('sh', 'shn', 654, 'Saint Helena').
	country('sn', 'sen', 686, 'Senegal').
	country('sl', 'sle', 694, 'Sierra Leone').
	country('tg', 'tgo', 768, 'Togo').
	country('ai', 'aia', 660, 'Anguilla').
	country('ag', 'atg', 28, 'Antigua and Barbuda').
	country('aw', 'abw', 533, 'Aruba').
	country('bs', 'bhs', 44, 'Bahamas').
	country('bb', 'brb', 52, 'Barbados').
	country('bq', 'bes', 535, 'Bonaire, Sint Eustatius and Saba').
	country('vg', 'vgb', 92, 'British Virgin Islands').
	country('ky', 'cym', 136, 'Cayman Islands').
	country('cu', 'cub', 192, 'Cuba').
	country('cw', 'cuw', 531, 'Curaçao').
	country('dm', 'dma', 212, 'Dominica').
	country('do', 'dom', 214, 'Dominican Republic').
	country('gd', 'grd', 308, 'Grenada').
	country('gp', 'glp', 312, 'Guadeloupe').
	country('ht', 'hti', 332, 'Haiti').
	country('jm', 'jam', 388, 'Jamaica').
	country('mq', 'mtq', 474, 'Martinique').
	country('ms', 'msr', 500, 'Montserrat').
	country('pr', 'pri', 630, 'Puerto Rico').
	country('bl', 'blm', 652, 'Saint Barthélemy').
	country('kn', 'kna', 659, 'Saint Kitts and Nevis').
	country('lc', 'lca', 662, 'Saint Lucia').
	country('mf', 'maf', 663, 'Saint Martin (French Part)').
	country('vc', 'vct', 670, 'Saint Vincent and the Grenadines').
	country('sx', 'sxm', 534, 'Sint Maarten (Dutch part)').
	country('tt', 'tto', 780, 'Trinidad and Tobago').
	country('tc', 'tca', 796, 'Turks and Caicos Islands').
	country('vi', 'vir', 850, 'United States Virgin Islands').
	country('bz', 'blz', 84, 'Belize').
	country('cr', 'cri', 188, 'Costa Rica').
	country('sv', 'slv', 222, 'El Salvador').
	country('gt', 'gtm', 320, 'Guatemala').
	country('hn', 'hnd', 340, 'Honduras').
	country('mx', 'mex', 484, 'Mexico').
	country('ni', 'nic', 558, 'Nicaragua').
	country('pa', 'pan', 591, 'Panama').
	country('ar', 'arg', 32, 'Argentina').
	country('bo', 'bol', 68, 'Bolivia (Plurinational State of)').
	country('bv', 'bvt', 74, 'Bouvet Island').
	country('br', 'bra', 76, 'Brazil').
	country('cl', 'chl', 152, 'Chile').
	country('co', 'col', 170, 'Colombia').
	country('ec', 'ecu', 218, 'Ecuador').
	country('fk', 'flk', 238, 'Falkland Islands (Malvinas)').
	country('gf', 'guf', 254, 'French Guiana').
	country('gy', 'guy', 328, 'Guyana').
	country('py', 'pry', 600, 'Paraguay').
	country('pe', 'per', 604, 'Peru').
	country('gs', 'sgs', 239, 'South Georgia and the South Sandwich Islands').
	country('sr', 'sur', 740, 'Suriname').
	country('uy', 'ury', 858, 'Uruguay').
	country('ve', 'ven', 862, 'Venezuela (Bolivarian Republic of)').
	country('bm', 'bmu', 60, 'Bermuda').
	country('ca', 'can', 124, 'Canada').
	country('gl', 'grl', 304, 'Greenland').
	country('pm', 'spm', 666, 'Saint Pierre and Miquelon').
	country('us', 'usa', 840, 'United States of America').
	country('aq', 'ata', 10, 'Antarctica').
	country('kz', 'kaz', 398, 'Kazakhstan').
	country('kg', 'kgz', 417, 'Kyrgyzstan').
	country('tj', 'tjk', 762, 'Tajikistan').
	country('tm', 'tkm', 795, 'Turkmenistan').
	country('uz', 'uzb', 860, 'Uzbekistan').
	country('cn', 'chn', 156, 'China').
	country('hk', 'hkg', 344, 'China, Hong Kong Special Administrative Region').
	country('mo', 'mac', 446, 'China, Macao Special Administrative Region').
	country('kp', 'prk', 408, 'Democratic People''s Republic of Korea').
	country('jp', 'jpn', 392, 'Japan').
	country('mn', 'mng', 496, 'Mongolia').
	country('kr', 'kor', 410, 'Republic of Korea').
	country('bn', 'brn', 96, 'Brunei Darussalam').
	country('kh', 'khm', 116, 'Cambodia').
	country('id', 'idn', 360, 'Indonesia').
	country('la', 'lao', 418, 'Lao People''s Democratic Republic').
	country('my', 'mys', 458, 'Malaysia').
	country('mm', 'mmr', 104, 'Myanmar').
	country('ph', 'phl', 608, 'Philippines').
	country('sg', 'sgp', 702, 'Singapore').
	country('th', 'tha', 764, 'Thailand').
	country('tl', 'tls', 626, 'Timor-Leste').
	country('vn', 'vnm', 704, 'Viet Nam').
	country('af', 'afg', 4, 'Afghanistan').
	country('bd', 'bgd', 50, 'Bangladesh').
	country('bt', 'btn', 64, 'Bhutan').
	country('in', 'ind', 356, 'India').
	country('ir', 'irn', 364, 'Iran (Islamic Republic of)').
	country('mv', 'mdv', 462, 'Maldives').
	country('np', 'npl', 524, 'Nepal').
	country('pk', 'pak', 586, 'Pakistan').
	country('lk', 'lka', 144, 'Sri Lanka').
	country('am', 'arm', 51, 'Armenia').
	country('az', 'aze', 31, 'Azerbaijan').
	country('bh', 'bhr', 48, 'Bahrain').
	country('cy', 'cyp', 196, 'Cyprus').
	country('ge', 'geo', 268, 'Georgia').
	country('iq', 'irq', 368, 'Iraq').
	country('il', 'isr', 376, 'Israel').
	country('jo', 'jor', 400, 'Jordan').
	country('kw', 'kwt', 414, 'Kuwait').
	country('lb', 'lbn', 422, 'Lebanon').
	country('om', 'omn', 512, 'Oman').
	country('qa', 'qat', 634, 'Qatar').
	country('sa', 'sau', 682, 'Saudi Arabia').
	country('ps', 'pse', 275, 'State of Palestine').
	country('sy', 'syr', 760, 'Syrian Arab Republic').
	country('tr', 'tur', 792, 'Türkiye').
	country('ae', 'are', 784, 'United Arab Emirates').
	country('ye', 'yem', 887, 'Yemen').
	country('by', 'blr', 112, 'Belarus').
	country('bg', 'bgr', 100, 'Bulgaria').
	country('cz', 'cze', 203, 'Czechia').
	country('hu', 'hun', 348, 'Hungary').
	country('pl', 'pol', 616, 'Poland').
	country('md', 'mda', 498, 'Republic of Moldova').
	country('ro', 'rou', 642, 'Romania').
	country('ru', 'rus', 643, 'Russian Federation').
	country('sk', 'svk', 703, 'Slovakia').
	country('ua', 'ukr', 804, 'Ukraine').
	country('ax', 'ala', 248, 'Åland Islands').
	country('dk', 'dnk', 208, 'Denmark').
	country('ee', 'est', 233, 'Estonia').
	country('fo', 'fro', 234, 'Faroe Islands').
	country('fi', 'fin', 246, 'Finland').
	country('gg', 'ggy', 831, 'Guernsey').
	country('is', 'isl', 352, 'Iceland').
	country('ie', 'irl', 372, 'Ireland').
	country('im', 'imn', 833, 'Isle of Man').
	country('je', 'jey', 832, 'Jersey').
	country('lv', 'lva', 428, 'Latvia').
	country('lt', 'ltu', 440, 'Lithuania').
	country('no', 'nor', 578, 'Norway').
	country('sj', 'sjm', 744, 'Svalbard and Jan Mayen Islands').
	country('se', 'swe', 752, 'Sweden').
	country('gb', 'gbr', 826, 'United Kingdom of Great Britain and Northern Ireland').
	country('al', 'alb', 8, 'Albania').
	country('ad', 'and', 20, 'Andorra').
	country('ba', 'bih', 70, 'Bosnia and Herzegovina').
	country('hr', 'hrv', 191, 'Croatia').
	country('gi', 'gib', 292, 'Gibraltar').
	country('gr', 'grc', 300, 'Greece').
	country('va', 'vat', 336, 'Holy See').
	country('it', 'ita', 380, 'Italy').
	country('mt', 'mlt', 470, 'Malta').
	country('me', 'mne', 499, 'Montenegro').
	country('mk', 'mkd', 807, 'North Macedonia').
	country('pt', 'prt', 620, 'Portugal').
	country('sm', 'smr', 674, 'San Marino').
	country('rs', 'srb', 688, 'Serbia').
	country('si', 'svn', 705, 'Slovenia').
	country('es', 'esp', 724, 'Spain').
	country('at', 'aut', 40, 'Austria').
	country('be', 'bel', 56, 'Belgium').
	country('fr', 'fra', 250, 'France').
	country('de', 'deu', 276, 'Germany').
	country('li', 'lie', 438, 'Liechtenstein').
	country('lu', 'lux', 442, 'Luxembourg').
	country('mc', 'mco', 492, 'Monaco').
	country('nl', 'nld', 528, 'Netherlands (Kingdom of the)').
	country('ch', 'che', 756, 'Switzerland').
	country('au', 'aus', 36, 'Australia').
	country('cx', 'cxr', 162, 'Christmas Island').
	country('cc', 'cck', 166, 'Cocos (Keeling) Islands').
	country('hm', 'hmd', 334, 'Heard Island and McDonald Islands').
	country('nz', 'nzl', 554, 'New Zealand').
	country('nf', 'nfk', 574, 'Norfolk Island').
	country('fj', 'fji', 242, 'Fiji').
	country('nc', 'ncl', 540, 'New Caledonia').
	country('pg', 'png', 598, 'Papua New Guinea').
	country('sb', 'slb', 90, 'Solomon Islands').
	country('vu', 'vut', 548, 'Vanuatu').
	country('gu', 'gum', 316, 'Guam').
	country('ki', 'kir', 296, 'Kiribati').
	country('mh', 'mhl', 584, 'Marshall Islands').
	country('fm', 'fsm', 583, 'Micronesia (Federated States of)').
	country('nr', 'nru', 520, 'Nauru').
	country('mp', 'mnp', 580, 'Northern Mariana Islands').
	country('pw', 'plw', 585, 'Palau').
	country('um', 'umi', 581, 'United States Minor Outlying Islands').
	country('as', 'asm', 16, 'American Samoa').
	country('ck', 'cok', 184, 'Cook Islands').
	country('pf', 'pyf', 258, 'French Polynesia').
	country('nu', 'niu', 570, 'Niue').
	country('pn', 'pcn', 612, 'Pitcairn').
	country('ws', 'wsm', 882, 'Samoa').
	country('tk', 'tkl', 772, 'Tokelau').
	country('to', 'ton', 776, 'Tonga').
	country('tv', 'tuv', 798, 'Tuvalu').
	country('wf', 'wlf', 876, 'Wallis and Futuna Islands').

:- end_object.
