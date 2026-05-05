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


:- object(iso_4217,
	implements(iso_4217_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 4217 active currency and fund facts extracted from the SIX list-one.xml snapshot.',
		remarks is [
			'Source publication' - '2026-01-01',
			'Generated currency entries' - '269',
			'Generated fund entries' - '8'
		]
	]).

	:- public(currency/5).
	:- mode(currency(?atom, ?integer, ?term, ?atom, ?atom), zero_or_more).
	:- info(currency/5, [
		comment is 'Generated ISO 4217 active non-fund currency fact table.',
		argnames is ['Alphabetic', 'Numeric', 'MinorUnit', 'Name', 'Entity']
	]).

	:- public(fund_currency/5).
	:- mode(fund_currency(?atom, ?integer, ?term, ?atom, ?atom), zero_or_more).
	:- info(fund_currency/5, [
		comment is 'Generated ISO 4217 active fund currency fact table.',
		argnames is ['Alphabetic', 'Numeric', 'MinorUnit', 'Name', 'Entity']
	]).

	currency('AFN', 971, 2, 'Afghani', 'AFGHANISTAN').
	currency('EUR', 978, 2, 'Euro', 'ÅLAND ISLANDS').
	currency('ALL', 8, 2, 'Lek', 'ALBANIA').
	currency('DZD', 12, 2, 'Algerian Dinar', 'ALGERIA').
	currency('USD', 840, 2, 'US Dollar', 'AMERICAN SAMOA').
	currency('EUR', 978, 2, 'Euro', 'ANDORRA').
	currency('AOA', 973, 2, 'Kwanza', 'ANGOLA').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'ANGUILLA').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'ANTIGUA AND BARBUDA').
	currency('XAD', 396, 2, 'Arab Accounting Dinar', 'ARAB MONETARY FUND').
	currency('ARS', 32, 2, 'Argentine Peso', 'ARGENTINA').
	currency('AMD', 51, 2, 'Armenian Dram', 'ARMENIA').
	currency('AWG', 533, 2, 'Aruban Florin', 'ARUBA').
	currency('AUD', 36, 2, 'Australian Dollar', 'AUSTRALIA').
	currency('EUR', 978, 2, 'Euro', 'AUSTRIA').
	currency('AZN', 944, 2, 'Azerbaijan Manat', 'AZERBAIJAN').
	currency('BSD', 44, 2, 'Bahamian Dollar', 'BAHAMAS (THE)').
	currency('BHD', 48, 3, 'Bahraini Dinar', 'BAHRAIN').
	currency('BDT', 50, 2, 'Taka', 'BANGLADESH').
	currency('BBD', 52, 2, 'Barbados Dollar', 'BARBADOS').
	currency('BYN', 933, 2, 'Belarusian Ruble', 'BELARUS').
	currency('EUR', 978, 2, 'Euro', 'BELGIUM').
	currency('BZD', 84, 2, 'Belize Dollar', 'BELIZE').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'BENIN').
	currency('BMD', 60, 2, 'Bermudian Dollar', 'BERMUDA').
	currency('INR', 356, 2, 'Indian Rupee', 'BHUTAN').
	currency('BTN', 64, 2, 'Ngultrum', 'BHUTAN').
	currency('BOB', 68, 2, 'Boliviano', 'BOLIVIA (PLURINATIONAL STATE OF)').
	currency('USD', 840, 2, 'US Dollar', 'BONAIRE, SINT EUSTATIUS AND SABA').
	currency('BAM', 977, 2, 'Convertible Mark', 'BOSNIA AND HERZEGOVINA').
	currency('BWP', 72, 2, 'Pula', 'BOTSWANA').
	currency('NOK', 578, 2, 'Norwegian Krone', 'BOUVET ISLAND').
	currency('BRL', 986, 2, 'Brazilian Real', 'BRAZIL').
	currency('USD', 840, 2, 'US Dollar', 'BRITISH INDIAN OCEAN TERRITORY (THE)').
	currency('BND', 96, 2, 'Brunei Dollar', 'BRUNEI DARUSSALAM').
	currency('EUR', 978, 2, 'Euro', 'BULGARIA').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'BURKINA FASO').
	currency('BIF', 108, 0, 'Burundi Franc', 'BURUNDI').
	currency('CVE', 132, 2, 'Cabo Verde Escudo', 'CABO VERDE').
	currency('KHR', 116, 2, 'Riel', 'CAMBODIA').
	currency('XAF', 950, 0, 'CFA Franc BEAC', 'CAMEROON').
	currency('CAD', 124, 2, 'Canadian Dollar', 'CANADA').
	currency('KYD', 136, 2, 'Cayman Islands Dollar', 'CAYMAN ISLANDS (THE)').
	currency('XAF', 950, 0, 'CFA Franc BEAC', 'CENTRAL AFRICAN REPUBLIC (THE)').
	currency('XAF', 950, 0, 'CFA Franc BEAC', 'CHAD').
	currency('CLP', 152, 0, 'Chilean Peso', 'CHILE').
	currency('CNY', 156, 2, 'Yuan Renminbi', 'CHINA').
	currency('AUD', 36, 2, 'Australian Dollar', 'CHRISTMAS ISLAND').
	currency('AUD', 36, 2, 'Australian Dollar', 'COCOS (KEELING) ISLANDS (THE)').
	currency('COP', 170, 2, 'Colombian Peso', 'COLOMBIA').
	currency('KMF', 174, 0, 'Comorian Franc', 'COMOROS (THE)').
	currency('CDF', 976, 2, 'Congolese Franc', 'CONGO (THE DEMOCRATIC REPUBLIC OF THE)').
	currency('XAF', 950, 0, 'CFA Franc BEAC', 'CONGO (THE)').
	currency('NZD', 554, 2, 'New Zealand Dollar', 'COOK ISLANDS (THE)').
	currency('CRC', 188, 2, 'Costa Rican Colon', 'COSTA RICA').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'CÔTE D''IVOIRE').
	currency('EUR', 978, 2, 'Euro', 'CROATIA').
	currency('CUP', 192, 2, 'Cuban Peso', 'CUBA').
	currency('XCG', 532, 2, 'Caribbean Guilder', 'CURAÇAO').
	currency('EUR', 978, 2, 'Euro', 'CYPRUS').
	currency('CZK', 203, 2, 'Czech Koruna', 'CZECHIA').
	currency('DKK', 208, 2, 'Danish Krone', 'DENMARK').
	currency('DJF', 262, 0, 'Djibouti Franc', 'DJIBOUTI').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'DOMINICA').
	currency('DOP', 214, 2, 'Dominican Peso', 'DOMINICAN REPUBLIC (THE)').
	currency('USD', 840, 2, 'US Dollar', 'ECUADOR').
	currency('EGP', 818, 2, 'Egyptian Pound', 'EGYPT').
	currency('SVC', 222, 2, 'El Salvador Colon', 'EL SALVADOR').
	currency('USD', 840, 2, 'US Dollar', 'EL SALVADOR').
	currency('XAF', 950, 0, 'CFA Franc BEAC', 'EQUATORIAL GUINEA').
	currency('ERN', 232, 2, 'Nakfa', 'ERITREA').
	currency('EUR', 978, 2, 'Euro', 'ESTONIA').
	currency('SZL', 748, 2, 'Lilangeni', 'ESWATINI').
	currency('ETB', 230, 2, 'Ethiopian Birr', 'ETHIOPIA').
	currency('EUR', 978, 2, 'Euro', 'EUROPEAN UNION').
	currency('FKP', 238, 2, 'Falkland Islands Pound', 'FALKLAND ISLANDS (THE) [MALVINAS]').
	currency('DKK', 208, 2, 'Danish Krone', 'FAROE ISLANDS (THE)').
	currency('FJD', 242, 2, 'Fiji Dollar', 'FIJI').
	currency('EUR', 978, 2, 'Euro', 'FINLAND').
	currency('EUR', 978, 2, 'Euro', 'FRANCE').
	currency('EUR', 978, 2, 'Euro', 'FRENCH GUIANA').
	currency('XPF', 953, 0, 'CFP Franc', 'FRENCH POLYNESIA').
	currency('EUR', 978, 2, 'Euro', 'FRENCH SOUTHERN TERRITORIES (THE)').
	currency('XAF', 950, 0, 'CFA Franc BEAC', 'GABON').
	currency('GMD', 270, 2, 'Dalasi', 'GAMBIA (THE)').
	currency('GEL', 981, 2, 'Lari', 'GEORGIA').
	currency('EUR', 978, 2, 'Euro', 'GERMANY').
	currency('GHS', 936, 2, 'Ghana Cedi', 'GHANA').
	currency('GIP', 292, 2, 'Gibraltar Pound', 'GIBRALTAR').
	currency('EUR', 978, 2, 'Euro', 'GREECE').
	currency('DKK', 208, 2, 'Danish Krone', 'GREENLAND').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'GRENADA').
	currency('EUR', 978, 2, 'Euro', 'GUADELOUPE').
	currency('USD', 840, 2, 'US Dollar', 'GUAM').
	currency('GTQ', 320, 2, 'Quetzal', 'GUATEMALA').
	currency('GBP', 826, 2, 'Pound Sterling', 'GUERNSEY').
	currency('GNF', 324, 0, 'Guinean Franc', 'GUINEA').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'GUINEA-BISSAU').
	currency('GYD', 328, 2, 'Guyana Dollar', 'GUYANA').
	currency('HTG', 332, 2, 'Gourde', 'HAITI').
	currency('USD', 840, 2, 'US Dollar', 'HAITI').
	currency('AUD', 36, 2, 'Australian Dollar', 'HEARD ISLAND AND McDONALD ISLANDS').
	currency('EUR', 978, 2, 'Euro', 'HOLY SEE (THE)').
	currency('HNL', 340, 2, 'Lempira', 'HONDURAS').
	currency('HKD', 344, 2, 'Hong Kong Dollar', 'HONG KONG').
	currency('HUF', 348, 2, 'Forint', 'HUNGARY').
	currency('ISK', 352, 0, 'Iceland Krona', 'ICELAND').
	currency('INR', 356, 2, 'Indian Rupee', 'INDIA').
	currency('IDR', 360, 2, 'Rupiah', 'INDONESIA').
	currency('XDR', 960, na, 'SDR (Special Drawing Right)', 'INTERNATIONAL MONETARY FUND (IMF)').
	currency('IRR', 364, 2, 'Iranian Rial', 'IRAN (ISLAMIC REPUBLIC OF)').
	currency('IQD', 368, 3, 'Iraqi Dinar', 'IRAQ').
	currency('EUR', 978, 2, 'Euro', 'IRELAND').
	currency('GBP', 826, 2, 'Pound Sterling', 'ISLE OF MAN').
	currency('ILS', 376, 2, 'New Israeli Sheqel', 'ISRAEL').
	currency('EUR', 978, 2, 'Euro', 'ITALY').
	currency('JMD', 388, 2, 'Jamaican Dollar', 'JAMAICA').
	currency('JPY', 392, 0, 'Yen', 'JAPAN').
	currency('GBP', 826, 2, 'Pound Sterling', 'JERSEY').
	currency('JOD', 400, 3, 'Jordanian Dinar', 'JORDAN').
	currency('KZT', 398, 2, 'Tenge', 'KAZAKHSTAN').
	currency('KES', 404, 2, 'Kenyan Shilling', 'KENYA').
	currency('AUD', 36, 2, 'Australian Dollar', 'KIRIBATI').
	currency('KPW', 408, 2, 'North Korean Won', 'KOREA (THE DEMOCRATIC PEOPLE’S REPUBLIC OF)').
	currency('KRW', 410, 0, 'Won', 'KOREA (THE REPUBLIC OF)').
	currency('KWD', 414, 3, 'Kuwaiti Dinar', 'KUWAIT').
	currency('KGS', 417, 2, 'Som', 'KYRGYZSTAN').
	currency('LAK', 418, 2, 'Lao Kip', 'LAO PEOPLE’S DEMOCRATIC REPUBLIC (THE)').
	currency('EUR', 978, 2, 'Euro', 'LATVIA').
	currency('LBP', 422, 2, 'Lebanese Pound', 'LEBANON').
	currency('LSL', 426, 2, 'Loti', 'LESOTHO').
	currency('ZAR', 710, 2, 'Rand', 'LESOTHO').
	currency('LRD', 430, 2, 'Liberian Dollar', 'LIBERIA').
	currency('LYD', 434, 3, 'Libyan Dinar', 'LIBYA').
	currency('CHF', 756, 2, 'Swiss Franc', 'LIECHTENSTEIN').
	currency('EUR', 978, 2, 'Euro', 'LITHUANIA').
	currency('EUR', 978, 2, 'Euro', 'LUXEMBOURG').
	currency('MOP', 446, 2, 'Pataca', 'MACAO').
	currency('MKD', 807, 2, 'Denar', 'NORTH MACEDONIA').
	currency('MGA', 969, 2, 'Malagasy Ariary', 'MADAGASCAR').
	currency('MWK', 454, 2, 'Malawi Kwacha', 'MALAWI').
	currency('MYR', 458, 2, 'Malaysian Ringgit', 'MALAYSIA').
	currency('MVR', 462, 2, 'Rufiyaa', 'MALDIVES').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'MALI').
	currency('EUR', 978, 2, 'Euro', 'MALTA').
	currency('USD', 840, 2, 'US Dollar', 'MARSHALL ISLANDS (THE)').
	currency('EUR', 978, 2, 'Euro', 'MARTINIQUE').
	currency('MRU', 929, 2, 'Ouguiya', 'MAURITANIA').
	currency('MUR', 480, 2, 'Mauritius Rupee', 'MAURITIUS').
	currency('EUR', 978, 2, 'Euro', 'MAYOTTE').
	currency('XUA', 965, na, 'ADB Unit of Account', 'MEMBER COUNTRIES OF THE AFRICAN DEVELOPMENT BANK GROUP').
	currency('MXN', 484, 2, 'Mexican Peso', 'MEXICO').
	currency('USD', 840, 2, 'US Dollar', 'MICRONESIA (FEDERATED STATES OF)').
	currency('MDL', 498, 2, 'Moldovan Leu', 'MOLDOVA (THE REPUBLIC OF)').
	currency('EUR', 978, 2, 'Euro', 'MONACO').
	currency('MNT', 496, 2, 'Tugrik', 'MONGOLIA').
	currency('EUR', 978, 2, 'Euro', 'MONTENEGRO').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'MONTSERRAT').
	currency('MAD', 504, 2, 'Moroccan Dirham', 'MOROCCO').
	currency('MZN', 943, 2, 'Mozambique Metical', 'MOZAMBIQUE').
	currency('MMK', 104, 2, 'Kyat', 'MYANMAR').
	currency('NAD', 516, 2, 'Namibia Dollar', 'NAMIBIA').
	currency('ZAR', 710, 2, 'Rand', 'NAMIBIA').
	currency('AUD', 36, 2, 'Australian Dollar', 'NAURU').
	currency('NPR', 524, 2, 'Nepalese Rupee', 'NEPAL').
	currency('EUR', 978, 2, 'Euro', 'NETHERLANDS (THE)').
	currency('XPF', 953, 0, 'CFP Franc', 'NEW CALEDONIA').
	currency('NZD', 554, 2, 'New Zealand Dollar', 'NEW ZEALAND').
	currency('NIO', 558, 2, 'Cordoba Oro', 'NICARAGUA').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'NIGER (THE)').
	currency('NGN', 566, 2, 'Naira', 'NIGERIA').
	currency('NZD', 554, 2, 'New Zealand Dollar', 'NIUE').
	currency('AUD', 36, 2, 'Australian Dollar', 'NORFOLK ISLAND').
	currency('USD', 840, 2, 'US Dollar', 'NORTHERN MARIANA ISLANDS (THE)').
	currency('NOK', 578, 2, 'Norwegian Krone', 'NORWAY').
	currency('OMR', 512, 3, 'Rial Omani', 'OMAN').
	currency('PKR', 586, 2, 'Pakistan Rupee', 'PAKISTAN').
	currency('USD', 840, 2, 'US Dollar', 'PALAU').
	currency('PAB', 590, 2, 'Balboa', 'PANAMA').
	currency('USD', 840, 2, 'US Dollar', 'PANAMA').
	currency('PGK', 598, 2, 'Kina', 'PAPUA NEW GUINEA').
	currency('PYG', 600, 0, 'Guarani', 'PARAGUAY').
	currency('PEN', 604, 2, 'Sol', 'PERU').
	currency('PHP', 608, 2, 'Philippine Peso', 'PHILIPPINES (THE)').
	currency('NZD', 554, 2, 'New Zealand Dollar', 'PITCAIRN').
	currency('PLN', 985, 2, 'Zloty', 'POLAND').
	currency('EUR', 978, 2, 'Euro', 'PORTUGAL').
	currency('USD', 840, 2, 'US Dollar', 'PUERTO RICO').
	currency('QAR', 634, 2, 'Qatari Rial', 'QATAR').
	currency('EUR', 978, 2, 'Euro', 'RÉUNION').
	currency('RON', 946, 2, 'Romanian Leu', 'ROMANIA').
	currency('RUB', 643, 2, 'Russian Ruble', 'RUSSIAN FEDERATION (THE)').
	currency('RWF', 646, 0, 'Rwanda Franc', 'RWANDA').
	currency('EUR', 978, 2, 'Euro', 'SAINT BARTHÉLEMY').
	currency('SHP', 654, 2, 'Saint Helena Pound', 'SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'SAINT KITTS AND NEVIS').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'SAINT LUCIA').
	currency('EUR', 978, 2, 'Euro', 'SAINT MARTIN (FRENCH PART)').
	currency('EUR', 978, 2, 'Euro', 'SAINT PIERRE AND MIQUELON').
	currency('XCD', 951, 2, 'East Caribbean Dollar', 'SAINT VINCENT AND THE GRENADINES').
	currency('WST', 882, 2, 'Tala', 'SAMOA').
	currency('EUR', 978, 2, 'Euro', 'SAN MARINO').
	currency('STN', 930, 2, 'Dobra', 'SAO TOME AND PRINCIPE').
	currency('SAR', 682, 2, 'Saudi Riyal', 'SAUDI ARABIA').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'SENEGAL').
	currency('RSD', 941, 2, 'Serbian Dinar', 'SERBIA').
	currency('SCR', 690, 2, 'Seychelles Rupee', 'SEYCHELLES').
	currency('SLE', 925, 2, 'Leone', 'SIERRA LEONE').
	currency('SGD', 702, 2, 'Singapore Dollar', 'SINGAPORE').
	currency('XCG', 532, 2, 'Caribbean Guilder', 'SINT MAARTEN (DUTCH PART)').
	currency('XSU', 994, na, 'Sucre', 'SISTEMA UNITARIO DE COMPENSACION REGIONAL DE PAGOS "SUCRE"').
	currency('EUR', 978, 2, 'Euro', 'SLOVAKIA').
	currency('EUR', 978, 2, 'Euro', 'SLOVENIA').
	currency('SBD', 90, 2, 'Solomon Islands Dollar', 'SOLOMON ISLANDS').
	currency('SOS', 706, 2, 'Somali Shilling', 'SOMALIA').
	currency('ZAR', 710, 2, 'Rand', 'SOUTH AFRICA').
	currency('SSP', 728, 2, 'South Sudanese Pound', 'SOUTH SUDAN').
	currency('EUR', 978, 2, 'Euro', 'SPAIN').
	currency('LKR', 144, 2, 'Sri Lanka Rupee', 'SRI LANKA').
	currency('SDG', 938, 2, 'Sudanese Pound', 'SUDAN (THE)').
	currency('SRD', 968, 2, 'Surinam Dollar', 'SURINAME').
	currency('NOK', 578, 2, 'Norwegian Krone', 'SVALBARD AND JAN MAYEN').
	currency('SEK', 752, 2, 'Swedish Krona', 'SWEDEN').
	currency('CHF', 756, 2, 'Swiss Franc', 'SWITZERLAND').
	currency('SYP', 760, 2, 'Syrian Pound', 'SYRIAN ARAB REPUBLIC').
	currency('TWD', 901, 2, 'New Taiwan Dollar', 'TAIWAN (PROVINCE OF CHINA)').
	currency('TJS', 972, 2, 'Somoni', 'TAJIKISTAN').
	currency('TZS', 834, 2, 'Tanzanian Shilling', 'TANZANIA, UNITED REPUBLIC OF').
	currency('THB', 764, 2, 'Baht', 'THAILAND').
	currency('USD', 840, 2, 'US Dollar', 'TIMOR-LESTE').
	currency('XOF', 952, 0, 'CFA Franc BCEAO', 'TOGO').
	currency('NZD', 554, 2, 'New Zealand Dollar', 'TOKELAU').
	currency('TOP', 776, 2, 'Pa’anga', 'TONGA').
	currency('TTD', 780, 2, 'Trinidad and Tobago Dollar', 'TRINIDAD AND TOBAGO').
	currency('TND', 788, 3, 'Tunisian Dinar', 'TUNISIA').
	currency('TRY', 949, 2, 'Turkish Lira', 'TÜRKİYE').
	currency('TMT', 934, 2, 'Turkmenistan New Manat', 'TURKMENISTAN').
	currency('USD', 840, 2, 'US Dollar', 'TURKS AND CAICOS ISLANDS (THE)').
	currency('AUD', 36, 2, 'Australian Dollar', 'TUVALU').
	currency('UGX', 800, 0, 'Uganda Shilling', 'UGANDA').
	currency('UAH', 980, 2, 'Hryvnia', 'UKRAINE').
	currency('AED', 784, 2, 'UAE Dirham', 'UNITED ARAB EMIRATES (THE)').
	currency('GBP', 826, 2, 'Pound Sterling', 'UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND (THE)').
	currency('USD', 840, 2, 'US Dollar', 'UNITED STATES MINOR OUTLYING ISLANDS (THE)').
	currency('USD', 840, 2, 'US Dollar', 'UNITED STATES OF AMERICA (THE)').
	currency('UYU', 858, 2, 'Peso Uruguayo', 'URUGUAY').
	currency('UYW', 927, 4, 'Unidad Previsional', 'URUGUAY').
	currency('UZS', 860, 2, 'Uzbekistan Sum', 'UZBEKISTAN').
	currency('VUV', 548, 0, 'Vatu', 'VANUATU').
	currency('VES', 928, 2, 'Bolívar Soberano', 'VENEZUELA (BOLIVARIAN REPUBLIC OF)').
	currency('VED', 926, 2, 'Bolívar Soberano', 'VENEZUELA (BOLIVARIAN REPUBLIC OF)').
	currency('VND', 704, 0, 'Dong', 'VIET NAM').
	currency('USD', 840, 2, 'US Dollar', 'VIRGIN ISLANDS (BRITISH)').
	currency('USD', 840, 2, 'US Dollar', 'VIRGIN ISLANDS (U.S.)').
	currency('XPF', 953, 0, 'CFP Franc', 'WALLIS AND FUTUNA').
	currency('MAD', 504, 2, 'Moroccan Dirham', 'WESTERN SAHARA').
	currency('YER', 886, 2, 'Yemeni Rial', 'YEMEN').
	currency('ZMW', 967, 2, 'Zambian Kwacha', 'ZAMBIA').
	currency('ZWG', 924, 2, 'Zimbabwe Gold', 'ZIMBABWE').
	currency('XBA', 955, na, 'Bond Markets Unit European Composite Unit (EURCO)', 'ZZ01_Bond Markets Unit European_EURCO').
	currency('XBB', 956, na, 'Bond Markets Unit European Monetary Unit (E.M.U.-6)', 'ZZ02_Bond Markets Unit European_EMU-6').
	currency('XBC', 957, na, 'Bond Markets Unit European Unit of Account 9 (E.U.A.-9)', 'ZZ03_Bond Markets Unit European_EUA-9').
	currency('XBD', 958, na, 'Bond Markets Unit European Unit of Account 17 (E.U.A.-17)', 'ZZ04_Bond Markets Unit European_EUA-17').
	currency('XTS', 963, na, 'Codes specifically reserved for testing purposes', 'ZZ06_Testing_Code').
	currency('XXX', 999, na, 'The codes assigned for transactions where no currency is involved', 'ZZ07_No_Currency').
	currency('XAU', 959, na, 'Gold', 'ZZ08_Gold').
	currency('XPD', 964, na, 'Palladium', 'ZZ09_Palladium').
	currency('XPT', 962, na, 'Platinum', 'ZZ10_Platinum').
	currency('XAG', 961, na, 'Silver', 'ZZ11_Silver').
	fund_currency('BOV', 984, 2, 'Mvdol', 'BOLIVIA (PLURINATIONAL STATE OF)').
	fund_currency('CLF', 990, 4, 'Unidad de Fomento', 'CHILE').
	fund_currency('COU', 970, 2, 'Unidad de Valor Real', 'COLOMBIA').
	fund_currency('MXV', 979, 2, 'Mexican Unidad de Inversion (UDI)', 'MEXICO').
	fund_currency('CHE', 947, 2, 'WIR Euro', 'SWITZERLAND').
	fund_currency('CHW', 948, 2, 'WIR Franc', 'SWITZERLAND').
	fund_currency('USN', 997, 2, 'US Dollar (Next day)', 'UNITED STATES OF AMERICA (THE)').
	fund_currency('UYI', 940, 0, 'Uruguay Peso en Unidades Indexadas (UI)', 'URUGUAY').

:- end_object.
