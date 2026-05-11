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


:- object(iso_639_3).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Generated ISO 639-3 fact table.',
		remarks is [
			'Source update' - '2026-04-15',
			'Generated entries' - '7929',
			'Source' - 'SIL ISO 639-3 tab-delimited code set.'
		]
	]).

	:- public(language/5).
	:- mode(language(?atom, ?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(language/5, [
		comment is 'Generated ISO 639-3 fact table.',
		argnames is ['Alpha3', 'Alpha2', 'Scope', 'Type', 'Name']
	]).

	language('aaa', '', individual, living, 'Ghotuo').
	language('aab', '', individual, living, 'Alumu-Tesu').
	language('aac', '', individual, living, 'Ari').
	language('aad', '', individual, living, 'Amal').
	language('aae', '', individual, living, 'Arbëreshë Albanian').
	language('aaf', '', individual, living, 'Aranadan').
	language('aag', '', individual, living, 'Ambrak').
	language('aah', '', individual, living, 'Abu'' Arapesh').
	language('aai', '', individual, living, 'Arifama-Miniafia').
	language('aak', '', individual, living, 'Ankave').
	language('aal', '', individual, living, 'Afade').
	language('aan', '', individual, living, 'Anambé').
	language('aao', '', individual, living, 'Algerian Saharan Arabic').
	language('aap', '', individual, living, 'Pará Arára').
	language('aaq', '', individual, extinct, 'Eastern Abnaki').
	language('aar', 'aa', individual, living, 'Afar').
	language('aas', '', individual, living, 'Aasáx').
	language('aat', '', individual, living, 'Arvanitika Albanian').
	language('aau', '', individual, living, 'Abau').
	language('aaw', '', individual, living, 'Solong').
	language('aax', '', individual, living, 'Mandobo Atas').
	language('aaz', '', individual, living, 'Amarasi').
	language('aba', '', individual, living, 'Abé').
	language('abb', '', individual, living, 'Bankon').
	language('abc', '', individual, living, 'Ambala Ayta').
	language('abd', '', individual, living, 'Manide').
	language('abe', '', individual, living, 'Western Abnaki').
	language('abf', '', individual, living, 'Abai Sungai').
	language('abg', '', individual, living, 'Abaga').
	language('abh', '', individual, living, 'Tajiki Arabic').
	language('abi', '', individual, living, 'Abidji').
	language('abj', '', individual, extinct, 'Aka-Bea').
	language('abk', 'ab', individual, living, 'Abkhazian').
	language('abl', '', individual, living, 'Lampung Nyo').
	language('abm', '', individual, living, 'Abanyom').
	language('abn', '', individual, living, 'Abua').
	language('abo', '', individual, living, 'Abon').
	language('abp', '', individual, living, 'Abellen Ayta').
	language('abq', '', individual, living, 'Abaza').
	language('abr', '', individual, living, 'Abron').
	language('abs', '', individual, living, 'Ambonese Malay').
	language('abt', '', individual, living, 'Ambulas').
	language('abu', '', individual, living, 'Abure').
	language('abv', '', individual, living, 'Baharna Arabic').
	language('abw', '', individual, living, 'Pal').
	language('abx', '', individual, living, 'Inabaknon').
	language('aby', '', individual, living, 'Aneme Wake').
	language('abz', '', individual, living, 'Abui').
	language('aca', '', individual, living, 'Achagua').
	language('acb', '', individual, living, 'Áncá').
	language('acd', '', individual, living, 'Gikyode').
	language('ace', '', individual, living, 'Achinese').
	language('acf', '', individual, living, 'Saint Lucian Creole French').
	language('ach', '', individual, living, 'Acoli').
	language('aci', '', individual, extinct, 'Aka-Cari').
	language('ack', '', individual, extinct, 'Aka-Kora').
	language('acl', '', individual, extinct, 'Akar-Bale').
	language('acm', '', individual, living, 'Mesopotamian Arabic').
	language('acn', '', individual, living, 'Achang').
	language('acp', '', individual, living, 'Eastern Acipa').
	language('acq', '', individual, living, 'Ta''izzi-Adeni Arabic').
	language('acr', '', individual, living, 'Achi').
	language('acs', '', individual, extinct, 'Acroá').
	language('act', '', individual, living, 'Achterhoeks').
	language('acu', '', individual, living, 'Achuar-Shiwiar').
	language('acv', '', individual, living, 'Achumawi').
	language('acw', '', individual, living, 'Hijazi Arabic').
	language('acx', '', individual, living, 'Omani Arabic').
	language('acy', '', individual, living, 'Cypriot Arabic').
	language('acz', '', individual, living, 'Acheron').
	language('ada', '', individual, living, 'Adangme').
	language('adb', '', individual, living, 'Atauran').
	language('add', '', individual, living, 'Lidzonka').
	language('ade', '', individual, living, 'Adele').
	language('adf', '', individual, living, 'Dhofari Arabic').
	language('adg', '', individual, living, 'Andegerebinha').
	language('adh', '', individual, living, 'Adhola').
	language('adi', '', individual, living, 'Adi').
	language('adj', '', individual, living, 'Adioukrou').
	language('adl', '', individual, living, 'Galo').
	language('adn', '', individual, living, 'Adang').
	language('ado', '', individual, living, 'Abu').
	language('adq', '', individual, living, 'Adangbe').
	language('adr', '', individual, living, 'Adonara').
	language('ads', '', individual, living, 'Adamorobe Sign Language').
	language('adt', '', individual, living, 'Adnyamathanha').
	language('adu', '', individual, living, 'Aduge').
	language('adw', '', individual, living, 'Amundava').
	language('adx', '', individual, living, 'Amdo Tibetan').
	language('ady', '', individual, living, 'Adyghe').
	language('adz', '', individual, living, 'Adzera').
	language('aea', '', individual, extinct, 'Areba').
	language('aeb', '', individual, living, 'Tunisian Arabic').
	language('aec', '', individual, living, 'Saidi Arabic').
	language('aed', '', individual, living, 'Argentine Sign Language').
	language('aee', '', individual, living, 'Northeast Pashai').
	language('aek', '', individual, living, 'Haeke').
	language('ael', '', individual, living, 'Ambele').
	language('aem', '', individual, living, 'Arem').
	language('aen', '', individual, living, 'Armenian Sign Language').
	language('aeq', '', individual, living, 'Aer').
	language('aer', '', individual, living, 'Eastern Arrernte').
	language('aes', '', individual, extinct, 'Alsea').
	language('aeu', '', individual, living, 'Akeu').
	language('aew', '', individual, living, 'Ambakich').
	language('aey', '', individual, living, 'Amele').
	language('aez', '', individual, living, 'Aeka').
	language('afb', '', individual, living, 'Gulf Arabic').
	language('afd', '', individual, living, 'Andai').
	language('afe', '', individual, living, 'Putukwam').
	language('afg', '', individual, living, 'Afghan Sign Language').
	language('afh', '', individual, constructed, 'Afrihili').
	language('afi', '', individual, living, 'Akrukay').
	language('afk', '', individual, living, 'Nanubae').
	language('afn', '', individual, living, 'Defaka').
	language('afo', '', individual, living, 'Eloyi').
	language('afp', '', individual, living, 'Tapei').
	language('afr', 'af', individual, living, 'Afrikaans').
	language('afs', '', individual, living, 'Afro-Seminole Creole').
	language('aft', '', individual, living, 'Afitti').
	language('afu', '', individual, living, 'Awutu').
	language('afz', '', individual, living, 'Obokuitai').
	language('aga', '', individual, extinct, 'Aguano').
	language('agb', '', individual, living, 'Legbo').
	language('agc', '', individual, living, 'Agatu').
	language('agd', '', individual, living, 'Agarabi').
	language('age', '', individual, living, 'Angal').
	language('agf', '', individual, living, 'Arguni').
	language('agg', '', individual, living, 'Angor').
	language('agh', '', individual, living, 'Ngelima').
	language('agi', '', individual, living, 'Agariya').
	language('agj', '', individual, living, 'Argobba').
	language('agk', '', individual, living, 'Isarog Agta').
	language('agl', '', individual, living, 'Fembe').
	language('agm', '', individual, living, 'Angaataha').
	language('agn', '', individual, living, 'Agutaynen').
	language('ago', '', individual, living, 'Tainae').
	language('agq', '', individual, living, 'Aghem').
	language('agr', '', individual, living, 'Aguaruna').
	language('ags', '', individual, living, 'Esimbi').
	language('agt', '', individual, living, 'Central Cagayan Agta').
	language('agu', '', individual, living, 'Aguacateco').
	language('agv', '', individual, living, 'Remontado Dumagat').
	language('agw', '', individual, living, 'Kahua').
	language('agx', '', individual, living, 'Aghul').
	language('agy', '', individual, living, 'Southern Alta').
	language('agz', '', individual, living, 'Mt. Iriga Agta').
	language('aha', '', individual, living, 'Ahanta').
	language('ahb', '', individual, living, 'Axamb').
	language('ahg', '', individual, living, 'Qimant').
	language('ahh', '', individual, living, 'Aghu').
	language('ahi', '', individual, living, 'Tiagbamrin Aizi').
	language('ahk', '', individual, living, 'Akha').
	language('ahl', '', individual, living, 'Igo').
	language('ahm', '', individual, living, 'Mobumrin Aizi').
	language('ahn', '', individual, living, 'Àhàn').
	language('aho', '', individual, extinct, 'Ahom').
	language('ahp', '', individual, living, 'Aproumu Aizi').
	language('ahr', '', individual, living, 'Ahirani').
	language('ahs', '', individual, living, 'Ashe').
	language('aht', '', individual, living, 'Ahtena').
	language('aia', '', individual, living, 'Arosi').
	language('aib', '', individual, living, 'Ainu (China)').
	language('aic', '', individual, living, 'Ainbai').
	language('aid', '', individual, extinct, 'Alngith').
	language('aie', '', individual, living, 'Amara').
	language('aif', '', individual, living, 'Agi').
	language('aig', '', individual, living, 'Antigua and Barbuda Creole English').
	language('aih', '', individual, living, 'Ai-Cham').
	language('aii', '', individual, living, 'Assyrian Neo-Aramaic').
	language('aij', '', individual, living, 'Lishanid Noshan').
	language('aik', '', individual, living, 'Ake').
	language('ail', '', individual, living, 'Aimele').
	language('aim', '', individual, living, 'Aimol').
	language('ain', '', individual, living, 'Ainu (Japan)').
	language('aio', '', individual, living, 'Aiton').
	language('aip', '', individual, living, 'Burumakok').
	language('aiq', '', individual, living, 'Aimaq').
	language('air', '', individual, living, 'Airoran').
	language('ait', '', individual, extinct, 'Arikem').
	language('aiw', '', individual, living, 'Aari').
	language('aix', '', individual, living, 'Aighon').
	language('aiy', '', individual, living, 'Ali').
	language('aja', '', individual, living, 'Aja (South Sudan)').
	language('ajg', '', individual, living, 'Aja (Benin)').
	language('aji', '', individual, living, 'Ajië').
	language('ajn', '', individual, living, 'Andajin').
	language('ajs', '', individual, living, 'Algerian Jewish Sign Language').
	language('aju', '', individual, living, 'Judeo-Moroccan Arabic').
	language('ajw', '', individual, extinct, 'Ajawa').
	language('ajz', '', individual, living, 'Amri Karbi').
	language('aka', 'ak', macrolanguage, living, 'Akan').
	language('akb', '', individual, living, 'Batak Angkola').
	language('akc', '', individual, living, 'Mpur').
	language('akd', '', individual, living, 'Ukpet-Ehom').
	language('ake', '', individual, living, 'Akawaio').
	language('akf', '', individual, living, 'Akpa').
	language('akg', '', individual, living, 'Anakalangu').
	language('akh', '', individual, living, 'Angal Heneng').
	language('aki', '', individual, living, 'Aiome').
	language('akj', '', individual, extinct, 'Aka-Jeru').
	language('akk', '', individual, special, 'Akkadian').
	language('akl', '', individual, living, 'Aklanon').
	language('akm', '', individual, extinct, 'Aka-Bo').
	language('ako', '', individual, living, 'Akurio').
	language('akp', '', individual, living, 'Siwu').
	language('akq', '', individual, living, 'Ak').
	language('akr', '', individual, living, 'Araki').
	language('aks', '', individual, living, 'Akaselem').
	language('akt', '', individual, living, 'Akolet').
	language('aku', '', individual, living, 'Akum').
	language('akv', '', individual, living, 'Akhvakh').
	language('akw', '', individual, living, 'Akwa').
	language('akx', '', individual, extinct, 'Aka-Kede').
	language('aky', '', individual, extinct, 'Aka-Kol').
	language('akz', '', individual, living, 'Alabama').
	language('ala', '', individual, living, 'Alago').
	language('alc', '', individual, living, 'Qawasqar').
	language('ald', '', individual, living, 'Alladian').
	language('ale', '', individual, living, 'Aleut').
	language('alf', '', individual, living, 'Alege').
	language('alh', '', individual, living, 'Alawa').
	language('ali', '', individual, living, 'Amaimon').
	language('alj', '', individual, living, 'Alangan').
	language('alk', '', individual, living, 'Alak').
	language('all', '', individual, living, 'Allar').
	language('alm', '', individual, living, 'Amblong').
	language('aln', '', individual, living, 'Gheg Albanian').
	language('alo', '', individual, living, 'Larike-Wakasihu').
	language('alp', '', individual, living, 'Alune').
	language('alq', '', individual, living, 'Algonquin').
	language('alr', '', individual, living, 'Alutor').
	language('als', '', individual, living, 'Tosk Albanian').
	language('alt', '', individual, living, 'Southern Altai').
	language('alu', '', individual, living, '''Are''are').
	language('alw', '', individual, living, 'Alaba-K’abeena').
	language('alx', '', individual, living, 'Amol').
	language('aly', '', individual, living, 'Alyawarr').
	language('alz', '', individual, living, 'Alur').
	language('ama', '', individual, extinct, 'Amanayé').
	language('amb', '', individual, living, 'Ambo').
	language('amc', '', individual, living, 'Amahuaca').
	language('ame', '', individual, living, 'Yanesha''').
	language('amf', '', individual, living, 'Hamer-Banna').
	language('amg', '', individual, living, 'Amurdak').
	language('amh', 'am', individual, living, 'Amharic').
	language('ami', '', individual, living, 'Amis').
	language('amj', '', individual, living, 'Amdang').
	language('amk', '', individual, living, 'Ambai').
	language('aml', '', individual, living, 'War-Jaintia').
	language('amm', '', individual, living, 'Ama (Papua New Guinea)').
	language('amn', '', individual, living, 'Amanab').
	language('amo', '', individual, living, 'Amo').
	language('amp', '', individual, living, 'Alamblak').
	language('amq', '', individual, living, 'Amahai').
	language('amr', '', individual, living, 'Amarakaeri').
	language('ams', '', individual, living, 'Southern Amami-Oshima').
	language('amt', '', individual, living, 'Amto').
	language('amu', '', individual, living, 'Guerrero Amuzgo').
	language('amv', '', individual, living, 'Ambelau').
	language('amw', '', individual, living, 'Western Neo-Aramaic').
	language('amx', '', individual, living, 'Anmatyerre').
	language('amy', '', individual, living, 'Ami').
	language('amz', '', individual, extinct, 'Atampaya').
	language('ana', '', individual, extinct, 'Andaqui').
	language('anb', '', individual, extinct, 'Andoa').
	language('anc', '', individual, living, 'Ngas').
	language('and', '', individual, living, 'Ansus').
	language('ane', '', individual, living, 'Xârâcùù').
	language('anf', '', individual, living, 'Animere').
	language('ang', '', individual, special, 'Old English (ca. 450-1100)').
	language('anh', '', individual, living, 'Nend').
	language('ani', '', individual, living, 'Andi').
	language('anj', '', individual, living, 'Anor').
	language('ank', '', individual, living, 'Goemai').
	language('anl', '', individual, living, 'Anu-Hkongso Chin').
	language('anm', '', individual, living, 'Anal').
	language('ann', '', individual, living, 'Obolo').
	language('ano', '', individual, living, 'Andoque').
	language('anp', '', individual, living, 'Angika').
	language('anq', '', individual, living, 'Jarawa (India)').
	language('anr', '', individual, living, 'Andh').
	language('ans', '', individual, extinct, 'Anserma').
	language('ant', '', individual, living, 'Antakarinya').
	language('anu', '', individual, living, 'Anuak').
	language('anv', '', individual, living, 'Denya').
	language('anw', '', individual, living, 'Anaang').
	language('anx', '', individual, living, 'Andra-Hus').
	language('any', '', individual, living, 'Anyin').
	language('anz', '', individual, living, 'Anem').
	language('aoa', '', individual, living, 'Angolar').
	language('aob', '', individual, living, 'Abom').
	language('aoc', '', individual, living, 'Pemon').
	language('aod', '', individual, living, 'Andarum').
	language('aoe', '', individual, living, 'Angal Enen').
	language('aof', '', individual, living, 'Bragat').
	language('aog', '', individual, living, 'Angoram').
	language('aoi', '', individual, living, 'Anindilyakwa').
	language('aoj', '', individual, living, 'Mufian').
	language('aok', '', individual, living, 'Arhö').
	language('aol', '', individual, living, 'Alor').
	language('aom', '', individual, living, 'Ömie').
	language('aon', '', individual, living, 'Bumbita Arapesh').
	language('aor', '', individual, extinct, 'Aore').
	language('aos', '', individual, living, 'Taikat').
	language('aot', '', individual, living, 'Atong (India)').
	language('aou', '', individual, living, 'A''ou').
	language('aox', '', individual, living, 'Atorada').
	language('aoz', '', individual, living, 'Uab Meto').
	language('apb', '', individual, living, 'Sa''a').
	language('apc', '', individual, living, 'Levantine Arabic').
	language('apd', '', individual, living, 'Sudanese Arabic').
	language('ape', '', individual, living, 'Bukiyip').
	language('apf', '', individual, living, 'Pahanan Agta').
	language('apg', '', individual, living, 'Ampanang').
	language('aph', '', individual, living, 'Athpariya').
	language('api', '', individual, living, 'Apiaká').
	language('apj', '', individual, living, 'Jicarilla Apache').
	language('apk', '', individual, living, 'Kiowa Apache').
	language('apl', '', individual, living, 'Lipan Apache').
	language('apm', '', individual, living, 'Mescalero-Chiricahua Apache').
	language('apn', '', individual, living, 'Apinayé').
	language('apo', '', individual, living, 'Ambul').
	language('app', '', individual, living, 'Apma').
	language('apq', '', individual, living, 'A-Pucikwar').
	language('apr', '', individual, living, 'Arop-Lokep').
	language('aps', '', individual, living, 'Arop-Sissano').
	language('apt', '', individual, living, 'Apatani').
	language('apu', '', individual, living, 'Apurinã').
	language('apv', '', individual, extinct, 'Alapmunte').
	language('apw', '', individual, living, 'Western Apache').
	language('apx', '', individual, living, 'Aputai').
	language('apy', '', individual, living, 'Apalaí').
	language('apz', '', individual, living, 'Safeyoka').
	language('aqc', '', individual, living, 'Archi').
	language('aqd', '', individual, living, 'Ampari Dogon').
	language('aqg', '', individual, living, 'Arigidi').
	language('aqk', '', individual, living, 'Aninka').
	language('aqm', '', individual, living, 'Atohwaim').
	language('aqn', '', individual, living, 'Northern Alta').
	language('aqp', '', individual, extinct, 'Atakapa').
	language('aqr', '', individual, living, 'Arhâ').
	language('aqt', '', individual, living, 'Angaité').
	language('aqz', '', individual, living, 'Akuntsu').
	language('ara', 'ar', macrolanguage, living, 'Arabic').
	language('arb', '', individual, living, 'Standard Arabic').
	language('arc', '', individual, special, 'Official Aramaic (700-300 BCE)').
	language('ard', '', individual, extinct, 'Arabana').
	language('are', '', individual, living, 'Western Arrarnta').
	language('arg', 'an', individual, living, 'Aragonese').
	language('arh', '', individual, living, 'Arhuaco').
	language('ari', '', individual, living, 'Arikara').
	language('arj', '', individual, extinct, 'Arapaso').
	language('ark', '', individual, living, 'Arikapú').
	language('arl', '', individual, living, 'Arabela').
	language('arn', '', individual, living, 'Mapudungun').
	language('aro', '', individual, living, 'Araona').
	language('arp', '', individual, living, 'Arapaho').
	language('arq', '', individual, living, 'Algerian Arabic').
	language('arr', '', individual, living, 'Karo (Brazil)').
	language('ars', '', individual, living, 'Najdi Arabic').
	language('aru', '', individual, extinct, 'Aruá (Amazonas State)').
	language('arv', '', individual, living, 'Arbore').
	language('arw', '', individual, living, 'Arawak').
	language('arx', '', individual, living, 'Aruá (Rodonia State)').
	language('ary', '', individual, living, 'Moroccan Arabic').
	language('arz', '', individual, living, 'Egyptian Arabic').
	language('asa', '', individual, living, 'Asu (Tanzania)').
	language('asb', '', individual, living, 'Assiniboine').
	language('asc', '', individual, living, 'Casuarina Coast Asmat').
	language('ase', '', individual, living, 'American Sign Language').
	language('asf', '', individual, living, 'Auslan').
	language('asg', '', individual, living, 'Cishingini').
	language('ash', '', individual, extinct, 'Abishira').
	language('asi', '', individual, living, 'Buruwai').
	language('asj', '', individual, living, 'Sari').
	language('ask', '', individual, living, 'Ashkun').
	language('asl', '', individual, living, 'Asilulu').
	language('asm', 'as', individual, living, 'Assamese').
	language('asn', '', individual, living, 'Xingú Asuriní').
	language('aso', '', individual, living, 'Dano').
	language('asp', '', individual, living, 'Algerian Sign Language').
	language('asq', '', individual, living, 'Austrian Sign Language').
	language('asr', '', individual, living, 'Asuri').
	language('ass', '', individual, living, 'Ipulo').
	language('ast', '', individual, living, 'Asturian').
	language('asu', '', individual, living, 'Tocantins Asurini').
	language('asv', '', individual, living, 'Asoa').
	language('asw', '', individual, living, 'Australian Aborigines Sign Language').
	language('asx', '', individual, living, 'Muratayak').
	language('asy', '', individual, living, 'Yaosakor Asmat').
	language('asz', '', individual, living, 'As').
	language('ata', '', individual, living, 'Pele-Ata').
	language('atb', '', individual, living, 'Zaiwa').
	language('atc', '', individual, extinct, 'Atsahuaca').
	language('atd', '', individual, living, 'Ata Manobo').
	language('ate', '', individual, living, 'Atemble').
	language('atg', '', individual, living, 'Ivbie North-Okpela-Arhe').
	language('ati', '', individual, living, 'Attié').
	language('atj', '', individual, living, 'Atikamekw').
	language('atk', '', individual, living, 'Ati').
	language('atl', '', individual, living, 'Mt. Iraya Agta').
	language('atm', '', individual, living, 'Ata').
	language('atn', '', individual, living, 'Ashtiani').
	language('ato', '', individual, living, 'Atong (Cameroon)').
	language('atp', '', individual, living, 'Pudtol Atta').
	language('atq', '', individual, living, 'Aralle-Tabulahan').
	language('atr', '', individual, living, 'Waimiri-Atroari').
	language('ats', '', individual, living, 'Gros Ventre').
	language('att', '', individual, living, 'Pamplona Atta').
	language('atu', '', individual, living, 'Reel').
	language('atv', '', individual, living, 'Northern Altai').
	language('atw', '', individual, living, 'Atsugewi').
	language('atx', '', individual, living, 'Arutani').
	language('aty', '', individual, living, 'Aneityum').
	language('atz', '', individual, living, 'Arta').
	language('aua', '', individual, living, 'Asumboa').
	language('aub', '', individual, living, 'Alugu').
	language('auc', '', individual, living, 'Waorani').
	language('aud', '', individual, living, 'Anuta').
	language('aug', '', individual, living, 'Aguna').
	language('auh', '', individual, living, 'Aushi').
	language('aui', '', individual, living, 'Anuki').
	language('auj', '', individual, living, 'Awjilah').
	language('auk', '', individual, living, 'Heyo').
	language('aul', '', individual, living, 'Aulua').
	language('aum', '', individual, living, 'Asu (Nigeria)').
	language('aun', '', individual, living, 'Molmo One').
	language('auo', '', individual, extinct, 'Auyokawa').
	language('aup', '', individual, living, 'Makayam').
	language('auq', '', individual, living, 'Anus').
	language('aur', '', individual, living, 'Aruek').
	language('aut', '', individual, living, 'Austral').
	language('auu', '', individual, living, 'Auye').
	language('auw', '', individual, living, 'Awyi').
	language('aux', '', individual, extinct, 'Aurá').
	language('auy', '', individual, living, 'Awiyaana').
	language('auz', '', individual, living, 'Uzbeki Arabic').
	language('ava', 'av', individual, living, 'Avaric').
	language('avb', '', individual, living, 'Avau').
	language('avd', '', individual, living, 'Alviri-Vidari').
	language('ave', 'ae', individual, special, 'Avestan').
	language('avi', '', individual, living, 'Avikam').
	language('avk', '', individual, constructed, 'Kotava').
	language('avl', '', individual, living, 'Eastern Egyptian Bedawi Arabic').
	language('avm', '', individual, extinct, 'Angkamuthi').
	language('avn', '', individual, living, 'Avatime').
	language('avo', '', individual, extinct, 'Agavotaguerra').
	language('avs', '', individual, extinct, 'Aushiri').
	language('avt', '', individual, living, 'Au').
	language('avu', '', individual, living, 'Avokaya').
	language('avv', '', individual, living, 'Avá-Canoeiro').
	language('awa', '', individual, living, 'Awadhi').
	language('awb', '', individual, living, 'Awa (Papua New Guinea)').
	language('awc', '', individual, living, 'Cicipu').
	language('awe', '', individual, living, 'Awetí').
	language('awg', '', individual, extinct, 'Anguthimri').
	language('awh', '', individual, living, 'Awbono').
	language('awi', '', individual, living, 'Aekyom').
	language('awk', '', individual, extinct, 'Awabakal').
	language('awm', '', individual, living, 'Arawum').
	language('awn', '', individual, living, 'Awngi').
	language('awo', '', individual, living, 'Awak').
	language('awr', '', individual, living, 'Awera').
	language('aws', '', individual, living, 'South Awyu').
	language('awt', '', individual, living, 'Araweté').
	language('awu', '', individual, living, 'Central Awyu').
	language('awv', '', individual, living, 'Jair Awyu').
	language('aww', '', individual, living, 'Awun').
	language('awx', '', individual, living, 'Awara').
	language('awy', '', individual, living, 'Edera Awyu').
	language('axb', '', individual, extinct, 'Abipon').
	language('axe', '', individual, extinct, 'Ayerrerenge').
	language('axg', '', individual, extinct, 'Mato Grosso Arára').
	language('axk', '', individual, living, 'Yaka (Central African Republic)').
	language('axl', '', individual, extinct, 'Lower Southern Aranda').
	language('axm', '', individual, special, 'Middle Armenian').
	language('axx', '', individual, living, 'Xârâgurè').
	language('aya', '', individual, living, 'Awar').
	language('ayb', '', individual, living, 'Ayizo Gbe').
	language('ayc', '', individual, living, 'Southern Aymara').
	language('ayd', '', individual, extinct, 'Ayabadhu').
	language('aye', '', individual, living, 'Ayere').
	language('ayg', '', individual, living, 'Ginyanga').
	language('ayh', '', individual, living, 'Hadrami Arabic').
	language('ayi', '', individual, living, 'Leyigha').
	language('ayk', '', individual, living, 'Akuku').
	language('ayl', '', individual, living, 'Libyan Arabic').
	language('aym', 'ay', macrolanguage, living, 'Aymara').
	language('ayn', '', individual, living, 'Sanaani Arabic').
	language('ayo', '', individual, living, 'Ayoreo').
	language('ayp', '', individual, living, 'North Mesopotamian Arabic').
	language('ayq', '', individual, living, 'Ayi (Papua New Guinea)').
	language('ayr', '', individual, living, 'Central Aymara').
	language('ays', '', individual, living, 'Sorsogon Ayta').
	language('ayt', '', individual, living, 'Magbukun Ayta').
	language('ayu', '', individual, living, 'Ayu').
	language('ayz', '', individual, living, 'Mai Brat').
	language('aza', '', individual, living, 'Azha').
	language('azb', '', individual, living, 'South Azerbaijani').
	language('azd', '', individual, living, 'Eastern Durango Nahuatl').
	language('aze', 'az', macrolanguage, living, 'Azerbaijani').
	language('azg', '', individual, living, 'San Pedro Amuzgos Amuzgo').
	language('azj', '', individual, living, 'North Azerbaijani').
	language('azm', '', individual, living, 'Ipalapa Amuzgo').
	language('azn', '', individual, living, 'Western Durango Nahuatl').
	language('azo', '', individual, living, 'Awing').
	language('azt', '', individual, living, 'Faire Atta').
	language('azz', '', individual, living, 'Highland Puebla Nahuatl').
	language('baa', '', individual, living, 'Babatana').
	language('bab', '', individual, living, 'Bainouk-Gunyuño').
	language('bac', '', individual, living, 'Badui').
	language('bae', '', individual, extinct, 'Baré').
	language('baf', '', individual, living, 'Nubaca').
	language('bag', '', individual, living, 'Tuki').
	language('bah', '', individual, living, 'Bahamas Creole English').
	language('baj', '', individual, living, 'Barakai').
	language('bak', 'ba', individual, living, 'Bashkir').
	language('bal', '', macrolanguage, living, 'Baluchi').
	language('bam', 'bm', individual, living, 'Bambara').
	language('ban', '', individual, living, 'Balinese').
	language('bao', '', individual, living, 'Waimaha').
	language('bap', '', individual, living, 'Bantawa').
	language('bar', '', individual, living, 'Bavarian').
	language('bas', '', individual, living, 'Basa (Cameroon)').
	language('bau', '', individual, living, 'Bada (Nigeria)').
	language('bav', '', individual, living, 'Vengo').
	language('baw', '', individual, living, 'Bambili-Bambui').
	language('bax', '', individual, living, 'Bamun').
	language('bay', '', individual, living, 'Batuley').
	language('bba', '', individual, living, 'Baatonum').
	language('bbb', '', individual, living, 'Barai').
	language('bbc', '', individual, living, 'Batak Toba').
	language('bbd', '', individual, living, 'Bau').
	language('bbe', '', individual, living, 'Bangba').
	language('bbf', '', individual, living, 'Baibai').
	language('bbg', '', individual, living, 'Barama').
	language('bbh', '', individual, living, 'Bugan').
	language('bbi', '', individual, living, 'Barombi').
	language('bbj', '', individual, living, 'Ghomálá''').
	language('bbk', '', individual, living, 'Babanki').
	language('bbl', '', individual, living, 'Bats').
	language('bbm', '', individual, living, 'Babango').
	language('bbn', '', individual, living, 'Uneapa').
	language('bbo', '', individual, living, 'Northern Bobo Madaré').
	language('bbp', '', individual, living, 'West Central Banda').
	language('bbq', '', individual, living, 'Bamali').
	language('bbr', '', individual, living, 'Girawa').
	language('bbs', '', individual, living, 'Bakpinka').
	language('bbt', '', individual, living, 'Mburku').
	language('bbu', '', individual, living, 'Kulung (Nigeria)').
	language('bbv', '', individual, living, 'Karnai').
	language('bbw', '', individual, living, 'Baba').
	language('bbx', '', individual, living, 'Bubia').
	language('bby', '', individual, living, 'Befang').
	language('bca', '', individual, living, 'Central Bai').
	language('bcb', '', individual, living, 'Bainouk-Samik').
	language('bcc', '', individual, living, 'Southern Balochi').
	language('bcd', '', individual, living, 'North Babar').
	language('bce', '', individual, living, 'Bamenyam').
	language('bcf', '', individual, living, 'Bamu').
	language('bcg', '', individual, living, 'Baga Pokur').
	language('bch', '', individual, living, 'Bariai').
	language('bci', '', individual, living, 'Baoulé').
	language('bcj', '', individual, living, 'Bardi').
	language('bck', '', individual, living, 'Bunuba').
	language('bcl', '', individual, living, 'Central Bikol').
	language('bcm', '', individual, living, 'Bannoni').
	language('bcn', '', individual, living, 'Bali (Nigeria)').
	language('bco', '', individual, living, 'Kaluli').
	language('bcp', '', individual, living, 'Bali (Democratic Republic of Congo)').
	language('bcq', '', individual, living, 'Bench').
	language('bcr', '', individual, living, 'Babine').
	language('bcs', '', individual, living, 'Kohumono').
	language('bct', '', individual, living, 'Bendi').
	language('bcu', '', individual, living, 'Awad Bing').
	language('bcv', '', individual, living, 'Shoo-Minda-Nye').
	language('bcw', '', individual, living, 'Bana').
	language('bcy', '', individual, living, 'Bacama').
	language('bcz', '', individual, living, 'Bainouk-Gunyaamolo').
	language('bda', '', individual, living, 'Bayot').
	language('bdb', '', individual, living, 'Basap').
	language('bdc', '', individual, living, 'Emberá-Baudó').
	language('bdd', '', individual, living, 'Bunama').
	language('bde', '', individual, living, 'Bade').
	language('bdf', '', individual, living, 'Biage').
	language('bdg', '', individual, living, 'Bonggi').
	language('bdh', '', individual, living, 'Baka (South Sudan)').
	language('bdi', '', individual, living, 'Burun').
	language('bdj', '', individual, living, 'Bai (South Sudan)').
	language('bdk', '', individual, living, 'Budukh').
	language('bdl', '', individual, living, 'Indonesian Bajau').
	language('bdm', '', individual, living, 'Buduma').
	language('bdn', '', individual, living, 'Baldemu').
	language('bdo', '', individual, living, 'Morom').
	language('bdp', '', individual, living, 'Bende').
	language('bdq', '', individual, living, 'Bahnar').
	language('bdr', '', individual, living, 'West Coast Bajau').
	language('bds', '', individual, living, 'Burunge').
	language('bdt', '', individual, living, 'Bokoto').
	language('bdu', '', individual, living, 'Oroko').
	language('bdv', '', individual, living, 'Bodo Parja').
	language('bdw', '', individual, living, 'Baham').
	language('bdx', '', individual, living, 'Budong-Budong').
	language('bdy', '', individual, living, 'Bandjalang').
	language('bdz', '', individual, living, 'Badeshi').
	language('bea', '', individual, living, 'Beaver').
	language('beb', '', individual, living, 'Bebele').
	language('bec', '', individual, living, 'Iceve-Maci').
	language('bed', '', individual, living, 'Bedoanas').
	language('bee', '', individual, living, 'Byangsi').
	language('bef', '', individual, living, 'Benabena').
	language('beg', '', individual, living, 'Belait').
	language('beh', '', individual, living, 'Biali').
	language('bei', '', individual, living, 'Bekati''').
	language('bej', '', individual, living, 'Beja').
	language('bek', '', individual, living, 'Bebeli').
	language('bel', 'be', individual, living, 'Belarusian').
	language('bem', '', individual, living, 'Bemba (Zambia)').
	language('ben', 'bn', individual, living, 'Bengali').
	language('beo', '', individual, living, 'Beami').
	language('bep', '', individual, living, 'Besoa').
	language('beq', '', individual, living, 'Beembe').
	language('bes', '', individual, living, 'Besme').
	language('bet', '', individual, living, 'Guiberoua Béte').
	language('beu', '', individual, living, 'Blagar').
	language('bev', '', individual, living, 'Daloa Bété').
	language('bew', '', individual, living, 'Betawi').
	language('bex', '', individual, living, 'Jur Modo').
	language('bey', '', individual, living, 'Beli (Papua New Guinea)').
	language('bez', '', individual, living, 'Bena (Tanzania)').
	language('bfa', '', individual, living, 'Bari').
	language('bfb', '', individual, living, 'Pauri Bareli').
	language('bfc', '', individual, living, 'Panyi Bai').
	language('bfd', '', individual, living, 'Bafut').
	language('bfe', '', individual, living, 'Betaf').
	language('bff', '', individual, living, 'Bofi').
	language('bfg', '', individual, living, 'Busang Kayan').
	language('bfh', '', individual, living, 'Blafe').
	language('bfi', '', individual, living, 'British Sign Language').
	language('bfj', '', individual, living, 'Bafanji').
	language('bfk', '', individual, living, 'Ban Khor Sign Language').
	language('bfl', '', individual, living, 'Banda-Ndélé').
	language('bfm', '', individual, living, 'Mmen').
	language('bfn', '', individual, living, 'Bunak').
	language('bfo', '', individual, living, 'Malba Birifor').
	language('bfp', '', individual, living, 'Beba').
	language('bfq', '', individual, living, 'Badaga').
	language('bfr', '', individual, living, 'Bazigar').
	language('bfs', '', individual, living, 'Southern Bai').
	language('bft', '', individual, living, 'Balti').
	language('bfu', '', individual, living, 'Gahri').
	language('bfw', '', individual, living, 'Bondo').
	language('bfx', '', individual, living, 'Bantayanon').
	language('bfy', '', individual, living, 'Bagheli').
	language('bfz', '', individual, living, 'Mahasu Pahari').
	language('bga', '', individual, living, 'Gwamhi-Wuri').
	language('bgb', '', individual, living, 'Bobongko').
	language('bgc', '', individual, living, 'Haryanvi').
	language('bgd', '', individual, living, 'Rathwi Bareli').
	language('bge', '', individual, living, 'Bauria').
	language('bgf', '', individual, living, 'Bangandu').
	language('bgg', '', individual, living, 'Bugun').
	language('bgi', '', individual, living, 'Giangan').
	language('bgj', '', individual, living, 'Bangolan').
	language('bgk', '', individual, living, 'Bit').
	language('bgl', '', individual, living, 'Bo (Laos)').
	language('bgn', '', individual, living, 'Western Balochi').
	language('bgo', '', individual, living, 'Baga Koga').
	language('bgp', '', individual, living, 'Eastern Balochi').
	language('bgq', '', individual, living, 'Bagri').
	language('bgr', '', individual, living, 'Bawm Chin').
	language('bgs', '', individual, living, 'Tagabawa').
	language('bgt', '', individual, living, 'Bughotu').
	language('bgu', '', individual, living, 'Mbongno').
	language('bgv', '', individual, living, 'Warkay-Bipim').
	language('bgw', '', individual, living, 'Bhatri').
	language('bgx', '', individual, living, 'Balkan Gagauz Turkish').
	language('bgy', '', individual, living, 'Benggoi').
	language('bgz', '', individual, living, 'Banggai').
	language('bha', '', individual, living, 'Bharia').
	language('bhb', '', individual, living, 'Bhili').
	language('bhc', '', individual, living, 'Biga').
	language('bhd', '', individual, living, 'Bhadrawahi').
	language('bhe', '', individual, living, 'Bhaya').
	language('bhf', '', individual, living, 'Odiai').
	language('bhg', '', individual, living, 'Binandere').
	language('bhh', '', individual, living, 'Bukharic').
	language('bhi', '', individual, living, 'Bhilali').
	language('bhj', '', individual, living, 'Bahing').
	language('bhl', '', individual, living, 'Bimin').
	language('bhm', '', individual, living, 'Bathari').
	language('bhn', '', individual, living, 'Bohtan Neo-Aramaic').
	language('bho', '', individual, living, 'Bhojpuri').
	language('bhp', '', individual, living, 'Bima').
	language('bhq', '', individual, living, 'Tukang Besi South').
	language('bhr', '', individual, living, 'Bara Malagasy').
	language('bhs', '', individual, living, 'Buwal').
	language('bht', '', individual, living, 'Bhattiyali').
	language('bhu', '', individual, living, 'Bhunjia').
	language('bhv', '', individual, living, 'Bahau').
	language('bhw', '', individual, living, 'Biak').
	language('bhx', '', individual, living, 'Bhalay').
	language('bhy', '', individual, living, 'Bhele').
	language('bhz', '', individual, living, 'Bada (Indonesia)').
	language('bia', '', individual, living, 'Badimaya').
	language('bib', '', individual, living, 'Bissa').
	language('bid', '', individual, living, 'Bidiyo').
	language('bie', '', individual, living, 'Bepour').
	language('bif', '', individual, living, 'Biafada').
	language('big', '', individual, living, 'Biangai').
	language('bik', '', macrolanguage, living, 'Bikol').
	language('bil', '', individual, living, 'Bile').
	language('bim', '', individual, living, 'Bimoba').
	language('bin', '', individual, living, 'Bini').
	language('bio', '', individual, living, 'Nai').
	language('bip', '', individual, living, 'Bila').
	language('biq', '', individual, living, 'Bipi').
	language('bir', '', individual, living, 'Bisorio').
	language('bis', 'bi', individual, living, 'Bislama').
	language('bit', '', individual, living, 'Berinomo').
	language('biu', '', individual, living, 'Biete').
	language('biv', '', individual, living, 'Southern Birifor').
	language('biw', '', individual, living, 'Kol (Cameroon)').
	language('bix', '', individual, living, 'Bijori').
	language('biy', '', individual, living, 'Birhor').
	language('biz', '', individual, living, 'Baloi').
	language('bja', '', individual, living, 'Budza').
	language('bjb', '', individual, extinct, 'Banggarla').
	language('bjc', '', individual, living, 'Bariji').
	language('bje', '', individual, living, 'Biao-Jiao Mien').
	language('bjf', '', individual, living, 'Barzani Jewish Neo-Aramaic').
	language('bjg', '', individual, living, 'Bidyogo').
	language('bjh', '', individual, living, 'Bahinemo').
	language('bji', '', individual, living, 'Burji').
	language('bjj', '', individual, living, 'Kanauji').
	language('bjk', '', individual, living, 'Barok').
	language('bjl', '', individual, living, 'Bulu (Papua New Guinea)').
	language('bjm', '', individual, living, 'Bajelani').
	language('bjn', '', individual, living, 'Banjar').
	language('bjo', '', individual, living, 'Mid-Southern Banda').
	language('bjp', '', individual, living, 'Fanamaket').
	language('bjr', '', individual, living, 'Binumarien').
	language('bjs', '', individual, living, 'Bajan').
	language('bjt', '', individual, living, 'Balanta-Ganja').
	language('bju', '', individual, living, 'Busuu').
	language('bjv', '', individual, living, 'Bedjond').
	language('bjw', '', individual, living, 'Bakwé').
	language('bjx', '', individual, living, 'Banao Itneg').
	language('bjy', '', individual, extinct, 'Bayali').
	language('bjz', '', individual, living, 'Baruga').
	language('bka', '', individual, living, 'Kyak').
	language('bkc', '', individual, living, 'Baka (Cameroon)').
	language('bkd', '', individual, living, 'Binukid').
	language('bkf', '', individual, living, 'Beeke').
	language('bkg', '', individual, living, 'Buraka').
	language('bkh', '', individual, living, 'Bakoko').
	language('bki', '', individual, living, 'Baki').
	language('bkj', '', individual, living, 'Pande').
	language('bkk', '', individual, living, 'Brokskat').
	language('bkl', '', individual, living, 'Berik').
	language('bkm', '', individual, living, 'Kom (Cameroon)').
	language('bkn', '', individual, living, 'Bukitan').
	language('bko', '', individual, living, 'Kwa''').
	language('bkp', '', individual, living, 'Boko (Democratic Republic of Congo)').
	language('bkq', '', individual, living, 'Bakairí').
	language('bkr', '', individual, living, 'Bakumpai').
	language('bks', '', individual, living, 'Northern Sorsoganon').
	language('bkt', '', individual, living, 'Boloki').
	language('bku', '', individual, living, 'Buhid').
	language('bkv', '', individual, living, 'Bekwarra').
	language('bkw', '', individual, living, 'Bekwel').
	language('bkx', '', individual, living, 'Baikeno').
	language('bky', '', individual, living, 'Bokyi').
	language('bkz', '', individual, living, 'Bungku').
	language('bla', '', individual, living, 'Siksika').
	language('blb', '', individual, living, 'Bilua').
	language('blc', '', individual, living, 'Bella Coola').
	language('bld', '', individual, living, 'Bolango').
	language('ble', '', individual, living, 'Balanta-Kentohe').
	language('blf', '', individual, living, 'Buol').
	language('blh', '', individual, living, 'Kuwaa').
	language('bli', '', individual, living, 'Bolia').
	language('blj', '', individual, living, 'Bolongan').
	language('blk', '', individual, living, 'Pa''o Karen').
	language('bll', '', individual, extinct, 'Biloxi').
	language('blm', '', individual, living, 'Beli (South Sudan)').
	language('bln', '', individual, living, 'Southern Catanduanes Bikol').
	language('blo', '', individual, living, 'Anii').
	language('blp', '', individual, living, 'Blablanga').
	language('blq', '', individual, living, 'Baluan-Pam').
	language('blr', '', individual, living, 'Blang').
	language('bls', '', individual, living, 'Balaesang').
	language('blt', '', individual, living, 'Tai Dam').
	language('blv', '', individual, living, 'Kibala').
	language('blw', '', individual, living, 'Balangao').
	language('blx', '', individual, living, 'Mag-Indi Ayta').
	language('bly', '', individual, living, 'Notre').
	language('blz', '', individual, living, 'Balantak').
	language('bma', '', individual, living, 'Lame').
	language('bmb', '', individual, living, 'Bembe').
	language('bmc', '', individual, living, 'Biem').
	language('bmd', '', individual, living, 'Baga Manduri').
	language('bme', '', individual, living, 'Limassa').
	language('bmf', '', individual, living, 'Bom-Kim').
	language('bmg', '', individual, living, 'Bamwe').
	language('bmh', '', individual, living, 'Kein').
	language('bmi', '', individual, living, 'Bagirmi').
	language('bmj', '', individual, living, 'Bote-Majhi').
	language('bmk', '', individual, living, 'Ghayavi').
	language('bml', '', individual, living, 'Bomboli').
	language('bmm', '', individual, living, 'Northern Betsimisaraka Malagasy').
	language('bmn', '', individual, extinct, 'Bina (Papua New Guinea)').
	language('bmo', '', individual, living, 'Bambalang').
	language('bmp', '', individual, living, 'Bulgebi').
	language('bmq', '', individual, living, 'Bomu').
	language('bmr', '', individual, living, 'Muinane').
	language('bms', '', individual, living, 'Bilma Kanuri').
	language('bmt', '', individual, living, 'Biao Mon').
	language('bmu', '', individual, living, 'Somba-Siawari').
	language('bmv', '', individual, living, 'Bum').
	language('bmw', '', individual, living, 'Bomwali').
	language('bmx', '', individual, living, 'Baimak').
	language('bmz', '', individual, living, 'Baramu').
	language('bna', '', individual, living, 'Bonerate').
	language('bnb', '', individual, living, 'Bookan').
	language('bnc', '', macrolanguage, living, 'Bontok').
	language('bnd', '', individual, living, 'Banda (Indonesia)').
	language('bne', '', individual, living, 'Bintauna').
	language('bnf', '', individual, living, 'Masiwang').
	language('bng', '', individual, living, 'Benga').
	language('bni', '', individual, living, 'Bangi').
	language('bnj', '', individual, living, 'Eastern Tawbuid').
	language('bnk', '', individual, living, 'Bierebo').
	language('bnl', '', individual, living, 'Boon').
	language('bnm', '', individual, living, 'Batanga').
	language('bnn', '', individual, living, 'Bunun').
	language('bno', '', individual, living, 'Bantoanon').
	language('bnp', '', individual, living, 'Bola').
	language('bnq', '', individual, living, 'Bantik').
	language('bnr', '', individual, living, 'Butmas-Tur').
	language('bns', '', individual, living, 'Bundeli').
	language('bnu', '', individual, living, 'Bentong').
	language('bnv', '', individual, living, 'Bonerif').
	language('bnw', '', individual, living, 'Bisis').
	language('bnx', '', individual, living, 'Bangubangu').
	language('bny', '', individual, living, 'Bintulu').
	language('bnz', '', individual, living, 'Beezen').
	language('boa', '', individual, living, 'Bora').
	language('bob', '', individual, living, 'Aweer').
	language('bod', 'bo', individual, living, 'Tibetan').
	language('boe', '', individual, living, 'Mundabli-Mufu').
	language('bof', '', individual, living, 'Bolon').
	language('bog', '', individual, living, 'Bamako Sign Language').
	language('boh', '', individual, living, 'Boma').
	language('boi', '', individual, extinct, 'Barbareño').
	language('boj', '', individual, living, 'Anjam').
	language('bok', '', individual, living, 'Bonjo').
	language('bol', '', individual, living, 'Bole').
	language('bom', '', individual, living, 'Berom').
	language('bon', '', individual, living, 'Bine').
	language('boo', '', individual, living, 'Tiemacèwè Bozo').
	language('bop', '', individual, living, 'Bonkiman').
	language('boq', '', individual, living, 'Bogaya').
	language('bor', '', individual, living, 'Borôro').
	language('bos', 'bs', individual, living, 'Bosnian').
	language('bot', '', individual, living, 'Bongo').
	language('bou', '', individual, living, 'Bondei').
	language('bov', '', individual, living, 'Tuwuli').
	language('bow', '', individual, extinct, 'Rema').
	language('box', '', individual, living, 'Buamu').
	language('boy', '', individual, living, 'Bodo (Central African Republic)').
	language('boz', '', individual, living, 'Tiéyaxo Bozo').
	language('bpa', '', individual, living, 'Daakaka').
	language('bpc', '', individual, living, 'Mbuk').
	language('bpd', '', individual, living, 'Banda-Banda').
	language('bpe', '', individual, living, 'Bauni').
	language('bpg', '', individual, living, 'Bonggo').
	language('bph', '', individual, living, 'Botlikh').
	language('bpi', '', individual, living, 'Bagupi').
	language('bpj', '', individual, living, 'Binji').
	language('bpk', '', individual, living, 'Orowe').
	language('bpl', '', individual, living, 'Broome Pearling Lugger Pidgin').
	language('bpm', '', individual, living, 'Biyom').
	language('bpn', '', individual, living, 'Dzao Min').
	language('bpo', '', individual, living, 'Anasi').
	language('bpp', '', individual, living, 'Kaure').
	language('bpq', '', individual, living, 'Banda Malay').
	language('bpr', '', individual, living, 'Koronadal Blaan').
	language('bps', '', individual, living, 'Sarangani Blaan').
	language('bpt', '', individual, extinct, 'Barrow Point').
	language('bpu', '', individual, living, 'Bongu').
	language('bpv', '', individual, living, 'Bian Marind').
	language('bpw', '', individual, living, 'Bo (Papua New Guinea)').
	language('bpx', '', individual, living, 'Palya Bareli').
	language('bpy', '', individual, living, 'Bishnupriya').
	language('bpz', '', individual, living, 'Bilba').
	language('bqa', '', individual, living, 'Tchumbuli').
	language('bqb', '', individual, living, 'Bagusa').
	language('bqc', '', individual, living, 'Boko (Benin)').
	language('bqd', '', individual, living, 'Bung').
	language('bqf', '', individual, extinct, 'Baga Kaloum').
	language('bqg', '', individual, living, 'Bago-Kusuntu').
	language('bqh', '', individual, living, 'Baima').
	language('bqi', '', individual, living, 'Bakhtiari').
	language('bqj', '', individual, living, 'Bandial').
	language('bqk', '', individual, living, 'Banda-Mbrès').
	language('bql', '', individual, living, 'Karian').
	language('bqm', '', individual, living, 'Wumboko').
	language('bqn', '', individual, living, 'Bulgarian Sign Language').
	language('bqo', '', individual, living, 'Balo').
	language('bqp', '', individual, living, 'Busa').
	language('bqq', '', individual, living, 'Biritai').
	language('bqr', '', individual, living, 'Burusu').
	language('bqs', '', individual, living, 'Bosngun').
	language('bqt', '', individual, living, 'Bamukumbit').
	language('bqu', '', individual, living, 'Boguru').
	language('bqv', '', individual, living, 'Koro Wachi').
	language('bqw', '', individual, living, 'Buru (Nigeria)').
	language('bqx', '', individual, living, 'Baangi').
	language('bqy', '', individual, living, 'Bengkala Sign Language').
	language('bqz', '', individual, living, 'Bakaka').
	language('bra', '', individual, living, 'Braj').
	language('brb', '', individual, living, 'Brao').
	language('brc', '', individual, extinct, 'Berbice Creole Dutch').
	language('brd', '', individual, living, 'Baraamu').
	language('bre', 'br', individual, living, 'Breton').
	language('brf', '', individual, living, 'Bira').
	language('brg', '', individual, living, 'Baure').
	language('brh', '', individual, living, 'Brahui').
	language('bri', '', individual, living, 'Mokpwe').
	language('brj', '', individual, living, 'Bieria').
	language('brk', '', individual, extinct, 'Birked').
	language('brl', '', individual, living, 'Birwa').
	language('brm', '', individual, living, 'Barambu').
	language('brn', '', individual, living, 'Boruca').
	language('bro', '', individual, living, 'Brokkat').
	language('brp', '', individual, living, 'Barapasi').
	language('brq', '', individual, living, 'Breri').
	language('brr', '', individual, living, 'Birao').
	language('brs', '', individual, living, 'Baras').
	language('brt', '', individual, living, 'Bitare').
	language('bru', '', individual, living, 'Eastern Bru').
	language('brv', '', individual, living, 'Western Bru').
	language('brw', '', individual, living, 'Bellari').
	language('brx', '', individual, living, 'Bodo (India)').
	language('bry', '', individual, living, 'Burui').
	language('brz', '', individual, living, 'Bilbil').
	language('bsa', '', individual, living, 'Abinomn').
	language('bsb', '', individual, living, 'Brunei Bisaya').
	language('bsc', '', individual, living, 'Bassari').
	language('bse', '', individual, living, 'Wushi').
	language('bsf', '', individual, living, 'Bauchi').
	language('bsg', '', individual, living, 'Bashkardi').
	language('bsh', '', individual, living, 'Kati').
	language('bsi', '', individual, living, 'Bassossi').
	language('bsj', '', individual, living, 'Bangwinji').
	language('bsk', '', individual, living, 'Burushaski').
	language('bsl', '', individual, extinct, 'Basa-Gumna').
	language('bsm', '', individual, living, 'Busami').
	language('bsn', '', individual, living, 'Barasana-Eduria').
	language('bso', '', individual, living, 'Buso').
	language('bsp', '', individual, living, 'Baga Sitemu').
	language('bsq', '', individual, living, 'Bassa').
	language('bsr', '', individual, living, 'Bassa-Kontagora').
	language('bss', '', individual, living, 'Akoose').
	language('bst', '', individual, living, 'Basketo').
	language('bsu', '', individual, living, 'Bahonsuai').
	language('bsv', '', individual, extinct, 'Baga Sobané').
	language('bsw', '', individual, living, 'Baiso').
	language('bsx', '', individual, living, 'Yangkam').
	language('bsy', '', individual, living, 'Sabah Bisaya').
	language('bta', '', individual, living, 'Bata').
	language('btc', '', individual, living, 'Bati (Cameroon)').
	language('btd', '', individual, living, 'Batak Dairi').
	language('bte', '', individual, extinct, 'Gamo-Ningi').
	language('btf', '', individual, living, 'Birgit').
	language('btg', '', individual, living, 'Gagnoa Bété').
	language('bth', '', individual, living, 'Biatah Bidayuh').
	language('bti', '', individual, living, 'Burate').
	language('btj', '', individual, living, 'Bacanese Malay').
	language('btm', '', individual, living, 'Batak Mandailing').
	language('btn', '', individual, living, 'Ratagnon').
	language('bto', '', individual, living, 'Rinconada Bikol').
	language('btp', '', individual, living, 'Budibud').
	language('btq', '', individual, living, 'Batek').
	language('btr', '', individual, living, 'Baetora').
	language('bts', '', individual, living, 'Batak Simalungun').
	language('btt', '', individual, living, 'Bete-Bendi').
	language('btu', '', individual, living, 'Batu').
	language('btv', '', individual, living, 'Bateri').
	language('btw', '', individual, living, 'Butuanon').
	language('btx', '', individual, living, 'Batak Karo').
	language('bty', '', individual, living, 'Bobot').
	language('btz', '', individual, living, 'Batak Alas-Kluet').
	language('bua', '', macrolanguage, living, 'Buriat').
	language('bub', '', individual, living, 'Bua').
	language('buc', '', individual, living, 'Bushi').
	language('bud', '', individual, living, 'Ntcham').
	language('bue', '', individual, extinct, 'Beothuk').
	language('buf', '', individual, living, 'Bushoong').
	language('bug', '', individual, living, 'Buginese').
	language('buh', '', individual, living, 'Younuo Bunu').
	language('bui', '', individual, living, 'Bongili').
	language('buj', '', individual, living, 'Basa-Gurmana').
	language('buk', '', individual, living, 'Bugawac').
	language('bul', 'bg', individual, living, 'Bulgarian').
	language('bum', '', individual, living, 'Bulu (Cameroon)').
	language('bun', '', individual, living, 'Sherbro').
	language('buo', '', individual, living, 'Terei').
	language('bup', '', individual, living, 'Busoa').
	language('buq', '', individual, living, 'Brem').
	language('bus', '', individual, living, 'Bokobaru').
	language('but', '', individual, living, 'Bungain').
	language('buu', '', individual, living, 'Budu').
	language('buv', '', individual, living, 'Bun').
	language('buw', '', individual, living, 'Bubi').
	language('bux', '', individual, living, 'Boghom').
	language('buy', '', individual, living, 'Bullom So').
	language('buz', '', individual, living, 'Bukwen').
	language('bva', '', individual, living, 'Barein').
	language('bvb', '', individual, living, 'Bube').
	language('bvc', '', individual, living, 'Baelelea').
	language('bvd', '', individual, living, 'Baeggu').
	language('bve', '', individual, living, 'Berau Malay').
	language('bvf', '', individual, living, 'Boor').
	language('bvg', '', individual, living, 'Bonkeng').
	language('bvh', '', individual, living, 'Bure').
	language('bvi', '', individual, living, 'Belanda Viri').
	language('bvj', '', individual, living, 'Baan').
	language('bvk', '', individual, living, 'Bukat').
	language('bvl', '', individual, living, 'Bolivian Sign Language').
	language('bvm', '', individual, living, 'Bamunka').
	language('bvn', '', individual, living, 'Buna').
	language('bvo', '', individual, living, 'Bolgo').
	language('bvp', '', individual, living, 'Bumang').
	language('bvq', '', individual, living, 'Birri').
	language('bvr', '', individual, living, 'Burarra').
	language('bvt', '', individual, living, 'Bati (Indonesia)').
	language('bvu', '', individual, living, 'Bukit Malay').
	language('bvv', '', individual, extinct, 'Baniva').
	language('bvw', '', individual, living, 'Boga').
	language('bvx', '', individual, living, 'Dibole').
	language('bvy', '', individual, living, 'Baybayanon').
	language('bvz', '', individual, living, 'Bauzi').
	language('bwa', '', individual, living, 'Bwatoo').
	language('bwb', '', individual, living, 'Namosi-Naitasiri-Serua').
	language('bwc', '', individual, living, 'Bwile').
	language('bwd', '', individual, living, 'Bwaidoka').
	language('bwe', '', individual, living, 'Bwe Karen').
	language('bwf', '', individual, living, 'Boselewa').
	language('bwg', '', individual, living, 'Barwe').
	language('bwh', '', individual, living, 'Bishuo').
	language('bwi', '', individual, living, 'Baniwa').
	language('bwj', '', individual, living, 'Láá Láá Bwamu').
	language('bwk', '', individual, living, 'Bauwaki').
	language('bwl', '', individual, living, 'Bwela').
	language('bwm', '', individual, living, 'Biwat').
	language('bwn', '', individual, living, 'Wunai Bunu').
	language('bwo', '', individual, living, 'Boro (Ethiopia)').
	language('bwp', '', individual, living, 'Mandobo Bawah').
	language('bwq', '', individual, living, 'Southern Bobo Madaré').
	language('bwr', '', individual, living, 'Bura-Pabir').
	language('bws', '', individual, living, 'Bomboma').
	language('bwt', '', individual, living, 'Bafaw-Balong').
	language('bwu', '', individual, living, 'Buli (Ghana)').
	language('bww', '', individual, living, 'Bwa').
	language('bwx', '', individual, living, 'Bu-Nao Bunu').
	language('bwy', '', individual, living, 'Cwi Bwamu').
	language('bwz', '', individual, living, 'Bwisi').
	language('bxa', '', individual, living, 'Tairaha').
	language('bxb', '', individual, living, 'Belanda Bor').
	language('bxc', '', individual, living, 'Molengue').
	language('bxd', '', individual, living, 'Pela').
	language('bxe', '', individual, living, 'Birale').
	language('bxf', '', individual, living, 'Bilur').
	language('bxg', '', individual, living, 'Bangala').
	language('bxh', '', individual, living, 'Buhutu').
	language('bxi', '', individual, extinct, 'Pirlatapa').
	language('bxj', '', individual, living, 'Bayungu').
	language('bxk', '', individual, living, 'Bukusu').
	language('bxl', '', individual, living, 'Jalkunan').
	language('bxm', '', individual, living, 'Mongolia Buriat').
	language('bxn', '', individual, living, 'Burduna').
	language('bxo', '', individual, living, 'Barikanchi').
	language('bxp', '', individual, living, 'Bebil').
	language('bxq', '', individual, living, 'Beele').
	language('bxr', '', individual, living, 'Russia Buriat').
	language('bxs', '', individual, living, 'Busam').
	language('bxu', '', individual, living, 'China Buriat').
	language('bxv', '', individual, living, 'Berakou').
	language('bxw', '', individual, living, 'Bankagooma').
	language('bxz', '', individual, living, 'Binahari').
	language('bya', '', individual, living, 'Batak').
	language('byb', '', individual, living, 'Bikya').
	language('byc', '', individual, living, 'Ubaghara').
	language('byd', '', individual, living, 'Benyadu''').
	language('bye', '', individual, living, 'Pouye').
	language('byf', '', individual, living, 'Bete').
	language('byg', '', individual, extinct, 'Baygo').
	language('byh', '', individual, living, 'Bhujel').
	language('byi', '', individual, living, 'Buyu').
	language('byj', '', individual, living, 'Bina (Nigeria)').
	language('byk', '', individual, living, 'Biao').
	language('byl', '', individual, living, 'Bayono').
	language('bym', '', individual, living, 'Bidjara').
	language('byn', '', individual, living, 'Bilin').
	language('byo', '', individual, living, 'Biyo').
	language('byp', '', individual, living, 'Bumaji').
	language('byq', '', individual, extinct, 'Basay').
	language('byr', '', individual, living, 'Baruya').
	language('bys', '', individual, living, 'Burak').
	language('byt', '', individual, extinct, 'Berti').
	language('byv', '', individual, living, 'Medumba').
	language('byw', '', individual, living, 'Belhariya').
	language('byx', '', individual, living, 'Qaqet').
	language('byz', '', individual, living, 'Banaro').
	language('bza', '', individual, living, 'Bandi').
	language('bzb', '', individual, living, 'Andio').
	language('bzc', '', individual, living, 'Southern Betsimisaraka Malagasy').
	language('bzd', '', individual, living, 'Bribri').
	language('bze', '', individual, living, 'Jenaama Bozo').
	language('bzf', '', individual, living, 'Boikin').
	language('bzg', '', individual, living, 'Babuza').
	language('bzh', '', individual, living, 'Mapos Buang').
	language('bzi', '', individual, living, 'Bisu').
	language('bzj', '', individual, living, 'Belize Kriol English').
	language('bzk', '', individual, living, 'Nicaragua Creole English').
	language('bzl', '', individual, living, 'Boano (Sulawesi)').
	language('bzm', '', individual, living, 'Bolondo').
	language('bzn', '', individual, living, 'Boano (Maluku)').
	language('bzo', '', individual, living, 'Bozaba').
	language('bzp', '', individual, living, 'Kemberano').
	language('bzq', '', individual, living, 'Buli (Indonesia)').
	language('bzr', '', individual, extinct, 'Biri').
	language('bzs', '', individual, living, 'Brazilian Sign Language').
	language('bzt', '', individual, constructed, 'Brithenig').
	language('bzu', '', individual, living, 'Burmeso').
	language('bzv', '', individual, living, 'Naami').
	language('bzw', '', individual, living, 'Basa (Nigeria)').
	language('bzx', '', individual, living, 'Kɛlɛngaxo Bozo').
	language('bzy', '', individual, living, 'Obanliku').
	language('bzz', '', individual, living, 'Evant').
	language('caa', '', individual, living, 'Chortí').
	language('cab', '', individual, living, 'Garifuna').
	language('cac', '', individual, living, 'Chuj').
	language('cad', '', individual, living, 'Caddo').
	language('cae', '', individual, living, 'Lehar').
	language('caf', '', individual, living, 'Southern Carrier').
	language('cag', '', individual, living, 'Nivaclé').
	language('cah', '', individual, living, 'Cahuarano').
	language('caj', '', individual, extinct, 'Chané').
	language('cak', '', individual, living, 'Kaqchikel').
	language('cal', '', individual, living, 'Carolinian').
	language('cam', '', individual, living, 'Cemuhî').
	language('can', '', individual, living, 'Chambri').
	language('cao', '', individual, living, 'Chácobo').
	language('cap', '', individual, living, 'Chipaya').
	language('caq', '', individual, living, 'Car Nicobarese').
	language('car', '', individual, living, 'Galibi Carib').
	language('cas', '', individual, living, 'Tsimané').
	language('cat', 'ca', individual, living, 'Catalan').
	language('cav', '', individual, living, 'Cavineña').
	language('caw', '', individual, living, 'Callawalla').
	language('cax', '', individual, living, 'Chiquitano').
	language('cay', '', individual, living, 'Cayuga').
	language('caz', '', individual, extinct, 'Canichana').
	language('cbb', '', individual, living, 'Cabiyarí').
	language('cbc', '', individual, living, 'Carapana').
	language('cbd', '', individual, living, 'Carijona').
	language('cbg', '', individual, living, 'Chimila').
	language('cbi', '', individual, living, 'Chachi').
	language('cbj', '', individual, living, 'Ede Cabe').
	language('cbk', '', individual, living, 'Chavacano').
	language('cbl', '', individual, living, 'Bualkhaw Chin').
	language('cbn', '', individual, living, 'Nyahkur').
	language('cbo', '', individual, living, 'Izora').
	language('cbq', '', individual, living, 'Tsucuba').
	language('cbr', '', individual, living, 'Cashibo-Cacataibo').
	language('cbs', '', individual, living, 'Cashinahua').
	language('cbt', '', individual, living, 'Chayahuita').
	language('cbu', '', individual, living, 'Candoshi-Shapra').
	language('cbv', '', individual, living, 'Cacua').
	language('cbw', '', individual, living, 'Kinabalian').
	language('cby', '', individual, living, 'Carabayo').
	language('ccc', '', individual, living, 'Chamicuro').
	language('ccd', '', individual, living, 'Cafundo Creole').
	language('cce', '', individual, living, 'Chopi').
	language('ccg', '', individual, living, 'Samba Daka').
	language('cch', '', individual, living, 'Atsam').
	language('ccj', '', individual, living, 'Kasanga').
	language('ccl', '', individual, living, 'Cutchi-Swahili').
	language('ccm', '', individual, living, 'Malaccan Creole Malay').
	language('cco', '', individual, living, 'Comaltepec Chinantec').
	language('ccp', '', individual, living, 'Chakma').
	language('ccr', '', individual, extinct, 'Cacaopera').
	language('cda', '', individual, living, 'Choni').
	language('cde', '', individual, living, 'Chenchu').
	language('cdf', '', individual, living, 'Chiru').
	language('cdh', '', individual, living, 'Chambeali').
	language('cdi', '', individual, living, 'Chodri').
	language('cdj', '', individual, living, 'Churahi').
	language('cdm', '', individual, living, 'Chepang').
	language('cdn', '', individual, living, 'Chaudangsi').
	language('cdo', '', individual, living, 'Min Dong Chinese').
	language('cdr', '', individual, living, 'Cinda-Regi-Tiyal').
	language('cds', '', individual, living, 'Chadian Sign Language').
	language('cdy', '', individual, living, 'Chadong').
	language('cdz', '', individual, living, 'Koda').
	language('cea', '', individual, extinct, 'Lower Chehalis').
	language('ceb', '', individual, living, 'Cebuano').
	language('ceg', '', individual, living, 'Chamacoco').
	language('cek', '', individual, living, 'Eastern Khumi Chin').
	language('cen', '', individual, living, 'Cen').
	language('ces', 'cs', individual, living, 'Czech').
	language('cet', '', individual, living, 'Centúúm').
	language('cey', '', individual, living, 'Ekai Chin').
	language('cfa', '', individual, living, 'Dijim-Bwilim').
	language('cfd', '', individual, living, 'Cara').
	language('cfg', '', individual, living, 'Como Karim').
	language('cfm', '', individual, living, 'Falam Chin').
	language('cga', '', individual, living, 'Changriwa').
	language('cgc', '', individual, living, 'Kagayanen').
	language('cgg', '', individual, living, 'Chiga').
	language('cgk', '', individual, living, 'Chocangacakha').
	language('cha', 'ch', individual, living, 'Chamorro').
	language('chb', '', individual, extinct, 'Chibcha').
	language('chc', '', individual, extinct, 'Catawba').
	language('chd', '', individual, living, 'Highland Oaxaca Chontal').
	language('che', 'ce', individual, living, 'Chechen').
	language('chf', '', individual, living, 'Tabasco Chontal').
	language('chg', '', individual, extinct, 'Chagatai').
	language('chh', '', individual, extinct, 'Chinook').
	language('chj', '', individual, living, 'Ojitlán Chinantec').
	language('chk', '', individual, living, 'Chuukese').
	language('chl', '', individual, living, 'Cahuilla').
	language('chm', '', macrolanguage, living, 'Mari (Russia)').
	language('chn', '', individual, living, 'Chinook jargon').
	language('cho', '', individual, living, 'Choctaw').
	language('chp', '', individual, living, 'Chipewyan').
	language('chq', '', individual, living, 'Quiotepec Chinantec').
	language('chr', '', individual, living, 'Cherokee').
	language('cht', '', individual, extinct, 'Cholón').
	language('chu', 'cu', individual, special, 'Church Slavic').
	language('chv', 'cv', individual, living, 'Chuvash').
	language('chw', '', individual, living, 'Chuwabu').
	language('chx', '', individual, living, 'Chantyal').
	language('chy', '', individual, living, 'Cheyenne').
	language('chz', '', individual, living, 'Ozumacín Chinantec').
	language('cia', '', individual, living, 'Cia-Cia').
	language('cib', '', individual, living, 'Ci Gbe').
	language('cic', '', individual, living, 'Chickasaw').
	language('cid', '', individual, extinct, 'Chimariko').
	language('cie', '', individual, living, 'Cineni').
	language('cih', '', individual, living, 'Chinali').
	language('cik', '', individual, living, 'Chitkuli Kinnauri').
	language('cim', '', individual, living, 'Cimbrian').
	language('cin', '', individual, living, 'Cinta Larga').
	language('cip', '', individual, living, 'Chiapanec').
	language('cir', '', individual, living, 'Tiri').
	language('ciw', '', individual, living, 'Chippewa').
	language('ciy', '', individual, living, 'Chaima').
	language('cja', '', individual, living, 'Western Cham').
	language('cje', '', individual, living, 'Chru').
	language('cjh', '', individual, extinct, 'Upper Chehalis').
	language('cji', '', individual, living, 'Chamalal').
	language('cjk', '', individual, living, 'Chokwe').
	language('cjm', '', individual, living, 'Eastern Cham').
	language('cjn', '', individual, living, 'Chenapian').
	language('cjo', '', individual, living, 'Ashéninka Pajonal').
	language('cjp', '', individual, living, 'Cabécar').
	language('cjs', '', individual, living, 'Shor').
	language('cjv', '', individual, living, 'Chuave').
	language('cjy', '', individual, living, 'Jinyu Chinese').
	language('ckb', '', individual, living, 'Central Kurdish').
	language('ckh', '', individual, living, 'Chak').
	language('ckl', '', individual, living, 'Cibak').
	language('ckm', '', individual, living, 'Chakavian').
	language('ckn', '', individual, living, 'Kaang Chin').
	language('cko', '', individual, living, 'Anufo').
	language('ckq', '', individual, living, 'Kajakse').
	language('ckr', '', individual, living, 'Kairak').
	language('cks', '', individual, living, 'Tayo').
	language('ckt', '', individual, living, 'Chukot').
	language('cku', '', individual, living, 'Koasati').
	language('ckv', '', individual, living, 'Kavalan').
	language('ckx', '', individual, living, 'Caka').
	language('cky', '', individual, living, 'Cakfem-Mushere').
	language('ckz', '', individual, living, 'Cakchiquel-Quiché Mixed Language').
	language('cla', '', individual, living, 'Ron').
	language('clc', '', individual, living, 'Chilcotin').
	language('cld', '', individual, living, 'Chaldean Neo-Aramaic').
	language('cle', '', individual, living, 'Lealao Chinantec').
	language('clh', '', individual, living, 'Chilisso').
	language('cli', '', individual, living, 'Chakali').
	language('clj', '', individual, living, 'Laitu Chin').
	language('clk', '', individual, living, 'Idu-Mishmi').
	language('cll', '', individual, living, 'Chala').
	language('clm', '', individual, living, 'Klallam').
	language('clo', '', individual, living, 'Lowland Oaxaca Chontal').
	language('cls', '', individual, special, 'Classical Sanskrit').
	language('clt', '', individual, living, 'Lautu Chin').
	language('clu', '', individual, living, 'Caluyanun').
	language('clw', '', individual, living, 'Chulym').
	language('cly', '', individual, living, 'Eastern Highland Chatino').
	language('cma', '', individual, living, 'Maa').
	language('cme', '', individual, living, 'Cerma').
	language('cmg', '', individual, special, 'Classical Mongolian').
	language('cmi', '', individual, living, 'Emberá-Chamí').
	language('cml', '', individual, living, 'Campalagian').
	language('cmm', '', individual, extinct, 'Michigamea').
	language('cmn', '', individual, living, 'Mandarin Chinese').
	language('cmo', '', individual, living, 'Central Mnong').
	language('cmr', '', individual, living, 'Mro-Khimi Chin').
	language('cms', '', individual, special, 'Messapic').
	language('cmt', '', individual, living, 'Camtho').
	language('cna', '', individual, living, 'Changthang').
	language('cnb', '', individual, living, 'Chinbon Chin').
	language('cnc', '', individual, living, 'Côông').
	language('cng', '', individual, living, 'Northern Qiang').
	language('cnh', '', individual, living, 'Hakha Chin').
	language('cni', '', individual, living, 'Asháninka').
	language('cnk', '', individual, living, 'Khumi Chin').
	language('cnl', '', individual, living, 'Lalana Chinantec').
	language('cno', '', individual, living, 'Con').
	language('cnp', '', individual, living, 'Northern Ping Chinese').
	language('cnq', '', individual, living, 'Chung').
	language('cnr', '', individual, living, 'Montenegrin').
	language('cns', '', individual, living, 'Central Asmat').
	language('cnt', '', individual, living, 'Tepetotutla Chinantec').
	language('cnu', '', individual, living, 'Chenoua').
	language('cnw', '', individual, living, 'Ngawn Chin').
	language('cnx', '', individual, special, 'Middle Cornish').
	language('coa', '', individual, living, 'Cocos Islands Malay').
	language('cob', '', individual, extinct, 'Chicomuceltec').
	language('coc', '', individual, living, 'Cocopa').
	language('cod', '', individual, living, 'Cocama-Cocamilla').
	language('coe', '', individual, living, 'Koreguaje').
	language('cof', '', individual, living, 'Colorado').
	language('cog', '', individual, living, 'Chong').
	language('coh', '', individual, living, 'Chonyi-Dzihana-Kauma').
	language('coj', '', individual, extinct, 'Cochimi').
	language('cok', '', individual, living, 'Santa Teresa Cora').
	language('col', '', individual, living, 'Columbia-Wenatchi').
	language('com', '', individual, living, 'Comanche').
	language('con', '', individual, living, 'Cofán').
	language('coo', '', individual, living, 'Comox').
	language('cop', '', individual, extinct, 'Coptic').
	language('coq', '', individual, extinct, 'Coquille').
	language('cor', 'kw', individual, living, 'Cornish').
	language('cos', 'co', individual, living, 'Corsican').
	language('cot', '', individual, living, 'Caquinte').
	language('cou', '', individual, living, 'Wamey').
	language('cov', '', individual, living, 'Cao Miao').
	language('cow', '', individual, extinct, 'Cowlitz').
	language('cox', '', individual, living, 'Nanti').
	language('coz', '', individual, living, 'Chochotec').
	language('cpa', '', individual, living, 'Palantla Chinantec').
	language('cpb', '', individual, living, 'Ucayali-Yurúa Ashéninka').
	language('cpc', '', individual, living, 'Ajyíninka Apurucayali').
	language('cpg', '', individual, extinct, 'Cappadocian Greek').
	language('cpi', '', individual, living, 'Chinese Pidgin English').
	language('cpn', '', individual, living, 'Cherepon').
	language('cpo', '', individual, living, 'Kpeego').
	language('cps', '', individual, living, 'Capiznon').
	language('cpu', '', individual, living, 'Pichis Ashéninka').
	language('cpx', '', individual, living, 'Pu-Xian Chinese').
	language('cpy', '', individual, living, 'South Ucayali Ashéninka').
	language('cqd', '', individual, living, 'Chuanqiandian Cluster Miao').
	language('cra', '', individual, living, 'Chara').
	language('crb', '', individual, extinct, 'Island Carib').
	language('crc', '', individual, living, 'Lonwolwol').
	language('crd', '', individual, living, 'Coeur d''Alene').
	language('cre', 'cr', macrolanguage, living, 'Cree').
	language('crf', '', individual, extinct, 'Caramanta').
	language('crg', '', individual, living, 'Michif').
	language('crh', '', individual, living, 'Crimean Tatar').
	language('cri', '', individual, living, 'Sãotomense').
	language('crj', '', individual, living, 'Southern East Cree').
	language('crk', '', individual, living, 'Plains Cree').
	language('crl', '', individual, living, 'Northern East Cree').
	language('crm', '', individual, living, 'Moose Cree').
	language('crn', '', individual, living, 'El Nayar Cora').
	language('cro', '', individual, living, 'Crow').
	language('crq', '', individual, living, 'Iyo''wujwa Chorote').
	language('crr', '', individual, extinct, 'Carolina Algonquian').
	language('crs', '', individual, living, 'Seselwa Creole French').
	language('crt', '', individual, living, 'Iyojwa''ja Chorote').
	language('crv', '', individual, living, 'Chaura').
	language('crw', '', individual, living, 'Chrau').
	language('crx', '', individual, living, 'Carrier').
	language('cry', '', individual, living, 'Cori').
	language('crz', '', individual, extinct, 'Cruzeño').
	language('csa', '', individual, living, 'Chiltepec Chinantec').
	language('csb', '', individual, living, 'Kashubian').
	language('csc', '', individual, living, 'Catalan Sign Language').
	language('csd', '', individual, living, 'Chiangmai Sign Language').
	language('cse', '', individual, living, 'Czech Sign Language').
	language('csf', '', individual, living, 'Cuba Sign Language').
	language('csg', '', individual, living, 'Chilean Sign Language').
	language('csh', '', individual, living, 'Asho Chin').
	language('csi', '', individual, extinct, 'Coast Miwok').
	language('csj', '', individual, living, 'Songlai Chin').
	language('csk', '', individual, living, 'Jola-Kasa').
	language('csl', '', individual, living, 'Chinese Sign Language').
	language('csm', '', individual, living, 'Central Sierra Miwok').
	language('csn', '', individual, living, 'Colombian Sign Language').
	language('cso', '', individual, living, 'Sochiapam Chinantec').
	language('csp', '', individual, living, 'Southern Ping Chinese').
	language('csq', '', individual, living, 'Croatia Sign Language').
	language('csr', '', individual, living, 'Costa Rican Sign Language').
	language('css', '', individual, extinct, 'Southern Ohlone').
	language('cst', '', individual, living, 'Northern Ohlone').
	language('csv', '', individual, living, 'Sumtu Chin').
	language('csw', '', individual, living, 'Swampy Cree').
	language('csx', '', individual, living, 'Cambodian Sign Language').
	language('csy', '', individual, living, 'Siyin Chin').
	language('csz', '', individual, living, 'Coos').
	language('cta', '', individual, living, 'Tataltepec Chatino').
	language('ctc', '', individual, extinct, 'Chetco').
	language('ctd', '', individual, living, 'Tedim Chin').
	language('cte', '', individual, living, 'Tepinapa Chinantec').
	language('ctg', '', individual, living, 'Chittagonian').
	language('cth', '', individual, living, 'Thaiphum Chin').
	language('ctl', '', individual, living, 'Tlacoatzintepec Chinantec').
	language('ctm', '', individual, extinct, 'Chitimacha').
	language('ctn', '', individual, living, 'Chhintange').
	language('cto', '', individual, living, 'Emberá-Catío').
	language('ctp', '', individual, living, 'Western Highland Chatino').
	language('cts', '', individual, living, 'Northern Catanduanes Bikol').
	language('ctt', '', individual, living, 'Wayanad Chetti').
	language('ctu', '', individual, living, 'Chol').
	language('cty', '', individual, living, 'Moundadan Chetty').
	language('ctz', '', individual, living, 'Zacatepec Chatino').
	language('cua', '', individual, living, 'Cua').
	language('cub', '', individual, living, 'Cubeo').
	language('cuc', '', individual, living, 'Usila Chinantec').
	language('cuh', '', individual, living, 'Chuka').
	language('cui', '', individual, living, 'Cuiba').
	language('cuj', '', individual, living, 'Mashco Piro').
	language('cuk', '', individual, living, 'San Blas Kuna').
	language('cul', '', individual, living, 'Culina').
	language('cuo', '', individual, extinct, 'Cumanagoto').
	language('cup', '', individual, extinct, 'Cupeño').
	language('cuq', '', individual, living, 'Cun').
	language('cur', '', individual, living, 'Chhulung').
	language('cut', '', individual, living, 'Teutila Cuicatec').
	language('cuu', '', individual, living, 'Tai Ya').
	language('cuv', '', individual, living, 'Cuvok').
	language('cuw', '', individual, living, 'Chukwa').
	language('cux', '', individual, living, 'Tepeuxila Cuicatec').
	language('cuy', '', individual, living, 'Cuitlatec').
	language('cvg', '', individual, living, 'Chug').
	language('cvn', '', individual, living, 'Valle Nacional Chinantec').
	language('cwa', '', individual, living, 'Kabwa').
	language('cwb', '', individual, living, 'Maindo').
	language('cwd', '', individual, living, 'Woods Cree').
	language('cwe', '', individual, living, 'Kwere').
	language('cwg', '', individual, living, 'Chewong').
	language('cwt', '', individual, living, 'Kuwaataay').
	language('cxh', '', individual, living, 'Cha''ari').
	language('cya', '', individual, living, 'Nopala Chatino').
	language('cyb', '', individual, extinct, 'Cayubaba').
	language('cym', 'cy', individual, living, 'Welsh').
	language('cyo', '', individual, living, 'Cuyonon').
	language('czh', '', individual, living, 'Huizhou Chinese').
	language('czk', '', individual, extinct, 'Knaanic').
	language('czn', '', individual, living, 'Zenzontepec Chatino').
	language('czo', '', individual, living, 'Min Zhong Chinese').
	language('czt', '', individual, living, 'Zotung Chin').
	language('daa', '', individual, living, 'Dangaléat').
	language('dac', '', individual, living, 'Dambi').
	language('dad', '', individual, living, 'Marik').
	language('dae', '', individual, living, 'Duupa').
	language('dag', '', individual, living, 'Dagbani').
	language('dah', '', individual, living, 'Gwahatike').
	language('dai', '', individual, living, 'Day').
	language('daj', '', individual, living, 'Dar Fur Daju').
	language('dak', '', individual, living, 'Dakota').
	language('dal', '', individual, living, 'Dahalo').
	language('dam', '', individual, living, 'Damakawa').
	language('dan', 'da', individual, living, 'Danish').
	language('dao', '', individual, living, 'Daai Chin').
	language('daq', '', individual, living, 'Dandami Maria').
	language('dar', '', individual, living, 'Dargwa').
	language('das', '', individual, living, 'Daho-Doo').
	language('dau', '', individual, living, 'Dar Sila Daju').
	language('dav', '', individual, living, 'Taita').
	language('daw', '', individual, living, 'Davawenyo').
	language('dax', '', individual, living, 'Dayi').
	language('daz', '', individual, living, 'Moi-Wadea').
	language('dba', '', individual, living, 'Bangime').
	language('dbb', '', individual, living, 'Deno').
	language('dbd', '', individual, living, 'Dadiya').
	language('dbe', '', individual, living, 'Dabe').
	language('dbf', '', individual, living, 'Edopi').
	language('dbg', '', individual, living, 'Dogul Dom Dogon').
	language('dbi', '', individual, living, 'Doka').
	language('dbj', '', individual, living, 'Ida''an').
	language('dbl', '', individual, living, 'Dyirbal').
	language('dbm', '', individual, living, 'Duguri').
	language('dbn', '', individual, living, 'Duriankere').
	language('dbo', '', individual, living, 'Dulbu').
	language('dbp', '', individual, living, 'Duwai').
	language('dbq', '', individual, living, 'Daba').
	language('dbr', '', individual, living, 'Dabarre').
	language('dbt', '', individual, living, 'Ben Tey Dogon').
	language('dbu', '', individual, living, 'Bondum Dom Dogon').
	language('dbv', '', individual, living, 'Dungu').
	language('dbw', '', individual, living, 'Bankan Tey Dogon').
	language('dby', '', individual, living, 'Dibiyaso').
	language('dcc', '', individual, living, 'Deccan').
	language('dcr', '', individual, extinct, 'Negerhollands').
	language('dda', '', individual, extinct, 'Dadi Dadi').
	language('ddd', '', individual, living, 'Dongotono').
	language('dde', '', individual, living, 'Doondo').
	language('ddg', '', individual, living, 'Fataluku').
	language('ddi', '', individual, living, 'West Goodenough').
	language('ddj', '', individual, living, 'Jaru').
	language('ddn', '', individual, living, 'Dendi (Benin)').
	language('ddo', '', individual, living, 'Dido').
	language('ddr', '', individual, extinct, 'Dhudhuroa').
	language('dds', '', individual, living, 'Donno So Dogon').
	language('ddw', '', individual, living, 'Dawera-Daweloor').
	language('dec', '', individual, living, 'Dagik').
	language('ded', '', individual, living, 'Dedua').
	language('dee', '', individual, living, 'Dewoin').
	language('def', '', individual, living, 'Dezfuli').
	language('deg', '', individual, living, 'Degema').
	language('deh', '', individual, living, 'Dehwari').
	language('dei', '', individual, living, 'Demisa').
	language('del', '', macrolanguage, living, 'Delaware').
	language('dem', '', individual, living, 'Dem').
	language('den', '', macrolanguage, living, 'Slavey').
	language('dep', '', individual, extinct, 'Pidgin Delaware').
	language('deq', '', individual, living, 'Dendi (Central African Republic)').
	language('der', '', individual, living, 'Deori').
	language('des', '', individual, living, 'Desano').
	language('deu', 'de', individual, living, 'German').
	language('dev', '', individual, living, 'Domung').
	language('dez', '', individual, living, 'Dengese').
	language('dga', '', individual, living, 'Southern Dagaare').
	language('dgb', '', individual, living, 'Bunoge Dogon').
	language('dgc', '', individual, living, 'Casiguran Dumagat Agta').
	language('dgd', '', individual, living, 'Dagaari Dioula').
	language('dge', '', individual, living, 'Degenan').
	language('dgg', '', individual, living, 'Doga').
	language('dgh', '', individual, living, 'Dghwede').
	language('dgi', '', individual, living, 'Northern Dagara').
	language('dgk', '', individual, living, 'Dagba').
	language('dgl', '', individual, living, 'Andaandi').
	language('dgn', '', individual, extinct, 'Dagoman').
	language('dgo', '', individual, living, 'Dogri (individual language)').
	language('dgr', '', individual, living, 'Tlicho').
	language('dgs', '', individual, living, 'Dogoso').
	language('dgt', '', individual, extinct, 'Ndra''ngith').
	language('dgw', '', individual, extinct, 'Daungwurrung').
	language('dgx', '', individual, living, 'Doghoro').
	language('dgz', '', individual, living, 'Daga').
	language('dhd', '', individual, living, 'Dhundari').
	language('dhg', '', individual, living, 'Dhangu-Djangu').
	language('dhi', '', individual, living, 'Dhimal').
	language('dhl', '', individual, living, 'Dhalandji').
	language('dhm', '', individual, living, 'Zemba').
	language('dhn', '', individual, living, 'Dhanki').
	language('dho', '', individual, living, 'Dhodia').
	language('dhr', '', individual, living, 'Dhargari').
	language('dhs', '', individual, living, 'Dhaiso').
	language('dhu', '', individual, extinct, 'Dhurga').
	language('dhv', '', individual, living, 'Dehu').
	language('dhw', '', individual, living, 'Dhanwar (Nepal)').
	language('dhx', '', individual, living, 'Dhungaloo').
	language('dia', '', individual, living, 'Dia').
	language('dib', '', individual, living, 'South Central Dinka').
	language('dic', '', individual, living, 'Lakota Dida').
	language('did', '', individual, living, 'Didinga').
	language('dif', '', individual, extinct, 'Dieri').
	language('dig', '', individual, living, 'Digo').
	language('dih', '', individual, living, 'Kumiai').
	language('dii', '', individual, living, 'Dimbong').
	language('dij', '', individual, living, 'Dai').
	language('dik', '', individual, living, 'Southwestern Dinka').
	language('dil', '', individual, living, 'Dilling').
	language('dim', '', individual, living, 'Dime').
	language('din', '', macrolanguage, living, 'Dinka').
	language('dio', '', individual, living, 'Dibo').
	language('dip', '', individual, living, 'Northeastern Dinka').
	language('diq', '', individual, living, 'Dimli (individual language)').
	language('dir', '', individual, living, 'Dirim').
	language('dis', '', individual, living, 'Dimasa').
	language('diu', '', individual, living, 'Diriku').
	language('div', 'dv', individual, living, 'Dhivehi').
	language('diw', '', individual, living, 'Northwestern Dinka').
	language('dix', '', individual, living, 'Dixon Reef').
	language('diy', '', individual, living, 'Diuwe').
	language('diz', '', individual, living, 'Ding').
	language('dja', '', individual, extinct, 'Djadjawurrung').
	language('djb', '', individual, living, 'Djinba').
	language('djc', '', individual, living, 'Dar Daju Daju').
	language('djd', '', individual, living, 'Djamindjung').
	language('dje', '', individual, living, 'Zarma').
	language('djf', '', individual, extinct, 'Djangun').
	language('dji', '', individual, living, 'Djinang').
	language('djj', '', individual, living, 'Djeebbana').
	language('djk', '', individual, living, 'Eastern Maroon Creole').
	language('djm', '', individual, living, 'Jamsay Dogon').
	language('djn', '', individual, living, 'Jawoyn').
	language('djo', '', individual, living, 'Jangkang').
	language('djr', '', individual, living, 'Djambarrpuyngu').
	language('dju', '', individual, living, 'Kapriman').
	language('djw', '', individual, extinct, 'Djawi').
	language('dka', '', individual, living, 'Dakpakha').
	language('dkg', '', individual, living, 'Kadung').
	language('dkk', '', individual, living, 'Dakka').
	language('dkr', '', individual, living, 'Kuijau').
	language('dks', '', individual, living, 'Southeastern Dinka').
	language('dkx', '', individual, living, 'Mazagway').
	language('dlg', '', individual, living, 'Dolgan').
	language('dlk', '', individual, living, 'Dahalik').
	language('dlm', '', individual, extinct, 'Dalmatian').
	language('dln', '', individual, living, 'Darlong').
	language('dma', '', individual, living, 'Duma').
	language('dmb', '', individual, living, 'Mombo Dogon').
	language('dmc', '', individual, living, 'Gavak').
	language('dmd', '', individual, extinct, 'Madhi Madhi').
	language('dme', '', individual, living, 'Dugwor').
	language('dmf', '', individual, extinct, 'Medefaidrin').
	language('dmg', '', individual, living, 'Upper Kinabatangan').
	language('dmk', '', individual, living, 'Domaaki').
	language('dml', '', individual, living, 'Dameli').
	language('dmm', '', individual, living, 'Dama').
	language('dmo', '', individual, living, 'Kemedzung').
	language('dmr', '', individual, living, 'East Damar').
	language('dms', '', individual, living, 'Dampelas').
	language('dmu', '', individual, living, 'Dubu').
	language('dmv', '', individual, living, 'Dumpas').
	language('dmw', '', individual, living, 'Mudburra').
	language('dmx', '', individual, living, 'Dema').
	language('dmy', '', individual, living, 'Demta').
	language('dna', '', individual, living, 'Upper Grand Valley Dani').
	language('dnd', '', individual, living, 'Daonda').
	language('dne', '', individual, living, 'Ndendeule').
	language('dng', '', individual, living, 'Dungan').
	language('dni', '', individual, living, 'Lower Grand Valley Dani').
	language('dnj', '', individual, living, 'Dan').
	language('dnk', '', individual, living, 'Dengka').
	language('dnn', '', individual, living, 'Dzùùngoo').
	language('dno', '', individual, living, 'Ndrulo').
	language('dnr', '', individual, living, 'Danaru').
	language('dnt', '', individual, living, 'Mid Grand Valley Dani').
	language('dnu', '', individual, living, 'Danau').
	language('dnv', '', individual, living, 'Danu').
	language('dnw', '', individual, living, 'Western Dani').
	language('dny', '', individual, living, 'Dení').
	language('doa', '', individual, living, 'Dom').
	language('dob', '', individual, living, 'Dobu').
	language('doc', '', individual, living, 'Northern Dong').
	language('doe', '', individual, living, 'Doe').
	language('dof', '', individual, living, 'Domu').
	language('doh', '', individual, living, 'Dong').
	language('doi', '', macrolanguage, living, 'Dogri (macrolanguage)').
	language('dok', '', individual, living, 'Dondo').
	language('dol', '', individual, living, 'Doso').
	language('don', '', individual, living, 'Toura (Papua New Guinea)').
	language('doo', '', individual, living, 'Dongo').
	language('dop', '', individual, living, 'Lukpa').
	language('doq', '', individual, living, 'Dominican Sign Language').
	language('dor', '', individual, living, 'Dori''o').
	language('dos', '', individual, living, 'Dogosé').
	language('dot', '', individual, living, 'Dass').
	language('dov', '', individual, living, 'Dombe').
	language('dow', '', individual, living, 'Doyayo').
	language('dox', '', individual, living, 'Bussa').
	language('doy', '', individual, living, 'Dompo').
	language('doz', '', individual, living, 'Dorze').
	language('dpp', '', individual, living, 'Papar').
	language('drb', '', individual, living, 'Dair').
	language('drc', '', individual, living, 'Minderico').
	language('drd', '', individual, living, 'Darmiya').
	language('dre', '', individual, living, 'Dolpo').
	language('drg', '', individual, living, 'Rungus').
	language('dri', '', individual, living, 'C''Lela').
	language('drl', '', individual, living, 'Paakantyi').
	language('drn', '', individual, living, 'West Damar').
	language('dro', '', individual, living, 'Daro-Matu Melanau').
	language('drq', '', individual, extinct, 'Dura').
	language('drs', '', individual, living, 'Gedeo').
	language('drt', '', individual, living, 'Drents').
	language('dru', '', individual, living, 'Rukai').
	language('dry', '', individual, living, 'Darai').
	language('dsb', '', individual, living, 'Lower Sorbian').
	language('dse', '', individual, living, 'Dutch Sign Language').
	language('dsh', '', individual, living, 'Daasanach').
	language('dsi', '', individual, living, 'Disa').
	language('dsk', '', individual, living, 'Dokshi').
	language('dsl', '', individual, living, 'Danish Sign Language').
	language('dsn', '', individual, extinct, 'Dusner').
	language('dso', '', individual, living, 'Desiya').
	language('dsq', '', individual, living, 'Tadaksahak').
	language('dsz', '', individual, living, 'Mardin Sign Language').
	language('dta', '', individual, living, 'Daur').
	language('dtb', '', individual, living, 'Labuk-Kinabatangan Kadazan').
	language('dtd', '', individual, living, 'Ditidaht').
	language('dth', '', individual, extinct, 'Adithinngithigh').
	language('dti', '', individual, living, 'Ana Tinga Dogon').
	language('dtk', '', individual, living, 'Tene Kan Dogon').
	language('dtm', '', individual, living, 'Tomo Kan Dogon').
	language('dtn', '', individual, living, 'Daatsʼíin').
	language('dto', '', individual, living, 'Tommo So Dogon').
	language('dtp', '', individual, living, 'Kadazan Dusun').
	language('dtr', '', individual, living, 'Lotud').
	language('dts', '', individual, living, 'Toro So Dogon').
	language('dtt', '', individual, living, 'Toro Tegu Dogon').
	language('dtu', '', individual, living, 'Tebul Ure Dogon').
	language('dty', '', individual, living, 'Dotyali').
	language('dua', '', individual, living, 'Duala').
	language('dub', '', individual, living, 'Dubli').
	language('duc', '', individual, living, 'Duna').
	language('due', '', individual, living, 'Umiray Dumaget Agta').
	language('duf', '', individual, living, 'Dumbea').
	language('dug', '', individual, living, 'Duruma').
	language('duh', '', individual, living, 'Dungra Bhil').
	language('dui', '', individual, living, 'Dumun').
	language('duk', '', individual, living, 'Uyajitaya').
	language('dul', '', individual, living, 'Alabat Island Agta').
	language('dum', '', individual, special, 'Middle Dutch (ca. 1050-1350)').
	language('dun', '', individual, living, 'Dusun Deyah').
	language('duo', '', individual, living, 'Dupaninan Agta').
	language('dup', '', individual, living, 'Duano').
	language('duq', '', individual, living, 'Dusun Malang').
	language('dur', '', individual, living, 'Dii').
	language('dus', '', individual, living, 'Dumi').
	language('duu', '', individual, living, 'Drung').
	language('duv', '', individual, living, 'Duvle').
	language('duw', '', individual, living, 'Dusun Witu').
	language('dux', '', individual, living, 'Duungooma').
	language('duy', '', individual, extinct, 'Dicamay Agta').
	language('duz', '', individual, extinct, 'Duli-Gey').
	language('dva', '', individual, living, 'Duau').
	language('dwa', '', individual, living, 'Diri').
	language('dwk', '', individual, living, 'Dawik Kui').
	language('dwr', '', individual, living, 'Dawro').
	language('dws', '', individual, constructed, 'Dutton World Speedwords').
	language('dwu', '', individual, living, 'Dhuwal').
	language('dww', '', individual, living, 'Dawawa').
	language('dwy', '', individual, living, 'Dhuwaya').
	language('dwz', '', individual, living, 'Dewas Rai').
	language('dya', '', individual, living, 'Dyan').
	language('dyb', '', individual, extinct, 'Dyaberdyaber').
	language('dyd', '', individual, extinct, 'Dyugun').
	language('dyg', '', individual, extinct, 'Villa Viciosa Agta').
	language('dyi', '', individual, living, 'Djimini Senoufo').
	language('dyl', '', individual, living, 'Bhutanese Sign Language').
	language('dym', '', individual, living, 'Yanda Dom Dogon').
	language('dyn', '', individual, living, 'Dyangadi').
	language('dyo', '', individual, living, 'Jola-Fonyi').
	language('dyr', '', individual, living, 'Dyarim').
	language('dyu', '', individual, living, 'Dyula').
	language('dyy', '', individual, living, 'Djabugay').
	language('dza', '', individual, living, 'Tunzu').
	language('dzd', '', individual, living, 'Daza').
	language('dze', '', individual, extinct, 'Djiwarli').
	language('dzg', '', individual, living, 'Dazaga').
	language('dzl', '', individual, living, 'Dzalakha').
	language('dzn', '', individual, living, 'Dzando').
	language('dzo', 'dz', individual, living, 'Dzongkha').
	language('eaa', '', individual, extinct, 'Karenggapa').
	language('ebc', '', individual, living, 'Beginci').
	language('ebg', '', individual, living, 'Ebughu').
	language('ebk', '', individual, living, 'Eastern Bontok').
	language('ebo', '', individual, living, 'Teke-Ebo').
	language('ebr', '', individual, living, 'Ebrié').
	language('ebu', '', individual, living, 'Embu').
	language('ecr', '', individual, special, 'Eteocretan').
	language('ecs', '', individual, living, 'Ecuadorian Sign Language').
	language('ecy', '', individual, special, 'Eteocypriot').
	language('eee', '', individual, living, 'E').
	language('efa', '', individual, living, 'Efai').
	language('efe', '', individual, living, 'Efe').
	language('efi', '', individual, living, 'Efik').
	language('ega', '', individual, living, 'Ega').
	language('egl', '', individual, living, 'Emilian').
	language('egm', '', individual, living, 'Benamanga').
	language('ego', '', individual, living, 'Eggon').
	language('egy', '', individual, special, 'Egyptian (Ancient)').
	language('ehs', '', individual, living, 'Miyakubo Sign Language').
	language('ehu', '', individual, living, 'Ehueun').
	language('eip', '', individual, living, 'Eipomek').
	language('eit', '', individual, living, 'Eitiep').
	language('eiv', '', individual, living, 'Askopan').
	language('eja', '', individual, living, 'Ejamat').
	language('eka', '', individual, living, 'Ekajuk').
	language('eke', '', individual, living, 'Ekit').
	language('ekg', '', individual, living, 'Ekari').
	language('eki', '', individual, living, 'Eki').
	language('ekk', '', individual, living, 'Standard Estonian').
	language('ekl', '', individual, living, 'Kol (Bangladesh)').
	language('ekm', '', individual, living, 'Elip').
	language('eko', '', individual, living, 'Koti').
	language('ekp', '', individual, living, 'Ekpeye').
	language('ekr', '', individual, living, 'Yace').
	language('eky', '', individual, living, 'Eastern Kayah').
	language('ele', '', individual, living, 'Elepi').
	language('elh', '', individual, living, 'El Hugeirat').
	language('eli', '', individual, extinct, 'Nding').
	language('elk', '', individual, living, 'Elkei').
	language('ell', 'el', individual, living, 'Modern Greek (1453-)').
	language('elm', '', individual, living, 'Eleme').
	language('elo', '', individual, living, 'El Molo').
	language('elu', '', individual, living, 'Elu').
	language('elx', '', individual, special, 'Elamite').
	language('ema', '', individual, living, 'Emai-Iuleha-Ora').
	language('emb', '', individual, living, 'Embaloh').
	language('eme', '', individual, living, 'Emerillon').
	language('emg', '', individual, living, 'Eastern Meohang').
	language('emi', '', individual, living, 'Mussau-Emira').
	language('emk', '', individual, living, 'Eastern Maninkakan').
	language('emm', '', individual, extinct, 'Mamulique').
	language('emn', '', individual, living, 'Eman').
	language('emp', '', individual, living, 'Northern Emberá').
	language('emq', '', individual, living, 'Eastern Minyag').
	language('ems', '', individual, living, 'Pacific Gulf Yupik').
	language('emu', '', individual, living, 'Eastern Muria').
	language('emw', '', individual, living, 'Emplawas').
	language('emx', '', individual, living, 'Erromintxela').
	language('emy', '', individual, special, 'Epigraphic Mayan').
	language('emz', '', individual, living, 'Mbessa').
	language('ena', '', individual, living, 'Apali').
	language('enb', '', individual, living, 'Markweeta').
	language('enc', '', individual, living, 'En').
	language('end', '', individual, living, 'Ende').
	language('enf', '', individual, living, 'Forest Enets').
	language('eng', 'en', individual, living, 'English').
	language('enh', '', individual, living, 'Tundra Enets').
	language('enl', '', individual, living, 'Enlhet').
	language('enm', '', individual, special, 'Middle English (1100-1500)').
	language('enn', '', individual, living, 'Engenni').
	language('eno', '', individual, living, 'Enggano').
	language('enq', '', individual, living, 'Enga').
	language('enr', '', individual, living, 'Emumu').
	language('enu', '', individual, living, 'Enu').
	language('env', '', individual, living, 'Enwan (Edo State)').
	language('enw', '', individual, living, 'Enwan (Akwa Ibom State)').
	language('enx', '', individual, living, 'Enxet').
	language('eot', '', individual, living, 'Beti (Côte d''Ivoire)').
	language('epi', '', individual, living, 'Epie').
	language('epo', 'eo', individual, constructed, 'Esperanto').
	language('era', '', individual, living, 'Eravallan').
	language('erg', '', individual, living, 'Sie').
	language('erh', '', individual, living, 'Eruwa').
	language('eri', '', individual, living, 'Ogea').
	language('erk', '', individual, living, 'South Efate').
	language('ero', '', individual, living, 'Horpa').
	language('err', '', individual, extinct, 'Erre').
	language('ers', '', individual, living, 'Ersu').
	language('ert', '', individual, living, 'Eritai').
	language('erw', '', individual, living, 'Erokwanas').
	language('ese', '', individual, living, 'Ese Ejja').
	language('esg', '', individual, living, 'Aheri Gondi').
	language('esh', '', individual, living, 'Eshtehardi').
	language('esi', '', individual, living, 'North Alaskan Inupiatun').
	language('esk', '', individual, living, 'Northwest Alaska Inupiatun').
	language('esl', '', individual, living, 'Egypt Sign Language').
	language('esm', '', individual, extinct, 'Esuma').
	language('esn', '', individual, living, 'Salvadoran Sign Language').
	language('eso', '', individual, living, 'Estonian Sign Language').
	language('esq', '', individual, extinct, 'Esselen').
	language('ess', '', individual, living, 'Central Siberian Yupik').
	language('est', 'et', macrolanguage, living, 'Estonian').
	language('esu', '', individual, living, 'Central Yupik').
	language('esy', '', individual, living, 'Eskayan').
	language('etb', '', individual, living, 'Etebi').
	language('etc', '', individual, extinct, 'Etchemin').
	language('eth', '', individual, living, 'Ethiopian Sign Language').
	language('etn', '', individual, living, 'Eton (Vanuatu)').
	language('eto', '', individual, living, 'Eton (Cameroon)').
	language('etr', '', individual, living, 'Edolo').
	language('ets', '', individual, living, 'Yekhee').
	language('ett', '', individual, special, 'Etruscan').
	language('etu', '', individual, living, 'Ejagham').
	language('etx', '', individual, living, 'Eten').
	language('etz', '', individual, living, 'Semimi').
	language('eud', '', individual, extinct, 'Eudeve').
	language('eus', 'eu', individual, living, 'Basque').
	language('eve', '', individual, living, 'Even').
	language('evh', '', individual, living, 'Uvbie').
	language('evn', '', individual, living, 'Evenki').
	language('ewe', 'ee', individual, living, 'Ewe').
	language('ewo', '', individual, living, 'Ewondo').
	language('ext', '', individual, living, 'Extremaduran').
	language('eya', '', individual, extinct, 'Eyak').
	language('eyo', '', individual, living, 'Keiyo').
	language('eza', '', individual, living, 'Ezaa').
	language('eze', '', individual, living, 'Uzekwe').
	language('faa', '', individual, living, 'Fasu').
	language('fab', '', individual, living, 'Fa d''Ambu').
	language('fad', '', individual, living, 'Wagi').
	language('faf', '', individual, living, 'Fagani').
	language('fag', '', individual, living, 'Finongan').
	language('fah', '', individual, living, 'Baissa Fali').
	language('fai', '', individual, living, 'Faiwol').
	language('faj', '', individual, living, 'Faita').
	language('fak', '', individual, living, 'Fang (Cameroon)').
	language('fal', '', individual, living, 'South Fali').
	language('fam', '', individual, living, 'Fam').
	language('fan', '', individual, living, 'Fang (Equatorial Guinea)').
	language('fao', 'fo', individual, living, 'Faroese').
	language('fap', '', individual, living, 'Paloor').
	language('far', '', individual, living, 'Fataleka').
	language('fas', 'fa', macrolanguage, living, 'Persian').
	language('fat', '', individual, living, 'Fanti').
	language('fau', '', individual, living, 'Fayu').
	language('fax', '', individual, living, 'Fala').
	language('fay', '', individual, living, 'Southwestern Fars').
	language('faz', '', individual, living, 'Northwestern Fars').
	language('fbl', '', individual, living, 'West Albay Bikol').
	language('fcs', '', individual, living, 'Quebec Sign Language').
	language('fer', '', individual, living, 'Feroge').
	language('ffi', '', individual, living, 'Foia Foia').
	language('ffm', '', individual, living, 'Maasina Fulfulde').
	language('fgr', '', individual, living, 'Fongoro').
	language('fia', '', individual, living, 'Nobiin').
	language('fie', '', individual, living, 'Fyer').
	language('fif', '', individual, living, 'Faifi').
	language('fij', 'fj', individual, living, 'Fijian').
	language('fil', '', individual, living, 'Filipino').
	language('fin', 'fi', individual, living, 'Finnish').
	language('fip', '', individual, living, 'Fipa').
	language('fir', '', individual, living, 'Firan').
	language('fit', '', individual, living, 'Tornedalen Finnish').
	language('fiw', '', individual, living, 'Fiwaga').
	language('fkk', '', individual, living, 'Kirya-Konzəl').
	language('fkv', '', individual, living, 'Kven Finnish').
	language('fla', '', individual, living, 'Kalispel-Pend d''Oreille').
	language('flh', '', individual, living, 'Foau').
	language('fli', '', individual, living, 'Fali').
	language('fll', '', individual, living, 'North Fali').
	language('fln', '', individual, extinct, 'Flinders Island').
	language('flr', '', individual, living, 'Fuliiru').
	language('fly', '', individual, living, 'Flaaitaal').
	language('fmp', '', individual, living, 'Fe''fe''').
	language('fmu', '', individual, living, 'Far Western Muria').
	language('fnb', '', individual, living, 'Fanbak').
	language('fng', '', individual, living, 'Fanagalo').
	language('fni', '', individual, living, 'Fania').
	language('fod', '', individual, living, 'Foodo').
	language('foi', '', individual, living, 'Foi').
	language('fom', '', individual, living, 'Foma').
	language('fon', '', individual, living, 'Fon').
	language('for', '', individual, living, 'Fore').
	language('fos', '', individual, extinct, 'Siraya').
	language('fpe', '', individual, living, 'Fernando Po Creole English').
	language('fqs', '', individual, living, 'Fas').
	language('fra', 'fr', individual, living, 'French').
	language('frc', '', individual, living, 'Cajun French').
	language('frd', '', individual, living, 'Fordata').
	language('frk', '', individual, special, 'Frankish').
	language('frm', '', individual, special, 'Middle French (ca. 1400-1600)').
	language('fro', '', individual, special, 'Old French (842-ca. 1400)').
	language('frp', '', individual, living, 'Arpitan').
	language('frq', '', individual, living, 'Forak').
	language('frr', '', individual, living, 'Northern Frisian').
	language('frs', '', individual, living, 'Eastern Frisian').
	language('frt', '', individual, living, 'Fortsenal').
	language('fry', 'fy', individual, living, 'Western Frisian').
	language('fse', '', individual, living, 'Finnish Sign Language').
	language('fsl', '', individual, living, 'French Sign Language').
	language('fss', '', individual, living, 'Finland-Swedish Sign Language').
	language('fub', '', individual, living, 'Adamawa Fulfulde').
	language('fuc', '', individual, living, 'Pulaar').
	language('fud', '', individual, living, 'East Futuna').
	language('fue', '', individual, living, 'Borgu Fulfulde').
	language('fuf', '', individual, living, 'Pular').
	language('fuh', '', individual, living, 'Western Niger Fulfulde').
	language('fui', '', individual, living, 'Bagirmi Fulfulde').
	language('fuj', '', individual, living, 'Ko').
	language('ful', 'ff', macrolanguage, living, 'Fulah').
	language('fum', '', individual, living, 'Fum').
	language('fun', '', individual, living, 'Fulniô').
	language('fuq', '', individual, living, 'Central-Eastern Niger Fulfulde').
	language('fur', '', individual, living, 'Friulian').
	language('fut', '', individual, living, 'Futuna-Aniwa').
	language('fuu', '', individual, living, 'Furu').
	language('fuv', '', individual, living, 'Nigerian Fulfulde').
	language('fuy', '', individual, living, 'Fuyug').
	language('fvr', '', individual, living, 'Fur').
	language('fwa', '', individual, living, 'Fwâi').
	language('fwe', '', individual, living, 'Fwe').
	language('gaa', '', individual, living, 'Ga').
	language('gab', '', individual, living, 'Gabri').
	language('gac', '', individual, living, 'Mixed Great Andamanese').
	language('gad', '', individual, living, 'Gaddang').
	language('gae', '', individual, living, 'Guarequena').
	language('gaf', '', individual, living, 'Gende').
	language('gag', '', individual, living, 'Gagauz').
	language('gah', '', individual, living, 'Alekano').
	language('gai', '', individual, living, 'Borei').
	language('gaj', '', individual, living, 'Gadsup').
	language('gak', '', individual, living, 'Gamkonora').
	language('gal', '', individual, living, 'Galolen').
	language('gam', '', individual, living, 'Kandawo').
	language('gan', '', individual, living, 'Gan Chinese').
	language('gao', '', individual, living, 'Gants').
	language('gap', '', individual, living, 'Gal').
	language('gaq', '', individual, living, 'Gata''').
	language('gar', '', individual, living, 'Galeya').
	language('gas', '', individual, living, 'Adiwasi Garasia').
	language('gat', '', individual, living, 'Kenati').
	language('gau', '', individual, living, 'Mudhili Gadaba').
	language('gaw', '', individual, living, 'Nobonob').
	language('gax', '', individual, living, 'Borana-Arsi-Guji Oromo').
	language('gay', '', individual, living, 'Gayo').
	language('gaz', '', individual, living, 'West Central Oromo').
	language('gba', '', macrolanguage, living, 'Gbaya (Central African Republic)').
	language('gbb', '', individual, living, 'Kaytetye').
	language('gbd', '', individual, living, 'Karajarri').
	language('gbe', '', individual, living, 'Niksek').
	language('gbf', '', individual, living, 'Gaikundi').
	language('gbg', '', individual, living, 'Gbanziri').
	language('gbh', '', individual, living, 'Defi Gbe').
	language('gbi', '', individual, living, 'Galela').
	language('gbj', '', individual, living, 'Bodo Gadaba').
	language('gbk', '', individual, living, 'Gaddi').
	language('gbl', '', individual, living, 'Gamit').
	language('gbm', '', individual, living, 'Garhwali').
	language('gbn', '', individual, living, 'Mo''da').
	language('gbo', '', individual, living, 'Northern Grebo').
	language('gbp', '', individual, living, 'Gbaya-Bossangoa').
	language('gbq', '', individual, living, 'Gbaya-Bozoum').
	language('gbr', '', individual, living, 'Gbagyi').
	language('gbs', '', individual, living, 'Gbesi Gbe').
	language('gbu', '', individual, living, 'Gagadu').
	language('gbv', '', individual, living, 'Gbanu').
	language('gbw', '', individual, living, 'Gabi-Gabi').
	language('gbx', '', individual, living, 'Eastern Xwla Gbe').
	language('gby', '', individual, living, 'Gbari').
	language('gbz', '', individual, living, 'Zoroastrian Dari').
	language('gcc', '', individual, living, 'Mali').
	language('gcd', '', individual, extinct, 'Ganggalida').
	language('gce', '', individual, extinct, 'Galice').
	language('gcf', '', individual, living, 'Guadeloupean Creole French').
	language('gcl', '', individual, living, 'Grenadian Creole English').
	language('gcn', '', individual, living, 'Gaina').
	language('gcr', '', individual, living, 'Guianese Creole French').
	language('gct', '', individual, living, 'Colonia Tovar German').
	language('gda', '', individual, living, 'Gade Lohar').
	language('gdb', '', individual, living, 'Pottangi Ollar Gadaba').
	language('gdc', '', individual, extinct, 'Gugu Badhun').
	language('gdd', '', individual, living, 'Gedaged').
	language('gde', '', individual, living, 'Gude').
	language('gdf', '', individual, living, 'Guduf-Gava').
	language('gdg', '', individual, living, 'Ga''dang').
	language('gdh', '', individual, living, 'Gadjerawang').
	language('gdi', '', individual, living, 'Gundi').
	language('gdj', '', individual, living, 'Gurdjar').
	language('gdk', '', individual, living, 'Gadang').
	language('gdl', '', individual, living, 'Dirasha').
	language('gdm', '', individual, living, 'Laal').
	language('gdn', '', individual, living, 'Umanakaina').
	language('gdo', '', individual, living, 'Ghodoberi').
	language('gdq', '', individual, living, 'Mehri').
	language('gdr', '', individual, living, 'Wipi').
	language('gds', '', individual, living, 'Ghandruk Sign Language').
	language('gdt', '', individual, extinct, 'Kungardutyi').
	language('gdu', '', individual, living, 'Gudu').
	language('gdx', '', individual, living, 'Godwari').
	language('gea', '', individual, living, 'Geruma').
	language('geb', '', individual, living, 'Kire').
	language('gec', '', individual, living, 'Gboloo Grebo').
	language('ged', '', individual, living, 'Gade').
	language('gef', '', individual, living, 'Gerai').
	language('geg', '', individual, living, 'Gengle').
	language('geh', '', individual, living, 'Hutterite German').
	language('gei', '', individual, living, 'Gebe').
	language('gej', '', individual, living, 'Gen').
	language('gek', '', individual, living, 'Ywom').
	language('gel', '', individual, living, 'ut-Ma''in').
	language('geq', '', individual, living, 'Geme').
	language('ges', '', individual, living, 'Geser-Gorom').
	language('gev', '', individual, living, 'Eviya').
	language('gew', '', individual, living, 'Gera').
	language('gex', '', individual, living, 'Garre').
	language('gey', '', individual, living, 'Enya').
	language('gez', '', individual, special, 'Geez').
	language('gfk', '', individual, living, 'Patpatar').
	language('gft', '', individual, extinct, 'Gafat').
	language('gga', '', individual, living, 'Gao').
	language('ggb', '', individual, living, 'Gbii').
	language('ggd', '', individual, extinct, 'Gugadj').
	language('gge', '', individual, living, 'Gurr-goni').
	language('ggg', '', individual, living, 'Gurgula').
	language('ggk', '', individual, extinct, 'Kungarakany').
	language('ggl', '', individual, living, 'Ganglau').
	language('ggt', '', individual, living, 'Gitua').
	language('ggu', '', individual, living, 'Gagu').
	language('ggw', '', individual, living, 'Gogodala').
	language('gha', '', individual, living, 'Ghadamès').
	language('ghc', '', individual, special, 'Hiberno-Scottish Gaelic').
	language('ghe', '', individual, living, 'Southern Ghale').
	language('ghh', '', individual, living, 'Northern Ghale').
	language('ghk', '', individual, living, 'Geko Karen').
	language('ghl', '', individual, living, 'Ghulfan').
	language('ghn', '', individual, living, 'Ghanongga').
	language('gho', '', individual, living, 'Ghomara').
	language('ghr', '', individual, living, 'Ghera').
	language('ghs', '', individual, living, 'Guhu-Samane').
	language('ght', '', individual, living, 'Kuke').
	language('gia', '', individual, living, 'Kija').
	language('gib', '', individual, living, 'Gibanawa').
	language('gic', '', individual, living, 'Gail').
	language('gid', '', individual, living, 'Gidar').
	language('gie', '', individual, living, 'Gaɓogbo').
	language('gig', '', individual, living, 'Goaria').
	language('gih', '', individual, living, 'Githabul').
	language('gii', '', individual, living, 'Girirra').
	language('gil', '', individual, living, 'Gilbertese').
	language('gim', '', individual, living, 'Gimi (Eastern Highlands)').
	language('gin', '', individual, living, 'Hinukh').
	language('gip', '', individual, living, 'Gimi (West New Britain)').
	language('giq', '', individual, living, 'Green Gelao').
	language('gir', '', individual, living, 'Red Gelao').
	language('gis', '', individual, living, 'North Giziga').
	language('git', '', individual, living, 'Gitxsan').
	language('giu', '', individual, living, 'Mulao').
	language('giw', '', individual, living, 'White Gelao').
	language('gix', '', individual, living, 'Gilima').
	language('giy', '', individual, living, 'Giyug').
	language('giz', '', individual, living, 'South Giziga').
	language('gjk', '', individual, living, 'Kachi Koli').
	language('gjm', '', individual, extinct, 'Gunditjmara').
	language('gjn', '', individual, living, 'Gonja').
	language('gjr', '', individual, living, 'Gurindji Kriol').
	language('gju', '', individual, living, 'Gujari').
	language('gka', '', individual, living, 'Guya').
	language('gkd', '', individual, living, 'Magɨ (Madang Province)').
	language('gke', '', individual, living, 'Ndai').
	language('gkn', '', individual, living, 'Gokana').
	language('gko', '', individual, extinct, 'Kok-Nar').
	language('gkp', '', individual, living, 'Guinea Kpelle').
	language('gku', '', individual, extinct, 'ǂUngkue').
	language('gla', 'gd', individual, living, 'Scottish Gaelic').
	language('glb', '', individual, living, 'Belning').
	language('glc', '', individual, living, 'Bon Gula').
	language('gld', '', individual, living, 'Nanai').
	language('gle', 'ga', individual, living, 'Irish').
	language('glg', 'gl', individual, living, 'Galician').
	language('glh', '', individual, living, 'Northwest Pashai').
	language('glj', '', individual, living, 'Gula Iro').
	language('glk', '', individual, living, 'Gilaki').
	language('gll', '', individual, extinct, 'Garlali').
	language('glo', '', individual, living, 'Galambu').
	language('glr', '', individual, living, 'Glaro-Twabo').
	language('glu', '', individual, living, 'Gula (Chad)').
	language('glv', 'gv', individual, living, 'Manx').
	language('glw', '', individual, living, 'Glavda').
	language('gly', '', individual, extinct, 'Gule').
	language('gma', '', individual, extinct, 'Gambera').
	language('gmb', '', individual, living, 'Gula''alaa').
	language('gmd', '', individual, living, 'Mághdì').
	language('gmg', '', individual, living, 'Magɨyi').
	language('gmh', '', individual, special, 'Middle High German (ca. 1050-1500)').
	language('gml', '', individual, special, 'Middle Low German').
	language('gmm', '', individual, living, 'Gbaya-Mbodomo').
	language('gmn', '', individual, living, 'Gimnime').
	language('gmr', '', individual, living, 'Mirning').
	language('gmu', '', individual, living, 'Gumalu').
	language('gmv', '', individual, living, 'Gamo').
	language('gmx', '', individual, living, 'Magoma').
	language('gmy', '', individual, special, 'Mycenaean Greek').
	language('gmz', '', individual, living, 'Mgbolizhia').
	language('gna', '', individual, living, 'Kaansa').
	language('gnb', '', individual, living, 'Gangte').
	language('gnc', '', individual, extinct, 'Guanche').
	language('gnd', '', individual, living, 'Zulgo-Gemzek').
	language('gne', '', individual, living, 'Ganang').
	language('gng', '', individual, living, 'Ngangam').
	language('gnh', '', individual, living, 'Lere').
	language('gni', '', individual, living, 'Gooniyandi').
	language('gnj', '', individual, living, 'Ngen').
	language('gnk', '', individual, living, 'ǁGana').
	language('gnl', '', individual, extinct, 'Gangulu').
	language('gnm', '', individual, living, 'Ginuman').
	language('gnn', '', individual, living, 'Gumatj').
	language('gno', '', individual, living, 'Northern Gondi').
	language('gnq', '', individual, living, 'Gana').
	language('gnr', '', individual, extinct, 'Gureng Gureng').
	language('gnt', '', individual, living, 'Guntai').
	language('gnu', '', individual, living, 'Gnau').
	language('gnw', '', individual, living, 'Western Bolivian Guaraní').
	language('gnz', '', individual, living, 'Ganzi').
	language('goa', '', individual, living, 'Guro').
	language('gob', '', individual, living, 'Playero').
	language('goc', '', individual, living, 'Gorakor').
	language('god', '', individual, living, 'Godié').
	language('goe', '', individual, living, 'Gongduk').
	language('gof', '', individual, living, 'Gofa').
	language('gog', '', individual, living, 'Gogo').
	language('goh', '', individual, special, 'Old High German (ca. 750-1050)').
	language('goi', '', individual, living, 'Gobasi').
	language('goj', '', individual, living, 'Gowlan').
	language('gok', '', individual, living, 'Gowli').
	language('gol', '', individual, living, 'Gola').
	language('gom', '', individual, living, 'Goan Konkani').
	language('gon', '', macrolanguage, living, 'Gondi').
	language('goo', '', individual, living, 'Gone Dau').
	language('gop', '', individual, living, 'Yeretuar').
	language('goq', '', individual, living, 'Gorap').
	language('gor', '', individual, living, 'Gorontalo').
	language('gos', '', individual, living, 'Gronings').
	language('got', '', individual, special, 'Gothic').
	language('gou', '', individual, living, 'Gavar').
	language('gov', '', individual, living, 'Goo').
	language('gow', '', individual, living, 'Gorowa').
	language('gox', '', individual, living, 'Gobu').
	language('goy', '', individual, living, 'Goundo').
	language('goz', '', individual, living, 'Gozarkhani').
	language('gpa', '', individual, living, 'Gupa-Abawa').
	language('gpe', '', individual, living, 'Ghanaian Pidgin English').
	language('gpn', '', individual, living, 'Taiap').
	language('gqa', '', individual, living, 'Ga''anda').
	language('gqi', '', individual, living, 'Guiqiong').
	language('gqn', '', individual, extinct, 'Guana (Brazil)').
	language('gqr', '', individual, living, 'Gor').
	language('gqu', '', individual, living, 'Qau').
	language('gra', '', individual, living, 'Rajput Garasia').
	language('grb', '', macrolanguage, living, 'Grebo').
	language('grc', '', individual, special, 'Ancient Greek (to 1453)').
	language('grd', '', individual, living, 'Guruntum-Mbaaru').
	language('grg', '', individual, living, 'Madi').
	language('grh', '', individual, living, 'Gbiri-Niragu').
	language('gri', '', individual, living, 'Ghari').
	language('grj', '', individual, living, 'Southern Grebo').
	language('grm', '', individual, living, 'Kota Marudu Talantang').
	language('grn', 'gn', macrolanguage, living, 'Guarani').
	language('gro', '', individual, living, 'Groma').
	language('grq', '', individual, living, 'Gorovu').
	language('grr', '', individual, living, 'Taznatit').
	language('grs', '', individual, living, 'Gresi').
	language('grt', '', individual, living, 'Garo').
	language('gru', '', individual, living, 'Kistane').
	language('grv', '', individual, living, 'Central Grebo').
	language('grw', '', individual, living, 'Gweda').
	language('grx', '', individual, living, 'Guriaso').
	language('gry', '', individual, living, 'Barclayville Grebo').
	language('grz', '', individual, living, 'Guramalum').
	language('gse', '', individual, living, 'Ghanaian Sign Language').
	language('gsg', '', individual, living, 'German Sign Language').
	language('gsl', '', individual, living, 'Gusilay').
	language('gsm', '', individual, living, 'Guatemalan Sign Language').
	language('gsn', '', individual, living, 'Nema').
	language('gso', '', individual, living, 'Southwest Gbaya').
	language('gsp', '', individual, living, 'Wasembo').
	language('gss', '', individual, living, 'Greek Sign Language').
	language('gsw', '', individual, living, 'Swiss German').
	language('gta', '', individual, living, 'Guató').
	language('gtu', '', individual, extinct, 'Aghu-Tharnggala').
	language('gua', '', individual, living, 'Shiki').
	language('gub', '', individual, living, 'Guajajára').
	language('guc', '', individual, living, 'Wayuu').
	language('gud', '', individual, living, 'Yocoboué Dida').
	language('gue', '', individual, living, 'Gurindji').
	language('guf', '', individual, living, 'Gupapuyngu').
	language('gug', '', individual, living, 'Paraguayan Guaraní').
	language('guh', '', individual, living, 'Guahibo').
	language('gui', '', individual, living, 'Eastern Bolivian Guaraní').
	language('guj', 'gu', individual, living, 'Gujarati').
	language('guk', '', individual, living, 'Gumuz').
	language('gul', '', individual, living, 'Sea Island Creole English').
	language('gum', '', individual, living, 'Guambiano').
	language('gun', '', individual, living, 'Mbyá Guaraní').
	language('guo', '', individual, living, 'Guayabero').
	language('gup', '', individual, living, 'Gunwinggu').
	language('guq', '', individual, living, 'Aché').
	language('gur', '', individual, living, 'Farefare').
	language('gus', '', individual, living, 'Guinean Sign Language').
	language('gut', '', individual, living, 'Maléku Jaíka').
	language('guu', '', individual, living, 'Yanomamö').
	language('guw', '', individual, living, 'Gun').
	language('gux', '', individual, living, 'Gourmanchéma').
	language('guz', '', individual, living, 'Gusii').
	language('gva', '', individual, living, 'Guana (Paraguay)').
	language('gvc', '', individual, living, 'Guanano').
	language('gve', '', individual, living, 'Duwet').
	language('gvf', '', individual, living, 'Golin').
	language('gvj', '', individual, living, 'Guajá').
	language('gvl', '', individual, living, 'Gulay').
	language('gvm', '', individual, living, 'Gurmana').
	language('gvn', '', individual, living, 'Kuku-Yalanji').
	language('gvo', '', individual, living, 'Gavião Do Jiparaná').
	language('gvp', '', individual, living, 'Pará Gavião').
	language('gvr', '', individual, living, 'Gurung').
	language('gvs', '', individual, living, 'Gumawana').
	language('gvy', '', individual, extinct, 'Guyani').
	language('gwa', '', individual, living, 'Mbato').
	language('gwb', '', individual, living, 'Gwa').
	language('gwc', '', individual, living, 'Gawri').
	language('gwd', '', individual, living, 'Gawwada').
	language('gwe', '', individual, living, 'Gweno').
	language('gwf', '', individual, living, 'Gowro').
	language('gwg', '', individual, living, 'Moo').
	language('gwi', '', individual, living, 'Gwichʼin').
	language('gwj', '', individual, living, 'ǀGwi').
	language('gwm', '', individual, extinct, 'Awngthim').
	language('gwn', '', individual, living, 'Gwandara').
	language('gwr', '', individual, living, 'Gwere').
	language('gwt', '', individual, living, 'Gawar-Bati').
	language('gwu', '', individual, extinct, 'Guwamu').
	language('gww', '', individual, living, 'Kwini').
	language('gwx', '', individual, living, 'Gua').
	language('gxx', '', individual, living, 'Wè Southern').
	language('gya', '', individual, living, 'Northwest Gbaya').
	language('gyb', '', individual, living, 'Garus').
	language('gyd', '', individual, living, 'Kayardild').
	language('gye', '', individual, living, 'Gyem').
	language('gyf', '', individual, extinct, 'Gungabula').
	language('gyg', '', individual, living, 'Gbayi').
	language('gyi', '', individual, living, 'Gyele').
	language('gyl', '', individual, living, 'Gayil').
	language('gym', '', individual, living, 'Ngäbere').
	language('gyn', '', individual, living, 'Guyanese Creole English').
	language('gyo', '', individual, living, 'Gyalsumdo').
	language('gyr', '', individual, living, 'Guarayu').
	language('gyy', '', individual, extinct, 'Gunya').
	language('gyz', '', individual, living, 'Geji').
	language('gza', '', individual, living, 'Ganza').
	language('gzi', '', individual, living, 'Gazi').
	language('gzn', '', individual, living, 'Gane').
	language('haa', '', individual, living, 'Hän').
	language('hab', '', individual, living, 'Hanoi Sign Language').
	language('hac', '', individual, living, 'Gurani').
	language('had', '', individual, living, 'Hatam').
	language('hae', '', individual, living, 'Eastern Oromo').
	language('haf', '', individual, living, 'Haiphong Sign Language').
	language('hag', '', individual, living, 'Hanga').
	language('hah', '', individual, living, 'Hahon').
	language('hai', '', macrolanguage, living, 'Haida').
	language('haj', '', individual, living, 'Hajong').
	language('hak', '', individual, living, 'Hakka Chinese').
	language('hal', '', individual, living, 'Halang').
	language('ham', '', individual, living, 'Hewa').
	language('han', '', individual, living, 'Hangaza').
	language('hao', '', individual, living, 'Hakö').
	language('hap', '', individual, living, 'Hupla').
	language('haq', '', individual, living, 'Ha').
	language('har', '', individual, living, 'Harari').
	language('has', '', individual, living, 'Haisla').
	language('hat', 'ht', individual, living, 'Haitian').
	language('hau', 'ha', individual, living, 'Hausa').
	language('hav', '', individual, living, 'Havu').
	language('haw', '', individual, living, 'Hawaiian').
	language('hax', '', individual, living, 'Southern Haida').
	language('hay', '', individual, living, 'Haya').
	language('haz', '', individual, living, 'Hazaragi').
	language('hba', '', individual, living, 'Hamba').
	language('hbb', '', individual, living, 'Huba').
	language('hbn', '', individual, living, 'Heiban').
	language('hbo', '', individual, special, 'Ancient Hebrew').
	language('hbs', 'sh', macrolanguage, living, 'Serbo-Croatian').
	language('hbu', '', individual, living, 'Habu').
	language('hca', '', individual, living, 'Andaman Creole Hindi').
	language('hch', '', individual, living, 'Huichol').
	language('hdn', '', individual, living, 'Northern Haida').
	language('hds', '', individual, living, 'Honduras Sign Language').
	language('hdy', '', individual, living, 'Hadiyya').
	language('hea', '', individual, living, 'Northern Qiandong Miao').
	language('heb', 'he', individual, living, 'Hebrew').
	language('hed', '', individual, living, 'Herdé').
	language('heg', '', individual, living, 'Helong').
	language('heh', '', individual, living, 'Hehe').
	language('hei', '', individual, living, 'Heiltsuk').
	language('hem', '', individual, living, 'Hemba').
	language('her', 'hz', individual, living, 'Herero').
	language('hgm', '', individual, living, 'Haiǁom').
	language('hgw', '', individual, living, 'Haigwai').
	language('hhi', '', individual, living, 'Hoia Hoia').
	language('hhr', '', individual, living, 'Kerak').
	language('hhy', '', individual, living, 'Hoyahoya').
	language('hia', '', individual, living, 'Lamang').
	language('hib', '', individual, extinct, 'Hibito').
	language('hid', '', individual, living, 'Hidatsa').
	language('hif', '', individual, living, 'Fiji Hindi').
	language('hig', '', individual, living, 'Kamwe').
	language('hih', '', individual, living, 'Pamosu').
	language('hii', '', individual, living, 'Hinduri').
	language('hij', '', individual, living, 'Hijuk').
	language('hik', '', individual, living, 'Seit-Kaitetu').
	language('hil', '', individual, living, 'Hiligaynon').
	language('hin', 'hi', individual, living, 'Hindi').
	language('hio', '', individual, living, 'Tsoa').
	language('hir', '', individual, living, 'Himarimã').
	language('hit', '', individual, special, 'Hittite').
	language('hiw', '', individual, living, 'Hiw').
	language('hix', '', individual, living, 'Hixkaryána').
	language('hji', '', individual, living, 'Haji').
	language('hka', '', individual, living, 'Kahe').
	language('hke', '', individual, living, 'Hunde').
	language('hkh', '', individual, living, 'Khah').
	language('hkk', '', individual, living, 'Hunjara-Kaina Ke').
	language('hkn', '', individual, living, 'Mel-Khaonh').
	language('hks', '', individual, living, 'Hong Kong Sign Language').
	language('hla', '', individual, living, 'Halia').
	language('hlb', '', individual, living, 'Halbi').
	language('hld', '', individual, living, 'Halang Doan').
	language('hle', '', individual, living, 'Hlersu').
	language('hlt', '', individual, living, 'Matu Chin').
	language('hlu', '', individual, special, 'Hieroglyphic Luwian').
	language('hma', '', individual, living, 'Southern Mashan Hmong').
	language('hmb', '', individual, living, 'Humburi Senni Songhay').
	language('hmc', '', individual, living, 'Central Huishui Hmong').
	language('hmd', '', individual, living, 'Large Flowery Miao').
	language('hme', '', individual, living, 'Eastern Huishui Hmong').
	language('hmf', '', individual, living, 'Hmong Don').
	language('hmg', '', individual, living, 'Southwestern Guiyang Hmong').
	language('hmh', '', individual, living, 'Southwestern Huishui Hmong').
	language('hmi', '', individual, living, 'Northern Huishui Hmong').
	language('hmj', '', individual, living, 'Ge').
	language('hmk', '', individual, special, 'Maek').
	language('hml', '', individual, living, 'Luopohe Hmong').
	language('hmm', '', individual, living, 'Central Mashan Hmong').
	language('hmn', '', macrolanguage, living, 'Hmong').
	language('hmo', 'ho', individual, living, 'Hiri Motu').
	language('hmp', '', individual, living, 'Northern Mashan Hmong').
	language('hmq', '', individual, living, 'Eastern Qiandong Miao').
	language('hmr', '', individual, living, 'Hmar').
	language('hms', '', individual, living, 'Southern Qiandong Miao').
	language('hmt', '', individual, living, 'Hamtai').
	language('hmu', '', individual, living, 'Hamap').
	language('hmv', '', individual, living, 'Hmong Dô').
	language('hmw', '', individual, living, 'Western Mashan Hmong').
	language('hmy', '', individual, living, 'Southern Guiyang Hmong').
	language('hmz', '', individual, living, 'Hmong Shua').
	language('hna', '', individual, living, 'Mina (Cameroon)').
	language('hnd', '', individual, living, 'Southern Hindko').
	language('hne', '', individual, living, 'Chhattisgarhi').
	language('hng', '', individual, living, 'Hungu').
	language('hnh', '', individual, living, 'ǁAni').
	language('hni', '', individual, living, 'Hani').
	language('hnj', '', individual, living, 'Hmong Njua').
	language('hnm', '', individual, living, 'Hainanese').
	language('hnn', '', individual, living, 'Hanunoo').
	language('hno', '', individual, living, 'Northern Hindko').
	language('hns', '', individual, living, 'Caribbean Hindustani').
	language('hnu', '', individual, living, 'Hung').
	language('hoa', '', individual, living, 'Hoava').
	language('hob', '', individual, living, 'Mari (Madang Province)').
	language('hoc', '', individual, living, 'Ho').
	language('hod', '', individual, extinct, 'Holma').
	language('hoe', '', individual, living, 'Horom').
	language('hoh', '', individual, living, 'Hobyót').
	language('hoi', '', individual, living, 'Holikachuk').
	language('hoj', '', individual, living, 'Hadothi').
	language('hol', '', individual, living, 'Holu').
	language('hom', '', individual, extinct, 'Homa').
	language('hoo', '', individual, living, 'Holoholo').
	language('hop', '', individual, living, 'Hopi').
	language('hor', '', individual, extinct, 'Horo').
	language('hos', '', individual, living, 'Ho Chi Minh City Sign Language').
	language('hot', '', individual, living, 'Hote').
	language('hov', '', individual, living, 'Hovongan').
	language('how', '', individual, living, 'Honi').
	language('hoy', '', individual, living, 'Holiya').
	language('hoz', '', individual, living, 'Hozo').
	language('hpo', '', individual, extinct, 'Hpon').
	language('hps', '', individual, living, 'Hawai''i Sign Language (HSL)').
	language('hra', '', individual, living, 'Hrangkhol').
	language('hrc', '', individual, living, 'Niwer Mil').
	language('hre', '', individual, living, 'Hre').
	language('hrk', '', individual, living, 'Haruku').
	language('hrm', '', individual, living, 'Horned Miao').
	language('hro', '', individual, living, 'Haroi').
	language('hrp', '', individual, extinct, 'Nhirrpi').
	language('hrt', '', individual, living, 'Hértevin').
	language('hru', '', individual, living, 'Hruso').
	language('hrv', 'hr', individual, living, 'Croatian').
	language('hrw', '', individual, living, 'Warwar Feni').
	language('hrx', '', individual, living, 'Hunsrik').
	language('hrz', '', individual, living, 'Harzani').
	language('hsb', '', individual, living, 'Upper Sorbian').
	language('hsh', '', individual, living, 'Hungarian Sign Language').
	language('hsl', '', individual, living, 'Hausa Sign Language').
	language('hsn', '', individual, living, 'Xiang Chinese').
	language('hss', '', individual, living, 'Harsusi').
	language('hti', '', individual, extinct, 'Hoti').
	language('hto', '', individual, living, 'Minica Huitoto').
	language('hts', '', individual, living, 'Hadza').
	language('htu', '', individual, living, 'Hitu').
	language('htx', '', individual, special, 'Middle Hittite').
	language('hub', '', individual, living, 'Huambisa').
	language('huc', '', individual, living, 'ǂHua').
	language('hud', '', individual, living, 'Huaulu').
	language('hue', '', individual, living, 'San Francisco Del Mar Huave').
	language('huf', '', individual, living, 'Humene').
	language('hug', '', individual, living, 'Huachipaeri').
	language('huh', '', individual, living, 'Huilliche').
	language('hui', '', individual, living, 'Huli').
	language('huj', '', individual, living, 'Northern Guiyang Hmong').
	language('huk', '', individual, extinct, 'Hulung').
	language('hul', '', individual, living, 'Hula').
	language('hum', '', individual, living, 'Hungana').
	language('hun', 'hu', individual, living, 'Hungarian').
	language('huo', '', individual, living, 'Hu').
	language('hup', '', individual, living, 'Hupa').
	language('huq', '', individual, living, 'Tsat').
	language('hur', '', individual, living, 'Halkomelem').
	language('hus', '', individual, living, 'Huastec').
	language('hut', '', individual, living, 'Humla').
	language('huu', '', individual, living, 'Murui Huitoto').
	language('huv', '', individual, living, 'San Mateo Del Mar Huave').
	language('huw', '', individual, extinct, 'Hukumina').
	language('hux', '', individual, living, 'Nüpode Huitoto').
	language('huy', '', individual, living, 'Hulaulá').
	language('huz', '', individual, living, 'Hunzib').
	language('hvc', '', individual, living, 'Haitian Vodoun Culture Language').
	language('hve', '', individual, living, 'San Dionisio Del Mar Huave').
	language('hvk', '', individual, living, 'Haveke').
	language('hvn', '', individual, living, 'Sabu').
	language('hvv', '', individual, living, 'Santa María Del Mar Huave').
	language('hwa', '', individual, living, 'Wané').
	language('hwc', '', individual, living, 'Hawai''i Creole English').
	language('hwo', '', individual, living, 'Hwana').
	language('hya', '', individual, living, 'Hya').
	language('hye', 'hy', individual, living, 'Armenian').
	language('hyw', '', individual, living, 'Western Armenian').
	language('iai', '', individual, living, 'Iaai').
	language('ian', '', individual, living, 'Iatmul').
	language('iar', '', individual, living, 'Purari').
	language('iba', '', individual, living, 'Iban').
	language('ibb', '', individual, living, 'Ibibio').
	language('ibd', '', individual, living, 'Iwaidja').
	language('ibe', '', individual, living, 'Akpes').
	language('ibg', '', individual, living, 'Ibanag').
	language('ibh', '', individual, living, 'Bih').
	language('ibl', '', individual, living, 'Ibaloi').
	language('ibm', '', individual, living, 'Agoi').
	language('ibn', '', individual, living, 'Ibino').
	language('ibo', 'ig', individual, living, 'Igbo').
	language('ibr', '', individual, living, 'Ibuoro').
	language('ibu', '', individual, living, 'Ibu').
	language('iby', '', individual, living, 'Ibani').
	language('ica', '', individual, living, 'Ede Ica').
	language('ich', '', individual, living, 'Etkywan').
	language('icl', '', individual, living, 'Icelandic Sign Language').
	language('icr', '', individual, living, 'Islander Creole English').
	language('ida', '', individual, living, 'Idakho-Isukha-Tiriki').
	language('idb', '', individual, living, 'Indo-Portuguese').
	language('idc', '', individual, living, 'Idon').
	language('idd', '', individual, living, 'Ede Idaca').
	language('ide', '', individual, living, 'Idere').
	language('idi', '', individual, living, 'Idi').
	language('ido', 'io', individual, constructed, 'Ido').
	language('idr', '', individual, living, 'Indri').
	language('ids', '', individual, living, 'Idesa').
	language('idt', '', individual, living, 'Idaté').
	language('idu', '', individual, living, 'Idoma').
	language('ifa', '', individual, living, 'Amganad Ifugao').
	language('ifb', '', individual, living, 'Batad Ifugao').
	language('ife', '', individual, living, 'Ifè').
	language('iff', '', individual, extinct, 'Ifo').
	language('ifk', '', individual, living, 'Tuwali Ifugao').
	language('ifm', '', individual, living, 'Teke-Fuumu').
	language('ifu', '', individual, living, 'Mayoyao Ifugao').
	language('ify', '', individual, living, 'Keley-I Kallahan').
	language('igb', '', individual, living, 'Ebira').
	language('ige', '', individual, living, 'Igede').
	language('igg', '', individual, living, 'Igana').
	language('igl', '', individual, living, 'Igala').
	language('igm', '', individual, living, 'Kanggape').
	language('ign', '', individual, living, 'Ignaciano').
	language('igo', '', individual, living, 'Isebe').
	language('igs', '', individual, constructed, 'Interglossa').
	language('igw', '', individual, living, 'Igwe').
	language('ihb', '', individual, living, 'Iha Based Pidgin').
	language('ihi', '', individual, living, 'Ihievbe').
	language('ihp', '', individual, living, 'Iha').
	language('ihw', '', individual, extinct, 'Bidhawal').
	language('iii', 'ii', individual, living, 'Sichuan Yi').
	language('iin', '', individual, extinct, 'Thiin').
	language('ijc', '', individual, living, 'Izon').
	language('ije', '', individual, living, 'Biseni').
	language('ijj', '', individual, living, 'Ede Ije').
	language('ijn', '', individual, living, 'Kalabari').
	language('ijs', '', individual, living, 'Southeast Ijo').
	language('ike', '', individual, living, 'Eastern Canadian Inuktitut').
	language('ikh', '', individual, living, 'Ikhin-Arokho').
	language('iki', '', individual, living, 'Iko').
	language('ikk', '', individual, living, 'Ika').
	language('ikl', '', individual, living, 'Ikulu').
	language('iko', '', individual, living, 'Olulumo-Ikom').
	language('ikp', '', individual, living, 'Ikpeshi').
	language('ikr', '', individual, extinct, 'Ikaranggal').
	language('iks', '', individual, living, 'Inuit Sign Language').
	language('ikt', '', individual, living, 'Inuinnaqtun').
	language('iku', 'iu', macrolanguage, living, 'Inuktitut').
	language('ikv', '', individual, living, 'Iku-Gora-Ankwa').
	language('ikw', '', individual, living, 'Ikwere').
	language('ikx', '', individual, living, 'Ik').
	language('ikz', '', individual, living, 'Ikizu').
	language('ila', '', individual, living, 'Ile Ape').
	language('ilb', '', individual, living, 'Ila').
	language('ile', 'ie', individual, constructed, 'Interlingue').
	language('ilg', '', individual, extinct, 'Garig-Ilgar').
	language('ili', '', individual, living, 'Ili Turki').
	language('ilk', '', individual, living, 'Ilongot').
	language('ilm', '', individual, living, 'Iranun (Malaysia)').
	language('ilo', '', individual, living, 'Iloko').
	language('ilp', '', individual, living, 'Iranun (Philippines)').
	language('ils', '', individual, living, 'International Sign').
	language('ilu', '', individual, living, 'Ili''uun').
	language('ilv', '', individual, living, 'Ilue').
	language('ima', '', individual, living, 'Mala Malasar').
	language('imi', '', individual, living, 'Anamgura').
	language('iml', '', individual, extinct, 'Miluk').
	language('imn', '', individual, living, 'Imonda').
	language('imo', '', individual, living, 'Imbongu').
	language('imr', '', individual, living, 'Imroing').
	language('ims', '', individual, special, 'Marsian').
	language('imt', '', individual, living, 'Imotong').
	language('imy', '', individual, special, 'Milyan').
	language('ina', 'ia', individual, constructed, 'Interlingua (IALA)').
	language('inb', '', individual, living, 'Inga').
	language('ind', 'id', individual, living, 'Indonesian').
	language('ing', '', individual, living, 'Degexit''an').
	language('inh', '', individual, living, 'Ingush').
	language('inj', '', individual, living, 'Jungle Inga').
	language('inl', '', individual, living, 'Indonesian Sign Language').
	language('inm', '', individual, special, 'Minaean').
	language('inn', '', individual, living, 'Isinai').
	language('ino', '', individual, living, 'Inoke-Yate').
	language('inp', '', individual, living, 'Iñapari').
	language('ins', '', individual, living, 'Indian Sign Language').
	language('int', '', individual, living, 'Intha').
	language('inz', '', individual, extinct, 'Ineseño').
	language('ior', '', individual, living, 'Inor').
	language('iou', '', individual, living, 'Tuma-Irumu').
	language('iow', '', individual, extinct, 'Iowa-Oto').
	language('ipi', '', individual, living, 'Ipili').
	language('ipk', 'ik', macrolanguage, living, 'Inupiaq').
	language('ipo', '', individual, living, 'Ipiko').
	language('iqu', '', individual, living, 'Iquito').
	language('iqw', '', individual, living, 'Ikwo').
	language('ire', '', individual, living, 'Iresim').
	language('irh', '', individual, living, 'Irarutu').
	language('iri', '', individual, living, 'Rigwe').
	language('irk', '', individual, living, 'Iraqw').
	language('irn', '', individual, living, 'Irántxe').
	language('irr', '', individual, living, 'Ir').
	language('iru', '', individual, living, 'Irula').
	language('irx', '', individual, living, 'Kamberau').
	language('iry', '', individual, living, 'Iraya').
	language('isa', '', individual, living, 'Isabi').
	language('isc', '', individual, living, 'Isconahua').
	language('isd', '', individual, living, 'Isnag').
	language('ise', '', individual, living, 'Italian Sign Language').
	language('isg', '', individual, living, 'Irish Sign Language').
	language('ish', '', individual, living, 'Esan').
	language('isi', '', individual, living, 'Nkem-Nkum').
	language('isk', '', individual, living, 'Ishkashimi').
	language('isl', 'is', individual, living, 'Icelandic').
	language('ism', '', individual, living, 'Masimasi').
	language('isn', '', individual, living, 'Isanzu').
	language('iso', '', individual, living, 'Isoko').
	language('isr', '', individual, living, 'Israeli Sign Language').
	language('ist', '', individual, living, 'Istriot').
	language('isu', '', individual, living, 'Isu').
	language('isv', '', individual, constructed, 'Interslavic').
	language('ita', 'it', individual, living, 'Italian').
	language('itb', '', individual, living, 'Binongan Itneg').
	language('itd', '', individual, living, 'Southern Tidung').
	language('ite', '', individual, extinct, 'Itene').
	language('iti', '', individual, living, 'Inlaod Itneg').
	language('itk', '', individual, living, 'Judeo-Italian').
	language('itl', '', individual, living, 'Itelmen').
	language('itm', '', individual, living, 'Itu Mbon Uzo').
	language('ito', '', individual, living, 'Itonama').
	language('itr', '', individual, living, 'Iteri').
	language('its', '', individual, living, 'Isekiri').
	language('itt', '', individual, living, 'Maeng Itneg').
	language('itv', '', individual, living, 'Itawit').
	language('itw', '', individual, living, 'Ito').
	language('itx', '', individual, living, 'Itik').
	language('ity', '', individual, living, 'Moyadan Itneg').
	language('itz', '', individual, living, 'Itzá').
	language('ium', '', individual, living, 'Iu Mien').
	language('ivb', '', individual, living, 'Ibatan').
	language('ivv', '', individual, living, 'Ivatan').
	language('iwk', '', individual, living, 'I-Wak').
	language('iwm', '', individual, living, 'Iwam').
	language('iwo', '', individual, living, 'Iwur').
	language('iws', '', individual, living, 'Sepik Iwam').
	language('ixc', '', individual, living, 'Ixcatec').
	language('ixl', '', individual, living, 'Ixil').
	language('iya', '', individual, living, 'Iyayu').
	language('iyo', '', individual, living, 'Mesaka').
	language('iyx', '', individual, living, 'Yaka (Congo)').
	language('izh', '', individual, living, 'Ingrian').
	language('izm', '', individual, living, 'Kizamani').
	language('izr', '', individual, living, 'Izere').
	language('izz', '', individual, living, 'Izii').
	language('jaa', '', individual, living, 'Jamamadí').
	language('jab', '', individual, living, 'Hyam').
	language('jac', '', individual, living, 'Popti''').
	language('jad', '', individual, living, 'Jahanka').
	language('jae', '', individual, living, 'Yabem').
	language('jaf', '', individual, living, 'Jara').
	language('jah', '', individual, living, 'Jah Hut').
	language('jaj', '', individual, living, 'Zazao').
	language('jak', '', individual, living, 'Jakun').
	language('jal', '', individual, living, 'Yalahatan').
	language('jam', '', individual, living, 'Jamaican Creole English').
	language('jan', '', individual, extinct, 'Jandai').
	language('jao', '', individual, living, 'Yanyuwa').
	language('jaq', '', individual, living, 'Yaqay').
	language('jas', '', individual, living, 'New Caledonian Javanese').
	language('jat', '', individual, living, 'Jakati').
	language('jau', '', individual, living, 'Yaur').
	language('jav', 'jv', individual, living, 'Javanese').
	language('jax', '', individual, living, 'Jambi Malay').
	language('jay', '', individual, living, 'Yan-nhangu').
	language('jaz', '', individual, living, 'Jawe').
	language('jbe', '', individual, living, 'Judeo-Berber').
	language('jbi', '', individual, extinct, 'Badjiri').
	language('jbj', '', individual, living, 'Arandai').
	language('jbk', '', individual, living, 'Barikewa').
	language('jbm', '', individual, living, 'Bijim').
	language('jbn', '', individual, living, 'Nafusi').
	language('jbo', '', individual, constructed, 'Lojban').
	language('jbr', '', individual, living, 'Jofotek-Bromnya').
	language('jbt', '', individual, living, 'Jabutí').
	language('jbu', '', individual, living, 'Jukun Takum').
	language('jbw', '', individual, extinct, 'Yawijibaya').
	language('jcs', '', individual, living, 'Jamaican Country Sign Language').
	language('jct', '', individual, living, 'Krymchak').
	language('jda', '', individual, living, 'Jad').
	language('jdg', '', individual, living, 'Jadgali').
	language('jdt', '', individual, living, 'Judeo-Tat').
	language('jeb', '', individual, living, 'Jebero').
	language('jee', '', individual, living, 'Jerung').
	language('jeh', '', individual, living, 'Jeh').
	language('jei', '', individual, living, 'Yei').
	language('jek', '', individual, living, 'Jeri Kuo').
	language('jel', '', individual, living, 'Yelmek').
	language('jen', '', individual, living, 'Dza').
	language('jer', '', individual, living, 'Jere').
	language('jet', '', individual, living, 'Manem').
	language('jeu', '', individual, living, 'Jonkor Bourmataguil').
	language('jgb', '', individual, extinct, 'Ngbee').
	language('jge', '', individual, living, 'Judeo-Georgian').
	language('jgk', '', individual, living, 'Gwak').
	language('jgo', '', individual, living, 'Ngomba').
	language('jhi', '', individual, living, 'Jehai').
	language('jhs', '', individual, living, 'Jhankot Sign Language').
	language('jia', '', individual, living, 'Jina').
	language('jib', '', individual, living, 'Jibu').
	language('jic', '', individual, living, 'Tol').
	language('jid', '', individual, living, 'Bu (Kaduna State)').
	language('jie', '', individual, living, 'Jilbe').
	language('jig', '', individual, living, 'Jingulu').
	language('jih', '', individual, living, 'sTodsde').
	language('jii', '', individual, living, 'Jiiddu').
	language('jil', '', individual, living, 'Jilim').
	language('jim', '', individual, living, 'Jimi (Cameroon)').
	language('jio', '', individual, living, 'Jiamao').
	language('jiq', '', individual, living, 'Guanyinqiao').
	language('jit', '', individual, living, 'Jita').
	language('jiu', '', individual, living, 'Youle Jinuo').
	language('jiv', '', individual, living, 'Shuar').
	language('jiy', '', individual, living, 'Buyuan Jinuo').
	language('jje', '', individual, living, 'Jejueo').
	language('jjr', '', individual, living, 'Bankal').
	language('jka', '', individual, living, 'Kaera').
	language('jkm', '', individual, living, 'Mobwa Karen').
	language('jko', '', individual, living, 'Kubo').
	language('jkp', '', individual, living, 'Paku Karen').
	language('jkr', '', individual, living, 'Koro (India)').
	language('jks', '', individual, living, 'Amami Koniya Sign Language').
	language('jku', '', individual, living, 'Labir').
	language('jle', '', individual, living, 'Ngile').
	language('jls', '', individual, living, 'Jamaican Sign Language').
	language('jma', '', individual, living, 'Dima').
	language('jmb', '', individual, living, 'Zumbun').
	language('jmc', '', individual, living, 'Machame').
	language('jmd', '', individual, living, 'Yamdena').
	language('jmi', '', individual, living, 'Jimi (Nigeria)').
	language('jml', '', individual, living, 'Jumli').
	language('jmn', '', individual, living, 'Makuri Naga').
	language('jmr', '', individual, living, 'Kamara').
	language('jms', '', individual, living, 'Mashi (Nigeria)').
	language('jmw', '', individual, living, 'Mouwase').
	language('jmx', '', individual, living, 'Western Juxtlahuaca Mixtec').
	language('jna', '', individual, living, 'Jangshung').
	language('jnd', '', individual, living, 'Jandavra').
	language('jng', '', individual, extinct, 'Yangman').
	language('jni', '', individual, living, 'Janji').
	language('jnj', '', individual, living, 'Yemsa').
	language('jnl', '', individual, living, 'Rawat').
	language('jns', '', individual, living, 'Jaunsari').
	language('job', '', individual, living, 'Joba').
	language('jod', '', individual, living, 'Wojenaka').
	language('jog', '', individual, living, 'Jogi').
	language('jor', '', individual, extinct, 'Jorá').
	language('jos', '', individual, living, 'Jordanian Sign Language').
	language('jow', '', individual, living, 'Jowulu').
	language('jpa', '', individual, special, 'Jewish Palestinian Aramaic').
	language('jpn', 'ja', individual, living, 'Japanese').
	language('jpr', '', individual, living, 'Judeo-Persian').
	language('jqr', '', individual, living, 'Jaqaru').
	language('jra', '', individual, living, 'Jarai').
	language('jrb', '', macrolanguage, living, 'Judeo-Arabic').
	language('jrr', '', individual, living, 'Jiru').
	language('jrt', '', individual, living, 'Jakattoe').
	language('jru', '', individual, living, 'Japrería').
	language('jsl', '', individual, living, 'Japanese Sign Language').
	language('jua', '', individual, living, 'Júma').
	language('jub', '', individual, living, 'Wannu').
	language('juc', '', individual, special, 'Jurchen').
	language('jud', '', individual, living, 'Worodougou').
	language('juh', '', individual, living, 'Hõne').
	language('jui', '', individual, extinct, 'Ngadjuri').
	language('juk', '', individual, living, 'Wapan').
	language('jul', '', individual, living, 'Jirel').
	language('jum', '', individual, living, 'Jumjum').
	language('jun', '', individual, living, 'Juang').
	language('juo', '', individual, living, 'Jiba').
	language('jup', '', individual, living, 'Hupdë').
	language('jur', '', individual, living, 'Jurúna').
	language('jus', '', individual, living, 'Jumla Sign Language').
	language('jut', '', individual, special, 'Jutish').
	language('juu', '', individual, living, 'Ju').
	language('juw', '', individual, living, 'Wãpha').
	language('juy', '', individual, living, 'Juray').
	language('jvd', '', individual, living, 'Javindo').
	language('jvn', '', individual, living, 'Caribbean Javanese').
	language('jwi', '', individual, living, 'Jwira-Pepesa').
	language('jya', '', individual, living, 'Jiarong').
	language('jye', '', individual, living, 'Judeo-Yemeni Arabic').
	language('jyy', '', individual, living, 'Jaya').
	language('kaa', '', individual, living, 'Kara-Kalpak').
	language('kab', '', individual, living, 'Kabyle').
	language('kac', '', individual, living, 'Kachin').
	language('kad', '', individual, living, 'Adara').
	language('kae', '', individual, extinct, 'Ketangalan').
	language('kaf', '', individual, living, 'Katso').
	language('kag', '', individual, living, 'Kajaman').
	language('kah', '', individual, living, 'Kara (Central African Republic)').
	language('kai', '', individual, living, 'Karekare').
	language('kaj', '', individual, living, 'Jju').
	language('kak', '', individual, living, 'Kalanguya').
	language('kal', 'kl', individual, living, 'Kalaallisut').
	language('kam', '', individual, living, 'Kamba (Kenya)').
	language('kan', 'kn', individual, living, 'Kannada').
	language('kao', '', individual, living, 'Xaasongaxango').
	language('kap', '', individual, living, 'Bezhta').
	language('kaq', '', individual, living, 'Capanahua').
	language('kas', 'ks', individual, living, 'Kashmiri').
	language('kat', 'ka', individual, living, 'Georgian').
	language('kau', 'kr', macrolanguage, living, 'Kanuri').
	language('kav', '', individual, living, 'Katukína').
	language('kaw', '', individual, special, 'Kawi').
	language('kax', '', individual, living, 'Kao').
	language('kay', '', individual, living, 'Kamayurá').
	language('kaz', 'kk', individual, living, 'Kazakh').
	language('kba', '', individual, extinct, 'Kalarko').
	language('kbb', '', individual, extinct, 'Kaxuiâna').
	language('kbc', '', individual, living, 'Kadiwéu').
	language('kbd', '', individual, living, 'Kabardian').
	language('kbe', '', individual, living, 'Kanju').
	language('kbg', '', individual, living, 'Khamba').
	language('kbh', '', individual, living, 'Camsá').
	language('kbi', '', individual, living, 'Kaptiau').
	language('kbj', '', individual, living, 'Kari').
	language('kbk', '', individual, living, 'Grass Koiari').
	language('kbl', '', individual, living, 'Kanembu').
	language('kbm', '', individual, living, 'Iwal').
	language('kbn', '', individual, living, 'Kare (Central African Republic)').
	language('kbo', '', individual, living, 'Keliko').
	language('kbp', '', individual, living, 'Kabiyè').
	language('kbq', '', individual, living, 'Kamano').
	language('kbr', '', individual, living, 'Kafa').
	language('kbs', '', individual, living, 'Kande').
	language('kbt', '', individual, living, 'Abadi').
	language('kbu', '', individual, living, 'Kabutra').
	language('kbv', '', individual, living, 'Dera (Indonesia)').
	language('kbw', '', individual, living, 'Kaiep').
	language('kbx', '', individual, living, 'Ap Ma').
	language('kby', '', individual, living, 'Manga Kanuri').
	language('kbz', '', individual, living, 'Duhwa').
	language('kca', '', individual, living, 'Khanty').
	language('kcb', '', individual, living, 'Kawacha').
	language('kcc', '', individual, living, 'Lubila').
	language('kcd', '', individual, living, 'Ngkâlmpw Kanum').
	language('kce', '', individual, living, 'Kaivi').
	language('kcf', '', individual, living, 'Ukaan').
	language('kcg', '', individual, living, 'Tyap').
	language('kch', '', individual, living, 'Vono').
	language('kci', '', individual, living, 'Ngyian').
	language('kcj', '', individual, living, 'Kobiana').
	language('kck', '', individual, living, 'Kalanga').
	language('kcl', '', individual, living, 'Kela (Papua New Guinea)').
	language('kcm', '', individual, living, 'Gula (Central African Republic)').
	language('kcn', '', individual, living, 'Nubi').
	language('kco', '', individual, living, 'Kinalakna').
	language('kcp', '', individual, living, 'Kanga').
	language('kcq', '', individual, living, 'Kamo').
	language('kcr', '', individual, living, 'Katla').
	language('kcs', '', individual, living, 'Koenoem').
	language('kct', '', individual, living, 'Kaian').
	language('kcu', '', individual, living, 'Kami (Tanzania)').
	language('kcv', '', individual, living, 'Kete').
	language('kcw', '', individual, living, 'Kabwari').
	language('kcx', '', individual, living, 'Kachama-Ganjule').
	language('kcy', '', individual, living, 'Korandje').
	language('kcz', '', individual, living, 'Konongo').
	language('kda', '', individual, extinct, 'Worimi').
	language('kdc', '', individual, living, 'Kutu').
	language('kdd', '', individual, living, 'Yankunytjatjara').
	language('kde', '', individual, living, 'Makonde').
	language('kdf', '', individual, living, 'Mamusi').
	language('kdg', '', individual, living, 'Seba').
	language('kdh', '', individual, living, 'Tem').
	language('kdi', '', individual, living, 'Kumam').
	language('kdj', '', individual, living, 'Karamojong').
	language('kdk', '', individual, living, 'Numèè').
	language('kdl', '', individual, living, 'Tsikimba').
	language('kdm', '', individual, living, 'Kagoma').
	language('kdn', '', individual, living, 'Kunda').
	language('kdp', '', individual, living, 'Kaningdon-Nindem').
	language('kdq', '', individual, living, 'Koch').
	language('kdr', '', individual, living, 'Karaim').
	language('kdt', '', individual, living, 'Kuy').
	language('kdu', '', individual, living, 'Kadaru').
	language('kdw', '', individual, living, 'Koneraw').
	language('kdx', '', individual, living, 'Kam').
	language('kdy', '', individual, living, 'Keder').
	language('kdz', '', individual, living, 'Kwaja').
	language('kea', '', individual, living, 'Kabuverdianu').
	language('keb', '', individual, living, 'Kélé').
	language('kec', '', individual, living, 'Keiga').
	language('ked', '', individual, living, 'Kerewe').
	language('kee', '', individual, living, 'Eastern Keres').
	language('kef', '', individual, living, 'Kpessi').
	language('keg', '', individual, living, 'Tese').
	language('keh', '', individual, living, 'Keak').
	language('kei', '', individual, living, 'Kei').
	language('kej', '', individual, living, 'Kadar').
	language('kek', '', individual, living, 'Kekchí').
	language('kel', '', individual, living, 'Kela (Democratic Republic of Congo)').
	language('kem', '', individual, living, 'Kemak').
	language('ken', '', individual, living, 'Kenyang').
	language('keo', '', individual, living, 'Kakwa').
	language('kep', '', individual, living, 'Kaikadi').
	language('keq', '', individual, living, 'Kamar').
	language('ker', '', individual, living, 'Kera').
	language('kes', '', individual, living, 'Kugbo').
	language('ket', '', individual, living, 'Ket').
	language('keu', '', individual, living, 'Akebu').
	language('kev', '', individual, living, 'Kanikkaran').
	language('kew', '', individual, living, 'West Kewa').
	language('kex', '', individual, living, 'Kukna').
	language('key', '', individual, living, 'Kupia').
	language('kez', '', individual, living, 'Kukele').
	language('kfa', '', individual, living, 'Kodava').
	language('kfb', '', individual, living, 'Northwestern Kolami').
	language('kfc', '', individual, living, 'Konda-Dora').
	language('kfd', '', individual, living, 'Korra Koraga').
	language('kfe', '', individual, living, 'Kota (India)').
	language('kff', '', individual, living, 'Koya').
	language('kfg', '', individual, living, 'Kudiya').
	language('kfh', '', individual, living, 'Kurichiya').
	language('kfi', '', individual, living, 'Kannada Kurumba').
	language('kfj', '', individual, living, 'Kemiehua').
	language('kfk', '', individual, living, 'Kinnauri').
	language('kfl', '', individual, living, 'Kung').
	language('kfm', '', individual, living, 'Khunsari').
	language('kfn', '', individual, living, 'Kuk').
	language('kfo', '', individual, living, 'Koro (Côte d''Ivoire)').
	language('kfp', '', individual, living, 'Korwa').
	language('kfq', '', individual, living, 'Korku').
	language('kfr', '', individual, living, 'Kachhi').
	language('kfs', '', individual, living, 'Bilaspuri').
	language('kft', '', individual, living, 'Kanjari').
	language('kfu', '', individual, living, 'Katkari').
	language('kfv', '', individual, living, 'Kurmukar').
	language('kfw', '', individual, living, 'Kharam Naga').
	language('kfx', '', individual, living, 'Kullu Pahari').
	language('kfy', '', individual, living, 'Kumaoni').
	language('kfz', '', individual, living, 'Koromfé').
	language('kga', '', individual, living, 'Koyaga').
	language('kgb', '', individual, living, 'Kawe').
	language('kge', '', individual, living, 'Komering').
	language('kgf', '', individual, living, 'Kube').
	language('kgg', '', individual, living, 'Kusunda').
	language('kgi', '', individual, living, 'Selangor Sign Language').
	language('kgj', '', individual, living, 'Gamale Kham').
	language('kgk', '', individual, living, 'Kaiwá').
	language('kgl', '', individual, extinct, 'Kunggari').
	language('kgn', '', individual, living, 'Karingani').
	language('kgo', '', individual, living, 'Krongo').
	language('kgp', '', individual, living, 'Kaingang').
	language('kgq', '', individual, living, 'Kamoro').
	language('kgr', '', individual, living, 'Abun').
	language('kgs', '', individual, living, 'Kumbainggar').
	language('kgt', '', individual, living, 'Somyev').
	language('kgu', '', individual, living, 'Kobol').
	language('kgv', '', individual, living, 'Karas').
	language('kgw', '', individual, living, 'Karon Dori').
	language('kgx', '', individual, living, 'Kamaru').
	language('kgy', '', individual, living, 'Kyerung').
	language('kha', '', individual, living, 'Khasi').
	language('khb', '', individual, living, 'Lü').
	language('khc', '', individual, living, 'Tukang Besi North').
	language('khd', '', individual, living, 'Bädi Kanum').
	language('khe', '', individual, living, 'Korowai').
	language('khf', '', individual, living, 'Khuen').
	language('khg', '', individual, living, 'Khams Tibetan').
	language('khh', '', individual, living, 'Kehu').
	language('khj', '', individual, living, 'Kuturmi').
	language('khk', '', individual, living, 'Halh Mongolian').
	language('khl', '', individual, living, 'Lusi').
	language('khm', 'km', individual, living, 'Khmer').
	language('khn', '', individual, living, 'Khandesi').
	language('kho', '', individual, special, 'Khotanese').
	language('khp', '', individual, living, 'Kapori').
	language('khq', '', individual, living, 'Koyra Chiini Songhay').
	language('khr', '', individual, living, 'Kharia').
	language('khs', '', individual, living, 'Kasua').
	language('kht', '', individual, living, 'Khamti').
	language('khu', '', individual, living, 'Nkhumbi').
	language('khv', '', individual, living, 'Khvarshi').
	language('khw', '', individual, living, 'Khowar').
	language('khx', '', individual, living, 'Kanu').
	language('khy', '', individual, living, 'Kele (Democratic Republic of Congo)').
	language('khz', '', individual, living, 'Keapara').
	language('kia', '', individual, living, 'Kim').
	language('kib', '', individual, living, 'Koalib').
	language('kic', '', individual, living, 'Kickapoo').
	language('kid', '', individual, living, 'Koshin').
	language('kie', '', individual, living, 'Kibet').
	language('kif', '', individual, living, 'Eastern Parbate Kham').
	language('kig', '', individual, living, 'Kimaama').
	language('kih', '', individual, living, 'Kilmeri').
	language('kii', '', individual, extinct, 'Kitsai').
	language('kij', '', individual, living, 'Kilivila').
	language('kik', 'ki', individual, living, 'Kikuyu').
	language('kil', '', individual, living, 'Kariya').
	language('kim', '', individual, living, 'Karagas').
	language('kin', 'rw', individual, living, 'Kinyarwanda').
	language('kio', '', individual, living, 'Kiowa').
	language('kip', '', individual, living, 'Sheshi Kham').
	language('kiq', '', individual, living, 'Kosadle').
	language('kir', 'ky', individual, living, 'Kirghiz').
	language('kis', '', individual, living, 'Kis').
	language('kit', '', individual, living, 'Agob').
	language('kiu', '', individual, living, 'Kirmanjki (individual language)').
	language('kiv', '', individual, living, 'Kimbu').
	language('kiw', '', individual, living, 'Northeast Kiwai').
	language('kix', '', individual, living, 'Khiamniungan Naga').
	language('kiy', '', individual, living, 'Kirikiri').
	language('kiz', '', individual, living, 'Kisi').
	language('kja', '', individual, living, 'Mlap').
	language('kjb', '', individual, living, 'Q''anjob''al').
	language('kjc', '', individual, living, 'Coastal Konjo').
	language('kjd', '', individual, living, 'Southern Kiwai').
	language('kje', '', individual, living, 'Kisar').
	language('kjg', '', individual, living, 'Khmu').
	language('kjh', '', individual, living, 'Khakas').
	language('kji', '', individual, living, 'Zabana').
	language('kjj', '', individual, living, 'Khinalugh').
	language('kjk', '', individual, living, 'Highland Konjo').
	language('kjl', '', individual, living, 'Western Parbate Kham').
	language('kjm', '', individual, living, 'Kháng').
	language('kjn', '', individual, living, 'Kunjen').
	language('kjo', '', individual, living, 'Kinnauri Pahari').
	language('kjp', '', individual, living, 'Pwo Eastern Karen').
	language('kjq', '', individual, living, 'Western Keres').
	language('kjr', '', individual, living, 'Kurudu').
	language('kjs', '', individual, living, 'East Kewa').
	language('kjt', '', individual, living, 'Phrae Pwo Karen').
	language('kju', '', individual, living, 'Kashaya').
	language('kjv', '', individual, special, 'Kaikavian Literary Language').
	language('kjx', '', individual, living, 'Ramopa').
	language('kjy', '', individual, living, 'Erave').
	language('kjz', '', individual, living, 'Bumthangkha').
	language('kka', '', individual, living, 'Kakanda').
	language('kkb', '', individual, living, 'Kwerisa').
	language('kkc', '', individual, living, 'Odoodee').
	language('kkd', '', individual, living, 'Kinuku').
	language('kke', '', individual, living, 'Kakabe').
	language('kkf', '', individual, living, 'Kalaktang Monpa').
	language('kkg', '', individual, living, 'Mabaka Valley Kalinga').
	language('kkh', '', individual, living, 'Khün').
	language('kki', '', individual, living, 'Kagulu').
	language('kkj', '', individual, living, 'Kako').
	language('kkk', '', individual, living, 'Kokota').
	language('kkl', '', individual, living, 'Kosarek Yale').
	language('kkm', '', individual, living, 'Kiong').
	language('kkn', '', individual, living, 'Kon Keu').
	language('kko', '', individual, living, 'Karko').
	language('kkp', '', individual, living, 'Gugubera').
	language('kkq', '', individual, living, 'Kaeku').
	language('kkr', '', individual, living, 'Kir-Balar').
	language('kks', '', individual, living, 'Giiwo').
	language('kkt', '', individual, living, 'Koi').
	language('kku', '', individual, living, 'Tumi').
	language('kkv', '', individual, living, 'Kangean').
	language('kkw', '', individual, living, 'Teke-Kukuya').
	language('kkx', '', individual, living, 'Kohin').
	language('kky', '', individual, living, 'Guugu Yimidhirr').
	language('kkz', '', individual, living, 'Kaska').
	language('kla', '', individual, extinct, 'Klamath-Modoc').
	language('klb', '', individual, living, 'Kiliwa').
	language('klc', '', individual, living, 'Kolbila').
	language('kld', '', individual, living, 'Gamilaraay').
	language('kle', '', individual, living, 'Kulung (Nepal)').
	language('klf', '', individual, living, 'Kendeje').
	language('klg', '', individual, living, 'Tagakaulo').
	language('klh', '', individual, living, 'Weliki').
	language('kli', '', individual, living, 'Kalumpang').
	language('klj', '', individual, living, 'Khalaj').
	language('klk', '', individual, living, 'Kono (Nigeria)').
	language('kll', '', individual, living, 'Kagan Kalagan').
	language('klm', '', individual, living, 'Migum').
	language('kln', '', macrolanguage, living, 'Kalenjin').
	language('klo', '', individual, living, 'Kapya').
	language('klp', '', individual, living, 'Kamasa').
	language('klq', '', individual, living, 'Rumu').
	language('klr', '', individual, living, 'Khaling').
	language('kls', '', individual, living, 'Kalasha').
	language('klt', '', individual, living, 'Nukna').
	language('klu', '', individual, living, 'Klao').
	language('klv', '', individual, living, 'Maskelynes').
	language('klw', '', individual, living, 'Tado').
	language('klx', '', individual, living, 'Koluwawa').
	language('kly', '', individual, living, 'Kalao').
	language('klz', '', individual, living, 'Kabola').
	language('kma', '', individual, living, 'Konni').
	language('kmb', '', individual, living, 'Kimbundu').
	language('kmc', '', individual, living, 'Southern Dong').
	language('kmd', '', individual, living, 'Majukayang Kalinga').
	language('kme', '', individual, living, 'Bakole').
	language('kmf', '', individual, living, 'Kare (Papua New Guinea)').
	language('kmg', '', individual, living, 'Kâte').
	language('kmh', '', individual, living, 'Kalam').
	language('kmi', '', individual, living, 'Kami (Nigeria)').
	language('kmj', '', individual, living, 'Kumarbhag Paharia').
	language('kmk', '', individual, living, 'Limos Kalinga').
	language('kml', '', individual, living, 'Tanudan Kalinga').
	language('kmm', '', individual, living, 'Kom (India)').
	language('kmn', '', individual, living, 'Awtuw').
	language('kmo', '', individual, living, 'Kwoma').
	language('kmp', '', individual, living, 'Gimme').
	language('kmq', '', individual, living, 'Kwama').
	language('kmr', '', individual, living, 'Northern Kurdish').
	language('kms', '', individual, living, 'Kamasau').
	language('kmt', '', individual, living, 'Kemtuik').
	language('kmu', '', individual, living, 'Kanite').
	language('kmv', '', individual, living, 'Karipúna Creole French').
	language('kmw', '', individual, living, 'Komo (Democratic Republic of Congo)').
	language('kmx', '', individual, living, 'Waboda').
	language('kmy', '', individual, living, 'Koma').
	language('kmz', '', individual, living, 'Khorasani Turkish').
	language('kna', '', individual, living, 'Dera (Nigeria)').
	language('knb', '', individual, living, 'Lubuagan Kalinga').
	language('knc', '', individual, living, 'Central Kanuri').
	language('knd', '', individual, living, 'Konda').
	language('kne', '', individual, living, 'Kankanaey').
	language('knf', '', individual, living, 'Mankanya').
	language('kng', '', individual, living, 'Koongo').
	language('kni', '', individual, living, 'Kanufi').
	language('knj', '', individual, living, 'Western Kanjobal').
	language('knk', '', individual, living, 'Kuranko').
	language('knl', '', individual, living, 'Keninjal').
	language('knm', '', individual, living, 'Kanamarí').
	language('knn', '', individual, living, 'Konkani (individual language)').
	language('kno', '', individual, living, 'Kono (Sierra Leone)').
	language('knp', '', individual, living, 'Kwanja').
	language('knq', '', individual, living, 'Kintaq').
	language('knr', '', individual, living, 'Kaningra').
	language('kns', '', individual, living, 'Kensiu').
	language('knt', '', individual, living, 'Panoan Katukína').
	language('knu', '', individual, living, 'Kono (Guinea)').
	language('knv', '', individual, living, 'Tabo').
	language('knw', '', individual, living, 'Kung-Ekoka').
	language('knx', '', individual, living, 'Kendayan').
	language('kny', '', individual, living, 'Kanyok').
	language('knz', '', individual, living, 'Kalamsé').
	language('koa', '', individual, living, 'Konomala').
	language('koc', '', individual, extinct, 'Kpati').
	language('kod', '', individual, living, 'Kodi').
	language('koe', '', individual, living, 'Kacipo-Bale Suri').
	language('kof', '', individual, extinct, 'Kubi').
	language('kog', '', individual, living, 'Cogui').
	language('koh', '', individual, living, 'Koyo').
	language('koi', '', individual, living, 'Komi-Permyak').
	language('kok', '', macrolanguage, living, 'Konkani (macrolanguage)').
	language('kol', '', individual, living, 'Kol (Papua New Guinea)').
	language('kom', 'kv', macrolanguage, living, 'Komi').
	language('kon', 'kg', macrolanguage, living, 'Kongo').
	language('koo', '', individual, living, 'Konzo').
	language('kop', '', individual, living, 'Waube').
	language('koq', '', individual, living, 'Kota (Gabon)').
	language('kor', 'ko', individual, living, 'Korean').
	language('kos', '', individual, living, 'Kosraean').
	language('kot', '', individual, living, 'Lagwan').
	language('kou', '', individual, living, 'Koke').
	language('kov', '', individual, living, 'Kudu-Camo').
	language('kow', '', individual, living, 'Kugama').
	language('koy', '', individual, living, 'Koyukon').
	language('koz', '', individual, living, 'Korak').
	language('kpa', '', individual, living, 'Kutto').
	language('kpb', '', individual, living, 'Mullu Kurumba').
	language('kpc', '', individual, living, 'Curripaco').
	language('kpd', '', individual, living, 'Koba').
	language('kpe', '', macrolanguage, living, 'Kpelle').
	language('kpf', '', individual, living, 'Komba').
	language('kpg', '', individual, living, 'Kapingamarangi').
	language('kph', '', individual, living, 'Kplang').
	language('kpi', '', individual, living, 'Kofei').
	language('kpj', '', individual, living, 'Karajá').
	language('kpk', '', individual, living, 'Kpan').
	language('kpl', '', individual, living, 'Kpala').
	language('kpm', '', individual, living, 'Koho').
	language('kpn', '', individual, extinct, 'Kepkiriwát').
	language('kpo', '', individual, living, 'Ikposo').
	language('kpq', '', individual, living, 'Korupun-Sela').
	language('kpr', '', individual, living, 'Korafe-Yegha').
	language('kps', '', individual, living, 'Tehit').
	language('kpt', '', individual, living, 'Karata').
	language('kpu', '', individual, living, 'Kafoa').
	language('kpv', '', individual, living, 'Komi-Zyrian').
	language('kpw', '', individual, living, 'Kobon').
	language('kpx', '', individual, living, 'Mountain Koiali').
	language('kpy', '', individual, living, 'Koryak').
	language('kpz', '', individual, living, 'Kupsabiny').
	language('kqa', '', individual, living, 'Mum').
	language('kqb', '', individual, living, 'Kovai').
	language('kqc', '', individual, living, 'Doromu-Koki').
	language('kqd', '', individual, living, 'Koy Sanjaq Surat').
	language('kqe', '', individual, living, 'Kalagan').
	language('kqf', '', individual, living, 'Kakabai').
	language('kqg', '', individual, living, 'Khe').
	language('kqh', '', individual, living, 'Kisankasa').
	language('kqi', '', individual, living, 'Koitabu').
	language('kqj', '', individual, living, 'Koromira').
	language('kqk', '', individual, living, 'Kotafon Gbe').
	language('kql', '', individual, living, 'Kyenele').
	language('kqm', '', individual, living, 'Khisa').
	language('kqn', '', individual, living, 'Kaonde').
	language('kqo', '', individual, living, 'Eastern Krahn').
	language('kqp', '', individual, living, 'Kimré').
	language('kqq', '', individual, living, 'Krenak').
	language('kqr', '', individual, living, 'Kimaragang').
	language('kqs', '', individual, living, 'Northern Kissi').
	language('kqt', '', individual, living, 'Klias River Kadazan').
	language('kqu', '', individual, extinct, 'Seroa').
	language('kqv', '', individual, living, 'Okolod').
	language('kqw', '', individual, living, 'Kandas').
	language('kqx', '', individual, living, 'Mser').
	language('kqy', '', individual, living, 'Koorete').
	language('kqz', '', individual, extinct, 'Korana').
	language('kra', '', individual, living, 'Kumhali').
	language('krb', '', individual, extinct, 'Karkin').
	language('krc', '', individual, living, 'Karachay-Balkar').
	language('krd', '', individual, living, 'Kairui-Midiki').
	language('kre', '', individual, living, 'Panará').
	language('krf', '', individual, living, 'Koro (Vanuatu)').
	language('krh', '', individual, living, 'Kurama').
	language('kri', '', individual, living, 'Krio').
	language('krj', '', individual, living, 'Kinaray-A').
	language('krk', '', individual, extinct, 'Kerek').
	language('krl', '', individual, living, 'Karelian').
	language('krn', '', individual, living, 'Sapo').
	language('krp', '', individual, living, 'Durop').
	language('krr', '', individual, living, 'Krung').
	language('krs', '', individual, living, 'Gbaya (Sudan)').
	language('krt', '', individual, living, 'Tumari Kanuri').
	language('kru', '', individual, living, 'Kurukh').
	language('krv', '', individual, living, 'Kavet').
	language('krw', '', individual, living, 'Western Krahn').
	language('krx', '', individual, living, 'Karon').
	language('kry', '', individual, living, 'Kryts').
	language('krz', '', individual, living, 'Sota Kanum').
	language('ksb', '', individual, living, 'Shambala').
	language('ksc', '', individual, living, 'Southern Kalinga').
	language('ksd', '', individual, living, 'Kuanua').
	language('kse', '', individual, living, 'Kuni').
	language('ksf', '', individual, living, 'Bafia').
	language('ksg', '', individual, living, 'Kusaghe').
	language('ksh', '', individual, living, 'Kölsch').
	language('ksi', '', individual, living, 'Krisa').
	language('ksj', '', individual, living, 'Uare').
	language('ksk', '', individual, living, 'Kansa').
	language('ksl', '', individual, living, 'Kumalu').
	language('ksm', '', individual, living, 'Kumba').
	language('ksn', '', individual, living, 'Kasiguranin').
	language('kso', '', individual, living, 'Kofa').
	language('ksp', '', individual, living, 'Kaba').
	language('ksq', '', individual, living, 'Kwaami').
	language('ksr', '', individual, living, 'Borong').
	language('kss', '', individual, living, 'Southern Kisi').
	language('kst', '', individual, living, 'Winyé').
	language('ksu', '', individual, living, 'Khamyang').
	language('ksv', '', individual, living, 'Kusu').
	language('ksw', '', individual, living, 'S''gaw Karen').
	language('ksx', '', individual, living, 'Kedang').
	language('ksy', '', individual, living, 'Kharia Thar').
	language('ksz', '', individual, living, 'Kodaku').
	language('kta', '', individual, living, 'Katua').
	language('ktb', '', individual, living, 'Kambaata').
	language('ktc', '', individual, living, 'Kholok').
	language('ktd', '', individual, living, 'Kokata').
	language('kte', '', individual, living, 'Nubri').
	language('ktf', '', individual, living, 'Kwami').
	language('ktg', '', individual, extinct, 'Kalkutung').
	language('kth', '', individual, living, 'Karanga').
	language('kti', '', individual, living, 'North Muyu').
	language('ktj', '', individual, living, 'Plapo Krumen').
	language('ktk', '', individual, extinct, 'Kaniet').
	language('ktl', '', individual, living, 'Koroshi').
	language('ktm', '', individual, living, 'Kurti').
	language('ktn', '', individual, living, 'Karitiâna').
	language('kto', '', individual, living, 'Kuot').
	language('ktp', '', individual, living, 'Kaduo').
	language('ktq', '', individual, extinct, 'Katabaga').
	language('kts', '', individual, living, 'South Muyu').
	language('ktt', '', individual, living, 'Ketum').
	language('ktu', '', individual, living, 'Kituba (Democratic Republic of Congo)').
	language('ktv', '', individual, living, 'Eastern Katu').
	language('ktw', '', individual, extinct, 'Kato').
	language('ktx', '', individual, living, 'Kaxararí').
	language('kty', '', individual, living, 'Kango (Bas-Uélé District)').
	language('ktz', '', individual, living, 'Juǀʼhoan').
	language('kua', 'kj', individual, living, 'Kuanyama').
	language('kub', '', individual, living, 'Kutep').
	language('kuc', '', individual, living, 'Kwinsu').
	language('kud', '', individual, living, '''Auhelawa').
	language('kue', '', individual, living, 'Kuman (Papua New Guinea)').
	language('kuf', '', individual, living, 'Western Katu').
	language('kug', '', individual, living, 'Kupa').
	language('kuh', '', individual, living, 'Kushi').
	language('kui', '', individual, living, 'Kuikúro-Kalapálo').
	language('kuj', '', individual, living, 'Kuria').
	language('kuk', '', individual, living, 'Kepo''').
	language('kul', '', individual, living, 'Kulere').
	language('kum', '', individual, living, 'Kumyk').
	language('kun', '', individual, living, 'Kunama').
	language('kuo', '', individual, living, 'Kumukio').
	language('kup', '', individual, living, 'Kunimaipa').
	language('kuq', '', individual, living, 'Karipuna').
	language('kur', 'ku', macrolanguage, living, 'Kurdish').
	language('kus', '', individual, living, 'Kusaal').
	language('kut', '', individual, living, 'Ktunaxa').
	language('kuu', '', individual, living, 'Upper Kuskokwim').
	language('kuv', '', individual, living, 'Kur').
	language('kuw', '', individual, living, 'Kpagua').
	language('kux', '', individual, living, 'Kukatja').
	language('kuy', '', individual, living, 'Kuuku-Ya''u').
	language('kuz', '', individual, extinct, 'Kunza').
	language('kva', '', individual, living, 'Bagvalal').
	language('kvb', '', individual, living, 'Kubu').
	language('kvc', '', individual, living, 'Kove').
	language('kvd', '', individual, living, 'Kui (Indonesia)').
	language('kve', '', individual, living, 'Kalabakan').
	language('kvf', '', individual, living, 'Kabalai').
	language('kvg', '', individual, living, 'Kuni-Boazi').
	language('kvh', '', individual, living, 'Komodo').
	language('kvi', '', individual, living, 'Kwang').
	language('kvj', '', individual, living, 'Psikye').
	language('kvk', '', individual, living, 'Korean Sign Language').
	language('kvl', '', individual, living, 'Kayaw').
	language('kvm', '', individual, living, 'Kendem').
	language('kvn', '', individual, living, 'Border Kuna').
	language('kvo', '', individual, living, 'Dobel').
	language('kvp', '', individual, living, 'Kompane').
	language('kvq', '', individual, living, 'Geba Karen').
	language('kvr', '', individual, living, 'Kerinci').
	language('kvt', '', individual, living, 'Lahta Karen').
	language('kvu', '', individual, living, 'Yinbaw Karen').
	language('kvv', '', individual, living, 'Kola').
	language('kvw', '', individual, living, 'Wersing').
	language('kvx', '', individual, living, 'Parkari Koli').
	language('kvy', '', individual, living, 'Yintale Karen').
	language('kvz', '', individual, living, 'Tsakwambo').
	language('kwa', '', individual, living, 'Dâw').
	language('kwb', '', individual, living, 'Kwa').
	language('kwc', '', individual, living, 'Likwala').
	language('kwd', '', individual, living, 'Kwaio').
	language('kwe', '', individual, living, 'Kwerba').
	language('kwf', '', individual, living, 'Kwara''ae').
	language('kwg', '', individual, living, 'Sara Kaba Deme').
	language('kwh', '', individual, living, 'Kowiai').
	language('kwi', '', individual, living, 'Awa-Cuaiquer').
	language('kwj', '', individual, living, 'Kwanga').
	language('kwk', '', individual, living, 'Kwak''wala').
	language('kwl', '', individual, living, 'Kofyar').
	language('kwm', '', individual, living, 'Kwambi').
	language('kwn', '', individual, living, 'Kwangali').
	language('kwo', '', individual, living, 'Kwomtari').
	language('kwp', '', individual, living, 'Kodia').
	language('kwr', '', individual, living, 'Kwer').
	language('kws', '', individual, living, 'Kwese').
	language('kwt', '', individual, living, 'Kwesten').
	language('kwu', '', individual, living, 'Kwakum').
	language('kwv', '', individual, living, 'Sara Kaba Náà').
	language('kww', '', individual, living, 'Kwinti').
	language('kwx', '', individual, living, 'Khirwar').
	language('kwy', '', individual, living, 'San Salvador Kongo').
	language('kwz', '', individual, extinct, 'Kwadi').
	language('kxa', '', individual, living, 'Kairiru').
	language('kxb', '', individual, living, 'Krobu').
	language('kxc', '', individual, living, 'Konso').
	language('kxd', '', individual, living, 'Brunei').
	language('kxf', '', individual, living, 'Manumanaw Karen').
	language('kxh', '', individual, living, 'Karo (Ethiopia)').
	language('kxi', '', individual, living, 'Keningau Murut').
	language('kxj', '', individual, living, 'Kulfa').
	language('kxk', '', individual, living, 'Zayein Karen').
	language('kxm', '', individual, living, 'Northern Khmer').
	language('kxn', '', individual, living, 'Kanowit-Tanjong Melanau').
	language('kxo', '', individual, extinct, 'Kanoé').
	language('kxp', '', individual, living, 'Wadiyara Koli').
	language('kxq', '', individual, living, 'Smärky Kanum').
	language('kxr', '', individual, living, 'Koro (Papua New Guinea)').
	language('kxs', '', individual, living, 'Kangjia').
	language('kxt', '', individual, living, 'Koiwat').
	language('kxv', '', individual, living, 'Kuvi').
	language('kxw', '', individual, living, 'Konai').
	language('kxx', '', individual, living, 'Likuba').
	language('kxy', '', individual, living, 'Kayong').
	language('kxz', '', individual, living, 'Kerewo').
	language('kya', '', individual, living, 'Kwaya').
	language('kyb', '', individual, living, 'Butbut Kalinga').
	language('kyc', '', individual, living, 'Kyaka').
	language('kyd', '', individual, living, 'Karey').
	language('kye', '', individual, living, 'Krache').
	language('kyf', '', individual, living, 'Kouya').
	language('kyg', '', individual, living, 'Keyagana').
	language('kyh', '', individual, living, 'Karok').
	language('kyi', '', individual, living, 'Kiput').
	language('kyj', '', individual, living, 'Karao').
	language('kyk', '', individual, living, 'Kamayo').
	language('kyl', '', individual, living, 'Kalapuya').
	language('kym', '', individual, living, 'Kpatili').
	language('kyn', '', individual, living, 'Northern Binukidnon').
	language('kyo', '', individual, living, 'Kelon').
	language('kyp', '', individual, living, 'Kang').
	language('kyq', '', individual, living, 'Kenga').
	language('kyr', '', individual, living, 'Kuruáya').
	language('kys', '', individual, living, 'Baram Kayan').
	language('kyt', '', individual, living, 'Kayagar').
	language('kyu', '', individual, living, 'Western Kayah').
	language('kyv', '', individual, living, 'Kayort').
	language('kyw', '', individual, living, 'Kudmali').
	language('kyx', '', individual, living, 'Rapoisi').
	language('kyy', '', individual, living, 'Kambaira').
	language('kyz', '', individual, living, 'Kayabí').
	language('kza', '', individual, living, 'Western Karaboro').
	language('kzb', '', individual, living, 'Kaibobo').
	language('kzc', '', individual, living, 'Bondoukou Kulango').
	language('kzd', '', individual, living, 'Kadai').
	language('kze', '', individual, living, 'Kosena').
	language('kzf', '', individual, living, 'Da''a Kaili').
	language('kzg', '', individual, living, 'Kikai').
	language('kzi', '', individual, living, 'Kelabit').
	language('kzk', '', individual, extinct, 'Kazukuru').
	language('kzl', '', individual, living, 'Kayeli').
	language('kzm', '', individual, living, 'Kais').
	language('kzn', '', individual, living, 'Kokola').
	language('kzo', '', individual, living, 'Kaningi').
	language('kzp', '', individual, living, 'Kaidipang').
	language('kzq', '', individual, living, 'Kaike').
	language('kzr', '', individual, living, 'Karang').
	language('kzs', '', individual, living, 'Sugut Dusun').
	language('kzu', '', individual, living, 'Kayupulau').
	language('kzv', '', individual, living, 'Komyandaret').
	language('kzw', '', individual, extinct, 'Karirí-Xocó').
	language('kzx', '', individual, extinct, 'Kamarian').
	language('kzy', '', individual, living, 'Kango (Tshopo District)').
	language('kzz', '', individual, living, 'Kalabra').
	language('laa', '', individual, living, 'Southern Subanen').
	language('lab', '', individual, special, 'Linear A').
	language('lac', '', individual, living, 'Lacandon').
	language('lad', '', individual, living, 'Ladino').
	language('lae', '', individual, living, 'Pattani').
	language('laf', '', individual, living, 'Lafofa').
	language('lag', '', individual, living, 'Rangi').
	language('lah', '', macrolanguage, living, 'Lahnda').
	language('lai', '', individual, living, 'Lambya').
	language('laj', '', individual, living, 'Lango (Uganda)').
	language('lal', '', individual, living, 'Lalia').
	language('lam', '', individual, living, 'Lamba').
	language('lan', '', individual, living, 'Laru').
	language('lao', 'lo', individual, living, 'Lao').
	language('lap', '', individual, living, 'Laka (Chad)').
	language('laq', '', individual, living, 'Qabiao').
	language('lar', '', individual, living, 'Larteh').
	language('las', '', individual, living, 'Lama (Togo)').
	language('lat', 'la', individual, special, 'Latin').
	language('lau', '', individual, living, 'Laba').
	language('lav', 'lv', macrolanguage, living, 'Latvian').
	language('law', '', individual, living, 'Lauje').
	language('lax', '', individual, living, 'Tiwa').
	language('lay', '', individual, living, 'Lama Bai').
	language('laz', '', individual, extinct, 'Aribwatsa').
	language('lbb', '', individual, living, 'Label').
	language('lbc', '', individual, living, 'Lakkia').
	language('lbe', '', individual, living, 'Lak').
	language('lbf', '', individual, living, 'Tinani').
	language('lbg', '', individual, living, 'Laopang').
	language('lbi', '', individual, living, 'La''bi').
	language('lbj', '', individual, living, 'Ladakhi').
	language('lbk', '', individual, living, 'Central Bontok').
	language('lbl', '', individual, living, 'Libon Bikol').
	language('lbm', '', individual, living, 'Lodhi').
	language('lbn', '', individual, living, 'Rmeet').
	language('lbo', '', individual, living, 'Laven').
	language('lbq', '', individual, living, 'Wampar').
	language('lbr', '', individual, living, 'Lohorung').
	language('lbs', '', individual, living, 'Libyan Sign Language').
	language('lbt', '', individual, living, 'Lachi').
	language('lbu', '', individual, living, 'Labu').
	language('lbv', '', individual, living, 'Lavatbura-Lamusong').
	language('lbw', '', individual, living, 'Tolaki').
	language('lbx', '', individual, living, 'Lawangan').
	language('lby', '', individual, extinct, 'Lamalama').
	language('lbz', '', individual, living, 'Lardil').
	language('lcc', '', individual, living, 'Legenyem').
	language('lcd', '', individual, living, 'Lola').
	language('lce', '', individual, living, 'Loncong').
	language('lcf', '', individual, living, 'Lubu').
	language('lch', '', individual, living, 'Luchazi').
	language('lcl', '', individual, living, 'Lisela').
	language('lcm', '', individual, living, 'Tungag').
	language('lcp', '', individual, living, 'Western Lawa').
	language('lcq', '', individual, living, 'Luhu').
	language('lcs', '', individual, living, 'Lisabata-Nuniali').
	language('lda', '', individual, living, 'Kla-Dan').
	language('ldb', '', individual, living, 'Dũya').
	language('ldd', '', individual, living, 'Luri').
	language('ldg', '', individual, living, 'Lenyima').
	language('ldh', '', individual, living, 'Lamja-Dengsa-Tola').
	language('ldi', '', individual, living, 'Laari').
	language('ldj', '', individual, living, 'Lemoro').
	language('ldk', '', individual, living, 'Leelau').
	language('ldl', '', individual, living, 'Kaan').
	language('ldm', '', individual, living, 'Landoma').
	language('ldn', '', individual, constructed, 'Láadan').
	language('ldo', '', individual, living, 'Loo').
	language('ldp', '', individual, living, 'Tso').
	language('ldq', '', individual, living, 'Lufu').
	language('lea', '', individual, living, 'Lega-Shabunda').
	language('leb', '', individual, living, 'Lala-Bisa').
	language('lec', '', individual, living, 'Leco').
	language('led', '', individual, living, 'Lendu').
	language('lee', '', individual, living, 'Lyélé').
	language('lef', '', individual, living, 'Lelemi').
	language('leh', '', individual, living, 'Lenje').
	language('lei', '', individual, living, 'Lemio').
	language('lej', '', individual, living, 'Lengola').
	language('lek', '', individual, living, 'Leipon').
	language('lel', '', individual, living, 'Lele (Democratic Republic of Congo)').
	language('lem', '', individual, living, 'Nomaande').
	language('len', '', individual, extinct, 'Lenca').
	language('leo', '', individual, living, 'Leti (Cameroon)').
	language('lep', '', individual, living, 'Lepcha').
	language('leq', '', individual, living, 'Lembena').
	language('ler', '', individual, living, 'Lenkau').
	language('les', '', individual, living, 'Lese').
	language('let', '', individual, living, 'Lesing-Gelimi').
	language('leu', '', individual, living, 'Kara (Papua New Guinea)').
	language('lev', '', individual, living, 'Lamma').
	language('lew', '', individual, living, 'Ledo Kaili').
	language('lex', '', individual, living, 'Luang').
	language('ley', '', individual, living, 'Lemolang').
	language('lez', '', individual, living, 'Lezghian').
	language('lfa', '', individual, living, 'Lefa').
	language('lfb', '', individual, living, 'Buu (Cameroon)').
	language('lfn', '', individual, constructed, 'Lingua Franca Nova').
	language('lga', '', individual, living, 'Lungga').
	language('lgb', '', individual, living, 'Laghu').
	language('lgg', '', individual, living, 'Lugbara').
	language('lgh', '', individual, living, 'Laghuu').
	language('lgi', '', individual, living, 'Lengilu').
	language('lgk', '', individual, living, 'Lingarak').
	language('lgl', '', individual, living, 'Wala').
	language('lgm', '', individual, living, 'Lega-Mwenga').
	language('lgn', '', individual, living, 'T''apo').
	language('lgo', '', individual, living, 'Lango (South Sudan)').
	language('lgq', '', individual, living, 'Logba').
	language('lgr', '', individual, living, 'Lengo').
	language('lgs', '', individual, living, 'Guinea-Bissau Sign Language').
	language('lgt', '', individual, living, 'Pahi').
	language('lgu', '', individual, living, 'Longgu').
	language('lgz', '', individual, living, 'Ligenza').
	language('lha', '', individual, living, 'Laha (Viet Nam)').
	language('lhh', '', individual, living, 'Laha (Indonesia)').
	language('lhi', '', individual, living, 'Lahu Shi').
	language('lhl', '', individual, living, 'Lahul Lohar').
	language('lhm', '', individual, living, 'Lhomi').
	language('lhn', '', individual, living, 'Lahanan').
	language('lhp', '', individual, living, 'Lhokpu').
	language('lhs', '', individual, extinct, 'Mlahsö').
	language('lht', '', individual, living, 'Lo-Toga').
	language('lhu', '', individual, living, 'Lahu').
	language('lia', '', individual, living, 'West-Central Limba').
	language('lib', '', individual, living, 'Likum').
	language('lic', '', individual, living, 'Hlai').
	language('lid', '', individual, living, 'Nyindrou').
	language('lie', '', individual, living, 'Likila').
	language('lif', '', individual, living, 'Limbu').
	language('lig', '', individual, living, 'Ligbi').
	language('lih', '', individual, living, 'Lihir').
	language('lij', '', individual, living, 'Ligurian').
	language('lik', '', individual, living, 'Lika').
	language('lil', '', individual, living, 'Lillooet').
	language('lim', 'li', individual, living, 'Limburgan').
	language('lin', 'ln', individual, living, 'Lingala').
	language('lio', '', individual, living, 'Liki').
	language('lip', '', individual, living, 'Sekpele').
	language('liq', '', individual, living, 'Libido').
	language('lir', '', individual, living, 'Liberian English').
	language('lis', '', individual, living, 'Lisu').
	language('lit', 'lt', individual, living, 'Lithuanian').
	language('liu', '', individual, living, 'Logorik').
	language('liv', '', individual, living, 'Liv').
	language('liw', '', individual, living, 'Col').
	language('lix', '', individual, living, 'Liabuku').
	language('liy', '', individual, living, 'Banda-Bambari').
	language('liz', '', individual, living, 'Libinza').
	language('lja', '', individual, extinct, 'Golpa').
	language('lje', '', individual, living, 'Rampi').
	language('lji', '', individual, living, 'Laiyolo').
	language('ljl', '', individual, living, 'Li''o').
	language('ljp', '', individual, living, 'Lampung Api').
	language('ljw', '', individual, living, 'Yirandali').
	language('ljx', '', individual, extinct, 'Yuru').
	language('lka', '', individual, living, 'Lakalei').
	language('lkb', '', individual, living, 'Kabras').
	language('lkc', '', individual, living, 'Kucong').
	language('lkd', '', individual, living, 'Lakondê').
	language('lke', '', individual, living, 'Kenyi').
	language('lkh', '', individual, living, 'Lakha').
	language('lki', '', individual, living, 'Laki').
	language('lkj', '', individual, living, 'Remun').
	language('lkl', '', individual, living, 'Laeko-Libuat').
	language('lkm', '', individual, extinct, 'Kalaamaya').
	language('lkn', '', individual, living, 'Lakon').
	language('lko', '', individual, living, 'Khayo').
	language('lkr', '', individual, living, 'Päri').
	language('lks', '', individual, living, 'Kisa').
	language('lkt', '', individual, living, 'Lakota').
	language('lku', '', individual, extinct, 'Kungkari').
	language('lky', '', individual, living, 'Lokoya').
	language('lla', '', individual, living, 'Lala-Roba').
	language('llb', '', individual, living, 'Lolo').
	language('llc', '', individual, living, 'Lele (Guinea)').
	language('lld', '', individual, living, 'Ladin').
	language('lle', '', individual, living, 'Lele (Papua New Guinea)').
	language('llf', '', individual, extinct, 'Hermit').
	language('llg', '', individual, living, 'Lole').
	language('llh', '', individual, living, 'Lamu').
	language('lli', '', individual, living, 'Teke-Laali').
	language('llj', '', individual, extinct, 'Ladji Ladji').
	language('llk', '', individual, extinct, 'Lelak').
	language('lll', '', individual, living, 'Lilau').
	language('llm', '', individual, living, 'Lasalimu').
	language('lln', '', individual, living, 'Lele (Chad)').
	language('llp', '', individual, living, 'North Efate').
	language('llq', '', individual, living, 'Lolak').
	language('lls', '', individual, living, 'Lithuanian Sign Language').
	language('llu', '', individual, living, 'Lau').
	language('llx', '', individual, living, 'Lauan').
	language('lma', '', individual, living, 'East Limba').
	language('lmb', '', individual, living, 'Merei').
	language('lmc', '', individual, extinct, 'Limilngan').
	language('lmd', '', individual, living, 'Lumun').
	language('lme', '', individual, living, 'Pévé').
	language('lmf', '', individual, living, 'South Lembata').
	language('lmg', '', individual, living, 'Lamogai').
	language('lmh', '', individual, living, 'Lambichhong').
	language('lmi', '', individual, living, 'Lombi').
	language('lmj', '', individual, living, 'West Lembata').
	language('lmk', '', individual, living, 'Lamkang').
	language('lml', '', individual, living, 'Hano').
	language('lmn', '', individual, living, 'Lambadi').
	language('lmo', '', individual, living, 'Lombard').
	language('lmp', '', individual, living, 'Limbum').
	language('lmq', '', individual, living, 'Lamatuka').
	language('lmr', '', individual, living, 'Lamalera').
	language('lmu', '', individual, living, 'Lamenu').
	language('lmv', '', individual, living, 'Lomaiviti').
	language('lmw', '', individual, living, 'Lake Miwok').
	language('lmx', '', individual, living, 'Laimbue').
	language('lmy', '', individual, living, 'Lamboya').
	language('lna', '', individual, living, 'Langbashe').
	language('lnb', '', individual, living, 'Mbalanhu').
	language('lnd', '', individual, living, 'Lundayeh').
	language('lng', '', individual, special, 'Langobardic').
	language('lnh', '', individual, living, 'Lanoh').
	language('lni', '', individual, living, 'Daantanai''').
	language('lnj', '', individual, extinct, 'Leningitij').
	language('lnl', '', individual, living, 'South Central Banda').
	language('lnm', '', individual, living, 'Langam').
	language('lnn', '', individual, living, 'Lorediakarkar').
	language('lns', '', individual, living, 'Lamnso''').
	language('lnu', '', individual, living, 'Longuda').
	language('lnw', '', individual, extinct, 'Lanima').
	language('lnz', '', individual, living, 'Lonzo').
	language('loa', '', individual, living, 'Loloda').
	language('lob', '', individual, living, 'Lobi').
	language('loc', '', individual, living, 'Inonhan').
	language('loe', '', individual, living, 'Saluan').
	language('lof', '', individual, living, 'Logol').
	language('log', '', individual, living, 'Logo').
	language('loh', '', individual, living, 'Laarim').
	language('loi', '', individual, living, 'Loma (Côte d''Ivoire)').
	language('loj', '', individual, living, 'Lou').
	language('lok', '', individual, living, 'Loko').
	language('lol', '', individual, living, 'Mongo').
	language('lom', '', individual, living, 'Loma (Liberia)').
	language('lon', '', individual, living, 'Malawi Lomwe').
	language('loo', '', individual, living, 'Lombo').
	language('lop', '', individual, living, 'Lopa').
	language('loq', '', individual, living, 'Lobala').
	language('lor', '', individual, living, 'Téén').
	language('los', '', individual, living, 'Loniu').
	language('lot', '', individual, living, 'Otuho').
	language('lou', '', individual, living, 'Louisiana Creole').
	language('lov', '', individual, living, 'Lopi').
	language('low', '', individual, living, 'Tampias Lobu').
	language('lox', '', individual, living, 'Loun').
	language('loy', '', individual, living, 'Loke').
	language('loz', '', individual, living, 'Lozi').
	language('lpa', '', individual, living, 'Lelepa').
	language('lpe', '', individual, living, 'Lepki').
	language('lpn', '', individual, living, 'Long Phuri Naga').
	language('lpo', '', individual, living, 'Lipo').
	language('lpx', '', individual, living, 'Lopit').
	language('lqr', '', individual, living, 'Logir').
	language('lra', '', individual, living, 'Rara Bakati''').
	language('lrc', '', individual, living, 'Northern Luri').
	language('lre', '', individual, extinct, 'Laurentian').
	language('lrg', '', individual, extinct, 'Laragia').
	language('lri', '', individual, living, 'Marachi').
	language('lrk', '', individual, living, 'Loarki').
	language('lrl', '', individual, living, 'Lari').
	language('lrm', '', individual, living, 'Marama').
	language('lrn', '', individual, living, 'Lorang').
	language('lro', '', individual, living, 'Laro').
	language('lrr', '', individual, living, 'Southern Yamphu').
	language('lrt', '', individual, living, 'Larantuka Malay').
	language('lrv', '', individual, living, 'Larevat').
	language('lrz', '', individual, living, 'Lemerig').
	language('lsa', '', individual, living, 'Lasgerdi').
	language('lsb', '', individual, living, 'Burundian Sign Language').
	language('lsc', '', individual, living, 'Albarradas Sign Language').
	language('lsd', '', individual, living, 'Lishana Deni').
	language('lse', '', individual, living, 'Lusengo').
	language('lsh', '', individual, living, 'Lish').
	language('lsi', '', individual, living, 'Lashi').
	language('lsl', '', individual, living, 'Latvian Sign Language').
	language('lsm', '', individual, living, 'Saamia').
	language('lsn', '', individual, living, 'Tibetan Sign Language').
	language('lso', '', individual, living, 'Laos Sign Language').
	language('lsp', '', individual, living, 'Panamanian Sign Language').
	language('lsr', '', individual, living, 'Aruop').
	language('lss', '', individual, living, 'Lasi').
	language('lst', '', individual, living, 'Trinidad and Tobago Sign Language').
	language('lsv', '', individual, living, 'Sivia Sign Language').
	language('lsw', '', individual, living, 'Seychelles Sign Language').
	language('lsy', '', individual, living, 'Mauritian Sign Language').
	language('ltc', '', individual, special, 'Late Middle Chinese').
	language('ltg', '', individual, living, 'Latgalian').
	language('lth', '', individual, living, 'Thur').
	language('lti', '', individual, living, 'Leti (Indonesia)').
	language('ltn', '', individual, living, 'Latundê').
	language('lto', '', individual, living, 'Tsotso').
	language('lts', '', individual, living, 'Tachoni').
	language('ltu', '', individual, living, 'Latu').
	language('ltz', 'lb', individual, living, 'Luxembourgish').
	language('lua', '', individual, living, 'Luba-Lulua').
	language('lub', 'lu', individual, living, 'Luba-Katanga').
	language('luc', '', individual, living, 'Aringa').
	language('lud', '', individual, living, 'Ludian').
	language('lue', '', individual, living, 'Luvale').
	language('luf', '', individual, living, 'Laua').
	language('lug', 'lg', individual, living, 'Ganda').
	language('luh', '', individual, living, 'Leizhou Chinese').
	language('lui', '', individual, extinct, 'Luiseño').
	language('luj', '', individual, living, 'Luna').
	language('luk', '', individual, living, 'Lunanakha').
	language('lul', '', individual, living, 'Olu''bo').
	language('lum', '', individual, living, 'Luimbi').
	language('lun', '', individual, living, 'Lunda').
	language('luo', '', individual, living, 'Luo (Kenya and Tanzania)').
	language('lup', '', individual, living, 'Lumbu').
	language('luq', '', individual, living, 'Lucumi').
	language('lur', '', individual, living, 'Laura').
	language('lus', '', individual, living, 'Lushai').
	language('lut', '', individual, extinct, 'Lushootseed').
	language('luu', '', individual, living, 'Lumba-Yakkha').
	language('luv', '', individual, living, 'Luwati').
	language('luw', '', individual, living, 'Luo (Cameroon)').
	language('luy', '', macrolanguage, living, 'Luyia').
	language('luz', '', individual, living, 'Southern Luri').
	language('lva', '', individual, living, 'Maku''a').
	language('lvi', '', individual, living, 'Lavi').
	language('lvk', '', individual, living, 'Lavukaleve').
	language('lvl', '', individual, living, 'Lwel').
	language('lvs', '', individual, living, 'Standard Latvian').
	language('lvu', '', individual, living, 'Levuka').
	language('lwa', '', individual, living, 'Lwalu').
	language('lwe', '', individual, living, 'Lewo Eleng').
	language('lwg', '', individual, living, 'Wanga').
	language('lwh', '', individual, living, 'White Lachi').
	language('lwl', '', individual, living, 'Eastern Lawa').
	language('lwm', '', individual, living, 'Laomian').
	language('lwo', '', individual, living, 'Luwo').
	language('lws', '', individual, living, 'Malawian Sign Language').
	language('lwt', '', individual, living, 'Lewotobi').
	language('lwu', '', individual, living, 'Lawu').
	language('lww', '', individual, living, 'Lewo').
	language('lxm', '', individual, living, 'Lakurumau').
	language('lya', '', individual, living, 'Layakha').
	language('lyg', '', individual, living, 'Lyngngam').
	language('lyn', '', individual, living, 'Luyana').
	language('lzh', '', individual, special, 'Literary Chinese').
	language('lzl', '', individual, living, 'Litzlitz').
	language('lzn', '', individual, living, 'Leinong Naga').
	language('lzz', '', individual, living, 'Laz').
	language('maa', '', individual, living, 'San Jerónimo Tecóatl Mazatec').
	language('mab', '', individual, living, 'Yutanduchi Mixtec').
	language('mad', '', individual, living, 'Madurese').
	language('mae', '', individual, living, 'Bo-Rukul').
	language('maf', '', individual, living, 'Mafa').
	language('mag', '', individual, living, 'Magahi').
	language('mah', 'mh', individual, living, 'Marshallese').
	language('mai', '', individual, living, 'Maithili').
	language('maj', '', individual, living, 'Jalapa De Díaz Mazatec').
	language('mak', '', individual, living, 'Makasar').
	language('mal', 'ml', individual, living, 'Malayalam').
	language('mam', '', individual, living, 'Mam').
	language('man', '', macrolanguage, living, 'Mandingo').
	language('maq', '', individual, living, 'Chiquihuitlán Mazatec').
	language('mar', 'mr', individual, living, 'Marathi').
	language('mas', '', individual, living, 'Masai').
	language('mat', '', individual, living, 'San Francisco Matlatzinca').
	language('mau', '', individual, living, 'Huautla Mazatec').
	language('mav', '', individual, living, 'Sateré-Mawé').
	language('maw', '', individual, living, 'Mampruli').
	language('max', '', individual, living, 'North Moluccan Malay').
	language('maz', '', individual, living, 'Central Mazahua').
	language('mba', '', individual, living, 'Higaonon').
	language('mbb', '', individual, living, 'Western Bukidnon Manobo').
	language('mbc', '', individual, living, 'Macushi').
	language('mbd', '', individual, living, 'Dibabawon Manobo').
	language('mbe', '', individual, extinct, 'Molale').
	language('mbf', '', individual, living, 'Baba Malay').
	language('mbh', '', individual, living, 'Mangseng').
	language('mbi', '', individual, living, 'Ilianen Manobo').
	language('mbj', '', individual, living, 'Nadëb').
	language('mbk', '', individual, living, 'Malol').
	language('mbl', '', individual, living, 'Maxakalí').
	language('mbm', '', individual, living, 'Ombamba').
	language('mbn', '', individual, living, 'Macaguán').
	language('mbo', '', individual, living, 'Mbo (Cameroon)').
	language('mbp', '', individual, living, 'Malayo').
	language('mbq', '', individual, living, 'Maisin').
	language('mbr', '', individual, living, 'Nukak Makú').
	language('mbs', '', individual, living, 'Sarangani Manobo').
	language('mbt', '', individual, living, 'Matigsalug Manobo').
	language('mbu', '', individual, living, 'Mbula-Bwazza').
	language('mbv', '', individual, living, 'Mbulungish').
	language('mbw', '', individual, living, 'Maring').
	language('mbx', '', individual, living, 'Mari (East Sepik Province)').
	language('mby', '', individual, living, 'Memoni').
	language('mbz', '', individual, living, 'Amoltepec Mixtec').
	language('mca', '', individual, living, 'Maca').
	language('mcb', '', individual, living, 'Machiguenga').
	language('mcc', '', individual, living, 'Bitur').
	language('mcd', '', individual, living, 'Sharanahua').
	language('mce', '', individual, living, 'Itundujia Mixtec').
	language('mcf', '', individual, living, 'Matsés').
	language('mcg', '', individual, living, 'Mapoyo').
	language('mch', '', individual, living, 'Maquiritari').
	language('mci', '', individual, living, 'Mese').
	language('mcj', '', individual, living, 'Mvanip').
	language('mck', '', individual, living, 'Mbunda').
	language('mcl', '', individual, extinct, 'Macaguaje').
	language('mcm', '', individual, living, 'Malaccan Creole Portuguese').
	language('mcn', '', individual, living, 'Masana').
	language('mco', '', individual, living, 'Coatlán Mixe').
	language('mcp', '', individual, living, 'Makaa').
	language('mcq', '', individual, living, 'Ese').
	language('mcr', '', individual, living, 'Menya').
	language('mcs', '', individual, living, 'Mambai').
	language('mct', '', individual, living, 'Mengisa').
	language('mcu', '', individual, living, 'Cameroon Mambila').
	language('mcv', '', individual, living, 'Minanibai').
	language('mcw', '', individual, living, 'Mawa (Chad)').
	language('mcx', '', individual, living, 'Mpiemo').
	language('mcy', '', individual, living, 'South Watut').
	language('mcz', '', individual, living, 'Mawan').
	language('mda', '', individual, living, 'Mada (Nigeria)').
	language('mdb', '', individual, living, 'Morigi').
	language('mdc', '', individual, living, 'Soq').
	language('mdd', '', individual, living, 'Mbum').
	language('mde', '', individual, living, 'Maba (Chad)').
	language('mdf', '', individual, living, 'Moksha').
	language('mdg', '', individual, living, 'Massalat').
	language('mdh', '', individual, living, 'Maguindanaon').
	language('mdi', '', individual, living, 'Mamvu').
	language('mdj', '', individual, living, 'Mangbetu').
	language('mdk', '', individual, living, 'Mangbutu').
	language('mdl', '', individual, living, 'Maltese Sign Language').
	language('mdm', '', individual, living, 'Mayogo').
	language('mdn', '', individual, living, 'Mbati').
	language('mdp', '', individual, living, 'Mbala').
	language('mdq', '', individual, living, 'Mbole').
	language('mdr', '', individual, living, 'Mandar').
	language('mds', '', individual, living, 'Maria (Papua New Guinea)').
	language('mdt', '', individual, living, 'Mbere').
	language('mdu', '', individual, living, 'Mboko').
	language('mdv', '', individual, living, 'Santa Lucía Monteverde Mixtec').
	language('mdw', '', individual, living, 'Mbosi').
	language('mdx', '', individual, living, 'Dizin').
	language('mdy', '', individual, living, 'Male').
	language('mdz', '', individual, living, 'Suruí Do Pará').
	language('mea', '', individual, living, 'Menka').
	language('meb', '', individual, living, 'Ikobi').
	language('mec', '', individual, living, 'Marra').
	language('med', '', individual, living, 'Melpa').
	language('mee', '', individual, living, 'Mengen').
	language('mef', '', individual, living, 'Megam').
	language('meh', '', individual, living, 'Southwestern Tlaxiaco Mixtec').
	language('mei', '', individual, living, 'Midob').
	language('mej', '', individual, living, 'Meyah').
	language('mek', '', individual, living, 'Mekeo').
	language('mel', '', individual, living, 'Central Melanau').
	language('mem', '', individual, extinct, 'Mangala').
	language('men', '', individual, living, 'Mende (Sierra Leone)').
	language('meo', '', individual, living, 'Kedah Malay').
	language('mep', '', individual, living, 'Miriwoong').
	language('meq', '', individual, living, 'Merey').
	language('mer', '', individual, living, 'Meru').
	language('mes', '', individual, living, 'Masmaje').
	language('met', '', individual, living, 'Mato').
	language('meu', '', individual, living, 'Motu').
	language('mev', '', individual, living, 'Mano').
	language('mew', '', individual, living, 'Maaka').
	language('mey', '', individual, living, 'Hassaniyya').
	language('mez', '', individual, living, 'Menominee').
	language('mfa', '', individual, living, 'Pattani Malay').
	language('mfb', '', individual, living, 'Bangka').
	language('mfc', '', individual, living, 'Mba').
	language('mfd', '', individual, living, 'Mendankwe-Nkwen').
	language('mfe', '', individual, living, 'Morisyen').
	language('mff', '', individual, living, 'Naki').
	language('mfg', '', individual, living, 'Mogofin').
	language('mfh', '', individual, living, 'Matal').
	language('mfi', '', individual, living, 'Wandala').
	language('mfj', '', individual, living, 'Mefele').
	language('mfk', '', individual, living, 'North Mofu').
	language('mfl', '', individual, living, 'Putai').
	language('mfm', '', individual, living, 'Marghi South').
	language('mfn', '', individual, living, 'Cross River Mbembe').
	language('mfo', '', individual, living, 'Mbe').
	language('mfp', '', individual, living, 'Makassar Malay').
	language('mfq', '', individual, living, 'Moba').
	language('mfr', '', individual, living, 'Marrithiyel').
	language('mfs', '', individual, living, 'Mexican Sign Language').
	language('mft', '', individual, living, 'Mokerang').
	language('mfu', '', individual, living, 'Mbwela').
	language('mfv', '', individual, living, 'Mandjak').
	language('mfw', '', individual, extinct, 'Mulaha').
	language('mfx', '', individual, living, 'Melo').
	language('mfy', '', individual, living, 'Mayo').
	language('mfz', '', individual, living, 'Mabaan').
	language('mga', '', individual, special, 'Middle Irish (900-1200)').
	language('mgb', '', individual, living, 'Mararit').
	language('mgc', '', individual, living, 'Morokodo').
	language('mgd', '', individual, living, 'Moru').
	language('mge', '', individual, living, 'Mango').
	language('mgf', '', individual, living, 'Maklew').
	language('mgg', '', individual, living, 'Mpumpong').
	language('mgh', '', individual, living, 'Makhuwa-Meetto').
	language('mgi', '', individual, living, 'Lijili').
	language('mgj', '', individual, living, 'Abureni').
	language('mgk', '', individual, living, 'Mawes').
	language('mgl', '', individual, living, 'Maleu-Kilenge').
	language('mgm', '', individual, living, 'Mambae').
	language('mgn', '', individual, living, 'Mbangi').
	language('mgo', '', individual, living, 'Meta''').
	language('mgp', '', individual, living, 'Eastern Magar').
	language('mgq', '', individual, living, 'Malila').
	language('mgr', '', individual, living, 'Mambwe-Lungu').
	language('mgs', '', individual, living, 'Manda (Tanzania)').
	language('mgt', '', individual, living, 'Mongol').
	language('mgu', '', individual, living, 'Mailu').
	language('mgv', '', individual, living, 'Matengo').
	language('mgw', '', individual, living, 'Matumbi').
	language('mgy', '', individual, living, 'Mbunga').
	language('mgz', '', individual, living, 'Mbugwe').
	language('mha', '', individual, living, 'Manda (India)').
	language('mhb', '', individual, living, 'Mahongwe').
	language('mhc', '', individual, living, 'Mocho').
	language('mhd', '', individual, living, 'Mbugu').
	language('mhe', '', individual, living, 'Besisi').
	language('mhf', '', individual, living, 'Mamaa').
	language('mhg', '', individual, living, 'Margu').
	language('mhi', '', individual, living, 'Ma''di').
	language('mhj', '', individual, living, 'Mogholi').
	language('mhk', '', individual, living, 'Mungaka').
	language('mhl', '', individual, living, 'Mauwake').
	language('mhm', '', individual, living, 'Makhuwa-Moniga').
	language('mhn', '', individual, living, 'Mòcheno').
	language('mho', '', individual, living, 'Mashi (Zambia)').
	language('mhp', '', individual, living, 'Balinese Malay').
	language('mhq', '', individual, living, 'Mandan').
	language('mhr', '', individual, living, 'Eastern Mari').
	language('mhs', '', individual, living, 'Buru (Indonesia)').
	language('mht', '', individual, living, 'Mandahuaca').
	language('mhu', '', individual, living, 'Digaro-Mishmi').
	language('mhw', '', individual, living, 'Mbukushu').
	language('mhx', '', individual, living, 'Maru').
	language('mhy', '', individual, living, 'Ma''anyan').
	language('mhz', '', individual, living, 'Mor (Mor Islands)').
	language('mia', '', individual, living, 'Miami').
	language('mib', '', individual, living, 'Atatláhuca Mixtec').
	language('mic', '', individual, living, 'Mi''kmaq').
	language('mid', '', individual, living, 'Mandaic').
	language('mie', '', individual, living, 'Ocotepec Mixtec').
	language('mif', '', individual, living, 'Mofu-Gudur').
	language('mig', '', individual, living, 'San Miguel El Grande Mixtec').
	language('mih', '', individual, living, 'Chayuco Mixtec').
	language('mii', '', individual, living, 'Chigmecatitlán Mixtec').
	language('mij', '', individual, living, 'Abar').
	language('mik', '', individual, living, 'Mikasuki').
	language('mil', '', individual, living, 'Peñoles Mixtec').
	language('mim', '', individual, living, 'Alacatlatzala Mixtec').
	language('min', '', individual, living, 'Minangkabau').
	language('mio', '', individual, living, 'Pinotepa Nacional Mixtec').
	language('mip', '', individual, living, 'Apasco-Apoala Mixtec').
	language('miq', '', individual, living, 'Mískito').
	language('mir', '', individual, living, 'Isthmus Mixe').
	language('mis', '', special, special, 'Uncoded languages').
	language('mit', '', individual, living, 'Southern Puebla Mixtec').
	language('miu', '', individual, living, 'Cacaloxtepec Mixtec').
	language('miw', '', individual, living, 'Akoye').
	language('mix', '', individual, living, 'Mixtepec Mixtec').
	language('miy', '', individual, living, 'Ayutla Mixtec').
	language('miz', '', individual, living, 'Coatzospan Mixtec').
	language('mjb', '', individual, living, 'Makalero').
	language('mjc', '', individual, living, 'San Juan Colorado Mixtec').
	language('mjd', '', individual, living, 'Northwest Maidu').
	language('mje', '', individual, extinct, 'Muskum').
	language('mjg', '', individual, living, 'Tu').
	language('mjh', '', individual, living, 'Mwera (Nyasa)').
	language('mji', '', individual, living, 'Kim Mun').
	language('mjj', '', individual, living, 'Mawak').
	language('mjk', '', individual, living, 'Matukar').
	language('mjl', '', individual, living, 'Mandeali').
	language('mjm', '', individual, living, 'Medebur').
	language('mjn', '', individual, living, 'Ma (Papua New Guinea)').
	language('mjo', '', individual, living, 'Malankuravan').
	language('mjp', '', individual, living, 'Malapandaram').
	language('mjq', '', individual, extinct, 'Malaryan').
	language('mjr', '', individual, living, 'Malavedan').
	language('mjs', '', individual, living, 'Miship').
	language('mjt', '', individual, living, 'Sauria Paharia').
	language('mju', '', individual, living, 'Manna-Dora').
	language('mjv', '', individual, living, 'Mannan').
	language('mjw', '', individual, living, 'Karbi').
	language('mjx', '', individual, living, 'Mahali').
	language('mjy', '', individual, extinct, 'Mahican').
	language('mjz', '', individual, living, 'Majhi').
	language('mka', '', individual, living, 'Mbre').
	language('mkb', '', individual, living, 'Mal Paharia').
	language('mkc', '', individual, living, 'Siliput').
	language('mkd', 'mk', individual, living, 'Macedonian').
	language('mke', '', individual, living, 'Mawchi').
	language('mkf', '', individual, living, 'Miya').
	language('mkg', '', individual, living, 'Mak (China)').
	language('mki', '', individual, living, 'Dhatki').
	language('mkj', '', individual, living, 'Mokilese').
	language('mkk', '', individual, living, 'Byep').
	language('mkl', '', individual, living, 'Mokole').
	language('mkm', '', individual, living, 'Moklen').
	language('mkn', '', individual, living, 'Kupang Malay').
	language('mko', '', individual, living, 'Mingang Doso').
	language('mkp', '', individual, living, 'Moikodi').
	language('mkq', '', individual, extinct, 'Bay Miwok').
	language('mkr', '', individual, living, 'Malas').
	language('mks', '', individual, living, 'Silacayoapan Mixtec').
	language('mkt', '', individual, living, 'Vamale').
	language('mku', '', individual, living, 'Konyanka Maninka').
	language('mkv', '', individual, living, 'Mafea').
	language('mkw', '', individual, living, 'Kituba (Congo)').
	language('mkx', '', individual, living, 'Kinamiging Manobo').
	language('mky', '', individual, living, 'East Makian').
	language('mkz', '', individual, living, 'Makasae').
	language('mla', '', individual, living, 'Malo').
	language('mlb', '', individual, living, 'Mbule').
	language('mlc', '', individual, living, 'Cao Lan').
	language('mle', '', individual, living, 'Manambu').
	language('mlf', '', individual, living, 'Mal').
	language('mlg', 'mg', macrolanguage, living, 'Malagasy').
	language('mlh', '', individual, living, 'Mape').
	language('mli', '', individual, living, 'Malimpung').
	language('mlj', '', individual, living, 'Miltu').
	language('mlk', '', individual, living, 'Ilwana').
	language('mll', '', individual, living, 'Malua Bay').
	language('mlm', '', individual, living, 'Mulam').
	language('mln', '', individual, living, 'Malango').
	language('mlo', '', individual, living, 'Mlomp').
	language('mlp', '', individual, living, 'Bargam').
	language('mlq', '', individual, living, 'Western Maninkakan').
	language('mlr', '', individual, living, 'Vame').
	language('mls', '', individual, living, 'Masalit').
	language('mlt', 'mt', individual, living, 'Maltese').
	language('mlu', '', individual, living, 'To''abaita').
	language('mlv', '', individual, living, 'Motlav').
	language('mlw', '', individual, living, 'Moloko').
	language('mlx', '', individual, living, 'Malfaxal').
	language('mlz', '', individual, living, 'Malaynon').
	language('mma', '', individual, living, 'Mama').
	language('mmb', '', individual, living, 'Momina').
	language('mmc', '', individual, living, 'Michoacán Mazahua').
	language('mmd', '', individual, living, 'Maonan').
	language('mme', '', individual, living, 'Mae').
	language('mmf', '', individual, living, 'Mundat').
	language('mmg', '', individual, living, 'North Ambrym').
	language('mmh', '', individual, living, 'Mehináku').
	language('mmi', '', individual, living, 'Hember Avu').
	language('mmj', '', individual, living, 'Majhwar').
	language('mmk', '', individual, living, 'Mukha-Dora').
	language('mml', '', individual, living, 'Man Met').
	language('mmm', '', individual, living, 'Maii').
	language('mmn', '', individual, living, 'Mamanwa').
	language('mmo', '', individual, living, 'Mangga Buang').
	language('mmp', '', individual, living, 'Siawi').
	language('mmq', '', individual, living, 'Musak').
	language('mmr', '', individual, living, 'Western Xiangxi Miao').
	language('mmt', '', individual, living, 'Malalamai').
	language('mmu', '', individual, living, 'Mmaala').
	language('mmv', '', individual, extinct, 'Miriti').
	language('mmw', '', individual, living, 'Emae').
	language('mmx', '', individual, living, 'Madak').
	language('mmy', '', individual, living, 'Migaama').
	language('mmz', '', individual, living, 'Mabaale').
	language('mna', '', individual, living, 'Mbula').
	language('mnb', '', individual, living, 'Muna').
	language('mnc', '', individual, living, 'Manchu').
	language('mnd', '', individual, living, 'Mondé').
	language('mne', '', individual, living, 'Naba').
	language('mnf', '', individual, living, 'Mundani').
	language('mng', '', individual, living, 'Eastern Mnong').
	language('mnh', '', individual, living, 'Mono (Democratic Republic of Congo)').
	language('mni', '', individual, living, 'Manipuri').
	language('mnj', '', individual, living, 'Munji').
	language('mnk', '', individual, living, 'Mandinka').
	language('mnl', '', individual, living, 'Tiale').
	language('mnm', '', individual, living, 'Mapena').
	language('mnn', '', individual, living, 'Southern Mnong').
	language('mnp', '', individual, living, 'Min Bei Chinese').
	language('mnq', '', individual, living, 'Minriq').
	language('mnr', '', individual, living, 'Mono (USA)').
	language('mns', '', individual, living, 'Mansi').
	language('mnu', '', individual, living, 'Mer').
	language('mnv', '', individual, living, 'Rennell-Bellona').
	language('mnw', '', individual, living, 'Mon').
	language('mnx', '', individual, living, 'Manikion').
	language('mny', '', individual, living, 'Manyawa').
	language('mnz', '', individual, living, 'Moni').
	language('moa', '', individual, living, 'Mwan').
	language('moc', '', individual, living, 'Mocoví').
	language('mod', '', individual, extinct, 'Mobilian').
	language('moe', '', individual, living, 'Innu').
	language('mog', '', individual, living, 'Mongondow').
	language('moh', '', individual, living, 'Mohawk').
	language('moi', '', individual, living, 'Mboi').
	language('moj', '', individual, living, 'Monzombo').
	language('mok', '', individual, living, 'Morori').
	language('mom', '', individual, extinct, 'Mangue').
	language('mon', 'mn', macrolanguage, living, 'Mongolian').
	language('moo', '', individual, living, 'Monom').
	language('mop', '', individual, living, 'Mopán Maya').
	language('moq', '', individual, living, 'Mor (Bomberai Peninsula)').
	language('mor', '', individual, living, 'Moro').
	language('mos', '', individual, living, 'Mossi').
	language('mot', '', individual, living, 'Barí').
	language('mou', '', individual, living, 'Mogum').
	language('mov', '', individual, living, 'Mohave').
	language('mow', '', individual, living, 'Moi (Congo)').
	language('mox', '', individual, living, 'Molima').
	language('moy', '', individual, living, 'Shekkacho').
	language('moz', '', individual, living, 'Mukulu').
	language('mpa', '', individual, living, 'Mpoto').
	language('mpb', '', individual, living, 'Malak Malak').
	language('mpc', '', individual, living, 'Mangarrayi').
	language('mpd', '', individual, living, 'Machinere').
	language('mpe', '', individual, living, 'Majang').
	language('mpg', '', individual, living, 'Marba').
	language('mph', '', individual, living, 'Maung').
	language('mpi', '', individual, living, 'Mpade').
	language('mpj', '', individual, living, 'Martu Wangka').
	language('mpk', '', individual, living, 'Mbara (Chad)').
	language('mpl', '', individual, living, 'Middle Watut').
	language('mpm', '', individual, living, 'Yosondúa Mixtec').
	language('mpn', '', individual, living, 'Mindiri').
	language('mpo', '', individual, living, 'Miu').
	language('mpp', '', individual, living, 'Migabac').
	language('mpq', '', individual, living, 'Matís').
	language('mpr', '', individual, living, 'Vangunu').
	language('mps', '', individual, living, 'Dadibi').
	language('mpt', '', individual, living, 'Mian').
	language('mpu', '', individual, living, 'Makuráp').
	language('mpv', '', individual, living, 'Mungkip').
	language('mpw', '', individual, living, 'Mapidian').
	language('mpx', '', individual, living, 'Misima-Panaeati').
	language('mpy', '', individual, living, 'Mapia').
	language('mpz', '', individual, living, 'Mpi').
	language('mqa', '', individual, living, 'Maba (Indonesia)').
	language('mqb', '', individual, living, 'Mbuko').
	language('mqc', '', individual, living, 'Mangole').
	language('mqe', '', individual, living, 'Matepi').
	language('mqf', '', individual, living, 'Momuna').
	language('mqg', '', individual, living, 'Kota Bangun Kutai Malay').
	language('mqh', '', individual, living, 'Tlazoyaltepec Mixtec').
	language('mqi', '', individual, living, 'Mariri').
	language('mqj', '', individual, living, 'Mamasa').
	language('mqk', '', individual, living, 'Rajah Kabunsuwan Manobo').
	language('mql', '', individual, living, 'Mbelime').
	language('mqm', '', individual, living, 'South Marquesan').
	language('mqn', '', individual, living, 'Moronene').
	language('mqo', '', individual, living, 'Modole').
	language('mqp', '', individual, living, 'Manipa').
	language('mqq', '', individual, living, 'Minokok').
	language('mqr', '', individual, living, 'Mander').
	language('mqs', '', individual, living, 'West Makian').
	language('mqt', '', individual, living, 'Mok').
	language('mqu', '', individual, living, 'Mandari').
	language('mqv', '', individual, living, 'Mosimo').
	language('mqw', '', individual, living, 'Murupi').
	language('mqx', '', individual, living, 'Mamuju').
	language('mqy', '', individual, living, 'Manggarai').
	language('mqz', '', individual, living, 'Pano').
	language('mra', '', individual, living, 'Mlabri').
	language('mrb', '', individual, living, 'Marino').
	language('mrc', '', individual, living, 'Maricopa').
	language('mrd', '', individual, living, 'Western Magar').
	language('mre', '', individual, extinct, 'Martha''s Vineyard Sign Language').
	language('mrf', '', individual, living, 'Elseng').
	language('mrg', '', individual, living, 'Mising').
	language('mrh', '', individual, living, 'Mara Chin').
	language('mri', 'mi', individual, living, 'Maori').
	language('mrj', '', individual, living, 'Western Mari').
	language('mrk', '', individual, living, 'Hmwaveke').
	language('mrl', '', individual, living, 'Mortlockese').
	language('mrm', '', individual, living, 'Merlav').
	language('mrn', '', individual, living, 'Cheke Holo').
	language('mro', '', individual, living, 'Mru').
	language('mrp', '', individual, living, 'Morouas').
	language('mrq', '', individual, living, 'North Marquesan').
	language('mrr', '', individual, living, 'Maria (India)').
	language('mrs', '', individual, living, 'Maragus').
	language('mrt', '', individual, living, 'Marghi Central').
	language('mru', '', individual, living, 'Mono (Cameroon)').
	language('mrv', '', individual, living, 'Mangareva').
	language('mrw', '', individual, living, 'Maranao').
	language('mrx', '', individual, living, 'Maremgi').
	language('mry', '', individual, living, 'Mandaya').
	language('mrz', '', individual, living, 'Marind').
	language('msa', 'ms', macrolanguage, living, 'Malay (macrolanguage)').
	language('msb', '', individual, living, 'Masbatenyo').
	language('msc', '', individual, living, 'Sankaran Maninka').
	language('msd', '', individual, living, 'Yucatec Maya Sign Language').
	language('mse', '', individual, living, 'Musey').
	language('msf', '', individual, living, 'Mekwei').
	language('msg', '', individual, living, 'Moraid').
	language('msh', '', individual, living, 'Masikoro Malagasy').
	language('msi', '', individual, living, 'Sabah Malay').
	language('msj', '', individual, living, 'Ma (Democratic Republic of Congo)').
	language('msk', '', individual, living, 'Mansaka').
	language('msl', '', individual, living, 'Molof').
	language('msm', '', individual, living, 'Agusan Manobo').
	language('msn', '', individual, living, 'Vurës').
	language('mso', '', individual, living, 'Mombum').
	language('msp', '', individual, extinct, 'Maritsauá').
	language('msq', '', individual, living, 'Caac').
	language('msr', '', individual, living, 'Mongolian Sign Language').
	language('mss', '', individual, living, 'West Masela').
	language('msu', '', individual, living, 'Musom').
	language('msv', '', individual, living, 'Maslam').
	language('msw', '', individual, living, 'Mansoanka').
	language('msx', '', individual, living, 'Moresada').
	language('msy', '', individual, living, 'Aruamu').
	language('msz', '', individual, living, 'Momare').
	language('mta', '', individual, living, 'Cotabato Manobo').
	language('mtb', '', individual, living, 'Anyin Morofo').
	language('mtc', '', individual, living, 'Munit').
	language('mtd', '', individual, living, 'Mualang').
	language('mte', '', individual, living, 'Mono (Solomon Islands)').
	language('mtf', '', individual, living, 'Murik (Papua New Guinea)').
	language('mtg', '', individual, living, 'Una').
	language('mth', '', individual, living, 'Munggui').
	language('mti', '', individual, living, 'Maiwa (Papua New Guinea)').
	language('mtj', '', individual, living, 'Moskona').
	language('mtk', '', individual, living, 'Mbe''').
	language('mtl', '', individual, living, 'Montol').
	language('mtm', '', individual, extinct, 'Mator').
	language('mtn', '', individual, extinct, 'Matagalpa').
	language('mto', '', individual, living, 'Totontepec Mixe').
	language('mtp', '', individual, living, 'Wichí Lhamtés Nocten').
	language('mtq', '', individual, living, 'Muong').
	language('mtr', '', individual, living, 'Mewari').
	language('mts', '', individual, living, 'Yora').
	language('mtt', '', individual, living, 'Mota').
	language('mtu', '', individual, living, 'Tututepec Mixtec').
	language('mtv', '', individual, living, 'Asaro''o').
	language('mtw', '', individual, living, 'Southern Binukidnon').
	language('mtx', '', individual, living, 'Tidaá Mixtec').
	language('mty', '', individual, living, 'Nabi').
	language('mua', '', individual, living, 'Mundang').
	language('mub', '', individual, living, 'Mubi').
	language('muc', '', individual, living, 'Ajumbu').
	language('mud', '', individual, living, 'Mednyj Aleut').
	language('mue', '', individual, living, 'Media Lengua').
	language('mug', '', individual, living, 'Musgu').
	language('muh', '', individual, living, 'Mündü').
	language('mui', '', individual, living, 'Musi').
	language('muj', '', individual, living, 'Mabire').
	language('muk', '', individual, living, 'Mugom').
	language('mul', '', special, special, 'Multiple languages').
	language('mum', '', individual, living, 'Maiwala').
	language('muo', '', individual, living, 'Nyong').
	language('mup', '', individual, living, 'Malvi').
	language('muq', '', individual, living, 'Eastern Xiangxi Miao').
	language('mur', '', individual, living, 'Murle').
	language('mus', '', individual, living, 'Creek').
	language('mut', '', individual, living, 'Western Muria').
	language('muu', '', individual, living, 'Yaaku').
	language('muv', '', individual, living, 'Muthuvan').
	language('mux', '', individual, living, 'Bo-Ung').
	language('muy', '', individual, living, 'Muyang').
	language('muz', '', individual, living, 'Mursi').
	language('mva', '', individual, living, 'Manam').
	language('mvb', '', individual, extinct, 'Mattole').
	language('mvd', '', individual, living, 'Mamboru').
	language('mve', '', individual, living, 'Marwari (Pakistan)').
	language('mvf', '', individual, living, 'Peripheral Mongolian').
	language('mvg', '', individual, living, 'Yucuañe Mixtec').
	language('mvh', '', individual, living, 'Mulgi').
	language('mvi', '', individual, living, 'Miyako').
	language('mvk', '', individual, living, 'Mekmek').
	language('mvl', '', individual, extinct, 'Mbara (Australia)').
	language('mvn', '', individual, living, 'Minaveha').
	language('mvo', '', individual, living, 'Marovo').
	language('mvp', '', individual, living, 'Duri').
	language('mvq', '', individual, living, 'Moere').
	language('mvr', '', individual, living, 'Marau').
	language('mvs', '', individual, living, 'Massep').
	language('mvt', '', individual, living, 'Mpotovoro').
	language('mvu', '', individual, living, 'Marfa').
	language('mvv', '', individual, living, 'Tagal Murut').
	language('mvw', '', individual, living, 'Machinga').
	language('mvx', '', individual, living, 'Meoswar').
	language('mvy', '', individual, living, 'Indus Kohistani').
	language('mvz', '', individual, living, 'Mesqan').
	language('mwa', '', individual, living, 'Mwatebu').
	language('mwb', '', individual, living, 'Juwal').
	language('mwc', '', individual, living, 'Are').
	language('mwe', '', individual, living, 'Mwera (Chimwera)').
	language('mwf', '', individual, living, 'Murrinh-Patha').
	language('mwg', '', individual, living, 'Aiklep').
	language('mwh', '', individual, living, 'Mouk-Aria').
	language('mwi', '', individual, living, 'Labo').
	language('mwk', '', individual, living, 'Kita Maninkakan').
	language('mwl', '', individual, living, 'Mirandese').
	language('mwm', '', individual, living, 'Sar').
	language('mwn', '', individual, living, 'Nyamwanga').
	language('mwo', '', individual, living, 'Central Maewo').
	language('mwp', '', individual, living, 'Kala Lagaw Ya').
	language('mwq', '', individual, living, 'Mün Chin').
	language('mwr', '', macrolanguage, living, 'Marwari').
	language('mws', '', individual, living, 'Mwimbi-Muthambi').
	language('mwt', '', individual, living, 'Moken').
	language('mwu', '', individual, extinct, 'Mittu').
	language('mwv', '', individual, living, 'Mentawai').
	language('mww', '', individual, living, 'Hmong Daw').
	language('mwz', '', individual, living, 'Moingi').
	language('mxa', '', individual, living, 'Northwest Oaxaca Mixtec').
	language('mxb', '', individual, living, 'Tezoatlán Mixtec').
	language('mxc', '', individual, living, 'Manyika').
	language('mxd', '', individual, living, 'Modang').
	language('mxe', '', individual, living, 'Mele-Fila').
	language('mxf', '', individual, living, 'Malgbe').
	language('mxg', '', individual, living, 'Mbangala').
	language('mxh', '', individual, living, 'Mvuba').
	language('mxi', '', individual, special, 'Mozarabic').
	language('mxj', '', individual, living, 'Miju-Mishmi').
	language('mxk', '', individual, living, 'Monumbo').
	language('mxl', '', individual, living, 'Maxi Gbe').
	language('mxm', '', individual, living, 'Meramera').
	language('mxn', '', individual, living, 'Moi (Indonesia)').
	language('mxo', '', individual, living, 'Mbowe').
	language('mxp', '', individual, living, 'Tlahuitoltepec Mixe').
	language('mxq', '', individual, living, 'Juquila Mixe').
	language('mxr', '', individual, living, 'Murik (Malaysia)').
	language('mxs', '', individual, living, 'Huitepec Mixtec').
	language('mxt', '', individual, living, 'Jamiltepec Mixtec').
	language('mxu', '', individual, living, 'Mada (Cameroon)').
	language('mxv', '', individual, living, 'Metlatónoc Mixtec').
	language('mxw', '', individual, living, 'Namo').
	language('mxx', '', individual, living, 'Mahou').
	language('mxy', '', individual, living, 'Southeastern Nochixtlán Mixtec').
	language('mxz', '', individual, living, 'Central Masela').
	language('mya', 'my', individual, living, 'Burmese').
	language('myb', '', individual, living, 'Mbay').
	language('myc', '', individual, living, 'Mayeka').
	language('mye', '', individual, living, 'Myene').
	language('myf', '', individual, living, 'Bambassi').
	language('myg', '', individual, living, 'Manta').
	language('myh', '', individual, living, 'Makah').
	language('myj', '', individual, living, 'Mangayat').
	language('myk', '', individual, living, 'Mamara Senoufo').
	language('myl', '', individual, living, 'Moma').
	language('mym', '', individual, living, 'Me''en').
	language('myo', '', individual, living, 'Anfillo').
	language('myp', '', individual, living, 'Pirahã').
	language('myr', '', individual, living, 'Muniche').
	language('mys', '', individual, extinct, 'Mesmes').
	language('myu', '', individual, living, 'Mundurukú').
	language('myv', '', individual, living, 'Erzya').
	language('myw', '', individual, living, 'Muyuw').
	language('myx', '', individual, living, 'Masaaba').
	language('myy', '', individual, living, 'Macuna').
	language('myz', '', individual, special, 'Classical Mandaic').
	language('mza', '', individual, living, 'Santa María Zacatepec Mixtec').
	language('mzb', '', individual, living, 'Tumzabt').
	language('mzc', '', individual, living, 'Madagascar Sign Language').
	language('mzd', '', individual, living, 'Malimba').
	language('mze', '', individual, living, 'Morawa').
	language('mzg', '', individual, living, 'Monastic Sign Language').
	language('mzh', '', individual, living, 'Wichí Lhamtés Güisnay').
	language('mzi', '', individual, living, 'Ixcatlán Mazatec').
	language('mzj', '', individual, living, 'Manya').
	language('mzk', '', individual, living, 'Nigeria Mambila').
	language('mzl', '', individual, living, 'Mazatlán Mixe').
	language('mzm', '', individual, living, 'Mumuye').
	language('mzn', '', individual, living, 'Mazanderani').
	language('mzo', '', individual, extinct, 'Matipuhy').
	language('mzp', '', individual, living, 'Movima').
	language('mzq', '', individual, living, 'Mori Atas').
	language('mzr', '', individual, living, 'Marúbo').
	language('mzs', '', individual, living, 'Macanese').
	language('mzt', '', individual, living, 'Mintil').
	language('mzu', '', individual, living, 'Inapang').
	language('mzv', '', individual, living, 'Manza').
	language('mzw', '', individual, living, 'Deg').
	language('mzx', '', individual, living, 'Mawayana').
	language('mzy', '', individual, living, 'Mozambican Sign Language').
	language('mzz', '', individual, living, 'Maiadomu').
	language('naa', '', individual, living, 'Namla').
	language('nab', '', individual, living, 'Southern Nambikuára').
	language('nac', '', individual, living, 'Narak').
	language('nae', '', individual, extinct, 'Naka''ela').
	language('naf', '', individual, living, 'Nabak').
	language('nag', '', individual, living, 'Naga Pidgin').
	language('naj', '', individual, living, 'Nalu').
	language('nak', '', individual, living, 'Nakanai').
	language('nal', '', individual, living, 'Nalik').
	language('nam', '', individual, living, 'Ngan''gityemerri').
	language('nan', '', individual, living, 'Min Nan Chinese').
	language('nao', '', individual, living, 'Naaba').
	language('nap', '', individual, living, 'Neapolitan').
	language('naq', '', individual, living, 'Khoekhoe').
	language('nar', '', individual, living, 'Iguta').
	language('nas', '', individual, living, 'Naasioi').
	language('nat', '', individual, living, 'Ca̱hungwa̱rya̱').
	language('nau', 'na', individual, living, 'Nauru').
	language('nav', 'nv', individual, living, 'Navajo').
	language('naw', '', individual, living, 'Nawuri').
	language('nax', '', individual, living, 'Nakwi').
	language('nay', '', individual, extinct, 'Ngarrindjeri').
	language('naz', '', individual, living, 'Coatepec Nahuatl').
	language('nba', '', individual, living, 'Nyemba').
	language('nbb', '', individual, living, 'Ndoe').
	language('nbc', '', individual, living, 'Chang Naga').
	language('nbd', '', individual, living, 'Ngbinda').
	language('nbe', '', individual, living, 'Konyak Naga').
	language('nbg', '', individual, living, 'Nagarchal').
	language('nbh', '', individual, living, 'Ngamo').
	language('nbi', '', individual, living, 'Mao Naga').
	language('nbj', '', individual, living, 'Ngarinyman').
	language('nbk', '', individual, living, 'Nake').
	language('nbl', 'nr', individual, living, 'South Ndebele').
	language('nbm', '', individual, living, 'Ngbaka Ma''bo').
	language('nbn', '', individual, living, 'Kuri').
	language('nbo', '', individual, living, 'Nkukoli').
	language('nbp', '', individual, living, 'Nnam').
	language('nbq', '', individual, living, 'Nggem').
	language('nbr', '', individual, living, 'Numana').
	language('nbs', '', individual, living, 'Namibian Sign Language').
	language('nbt', '', individual, living, 'Na').
	language('nbu', '', individual, living, 'Rongmei Naga').
	language('nbv', '', individual, living, 'Ngamambo').
	language('nbw', '', individual, living, 'Southern Ngbandi').
	language('nby', '', individual, living, 'Ningera').
	language('nca', '', individual, living, 'Iyo').
	language('ncb', '', individual, living, 'Central Nicobarese').
	language('ncc', '', individual, living, 'Ponam').
	language('ncd', '', individual, living, 'Nachering').
	language('nce', '', individual, living, 'Yale').
	language('ncf', '', individual, living, 'Notsi').
	language('ncg', '', individual, living, 'Nisga''a').
	language('nch', '', individual, living, 'Central Huasteca Nahuatl').
	language('nci', '', individual, special, 'Classical Nahuatl').
	language('ncj', '', individual, living, 'Northern Puebla Nahuatl').
	language('nck', '', individual, living, 'Na-kara').
	language('ncl', '', individual, living, 'Michoacán Nahuatl').
	language('ncm', '', individual, living, 'Nambo').
	language('ncn', '', individual, living, 'Nauna').
	language('nco', '', individual, living, 'Sibe').
	language('ncq', '', individual, living, 'Northern Katang').
	language('ncr', '', individual, living, 'Ncane').
	language('ncs', '', individual, living, 'Nicaraguan Sign Language').
	language('nct', '', individual, living, 'Chothe Naga').
	language('ncu', '', individual, living, 'Chumburung').
	language('ncx', '', individual, living, 'Central Puebla Nahuatl').
	language('ncz', '', individual, extinct, 'Natchez').
	language('nda', '', individual, living, 'Ndasa').
	language('ndb', '', individual, living, 'Kenswei Nsei').
	language('ndc', '', individual, living, 'Ndau').
	language('ndd', '', individual, living, 'Nde-Nsele-Nta').
	language('nde', 'nd', individual, living, 'North Ndebele').
	language('ndf', '', individual, special, 'Nadruvian').
	language('ndg', '', individual, living, 'Ndengereko').
	language('ndh', '', individual, living, 'Ndali').
	language('ndi', '', individual, living, 'Samba Leko').
	language('ndj', '', individual, living, 'Ndamba').
	language('ndk', '', individual, living, 'Ndaka').
	language('ndl', '', individual, living, 'Ndolo').
	language('ndm', '', individual, living, 'Ndam').
	language('ndn', '', individual, living, 'Ngundi').
	language('ndo', 'ng', individual, living, 'Ndonga').
	language('ndp', '', individual, living, 'Ndo').
	language('ndq', '', individual, living, 'Ndombe').
	language('ndr', '', individual, living, 'Ndoola').
	language('nds', '', individual, living, 'Low German').
	language('ndt', '', individual, living, 'Ndunga').
	language('ndu', '', individual, living, 'Dugun').
	language('ndv', '', individual, living, 'Ndut').
	language('ndw', '', individual, living, 'Ndobo').
	language('ndx', '', individual, living, 'Nduga').
	language('ndy', '', individual, living, 'Lutos').
	language('ndz', '', individual, living, 'Ndogo').
	language('nea', '', individual, living, 'Eastern Ngad''a').
	language('neb', '', individual, living, 'Toura (Côte d''Ivoire)').
	language('nec', '', individual, living, 'Nedebang').
	language('ned', '', individual, living, 'Nde-Gbite').
	language('nee', '', individual, living, 'Nêlêmwa-Nixumwak').
	language('nef', '', individual, living, 'Nefamese').
	language('neg', '', individual, living, 'Negidal').
	language('neh', '', individual, living, 'Nyenkha').
	language('nei', '', individual, special, 'Neo-Hittite').
	language('nej', '', individual, living, 'Neko').
	language('nek', '', individual, living, 'Neku').
	language('nem', '', individual, living, 'Nemi').
	language('nen', '', individual, living, 'Nengone').
	language('neo', '', individual, living, 'Ná-Meo').
	language('nep', 'ne', macrolanguage, living, 'Nepali (macrolanguage)').
	language('neq', '', individual, living, 'North Central Mixe').
	language('ner', '', individual, living, 'Yahadian').
	language('nes', '', individual, living, 'Bhoti Kinnauri').
	language('net', '', individual, living, 'Nete').
	language('neu', '', individual, constructed, 'Neo').
	language('nev', '', individual, living, 'Nyaheun').
	language('new', '', individual, living, 'Nepal Bhasa').
	language('nex', '', individual, living, 'Neme').
	language('ney', '', individual, living, 'Neyo').
	language('nez', '', individual, living, 'Nez Perce').
	language('nfa', '', individual, living, 'Dhao').
	language('nfd', '', individual, living, 'Ahwai').
	language('nfl', '', individual, living, 'Ayiwo').
	language('nfr', '', individual, living, 'Nafaanra').
	language('nfu', '', individual, living, 'Mfumte').
	language('nga', '', individual, living, 'Ngbaka').
	language('ngb', '', individual, living, 'Northern Ngbandi').
	language('ngc', '', individual, living, 'Ngombe (Democratic Republic of Congo)').
	language('ngd', '', individual, living, 'Ngando (Central African Republic)').
	language('nge', '', individual, living, 'Ngemba').
	language('ngg', '', individual, living, 'Ngbaka Manza').
	language('ngh', '', individual, living, 'Nǁng').
	language('ngi', '', individual, living, 'Ngizim').
	language('ngj', '', individual, living, 'Ngie').
	language('ngk', '', individual, living, 'Dalabon').
	language('ngl', '', individual, living, 'Lomwe').
	language('ngm', '', individual, living, 'Ngatik Men''s Creole').
	language('ngn', '', individual, living, 'Ngwo').
	language('ngp', '', individual, living, 'Ngulu').
	language('ngq', '', individual, living, 'Ngurimi').
	language('ngr', '', individual, living, 'Engdewu').
	language('ngs', '', individual, living, 'Gvoko').
	language('ngt', '', individual, living, 'Kriang').
	language('ngu', '', individual, living, 'Guerrero Nahuatl').
	language('ngv', '', individual, extinct, 'Nagumi').
	language('ngw', '', individual, living, 'Ngwaba').
	language('ngx', '', individual, living, 'Nggwahyi').
	language('ngy', '', individual, living, 'Tibea').
	language('ngz', '', individual, living, 'Ngungwel').
	language('nha', '', individual, living, 'Nhanda').
	language('nhb', '', individual, living, 'Beng').
	language('nhc', '', individual, extinct, 'Tabasco Nahuatl').
	language('nhd', '', individual, living, 'Chiripá').
	language('nhe', '', individual, living, 'Eastern Huasteca Nahuatl').
	language('nhf', '', individual, living, 'Nhuwala').
	language('nhg', '', individual, living, 'Tetelcingo Nahuatl').
	language('nhh', '', individual, living, 'Nahari').
	language('nhi', '', individual, living, 'Zacatlán-Ahuacatlán-Tepetzintla Nahuatl').
	language('nhk', '', individual, living, 'Isthmus-Cosoleacaque Nahuatl').
	language('nhm', '', individual, living, 'Morelos Nahuatl').
	language('nhn', '', individual, living, 'Central Nahuatl').
	language('nho', '', individual, living, 'Takuu').
	language('nhp', '', individual, living, 'Isthmus-Pajapan Nahuatl').
	language('nhq', '', individual, living, 'Huaxcaleca Nahuatl').
	language('nhr', '', individual, living, 'Naro').
	language('nht', '', individual, living, 'Ometepec Nahuatl').
	language('nhu', '', individual, living, 'Noone').
	language('nhv', '', individual, living, 'Temascaltepec Nahuatl').
	language('nhw', '', individual, living, 'Western Huasteca Nahuatl').
	language('nhx', '', individual, living, 'Isthmus-Mecayapan Nahuatl').
	language('nhy', '', individual, living, 'Northern Oaxaca Nahuatl').
	language('nhz', '', individual, living, 'Santa María La Alta Nahuatl').
	language('nia', '', individual, living, 'Nias').
	language('nib', '', individual, living, 'Nakame').
	language('nid', '', individual, extinct, 'Ngandi').
	language('nie', '', individual, living, 'Niellim').
	language('nif', '', individual, living, 'Nek').
	language('nig', '', individual, extinct, 'Ngalakgan').
	language('nih', '', individual, living, 'Nyiha (Tanzania)').
	language('nii', '', individual, living, 'Nii').
	language('nij', '', individual, living, 'Ngaju').
	language('nik', '', individual, living, 'Southern Nicobarese').
	language('nil', '', individual, living, 'Nila').
	language('nim', '', individual, living, 'Nilamba').
	language('nin', '', individual, living, 'Ninzo').
	language('nio', '', individual, living, 'Nganasan').
	language('niq', '', individual, living, 'Nandi').
	language('nir', '', individual, living, 'Nimboran').
	language('nis', '', individual, living, 'Nimi').
	language('nit', '', individual, living, 'Southeastern Kolami').
	language('niu', '', individual, living, 'Niuean').
	language('niv', '', individual, living, 'Gilyak').
	language('niw', '', individual, living, 'Nimo').
	language('nix', '', individual, living, 'Hema').
	language('niy', '', individual, living, 'Ngiti').
	language('niz', '', individual, living, 'Ningil').
	language('nja', '', individual, living, 'Nzanyi').
	language('njb', '', individual, living, 'Nocte Naga').
	language('njd', '', individual, living, 'Ndonde Hamba').
	language('njh', '', individual, living, 'Lotha Naga').
	language('nji', '', individual, living, 'Gudanji').
	language('njj', '', individual, living, 'Njen').
	language('njl', '', individual, living, 'Njalgulgule').
	language('njm', '', individual, living, 'Angami Naga').
	language('njn', '', individual, living, 'Liangmai Naga').
	language('njo', '', individual, living, 'Ao Naga').
	language('njr', '', individual, living, 'Njerep').
	language('njs', '', individual, living, 'Nisa').
	language('njt', '', individual, living, 'Ndyuka-Trio Pidgin').
	language('nju', '', individual, living, 'Ngadjunmaya').
	language('njx', '', individual, living, 'Kunyi').
	language('njy', '', individual, living, 'Njyem').
	language('njz', '', individual, living, 'Nyishi').
	language('nka', '', individual, living, 'Nkoya').
	language('nkb', '', individual, living, 'Khoibu Naga').
	language('nkc', '', individual, living, 'Nkongho').
	language('nkd', '', individual, living, 'Koireng').
	language('nke', '', individual, living, 'Duke').
	language('nkf', '', individual, living, 'Inpui Naga').
	language('nkg', '', individual, living, 'Nekgini').
	language('nkh', '', individual, living, 'Khezha Naga').
	language('nki', '', individual, living, 'Thangal Naga').
	language('nkj', '', individual, living, 'Nakai').
	language('nkk', '', individual, living, 'Nokuku').
	language('nkm', '', individual, living, 'Namat').
	language('nkn', '', individual, living, 'Nkangala').
	language('nko', '', individual, living, 'Nkonya').
	language('nkp', '', individual, extinct, 'Niuatoputapu').
	language('nkq', '', individual, living, 'Nkami').
	language('nkr', '', individual, living, 'Nukuoro').
	language('nks', '', individual, living, 'North Asmat').
	language('nkt', '', individual, living, 'Nyika (Tanzania)').
	language('nku', '', individual, living, 'Bouna Kulango').
	language('nkv', '', individual, living, 'Nyika (Malawi and Zambia)').
	language('nkw', '', individual, living, 'Nkutu').
	language('nkx', '', individual, living, 'Nkoroo').
	language('nkz', '', individual, living, 'Nkari').
	language('nla', '', individual, living, 'Ngombale').
	language('nlc', '', individual, living, 'Nalca').
	language('nld', 'nl', individual, living, 'Dutch').
	language('nle', '', individual, living, 'East Nyala').
	language('nlg', '', individual, living, 'Gela').
	language('nli', '', individual, living, 'Grangali').
	language('nlj', '', individual, living, 'Nyali').
	language('nlk', '', individual, living, 'Ninia Yali').
	language('nll', '', individual, living, 'Nihali').
	language('nlm', '', individual, living, 'Mankiyali').
	language('nlo', '', individual, living, 'Ngul').
	language('nlq', '', individual, living, 'Lao Naga').
	language('nlu', '', individual, living, 'Nchumbulu').
	language('nlv', '', individual, living, 'Orizaba Nahuatl').
	language('nlw', '', individual, extinct, 'Walangama').
	language('nlx', '', individual, living, 'Nahali').
	language('nly', '', individual, living, 'Nyamal').
	language('nlz', '', individual, living, 'Nalögo').
	language('nma', '', individual, living, 'Maram Naga').
	language('nmb', '', individual, living, 'Big Nambas').
	language('nmc', '', individual, living, 'Ngam').
	language('nmd', '', individual, living, 'Ndumu').
	language('nme', '', individual, living, 'Mzieme Naga').
	language('nmf', '', individual, living, 'Tangkhul Naga (India)').
	language('nmg', '', individual, living, 'Kwasio').
	language('nmh', '', individual, living, 'Monsang Naga').
	language('nmi', '', individual, living, 'Nyam').
	language('nmj', '', individual, living, 'Ngombe (Central African Republic)').
	language('nmk', '', individual, living, 'Namakura').
	language('nml', '', individual, living, 'Ndemli').
	language('nmm', '', individual, living, 'Manangba').
	language('nmn', '', individual, living, 'ǃXóõ').
	language('nmo', '', individual, living, 'Moyon Naga').
	language('nmp', '', individual, extinct, 'Nimanbur').
	language('nmq', '', individual, living, 'Nambya').
	language('nmr', '', individual, extinct, 'Nimbari').
	language('nms', '', individual, living, 'Letemboi').
	language('nmt', '', individual, living, 'Namonuito').
	language('nmu', '', individual, living, 'Northeast Maidu').
	language('nmv', '', individual, extinct, 'Ngamini').
	language('nmw', '', individual, living, 'Nimoa').
	language('nmx', '', individual, living, 'Nama (Papua New Guinea)').
	language('nmy', '', individual, living, 'Namuyi').
	language('nmz', '', individual, living, 'Nawdm').
	language('nna', '', individual, living, 'Nyangumarta').
	language('nnb', '', individual, living, 'Nande').
	language('nnc', '', individual, living, 'Nancere').
	language('nnd', '', individual, living, 'West Ambae').
	language('nne', '', individual, living, 'Ngandyera').
	language('nnf', '', individual, living, 'Ngaing').
	language('nng', '', individual, living, 'Maring Naga').
	language('nnh', '', individual, living, 'Ngiemboon').
	language('nni', '', individual, living, 'North Nuaulu').
	language('nnj', '', individual, living, 'Nyangatom').
	language('nnk', '', individual, living, 'Nankina').
	language('nnl', '', individual, living, 'Northern Rengma Naga').
	language('nnm', '', individual, living, 'Namia').
	language('nnn', '', individual, living, 'Ngete').
	language('nno', 'nn', individual, living, 'Norwegian Nynorsk').
	language('nnp', '', individual, living, 'Wancho Naga').
	language('nnq', '', individual, living, 'Ngindo').
	language('nnr', '', individual, extinct, 'Narungga').
	language('nnt', '', individual, extinct, 'Nanticoke').
	language('nnu', '', individual, living, 'Dwang').
	language('nnv', '', individual, extinct, 'Nugunu (Australia)').
	language('nnw', '', individual, living, 'Southern Nuni').
	language('nny', '', individual, extinct, 'Nyangga').
	language('nnz', '', individual, living, 'Nda''nda''').
	language('noa', '', individual, living, 'Woun Meu').
	language('nob', 'nb', individual, living, 'Norwegian Bokmål').
	language('noc', '', individual, living, 'Nuk').
	language('nod', '', individual, living, 'Northern Thai').
	language('noe', '', individual, living, 'Nimadi').
	language('nof', '', individual, living, 'Nomane').
	language('nog', '', individual, living, 'Nogai').
	language('noh', '', individual, living, 'Nomu').
	language('noi', '', individual, living, 'Noiri').
	language('noj', '', individual, living, 'Nonuya').
	language('nok', '', individual, extinct, 'Nooksack').
	language('nol', '', individual, extinct, 'Nomlaki').
	language('non', '', individual, special, 'Old Norse').
	language('nop', '', individual, living, 'Numanggang').
	language('noq', '', individual, living, 'Ngongo').
	language('nor', 'no', macrolanguage, living, 'Norwegian').
	language('nos', '', individual, living, 'Eastern Nisu').
	language('not', '', individual, living, 'Nomatsiguenga').
	language('nou', '', individual, living, 'Ewage-Notu').
	language('nov', '', individual, constructed, 'Novial').
	language('now', '', individual, living, 'Nyambo').
	language('noy', '', individual, living, 'Noy').
	language('noz', '', individual, living, 'Nayi').
	language('npa', '', individual, living, 'Nar Phu').
	language('npb', '', individual, living, 'Nupbikha').
	language('npg', '', individual, living, 'Ponyo-Gongwang Naga').
	language('nph', '', individual, living, 'Phom Naga').
	language('npi', '', individual, living, 'Nepali (individual language)').
	language('npl', '', individual, living, 'Southeastern Puebla Nahuatl').
	language('npn', '', individual, living, 'Mondropolon').
	language('npo', '', individual, living, 'Pochuri Naga').
	language('nps', '', individual, living, 'Nipsan').
	language('npu', '', individual, living, 'Puimei Naga').
	language('npx', '', individual, living, 'Noipx').
	language('npy', '', individual, living, 'Napu').
	language('nqg', '', individual, living, 'Southern Nago').
	language('nqk', '', individual, living, 'Kura Ede Nago').
	language('nql', '', individual, living, 'Ngendelengo').
	language('nqm', '', individual, living, 'Ndom').
	language('nqn', '', individual, living, 'Nen').
	language('nqo', '', individual, living, 'N''Ko').
	language('nqq', '', individual, living, 'Kyan-Karyaw Naga').
	language('nqt', '', individual, living, 'Nteng').
	language('nqy', '', individual, living, 'Akyaung Ari Naga').
	language('nra', '', individual, living, 'Ngom').
	language('nrb', '', individual, living, 'Nara').
	language('nrc', '', individual, special, 'Noric').
	language('nre', '', individual, living, 'Southern Rengma Naga').
	language('nrf', '', individual, living, 'Jèrriais').
	language('nrg', '', individual, living, 'Narango').
	language('nri', '', individual, living, 'Chokri Naga').
	language('nrk', '', individual, living, 'Ngarla').
	language('nrl', '', individual, living, 'Ngarluma').
	language('nrm', '', individual, living, 'Narom').
	language('nrn', '', individual, extinct, 'Norn').
	language('nrp', '', individual, special, 'North Picene').
	language('nrr', '', individual, extinct, 'Norra').
	language('nrt', '', individual, extinct, 'Northern Kalapuya').
	language('nru', '', individual, living, 'Narua').
	language('nrx', '', individual, extinct, 'Ngurmbur').
	language('nrz', '', individual, living, 'Lala').
	language('nsa', '', individual, living, 'Sangtam Naga').
	language('nsb', '', individual, extinct, 'Lower Nossob').
	language('nsc', '', individual, living, 'Nshi').
	language('nsd', '', individual, living, 'Southern Nisu').
	language('nse', '', individual, living, 'Nsenga').
	language('nsf', '', individual, living, 'Northwestern Nisu').
	language('nsg', '', individual, living, 'Ngasa').
	language('nsh', '', individual, living, 'Ngoshie').
	language('nsi', '', individual, living, 'Nigerian Sign Language').
	language('nsk', '', individual, living, 'Naskapi').
	language('nsl', '', individual, living, 'Norwegian Sign Language').
	language('nsm', '', individual, living, 'Sumi Naga').
	language('nsn', '', individual, living, 'Nehan').
	language('nso', '', individual, living, 'Pedi').
	language('nsp', '', individual, living, 'Nepalese Sign Language').
	language('nsq', '', individual, living, 'Northern Sierra Miwok').
	language('nsr', '', individual, living, 'Maritime Sign Language').
	language('nss', '', individual, living, 'Nali').
	language('nst', '', individual, living, 'Tase Naga').
	language('nsu', '', individual, living, 'Sierra Negra Nahuatl').
	language('nsv', '', individual, living, 'Southwestern Nisu').
	language('nsw', '', individual, living, 'Navut').
	language('nsx', '', individual, living, 'Nsongo').
	language('nsy', '', individual, living, 'Nasal').
	language('nsz', '', individual, living, 'Nisenan').
	language('ntd', '', individual, living, 'Northern Tidung').
	language('ntg', '', individual, extinct, 'Ngantangarra').
	language('nti', '', individual, living, 'Natioro').
	language('ntj', '', individual, living, 'Ngaanyatjarra').
	language('ntk', '', individual, living, 'Ikoma-Nata-Isenye').
	language('ntm', '', individual, living, 'Nateni').
	language('nto', '', individual, living, 'Ntomba').
	language('ntp', '', individual, living, 'Northern Tepehuan').
	language('ntr', '', individual, living, 'Delo').
	language('ntu', '', individual, living, 'Natügu').
	language('ntw', '', individual, extinct, 'Nottoway').
	language('ntx', '', individual, living, 'Tangkhul Naga (Myanmar)').
	language('nty', '', individual, living, 'Mantsi').
	language('ntz', '', individual, living, 'Natanzi').
	language('nua', '', individual, living, 'Yuanga').
	language('nuc', '', individual, extinct, 'Nukuini').
	language('nud', '', individual, living, 'Ngala').
	language('nue', '', individual, living, 'Ngundu').
	language('nuf', '', individual, living, 'Nusu').
	language('nug', '', individual, extinct, 'Nungali').
	language('nuh', '', individual, living, 'Ndunda').
	language('nui', '', individual, living, 'Ngumbi').
	language('nuj', '', individual, living, 'Nyole').
	language('nuk', '', individual, living, 'Nuu-chah-nulth').
	language('nul', '', individual, extinct, 'Nusa Laut').
	language('num', '', individual, living, 'Niuafo''ou').
	language('nun', '', individual, living, 'Anong').
	language('nuo', '', individual, living, 'Nguôn').
	language('nup', '', individual, living, 'Nupe-Nupe-Tako').
	language('nuq', '', individual, living, 'Nukumanu').
	language('nur', '', individual, living, 'Nukuria').
	language('nus', '', individual, living, 'Nuer').
	language('nut', '', individual, living, 'Nung (Viet Nam)').
	language('nuu', '', individual, living, 'Ngbundu').
	language('nuv', '', individual, living, 'Northern Nuni').
	language('nuw', '', individual, living, 'Nguluwan').
	language('nux', '', individual, living, 'Mehek').
	language('nuy', '', individual, living, 'Nunggubuyu').
	language('nuz', '', individual, living, 'Tlamacazapa Nahuatl').
	language('nvh', '', individual, living, 'Nasarian').
	language('nvm', '', individual, living, 'Namiae').
	language('nvo', '', individual, living, 'Nyokon').
	language('nwa', '', individual, extinct, 'Nawathinehena').
	language('nwb', '', individual, living, 'Nyabwa').
	language('nwc', '', individual, special, 'Classical Newari').
	language('nwe', '', individual, living, 'Ngwe').
	language('nwg', '', individual, extinct, 'Ngayawung').
	language('nwi', '', individual, living, 'Southwest Tanna').
	language('nwm', '', individual, living, 'Nyamusa-Molo').
	language('nwo', '', individual, extinct, 'Nauo').
	language('nwr', '', individual, living, 'Nawaru').
	language('nww', '', individual, living, 'Ndwewe').
	language('nwx', '', individual, special, 'Middle Newar').
	language('nwy', '', individual, extinct, 'Nottoway-Meherrin').
	language('nxa', '', individual, living, 'Nauete').
	language('nxd', '', individual, living, 'Ngando (Democratic Republic of Congo)').
	language('nxe', '', individual, living, 'Nage').
	language('nxg', '', individual, living, 'Ngad''a').
	language('nxi', '', individual, living, 'Nindi').
	language('nxk', '', individual, living, 'Koki Naga').
	language('nxl', '', individual, living, 'South Nuaulu').
	language('nxm', '', individual, special, 'Numidian').
	language('nxn', '', individual, extinct, 'Ngawun').
	language('nxo', '', individual, living, 'Ndambomo').
	language('nxq', '', individual, living, 'Naxi').
	language('nxr', '', individual, living, 'Ninggerum').
	language('nxx', '', individual, living, 'Nafri').
	language('nya', 'ny', individual, living, 'Chichewa').
	language('nyb', '', individual, living, 'Nyangbo').
	language('nyc', '', individual, living, 'Nyanga-li').
	language('nyd', '', individual, living, 'Nyore').
	language('nye', '', individual, living, 'Nyengo').
	language('nyf', '', individual, living, 'Giryama').
	language('nyg', '', individual, living, 'Nyindu').
	language('nyh', '', individual, living, 'Nyikina').
	language('nyi', '', individual, living, 'Ama (Sudan)').
	language('nyj', '', individual, living, 'Nyanga').
	language('nyk', '', individual, living, 'Nyaneka').
	language('nyl', '', individual, living, 'Nyeu').
	language('nym', '', individual, living, 'Nyamwezi').
	language('nyn', '', individual, living, 'Nyankole').
	language('nyo', '', individual, living, 'Nyoro').
	language('nyp', '', individual, extinct, 'Nyang''i').
	language('nyq', '', individual, living, 'Nayini').
	language('nyr', '', individual, living, 'Nyiha (Malawi)').
	language('nys', '', individual, living, 'Nyungar').
	language('nyt', '', individual, extinct, 'Nyawaygi').
	language('nyu', '', individual, living, 'Nyungwe').
	language('nyv', '', individual, extinct, 'Nyulnyul').
	language('nyw', '', individual, living, 'Nyaw').
	language('nyx', '', individual, extinct, 'Nganyaywana').
	language('nyy', '', individual, living, 'Nyakyusa-Ngonde').
	language('nza', '', individual, living, 'Tigon Mbembe').
	language('nzb', '', individual, living, 'Njebi').
	language('nzd', '', individual, living, 'Nzadi').
	language('nzi', '', individual, living, 'Nzima').
	language('nzk', '', individual, living, 'Nzakara').
	language('nzm', '', individual, living, 'Zeme Naga').
	language('nzr', '', individual, living, 'Dir-Nyamzak-Mbarimi').
	language('nzs', '', individual, living, 'New Zealand Sign Language').
	language('nzu', '', individual, living, 'Teke-Nzikou').
	language('nzy', '', individual, living, 'Nzakambay').
	language('nzz', '', individual, living, 'Nanga Dama Dogon').
	language('oaa', '', individual, living, 'Orok').
	language('oac', '', individual, living, 'Oroch').
	language('oak', '', individual, living, 'Noakhali').
	language('oar', '', individual, special, 'Old Aramaic (up to 700 BCE)').
	language('oav', '', individual, special, 'Old Avar').
	language('obi', '', individual, extinct, 'Obispeño').
	language('obk', '', individual, living, 'Southern Bontok').
	language('obl', '', individual, living, 'Oblo').
	language('obm', '', individual, special, 'Moabite').
	language('obo', '', individual, living, 'Obo Manobo').
	language('obr', '', individual, special, 'Old Burmese').
	language('obt', '', individual, special, 'Old Breton').
	language('obu', '', individual, living, 'Obulom').
	language('oca', '', individual, living, 'Ocaina').
	language('och', '', individual, special, 'Old Chinese').
	language('oci', 'oc', individual, living, 'Occitan (post 1500)').
	language('ocm', '', individual, special, 'Old Cham').
	language('oco', '', individual, special, 'Old Cornish').
	language('ocu', '', individual, living, 'Atzingo Matlatzinca').
	language('oda', '', individual, living, 'Odut').
	language('odk', '', individual, living, 'Od').
	language('odt', '', individual, special, 'Old Dutch').
	language('odu', '', individual, living, 'Odual').
	language('ofo', '', individual, extinct, 'Ofo').
	language('ofs', '', individual, special, 'Old Frisian').
	language('ofu', '', individual, living, 'Efutop').
	language('ogb', '', individual, living, 'Ogbia').
	language('ogc', '', individual, living, 'Ogbah').
	language('oge', '', individual, special, 'Old Georgian').
	language('ogg', '', individual, living, 'Ogbogolo').
	language('ogo', '', individual, living, 'Khana').
	language('ogu', '', individual, living, 'Ogbronuagum').
	language('oht', '', individual, special, 'Old Hittite').
	language('ohu', '', individual, special, 'Old Hungarian').
	language('oia', '', individual, living, 'Oirata').
	language('oie', '', individual, living, 'Okolie').
	language('oin', '', individual, living, 'Inebu One').
	language('ojb', '', individual, living, 'Northwestern Ojibwa').
	language('ojc', '', individual, living, 'Central Ojibwa').
	language('ojg', '', individual, living, 'Eastern Ojibwa').
	language('oji', 'oj', macrolanguage, living, 'Ojibwa').
	language('ojp', '', individual, special, 'Old Japanese').
	language('ojs', '', individual, living, 'Severn Ojibwa').
	language('ojv', '', individual, living, 'Ontong Java').
	language('ojw', '', individual, living, 'Western Ojibwa').
	language('oka', '', individual, living, 'Okanagan').
	language('okb', '', individual, living, 'Okobo').
	language('okc', '', individual, living, 'Kobo').
	language('okd', '', individual, living, 'Okodia').
	language('oke', '', individual, living, 'Okpe (Southwestern Edo)').
	language('okg', '', individual, extinct, 'Koko Babangk').
	language('okh', '', individual, living, 'Koresh-e Rostam').
	language('oki', '', individual, living, 'Okiek').
	language('okj', '', individual, extinct, 'Oko-Juwoi').
	language('okk', '', individual, living, 'Kwamtim One').
	language('okl', '', individual, extinct, 'Old Kentish Sign Language').
	language('okm', '', individual, special, 'Middle Korean (10th-16th cent.)').
	language('okn', '', individual, living, 'Oki-No-Erabu').
	language('oko', '', individual, special, 'Old Korean (3rd-9th cent.)').
	language('okr', '', individual, living, 'Kirike').
	language('oks', '', individual, living, 'Oko-Eni-Osayen').
	language('oku', '', individual, living, 'Oku').
	language('okv', '', individual, living, 'Orokaiva').
	language('okx', '', individual, living, 'Okpe (Northwestern Edo)').
	language('okz', '', individual, special, 'Old Khmer').
	language('ola', '', individual, living, 'Walungge').
	language('olb', '', individual, living, 'Oli-Bodiman').
	language('old', '', individual, living, 'Mochi').
	language('ole', '', individual, living, 'Olekha').
	language('olk', '', individual, extinct, 'Olkol').
	language('olm', '', individual, living, 'Oloma').
	language('olo', '', individual, living, 'Livvi').
	language('olr', '', individual, living, 'Olrat').
	language('olt', '', individual, special, 'Old Lithuanian').
	language('olu', '', individual, living, 'Kuvale').
	language('oma', '', individual, living, 'Omaha-Ponca').
	language('omb', '', individual, living, 'East Ambae').
	language('omc', '', individual, extinct, 'Mochica').
	language('omg', '', individual, living, 'Omagua').
	language('omi', '', individual, living, 'Omi').
	language('omk', '', individual, extinct, 'Omok').
	language('oml', '', individual, living, 'Ombo').
	language('omn', '', individual, special, 'Minoan').
	language('omo', '', individual, living, 'Utarmbung').
	language('omp', '', individual, special, 'Old Manipuri').
	language('omr', '', individual, special, 'Old Marathi').
	language('omt', '', individual, living, 'Omotik').
	language('omu', '', individual, extinct, 'Omurano').
	language('omw', '', individual, living, 'South Tairora').
	language('omx', '', individual, special, 'Old Mon').
	language('omy', '', individual, special, 'Old Malay').
	language('ona', '', individual, living, 'Ona').
	language('onb', '', individual, living, 'Lingao').
	language('one', '', individual, living, 'Oneida').
	language('ong', '', individual, living, 'Olo').
	language('oni', '', individual, living, 'Onin').
	language('onj', '', individual, living, 'Onjob').
	language('onk', '', individual, living, 'Kabore One').
	language('onn', '', individual, living, 'Onobasulu').
	language('ono', '', individual, living, 'Onondaga').
	language('onp', '', individual, living, 'Sartang').
	language('onr', '', individual, living, 'Northern One').
	language('ons', '', individual, living, 'Ono').
	language('ont', '', individual, living, 'Ontenu').
	language('onu', '', individual, living, 'Unua').
	language('onw', '', individual, special, 'Old Nubian').
	language('onx', '', individual, living, 'Onin Based Pidgin').
	language('ood', '', individual, living, 'Tohono O''odham').
	language('oog', '', individual, living, 'Ong').
	language('oon', '', individual, living, 'Önge').
	language('oor', '', individual, living, 'Oorlams').
	language('oos', '', individual, special, 'Old Ossetic').
	language('opa', '', individual, living, 'Okpamheri').
	language('opk', '', individual, living, 'Kopkaka').
	language('opm', '', individual, living, 'Oksapmin').
	language('opo', '', individual, living, 'Opao').
	language('opt', '', individual, extinct, 'Opata').
	language('opy', '', individual, living, 'Ofayé').
	language('ora', '', individual, living, 'Oroha').
	language('orc', '', individual, living, 'Orma').
	language('ore', '', individual, living, 'Orejón').
	language('org', '', individual, living, 'Oring').
	language('orh', '', individual, living, 'Oroqen').
	language('ori', 'or', macrolanguage, living, 'Oriya (macrolanguage)').
	language('orm', 'om', macrolanguage, living, 'Oromo').
	language('orn', '', individual, living, 'Orang Kanaq').
	language('oro', '', individual, living, 'Orokolo').
	language('orr', '', individual, living, 'Oruma').
	language('ors', '', individual, living, 'Orang Seletar').
	language('ort', '', individual, living, 'Adivasi Oriya').
	language('oru', '', individual, living, 'Ormuri').
	language('orv', '', individual, special, 'Old Russian').
	language('orw', '', individual, living, 'Oro Win').
	language('orx', '', individual, living, 'Oro').
	language('ory', '', individual, living, 'Odia').
	language('orz', '', individual, living, 'Ormu').
	language('osa', '', individual, living, 'Osage').
	language('osc', '', individual, special, 'Oscan').
	language('osd', '', individual, living, 'Digor Ossetic').
	language('osi', '', individual, living, 'Osing').
	language('osn', '', individual, special, 'Old Sundanese').
	language('oso', '', individual, living, 'Ososo').
	language('osp', '', individual, special, 'Old Spanish').
	language('oss', 'os', individual, living, 'Iron Ossetic').
	language('ost', '', individual, living, 'Osatu').
	language('osu', '', individual, living, 'Southern One').
	language('osx', '', individual, special, 'Old Saxon').
	language('ota', '', individual, special, 'Ottoman Turkish (1500-1928)').
	language('otb', '', individual, special, 'Old Tibetan').
	language('otd', '', individual, living, 'Ot Danum').
	language('ote', '', individual, living, 'Mezquital Otomi').
	language('oti', '', individual, extinct, 'Oti').
	language('otk', '', individual, special, 'Old Turkish').
	language('otl', '', individual, living, 'Tilapa Otomi').
	language('otm', '', individual, living, 'Eastern Highland Otomi').
	language('otn', '', individual, living, 'Tenango Otomi').
	language('otq', '', individual, living, 'Querétaro Otomi').
	language('otr', '', individual, living, 'Otoro').
	language('ots', '', individual, living, 'Estado de México Otomi').
	language('ott', '', individual, living, 'Temoaya Otomi').
	language('otu', '', individual, extinct, 'Otuke').
	language('otw', '', individual, living, 'Ottawa').
	language('otx', '', individual, living, 'Texcatepec Otomi').
	language('oty', '', individual, special, 'Old Tamil').
	language('otz', '', individual, living, 'Ixtenco Otomi').
	language('oua', '', individual, living, 'Tagargrent').
	language('oub', '', individual, living, 'Glio-Oubi').
	language('oue', '', individual, living, 'Oune').
	language('oui', '', individual, special, 'Old Uighur').
	language('oum', '', individual, extinct, 'Ouma').
	language('ovd', '', individual, living, 'Elfdalian').
	language('owi', '', individual, living, 'Owiniga').
	language('owl', '', individual, special, 'Old Welsh').
	language('oyb', '', individual, living, 'Oy').
	language('oyd', '', individual, living, 'Oyda').
	language('oym', '', individual, living, 'Wayampi').
	language('oyy', '', individual, living, 'Oya''oya').
	language('ozm', '', individual, living, 'Koonzime').
	language('pab', '', individual, living, 'Parecís').
	language('pac', '', individual, living, 'Pacoh').
	language('pad', '', individual, living, 'Paumarí').
	language('pae', '', individual, living, 'Pagibete').
	language('paf', '', individual, extinct, 'Paranawát').
	language('pag', '', individual, living, 'Pangasinan').
	language('pah', '', individual, living, 'Tenharim').
	language('pai', '', individual, living, 'Pe').
	language('pak', '', individual, living, 'Parakanã').
	language('pal', '', individual, special, 'Pahlavi').
	language('pam', '', individual, living, 'Pampanga').
	language('pan', 'pa', individual, living, 'Panjabi').
	language('pao', '', individual, living, 'Northern Paiute').
	language('pap', '', individual, living, 'Papiamento').
	language('paq', '', individual, living, 'Parya').
	language('par', '', individual, living, 'Panamint').
	language('pas', '', individual, living, 'Papasena').
	language('pau', '', individual, living, 'Palauan').
	language('pav', '', individual, living, 'Pakaásnovos').
	language('paw', '', individual, living, 'Pawnee').
	language('pax', '', individual, extinct, 'Pankararé').
	language('pay', '', individual, living, 'Pech').
	language('paz', '', individual, extinct, 'Pankararú').
	language('pbb', '', individual, living, 'Páez').
	language('pbc', '', individual, living, 'Patamona').
	language('pbe', '', individual, living, 'Mezontla Popoloca').
	language('pbf', '', individual, living, 'Coyotepec Popoloca').
	language('pbg', '', individual, extinct, 'Paraujano').
	language('pbh', '', individual, living, 'E''ñapa Woromaipu').
	language('pbi', '', individual, living, 'Parkwa').
	language('pbl', '', individual, living, 'Mak (Nigeria)').
	language('pbm', '', individual, living, 'Puebla Mazatec').
	language('pbn', '', individual, living, 'Kpasam').
	language('pbo', '', individual, living, 'Papel').
	language('pbp', '', individual, living, 'Badyara').
	language('pbr', '', individual, living, 'Pangwa').
	language('pbs', '', individual, living, 'Central Pame').
	language('pbt', '', individual, living, 'Southern Pashto').
	language('pbu', '', individual, living, 'Northern Pashto').
	language('pbv', '', individual, living, 'Pnar').
	language('pby', '', individual, living, 'Pyu (Papua New Guinea)').
	language('pca', '', individual, living, 'Santa Inés Ahuatempan Popoloca').
	language('pcb', '', individual, living, 'Pear').
	language('pcc', '', individual, living, 'Bouyei').
	language('pcd', '', individual, living, 'Picard').
	language('pce', '', individual, living, 'Ruching Palaung').
	language('pcf', '', individual, living, 'Paliyan').
	language('pcg', '', individual, living, 'Paniya').
	language('pch', '', individual, living, 'Pardhan').
	language('pci', '', individual, living, 'Duruwa').
	language('pcj', '', individual, living, 'Parenga').
	language('pck', '', individual, living, 'Paite Chin').
	language('pcl', '', individual, living, 'Pardhi').
	language('pcm', '', individual, living, 'Nigerian Pidgin').
	language('pcn', '', individual, living, 'Piti').
	language('pcp', '', individual, living, 'Pacahuara').
	language('pcw', '', individual, living, 'Pyapun').
	language('pda', '', individual, living, 'Anam').
	language('pdc', '', individual, living, 'Pennsylvania German').
	language('pdi', '', individual, living, 'Pa Di').
	language('pdn', '', individual, living, 'Podena').
	language('pdo', '', individual, living, 'Padoe').
	language('pdt', '', individual, living, 'Plautdietsch').
	language('pdu', '', individual, living, 'Kayan').
	language('pea', '', individual, living, 'Peranakan Indonesian').
	language('peb', '', individual, extinct, 'Eastern Pomo').
	language('ped', '', individual, living, 'Mala (Papua New Guinea)').
	language('pee', '', individual, living, 'Taje').
	language('pef', '', individual, extinct, 'Northeastern Pomo').
	language('peg', '', individual, living, 'Pengo').
	language('peh', '', individual, living, 'Bonan').
	language('pei', '', individual, living, 'Chichimeca-Jonaz').
	language('pej', '', individual, extinct, 'Northern Pomo').
	language('pek', '', individual, living, 'Penchal').
	language('pel', '', individual, living, 'Pekal').
	language('pem', '', individual, living, 'Phende').
	language('peo', '', individual, special, 'Old Persian (ca. 600-400 B.C.)').
	language('pep', '', individual, living, 'Kunja').
	language('peq', '', individual, living, 'Southern Pomo').
	language('pes', '', individual, living, 'Iranian Persian').
	language('pev', '', individual, living, 'Pémono').
	language('pex', '', individual, living, 'Petats').
	language('pey', '', individual, living, 'Petjo').
	language('pez', '', individual, living, 'Eastern Penan').
	language('pfa', '', individual, living, 'Pááfang').
	language('pfe', '', individual, living, 'Pere').
	language('pfl', '', individual, living, 'Pfaelzisch').
	language('pga', '', individual, living, 'Sudanese Creole Arabic').
	language('pgd', '', individual, special, 'Gāndhārī').
	language('pgg', '', individual, living, 'Pangwali').
	language('pgi', '', individual, living, 'Pagi').
	language('pgk', '', individual, living, 'Rerep').
	language('pgl', '', individual, special, 'Primitive Irish').
	language('pgn', '', individual, special, 'Paelignian').
	language('pgs', '', individual, living, 'Pangseng').
	language('pgu', '', individual, living, 'Pagu').
	language('pgz', '', individual, living, 'Papua New Guinean Sign Language').
	language('pha', '', individual, living, 'Pa-Hng').
	language('phd', '', individual, living, 'Phudagi').
	language('phg', '', individual, living, 'Phuong').
	language('phh', '', individual, living, 'Phukha').
	language('phj', '', individual, living, 'Pahari').
	language('phk', '', individual, living, 'Phake').
	language('phl', '', individual, living, 'Phalura').
	language('phm', '', individual, living, 'Phimbi').
	language('phn', '', individual, special, 'Phoenician').
	language('pho', '', individual, living, 'Phunoi').
	language('phq', '', individual, living, 'Phana''').
	language('phr', '', individual, living, 'Pahari-Potwari').
	language('pht', '', individual, living, 'Phu Thai').
	language('phu', '', individual, living, 'Phuan').
	language('phv', '', individual, living, 'Pahlavani').
	language('phw', '', individual, living, 'Phangduwali').
	language('pia', '', individual, living, 'Pima Bajo').
	language('pib', '', individual, living, 'Yine').
	language('pic', '', individual, living, 'Pinji').
	language('pid', '', individual, living, 'Piaroa').
	language('pie', '', individual, extinct, 'Piro').
	language('pif', '', individual, living, 'Pingelapese').
	language('pig', '', individual, living, 'Pisabo').
	language('pih', '', individual, living, 'Pitcairn-Norfolk').
	language('pij', '', individual, extinct, 'Pijao').
	language('pil', '', individual, living, 'Yom').
	language('pim', '', individual, extinct, 'Powhatan').
	language('pin', '', individual, living, 'Piame').
	language('pio', '', individual, living, 'Piapoco').
	language('pip', '', individual, living, 'Pero').
	language('pir', '', individual, living, 'Piratapuyo').
	language('pis', '', individual, living, 'Pijin').
	language('pit', '', individual, extinct, 'Pitta Pitta').
	language('piu', '', individual, living, 'Pintupi-Luritja').
	language('piv', '', individual, living, 'Pileni').
	language('piw', '', individual, living, 'Pimbwe').
	language('pix', '', individual, living, 'Piu').
	language('piy', '', individual, living, 'Piya-Kwonci').
	language('piz', '', individual, living, 'Pije').
	language('pjt', '', individual, living, 'Pitjantjatjara').
	language('pka', '', individual, special, 'Ardhamāgadhī Prākrit').
	language('pkb', '', individual, living, 'Pokomo').
	language('pkc', '', individual, special, 'Paekche').
	language('pkg', '', individual, living, 'Pak-Tong').
	language('pkh', '', individual, living, 'Pankhu').
	language('pkn', '', individual, living, 'Pakanha').
	language('pko', '', individual, living, 'Pökoot').
	language('pkp', '', individual, living, 'Pukapuka').
	language('pkr', '', individual, living, 'Attapady Kurumba').
	language('pks', '', individual, living, 'Pakistan Sign Language').
	language('pkt', '', individual, living, 'Maleng').
	language('pku', '', individual, living, 'Paku').
	language('pla', '', individual, living, 'Miani').
	language('plb', '', individual, living, 'Polonombauk').
	language('plc', '', individual, living, 'Central Palawano').
	language('pld', '', individual, living, 'Polari').
	language('ple', '', individual, living, 'Palu''e').
	language('plg', '', individual, living, 'Pilagá').
	language('plh', '', individual, living, 'Paulohi').
	language('pli', 'pi', individual, special, 'Pali').
	language('plk', '', individual, living, 'Kohistani Shina').
	language('pll', '', individual, living, 'Shwe Palaung').
	language('pln', '', individual, living, 'Palenquero').
	language('plo', '', individual, living, 'Oluta Popoluca').
	language('plq', '', individual, special, 'Palaic').
	language('plr', '', individual, living, 'Palaka Senoufo').
	language('pls', '', individual, living, 'San Marcos Tlacoyalco Popoloca').
	language('plt', '', individual, living, 'Plateau Malagasy').
	language('plu', '', individual, living, 'Palikúr').
	language('plv', '', individual, living, 'Southwest Palawano').
	language('plw', '', individual, living, 'Brooke''s Point Palawano').
	language('ply', '', individual, living, 'Bolyu').
	language('plz', '', individual, living, 'Paluan').
	language('pma', '', individual, living, 'Paama').
	language('pmb', '', individual, living, 'Pambia').
	language('pmd', '', individual, extinct, 'Pallanganmiddang').
	language('pme', '', individual, living, 'Pwaamei').
	language('pmf', '', individual, living, 'Pamona').
	language('pmh', '', individual, special, 'Māhārāṣṭri Prākrit').
	language('pmi', '', individual, living, 'Northern Pumi').
	language('pmj', '', individual, living, 'Southern Pumi').
	language('pml', '', individual, extinct, 'Lingua Franca').
	language('pmm', '', individual, living, 'Pomo').
	language('pmn', '', individual, living, 'Pam').
	language('pmo', '', individual, living, 'Pom').
	language('pmq', '', individual, living, 'Northern Pame').
	language('pmr', '', individual, living, 'Paynamar').
	language('pms', '', individual, living, 'Piemontese').
	language('pmt', '', individual, living, 'Tuamotuan').
	language('pmw', '', individual, living, 'Plains Miwok').
	language('pmx', '', individual, living, 'Poumei Naga').
	language('pmy', '', individual, living, 'Papuan Malay').
	language('pmz', '', individual, extinct, 'Southern Pame').
	language('pna', '', individual, living, 'Punan Bah-Biau').
	language('pnb', '', individual, living, 'Western Panjabi').
	language('pnc', '', individual, living, 'Pannei').
	language('pnd', '', individual, living, 'Mpinda').
	language('pne', '', individual, living, 'Western Penan').
	language('png', '', individual, living, 'Pangu').
	language('pnh', '', individual, living, 'Penrhyn').
	language('pni', '', individual, living, 'Aoheng').
	language('pnj', '', individual, extinct, 'Pinjarup').
	language('pnk', '', individual, living, 'Paunaka').
	language('pnl', '', individual, living, 'Paleni').
	language('pnm', '', individual, living, 'Punan Batu 1').
	language('pnn', '', individual, living, 'Pinai-Hagahai').
	language('pno', '', individual, extinct, 'Panobo').
	language('pnp', '', individual, living, 'Pancana').
	language('pnq', '', individual, living, 'Pana (Burkina Faso)').
	language('pnr', '', individual, living, 'Panim').
	language('pns', '', individual, living, 'Ponosakan').
	language('pnt', '', individual, living, 'Pontic').
	language('pnu', '', individual, living, 'Jiongnai Bunu').
	language('pnv', '', individual, living, 'Pinigura').
	language('pnw', '', individual, living, 'Banyjima').
	language('pnx', '', individual, living, 'Phong-Kniang').
	language('pny', '', individual, living, 'Pinyin').
	language('pnz', '', individual, living, 'Pana (Central African Republic)').
	language('poc', '', individual, living, 'Poqomam').
	language('poe', '', individual, living, 'San Juan Atzingo Popoloca').
	language('pof', '', individual, living, 'Poke').
	language('pog', '', individual, extinct, 'Potiguára').
	language('poh', '', individual, living, 'Poqomchi''').
	language('poi', '', individual, living, 'Highland Popoluca').
	language('pok', '', individual, living, 'Pokangá').
	language('pol', 'pl', individual, living, 'Polish').
	language('pom', '', individual, living, 'Southeastern Pomo').
	language('pon', '', individual, living, 'Pohnpeian').
	language('poo', '', individual, extinct, 'Central Pomo').
	language('pop', '', individual, living, 'Pwapwâ').
	language('poq', '', individual, living, 'Texistepec Popoluca').
	language('por', 'pt', individual, living, 'Portuguese').
	language('pos', '', individual, living, 'Sayula Popoluca').
	language('pot', '', individual, living, 'Potawatomi').
	language('pov', '', individual, living, 'Upper Guinea Crioulo').
	language('pow', '', individual, living, 'San Felipe Otlaltepec Popoloca').
	language('pox', '', individual, extinct, 'Polabian').
	language('poy', '', individual, living, 'Pogolo').
	language('ppe', '', individual, living, 'Papi').
	language('ppi', '', individual, living, 'Paipai').
	language('ppk', '', individual, living, 'Uma').
	language('ppl', '', individual, living, 'Pipil').
	language('ppm', '', individual, living, 'Papuma').
	language('ppn', '', individual, living, 'Papapana').
	language('ppo', '', individual, living, 'Folopa').
	language('ppp', '', individual, living, 'Pelende').
	language('ppq', '', individual, living, 'Pei').
	language('pps', '', individual, living, 'San Luís Temalacayuca Popoloca').
	language('ppt', '', individual, living, 'Pare').
	language('ppu', '', individual, extinct, 'Papora').
	language('pqa', '', individual, living, 'Pa''a').
	language('pqm', '', individual, living, 'Malecite-Passamaquoddy').
	language('prc', '', individual, living, 'Parachi').
	language('prd', '', individual, living, 'Parsi-Dari').
	language('pre', '', individual, living, 'Principense').
	language('prf', '', individual, living, 'Paranan').
	language('prg', '', individual, living, 'Prussian').
	language('prh', '', individual, living, 'Porohanon').
	language('pri', '', individual, living, 'Paicî').
	language('prk', '', individual, living, 'Parauk').
	language('prl', '', individual, living, 'Peruvian Sign Language').
	language('prm', '', individual, living, 'Kibiri').
	language('prn', '', individual, living, 'Prasuni').
	language('pro', '', individual, special, 'Old Provençal (to 1500)').
	language('prq', '', individual, living, 'Ashéninka Perené').
	language('prr', '', individual, extinct, 'Puri').
	language('prs', '', individual, living, 'Dari').
	language('prt', '', individual, living, 'Phai').
	language('pru', '', individual, living, 'Puragi').
	language('prw', '', individual, living, 'Parawen').
	language('prx', '', individual, living, 'Purik').
	language('prz', '', individual, living, 'Providencia Sign Language').
	language('psa', '', individual, living, 'Asue Awyu').
	language('psc', '', individual, living, 'Iranian Sign Language').
	language('psd', '', individual, living, 'Plains Indian Sign Language').
	language('pse', '', individual, living, 'Central Malay').
	language('psg', '', individual, living, 'Penang Sign Language').
	language('psh', '', individual, living, 'Southwest Pashai').
	language('psi', '', individual, living, 'Southeast Pashai').
	language('psl', '', individual, living, 'Puerto Rican Sign Language').
	language('psm', '', individual, extinct, 'Pauserna').
	language('psn', '', individual, living, 'Panasuan').
	language('pso', '', individual, living, 'Polish Sign Language').
	language('psp', '', individual, living, 'Philippine Sign Language').
	language('psq', '', individual, living, 'Pasi').
	language('psr', '', individual, living, 'Portuguese Sign Language').
	language('pss', '', individual, living, 'Kaulong').
	language('pst', '', individual, living, 'Central Pashto').
	language('psu', '', individual, special, 'Sauraseni Prākrit').
	language('psw', '', individual, living, 'Port Sandwich').
	language('psy', '', individual, extinct, 'Piscataway').
	language('pta', '', individual, living, 'Pai Tavytera').
	language('pth', '', individual, extinct, 'Pataxó Hã-Ha-Hãe').
	language('pti', '', individual, living, 'Pindiini').
	language('ptn', '', individual, living, 'Patani').
	language('pto', '', individual, living, 'Zo''é').
	language('ptp', '', individual, living, 'Patep').
	language('ptq', '', individual, living, 'Pattapu').
	language('ptr', '', individual, living, 'Piamatsina').
	language('ptt', '', individual, living, 'Enrekang').
	language('ptu', '', individual, living, 'Bambam').
	language('ptv', '', individual, living, 'Port Vato').
	language('ptw', '', individual, extinct, 'Pentlatch').
	language('pty', '', individual, living, 'Pathiya').
	language('pua', '', individual, living, 'Western Highland Purepecha').
	language('pub', '', individual, living, 'Purum').
	language('puc', '', individual, living, 'Punan Merap').
	language('pud', '', individual, living, 'Punan Aput').
	language('pue', '', individual, extinct, 'Puelche').
	language('puf', '', individual, living, 'Punan Merah').
	language('pug', '', individual, living, 'Phuie').
	language('pui', '', individual, living, 'Puinave').
	language('puj', '', individual, living, 'Punan Tubu').
	language('pum', '', individual, living, 'Puma').
	language('puo', '', individual, living, 'Puoc').
	language('pup', '', individual, living, 'Pulabu').
	language('puq', '', individual, extinct, 'Puquina').
	language('pur', '', individual, living, 'Puruborá').
	language('pus', 'ps', macrolanguage, living, 'Pushto').
	language('put', '', individual, living, 'Putoh').
	language('puu', '', individual, living, 'Punu').
	language('puw', '', individual, living, 'Puluwatese').
	language('pux', '', individual, living, 'Puare').
	language('puy', '', individual, extinct, 'Purisimeño').
	language('pwa', '', individual, living, 'Pawaia').
	language('pwb', '', individual, living, 'Panawa').
	language('pwg', '', individual, living, 'Gapapaiwa').
	language('pwi', '', individual, extinct, 'Patwin').
	language('pwm', '', individual, living, 'Molbog').
	language('pwn', '', individual, living, 'Paiwan').
	language('pwo', '', individual, living, 'Pwo Western Karen').
	language('pwr', '', individual, living, 'Powari').
	language('pww', '', individual, living, 'Pwo Northern Karen').
	language('pxm', '', individual, living, 'Quetzaltepec Mixe').
	language('pye', '', individual, living, 'Pye Krumen').
	language('pym', '', individual, living, 'Fyam').
	language('pyn', '', individual, living, 'Poyanáwa').
	language('pys', '', individual, living, 'Paraguayan Sign Language').
	language('pyu', '', individual, living, 'Puyuma').
	language('pyx', '', individual, special, 'Pyu (Myanmar)').
	language('pyy', '', individual, living, 'Pyen').
	language('pze', '', individual, living, 'Pesse').
	language('pzh', '', individual, living, 'Pazeh').
	language('pzn', '', individual, living, 'Jejara Naga').
	language('qua', '', individual, living, 'Quapaw').
	language('qub', '', individual, living, 'Huallaga Huánuco Quechua').
	language('quc', '', individual, living, 'K''iche''').
	language('qud', '', individual, living, 'Calderón Highland Quichua').
	language('que', 'qu', macrolanguage, living, 'Quechua').
	language('quf', '', individual, living, 'Lambayeque Quechua').
	language('qug', '', individual, living, 'Chimborazo Highland Quichua').
	language('quh', '', individual, living, 'South Bolivian Quechua').
	language('qui', '', individual, living, 'Quileute').
	language('quk', '', individual, living, 'Chachapoyas Quechua').
	language('qul', '', individual, living, 'North Bolivian Quechua').
	language('qum', '', individual, living, 'Sipacapense').
	language('qun', '', individual, extinct, 'Quinault').
	language('qup', '', individual, living, 'Southern Pastaza Quechua').
	language('quq', '', individual, living, 'Quinqui').
	language('qur', '', individual, living, 'Yanahuanca Pasco Quechua').
	language('qus', '', individual, living, 'Santiago del Estero Quichua').
	language('quv', '', individual, living, 'Sacapulteco').
	language('quw', '', individual, living, 'Tena Lowland Quichua').
	language('qux', '', individual, living, 'Yauyos Quechua').
	language('quy', '', individual, living, 'Ayacucho Quechua').
	language('quz', '', individual, living, 'Cusco Quechua').
	language('qva', '', individual, living, 'Ambo-Pasco Quechua').
	language('qvc', '', individual, living, 'Cajamarca Quechua').
	language('qve', '', individual, living, 'Eastern Apurímac Quechua').
	language('qvh', '', individual, living, 'Huamalíes-Dos de Mayo Huánuco Quechua').
	language('qvi', '', individual, living, 'Imbabura Highland Quichua').
	language('qvj', '', individual, living, 'Loja Highland Quichua').
	language('qvl', '', individual, living, 'Cajatambo North Lima Quechua').
	language('qvm', '', individual, living, 'Margos-Yarowilca-Lauricocha Quechua').
	language('qvn', '', individual, living, 'North Junín Quechua').
	language('qvo', '', individual, living, 'Napo Lowland Quechua').
	language('qvp', '', individual, living, 'Pacaraos Quechua').
	language('qvs', '', individual, living, 'San Martín Quechua').
	language('qvw', '', individual, living, 'Huaylla Wanca Quechua').
	language('qvy', '', individual, living, 'Queyu').
	language('qvz', '', individual, living, 'Northern Pastaza Quichua').
	language('qwa', '', individual, living, 'Corongo Ancash Quechua').
	language('qwc', '', individual, special, 'Classical Quechua').
	language('qwh', '', individual, living, 'Huaylas Ancash Quechua').
	language('qwm', '', individual, extinct, 'Kuman (Russia)').
	language('qws', '', individual, living, 'Sihuas Ancash Quechua').
	language('qwt', '', individual, extinct, 'Kwalhioqua-Tlatskanai').
	language('qxa', '', individual, living, 'Chiquián Ancash Quechua').
	language('qxc', '', individual, living, 'Chincha Quechua').
	language('qxh', '', individual, living, 'Panao Huánuco Quechua').
	language('qxl', '', individual, living, 'Salasaca Highland Quichua').
	language('qxn', '', individual, living, 'Northern Conchucos Ancash Quechua').
	language('qxo', '', individual, living, 'Southern Conchucos Ancash Quechua').
	language('qxp', '', individual, living, 'Puno Quechua').
	language('qxq', '', individual, living, 'Qashqa''i').
	language('qxr', '', individual, living, 'Cañar Highland Quichua').
	language('qxs', '', individual, living, 'Southern Qiang').
	language('qxt', '', individual, living, 'Santa Ana de Tusi Pasco Quechua').
	language('qxu', '', individual, living, 'Arequipa-La Unión Quechua').
	language('qxw', '', individual, living, 'Jauja Wanca Quechua').
	language('qya', '', individual, constructed, 'Quenya').
	language('qyp', '', individual, extinct, 'Quiripi').
	language('raa', '', individual, living, 'Dungmali').
	language('rab', '', individual, living, 'Camling').
	language('rac', '', individual, living, 'Rasawa').
	language('rad', '', individual, living, 'Rade').
	language('raf', '', individual, living, 'Western Meohang').
	language('rag', '', individual, living, 'Logooli').
	language('rah', '', individual, living, 'Rabha').
	language('rai', '', individual, living, 'Ramoaaina').
	language('raj', '', macrolanguage, living, 'Rajasthani').
	language('rak', '', individual, living, 'Tulu-Bohuai').
	language('ral', '', individual, living, 'Ralte').
	language('ram', '', individual, living, 'Canela').
	language('ran', '', individual, living, 'Riantana').
	language('rao', '', individual, living, 'Rao').
	language('rap', '', individual, living, 'Rapanui').
	language('raq', '', individual, living, 'Saam').
	language('rar', '', individual, living, 'Rarotongan').
	language('ras', '', individual, living, 'Tegali').
	language('rat', '', individual, living, 'Razajerdi').
	language('rau', '', individual, living, 'Raute').
	language('rav', '', individual, living, 'Sampang').
	language('raw', '', individual, living, 'Rawang').
	language('rax', '', individual, living, 'Rang').
	language('ray', '', individual, living, 'Rapa').
	language('raz', '', individual, living, 'Rahambuu').
	language('rbb', '', individual, living, 'Rumai Palaung').
	language('rbk', '', individual, living, 'Northern Bontok').
	language('rbl', '', individual, living, 'Miraya Bikol').
	language('rbp', '', individual, extinct, 'Barababaraba').
	language('rcf', '', individual, living, 'Réunion Creole French').
	language('rdb', '', individual, living, 'Rudbari').
	language('rea', '', individual, living, 'Rerau').
	language('reb', '', individual, living, 'Rembong').
	language('ree', '', individual, living, 'Rejang Kayan').
	language('reg', '', individual, living, 'Kara (Tanzania)').
	language('rei', '', individual, living, 'Reli').
	language('rej', '', individual, living, 'Rejang').
	language('rel', '', individual, living, 'Rendille').
	language('rem', '', individual, extinct, 'Remo').
	language('ren', '', individual, living, 'Rengao').
	language('rer', '', individual, extinct, 'Rer Bare').
	language('res', '', individual, living, 'Reshe').
	language('ret', '', individual, living, 'Retta').
	language('rey', '', individual, living, 'Reyesano').
	language('rga', '', individual, living, 'Roria').
	language('rge', '', individual, living, 'Romano-Greek').
	language('rgk', '', individual, extinct, 'Rangkas').
	language('rgn', '', individual, living, 'Romagnol').
	language('rgr', '', individual, living, 'Resígaro').
	language('rgs', '', individual, living, 'Southern Roglai').
	language('rgu', '', individual, living, 'Ringgou').
	language('rhg', '', individual, living, 'Rohingya').
	language('rhp', '', individual, living, 'Yahang').
	language('ria', '', individual, living, 'Riang (India)').
	language('rib', '', individual, living, 'Bribri Sign Language').
	language('rif', '', individual, living, 'Tarifit').
	language('ril', '', individual, living, 'Riang Lang').
	language('rim', '', individual, living, 'Nyaturu').
	language('rin', '', individual, living, 'Nungu').
	language('rir', '', individual, living, 'Ribun').
	language('rit', '', individual, living, 'Ritharrngu').
	language('riu', '', individual, living, 'Riung').
	language('rjg', '', individual, living, 'Rajong').
	language('rji', '', individual, living, 'Raji').
	language('rjs', '', individual, living, 'Rajbanshi').
	language('rka', '', individual, living, 'Kraol').
	language('rkb', '', individual, living, 'Rikbaktsa').
	language('rkh', '', individual, living, 'Rakahanga-Manihiki').
	language('rki', '', individual, living, 'Rakhine').
	language('rkm', '', individual, living, 'Marka').
	language('rkt', '', individual, living, 'Rangpuri').
	language('rkw', '', individual, extinct, 'Arakwal').
	language('rma', '', individual, living, 'Rama').
	language('rmb', '', individual, living, 'Rembarrnga').
	language('rmc', '', individual, living, 'Carpathian Romani').
	language('rmd', '', individual, extinct, 'Traveller Danish').
	language('rme', '', individual, living, 'Angloromani').
	language('rmf', '', individual, living, 'Kalo Finnish Romani').
	language('rmg', '', individual, living, 'Traveller Norwegian').
	language('rmh', '', individual, living, 'Murkim').
	language('rmi', '', individual, living, 'Lomavren').
	language('rmk', '', individual, living, 'Romkun').
	language('rml', '', individual, living, 'Baltic Romani').
	language('rmm', '', individual, living, 'Roma').
	language('rmn', '', individual, living, 'Balkan Romani').
	language('rmo', '', individual, living, 'Sinte Romani').
	language('rmp', '', individual, living, 'Rempi').
	language('rmq', '', individual, living, 'Caló').
	language('rms', '', individual, living, 'Romanian Sign Language').
	language('rmt', '', individual, living, 'Domari').
	language('rmu', '', individual, living, 'Tavringer Romani').
	language('rmv', '', individual, constructed, 'Romanova').
	language('rmw', '', individual, living, 'Welsh Romani').
	language('rmx', '', individual, living, 'Romam').
	language('rmy', '', individual, living, 'Vlax Romani').
	language('rmz', '', individual, living, 'Marma').
	language('rnb', '', individual, living, 'Brunca Sign Language').
	language('rnd', '', individual, living, 'Ruund').
	language('rng', '', individual, living, 'Ronga').
	language('rnl', '', individual, living, 'Ranglong').
	language('rnn', '', individual, living, 'Roon').
	language('rnp', '', individual, living, 'Rongpo').
	language('rnr', '', individual, extinct, 'Nari Nari').
	language('rnw', '', individual, living, 'Rungwa').
	language('rob', '', individual, living, 'Tae''').
	language('roc', '', individual, living, 'Cacgia Roglai').
	language('rod', '', individual, living, 'Rogo').
	language('roe', '', individual, living, 'Ronji').
	language('rof', '', individual, living, 'Rombo').
	language('rog', '', individual, living, 'Northern Roglai').
	language('roh', 'rm', individual, living, 'Romansh').
	language('rol', '', individual, living, 'Romblomanon').
	language('rom', '', macrolanguage, living, 'Romany').
	language('ron', 'ro', individual, living, 'Romanian').
	language('roo', '', individual, living, 'Rotokas').
	language('rop', '', individual, living, 'Kriol').
	language('ror', '', individual, living, 'Rongga').
	language('rou', '', individual, living, 'Runga').
	language('row', '', individual, living, 'Dela-Oenale').
	language('rpn', '', individual, living, 'Repanbitip').
	language('rpt', '', individual, living, 'Rapting').
	language('rri', '', individual, living, 'Ririo').
	language('rrm', '', individual, extinct, 'Moriori').
	language('rro', '', individual, living, 'Waima').
	language('rrt', '', individual, extinct, 'Arritinngithigh').
	language('rsb', '', individual, living, 'Romano-Serbian').
	language('rsk', '', individual, living, 'Ruthenian').
	language('rsl', '', individual, living, 'Russian Sign Language').
	language('rsm', '', individual, living, 'Miriwoong Sign Language').
	language('rsn', '', individual, living, 'Rwandan Sign Language').
	language('rsw', '', individual, living, 'Rishiwa').
	language('rtc', '', individual, living, 'Rungtu Chin').
	language('rth', '', individual, living, 'Ratahan').
	language('rtm', '', individual, living, 'Rotuman').
	language('rts', '', individual, extinct, 'Yurats').
	language('rtw', '', individual, living, 'Rathawi').
	language('rub', '', individual, living, 'Gungu').
	language('ruc', '', individual, living, 'Ruuli').
	language('rue', '', individual, living, 'Rusyn').
	language('ruf', '', individual, living, 'Luguru').
	language('rug', '', individual, living, 'Roviana').
	language('ruh', '', individual, living, 'Ruga').
	language('rui', '', individual, living, 'Rufiji').
	language('ruk', '', individual, living, 'Che').
	language('run', 'rn', individual, living, 'Rundi').
	language('ruo', '', individual, living, 'Istro Romanian').
	language('rup', '', individual, living, 'Macedo-Romanian').
	language('ruq', '', individual, living, 'Megleno Romanian').
	language('rus', 'ru', individual, living, 'Russian').
	language('rut', '', individual, living, 'Rutul').
	language('ruu', '', individual, living, 'Lanas Lobu').
	language('ruy', '', individual, living, 'Mala (Nigeria)').
	language('ruz', '', individual, living, 'Ruma').
	language('rwa', '', individual, living, 'Rawo').
	language('rwk', '', individual, living, 'Rwa').
	language('rwl', '', individual, living, 'Ruwila').
	language('rwm', '', individual, living, 'Amba (Uganda)').
	language('rwo', '', individual, living, 'Rawa').
	language('rwr', '', individual, living, 'Marwari (India)').
	language('rxd', '', individual, living, 'Ngardi').
	language('rxw', '', individual, extinct, 'Karuwali').
	language('ryn', '', individual, living, 'Northern Amami-Oshima').
	language('rys', '', individual, living, 'Yaeyama').
	language('ryu', '', individual, living, 'Central Okinawan').
	language('rzh', '', individual, living, 'Rāziḥī').
	language('saa', '', individual, living, 'Saba').
	language('sab', '', individual, living, 'Buglere').
	language('sac', '', individual, living, 'Meskwaki').
	language('sad', '', individual, living, 'Sandawe').
	language('sae', '', individual, living, 'Sabanê').
	language('saf', '', individual, living, 'Safaliba').
	language('sag', 'sg', individual, living, 'Sango').
	language('sah', '', individual, living, 'Yakut').
	language('saj', '', individual, living, 'Sahu').
	language('sak', '', individual, living, 'Sake').
	language('sam', '', individual, extinct, 'Samaritan Aramaic').
	language('san', 'sa', macrolanguage, special, 'Sanskrit').
	language('sao', '', individual, living, 'Sause').
	language('saq', '', individual, living, 'Samburu').
	language('sar', '', individual, extinct, 'Saraveca').
	language('sas', '', individual, living, 'Sasak').
	language('sat', '', individual, living, 'Santali').
	language('sau', '', individual, living, 'Saleman').
	language('sav', '', individual, living, 'Saafi-Saafi').
	language('saw', '', individual, living, 'Sawi').
	language('sax', '', individual, living, 'Sa').
	language('say', '', individual, living, 'Saya').
	language('saz', '', individual, living, 'Saurashtra').
	language('sba', '', individual, living, 'Ngambay').
	language('sbb', '', individual, living, 'Simbo').
	language('sbc', '', individual, living, 'Kele (Papua New Guinea)').
	language('sbd', '', individual, living, 'Southern Samo').
	language('sbe', '', individual, living, 'Saliba').
	language('sbf', '', individual, living, 'Chabu').
	language('sbg', '', individual, living, 'Seget').
	language('sbh', '', individual, living, 'Sori-Harengan').
	language('sbi', '', individual, living, 'Seti').
	language('sbj', '', individual, living, 'Surbakhal').
	language('sbk', '', individual, living, 'Safwa').
	language('sbl', '', individual, living, 'Botolan Sambal').
	language('sbm', '', individual, living, 'Sagala').
	language('sbn', '', individual, living, 'Sindhi Bhil').
	language('sbo', '', individual, living, 'Sabüm').
	language('sbp', '', individual, living, 'Sangu (Tanzania)').
	language('sbq', '', individual, living, 'Sileibi').
	language('sbr', '', individual, living, 'Sembakung Murut').
	language('sbs', '', individual, living, 'Subiya').
	language('sbt', '', individual, living, 'Kimki').
	language('sbu', '', individual, living, 'Stod Bhoti').
	language('sbv', '', individual, special, 'Sabine').
	language('sbw', '', individual, living, 'Simba').
	language('sbx', '', individual, living, 'Seberuang').
	language('sby', '', individual, living, 'Soli').
	language('sbz', '', individual, living, 'Sara Kaba').
	language('scb', '', individual, living, 'Chut').
	language('sce', '', individual, living, 'Dongxiang').
	language('scf', '', individual, living, 'San Miguel Creole French').
	language('scg', '', individual, living, 'Sanggau').
	language('sch', '', individual, living, 'Sakachep').
	language('sci', '', individual, living, 'Sri Lankan Creole Malay').
	language('sck', '', individual, living, 'Sadri').
	language('scl', '', individual, living, 'Shina').
	language('scn', '', individual, living, 'Sicilian').
	language('sco', '', individual, living, 'Scots').
	language('scp', '', individual, living, 'Hyolmo').
	language('scq', '', individual, living, 'Sa''och').
	language('scs', '', individual, living, 'North Slavey').
	language('sct', '', individual, living, 'Southern Katang').
	language('scu', '', individual, living, 'Shumcho').
	language('scv', '', individual, living, 'Sheni').
	language('scw', '', individual, living, 'Sha').
	language('scx', '', individual, special, 'Sicel').
	language('scz', '', individual, living, 'Shaetlan').
	language('sda', '', individual, living, 'Toraja-Sa''dan').
	language('sdb', '', individual, living, 'Shabak').
	language('sdc', '', individual, living, 'Sassarese Sardinian').
	language('sde', '', individual, living, 'Surubu').
	language('sdf', '', individual, living, 'Sarli').
	language('sdg', '', individual, living, 'Savi').
	language('sdh', '', individual, living, 'Southern Kurdish').
	language('sdj', '', individual, living, 'Suundi').
	language('sdk', '', individual, living, 'Sos Kundi').
	language('sdl', '', individual, living, 'Saudi Arabian Sign Language').
	language('sdn', '', individual, living, 'Gallurese Sardinian').
	language('sdo', '', individual, living, 'Bukar-Sadung Bidayuh').
	language('sdp', '', individual, living, 'Sherdukpen').
	language('sdq', '', individual, living, 'Semandang').
	language('sdr', '', individual, living, 'Oraon Sadri').
	language('sds', '', individual, extinct, 'Sened').
	language('sdt', '', individual, extinct, 'Shuadit').
	language('sdu', '', individual, living, 'Sarudu').
	language('sdx', '', individual, living, 'Sibu Melanau').
	language('sdz', '', individual, living, 'Sallands').
	language('sea', '', individual, living, 'Semai').
	language('seb', '', individual, living, 'Shempire Senoufo').
	language('sec', '', individual, living, 'Sechelt').
	language('sed', '', individual, living, 'Sedang').
	language('see', '', individual, living, 'Seneca').
	language('sef', '', individual, living, 'Cebaara Senoufo').
	language('seg', '', individual, living, 'Segeju').
	language('seh', '', individual, living, 'Sena').
	language('sei', '', individual, living, 'Seri').
	language('sej', '', individual, living, 'Sene').
	language('sek', '', individual, living, 'Sekani').
	language('sel', '', individual, living, 'Selkup').
	language('sen', '', individual, living, 'Nanerigé Sénoufo').
	language('seo', '', individual, living, 'Suarmin').
	language('sep', '', individual, living, 'Sìcìté Sénoufo').
	language('seq', '', individual, living, 'Senara Sénoufo').
	language('ser', '', individual, living, 'Serrano').
	language('ses', '', individual, living, 'Koyraboro Senni Songhai').
	language('set', '', individual, living, 'Sentani').
	language('seu', '', individual, living, 'Serui-Laut').
	language('sev', '', individual, living, 'Nyarafolo Senoufo').
	language('sew', '', individual, living, 'Sewa Bay').
	language('sey', '', individual, living, 'Secoya').
	language('sez', '', individual, living, 'Senthang Chin').
	language('sfb', '', individual, living, 'Langue des signes de Belgique Francophone').
	language('sfe', '', individual, living, 'Eastern Subanen').
	language('sfm', '', individual, living, 'Small Flowery Miao').
	language('sfs', '', individual, living, 'South African Sign Language').
	language('sfw', '', individual, living, 'Sehwi').
	language('sga', '', individual, special, 'Old Irish (to 900)').
	language('sgb', '', individual, living, 'Mag-antsi Ayta').
	language('sgc', '', individual, living, 'Kipsigis').
	language('sgd', '', individual, living, 'Surigaonon').
	language('sge', '', individual, living, 'Segai').
	language('sgg', '', individual, living, 'Swiss-German Sign Language').
	language('sgh', '', individual, living, 'Shughni').
	language('sgi', '', individual, living, 'Suga').
	language('sgj', '', individual, living, 'Surgujia').
	language('sgk', '', individual, living, 'Sangkong').
	language('sgm', '', individual, extinct, 'Singa').
	language('sgp', '', individual, living, 'Singpho').
	language('sgr', '', individual, living, 'Sangisari').
	language('sgs', '', individual, living, 'Samogitian').
	language('sgt', '', individual, living, 'Brokpake').
	language('sgu', '', individual, living, 'Salas').
	language('sgw', '', individual, living, 'Sebat Bet Gurage').
	language('sgx', '', individual, living, 'Sierra Leone Sign Language').
	language('sgy', '', individual, living, 'Sanglechi').
	language('sgz', '', individual, living, 'Sursurunga').
	language('sha', '', individual, living, 'Shall-Zwall').
	language('shb', '', individual, living, 'Ninam').
	language('shc', '', individual, living, 'Sonde').
	language('shd', '', individual, living, 'Kundal Shahi').
	language('she', '', individual, living, 'Sheko').
	language('shg', '', individual, living, 'Shua').
	language('shh', '', individual, living, 'Shoshoni').
	language('shi', '', individual, living, 'Tachelhit').
	language('shj', '', individual, living, 'Shatt').
	language('shk', '', individual, living, 'Shilluk').
	language('shl', '', individual, living, 'Shendu').
	language('shm', '', individual, living, 'Shahrudi').
	language('shn', '', individual, living, 'Shan').
	language('sho', '', individual, living, 'Shanga').
	language('shp', '', individual, living, 'Shipibo-Conibo').
	language('shq', '', individual, living, 'Sala').
	language('shr', '', individual, living, 'Shi').
	language('shs', '', individual, living, 'Shuswap').
	language('sht', '', individual, extinct, 'Shasta').
	language('shu', '', individual, living, 'Chadian Arabic').
	language('shv', '', individual, living, 'Shehri').
	language('shw', '', individual, living, 'Shwai').
	language('shx', '', individual, living, 'She').
	language('shy', '', individual, living, 'Tachawit').
	language('shz', '', individual, living, 'Syenara Senoufo').
	language('sia', '', individual, extinct, 'Akkala Sami').
	language('sib', '', individual, living, 'Sebop').
	language('sid', '', individual, living, 'Sidamo').
	language('sie', '', individual, living, 'Simaa').
	language('sif', '', individual, living, 'Siamou').
	language('sig', '', individual, living, 'Paasaal').
	language('sih', '', individual, living, 'Zire').
	language('sii', '', individual, living, 'Shom Peng').
	language('sij', '', individual, living, 'Numbami').
	language('sik', '', individual, living, 'Sikiana').
	language('sil', '', individual, living, 'Tumulung Sisaala').
	language('sim', '', individual, living, 'Mende (Papua New Guinea)').
	language('sin', 'si', individual, living, 'Sinhala').
	language('sip', '', individual, living, 'Sikkimese').
	language('siq', '', individual, living, 'Sonia').
	language('sir', '', individual, living, 'Siri').
	language('sis', '', individual, extinct, 'Siuslaw').
	language('siu', '', individual, living, 'Sinagen').
	language('siv', '', individual, living, 'Sumariup').
	language('siw', '', individual, living, 'Siwai').
	language('six', '', individual, living, 'Sumau').
	language('siy', '', individual, living, 'Sivandi').
	language('siz', '', individual, living, 'Siwi').
	language('sja', '', individual, living, 'Epena').
	language('sjb', '', individual, living, 'Sajau Basap').
	language('sjc', '', individual, living, 'Shaojiang Chinese').
	language('sjd', '', individual, living, 'Kildin Sami').
	language('sje', '', individual, living, 'Pite Sami').
	language('sjg', '', individual, living, 'Assangori').
	language('sjk', '', individual, extinct, 'Kemi Sami').
	language('sjl', '', individual, living, 'Sajalong').
	language('sjm', '', individual, living, 'Mapun').
	language('sjn', '', individual, constructed, 'Sindarin').
	language('sjo', '', individual, living, 'Xibe').
	language('sjp', '', individual, living, 'Surjapuri').
	language('sjr', '', individual, living, 'Siar-Lak').
	language('sjs', '', individual, extinct, 'Senhaja De Srair').
	language('sjt', '', individual, living, 'Ter Sami').
	language('sju', '', individual, living, 'Ume Sami').
	language('sjw', '', individual, living, 'Shawnee').
	language('ska', '', individual, living, 'Skagit').
	language('skb', '', individual, living, 'Saek').
	language('skc', '', individual, living, 'Ma Manda').
	language('skd', '', individual, living, 'Southern Sierra Miwok').
	language('ske', '', individual, living, 'Seke (Vanuatu)').
	language('skf', '', individual, living, 'Sakirabiá').
	language('skg', '', individual, living, 'Sakalava Malagasy').
	language('skh', '', individual, living, 'Sikule').
	language('ski', '', individual, living, 'Sika').
	language('skj', '', individual, living, 'Seke (Nepal)').
	language('skm', '', individual, living, 'Kutong').
	language('skn', '', individual, living, 'Kolibugan Subanon').
	language('sko', '', individual, living, 'Seko Tengah').
	language('skp', '', individual, living, 'Sekapan').
	language('skq', '', individual, living, 'Sininkere').
	language('skr', '', individual, living, 'Saraiki').
	language('sks', '', individual, living, 'Maia').
	language('skt', '', individual, living, 'Sakata').
	language('sku', '', individual, living, 'Sakao').
	language('skv', '', individual, living, 'Skou').
	language('skw', '', individual, extinct, 'Skepi Creole Dutch').
	language('skx', '', individual, living, 'Seko Padang').
	language('sky', '', individual, living, 'Sikaiana').
	language('skz', '', individual, living, 'Sekar').
	language('slc', '', individual, living, 'Sáliba').
	language('sld', '', individual, living, 'Sissala').
	language('sle', '', individual, living, 'Sholaga').
	language('slf', '', individual, living, 'Swiss-Italian Sign Language').
	language('slg', '', individual, living, 'Selungai Murut').
	language('slh', '', individual, living, 'Southern Puget Sound Salish').
	language('sli', '', individual, living, 'Lower Silesian').
	language('slj', '', individual, living, 'Salumá').
	language('slk', 'sk', individual, living, 'Slovak').
	language('sll', '', individual, living, 'Salt-Yui').
	language('slm', '', individual, living, 'Pangutaran Sama').
	language('sln', '', individual, extinct, 'Salinan').
	language('slp', '', individual, living, 'Lamaholot').
	language('slr', '', individual, living, 'Salar').
	language('sls', '', individual, living, 'Singapore Sign Language').
	language('slt', '', individual, living, 'Sila').
	language('slu', '', individual, living, 'Selaru').
	language('slv', 'sl', individual, living, 'Slovenian').
	language('slw', '', individual, living, 'Sialum').
	language('slx', '', individual, living, 'Salampasu').
	language('sly', '', individual, living, 'Selayar').
	language('slz', '', individual, living, 'Ma''ya').
	language('sma', '', individual, living, 'Southern Sami').
	language('smb', '', individual, living, 'Simbari').
	language('smc', '', individual, extinct, 'Som').
	language('sme', 'se', individual, living, 'Northern Sami').
	language('smf', '', individual, living, 'Auwe').
	language('smg', '', individual, living, 'Simbali').
	language('smh', '', individual, living, 'Samei').
	language('smj', '', individual, living, 'Lule Sami').
	language('smk', '', individual, living, 'Bolinao').
	language('sml', '', individual, living, 'Central Sama').
	language('smm', '', individual, living, 'Musasa').
	language('smn', '', individual, living, 'Inari Sami').
	language('smo', 'sm', individual, living, 'Samoan').
	language('smp', '', individual, extinct, 'Samaritan').
	language('smq', '', individual, living, 'Samo').
	language('smr', '', individual, living, 'Simeulue').
	language('sms', '', individual, living, 'Skolt Sami').
	language('smt', '', individual, living, 'Simte').
	language('smu', '', individual, extinct, 'Somray').
	language('smv', '', individual, living, 'Samvedi').
	language('smw', '', individual, living, 'Sumbawa').
	language('smx', '', individual, living, 'Samba').
	language('smy', '', individual, living, 'Semnani').
	language('smz', '', individual, living, 'Simeku').
	language('sna', 'sn', individual, living, 'Shona').
	language('snc', '', individual, living, 'Sinaugoro').
	language('snd', 'sd', individual, living, 'Sindhi').
	language('sne', '', individual, living, 'Bau Bidayuh').
	language('snf', '', individual, living, 'Noon').
	language('sng', '', individual, living, 'Sanga (Democratic Republic of Congo)').
	language('sni', '', individual, extinct, 'Sensi').
	language('snj', '', individual, living, 'Riverain Sango').
	language('snk', '', individual, living, 'Soninke').
	language('snl', '', individual, living, 'Sangil').
	language('snm', '', individual, living, 'Southern Ma''di').
	language('snn', '', individual, living, 'Siona').
	language('sno', '', individual, living, 'Snohomish').
	language('snp', '', individual, living, 'Siane').
	language('snq', '', individual, living, 'Sangu (Gabon)').
	language('snr', '', individual, living, 'Sihan').
	language('sns', '', individual, living, 'South West Bay').
	language('snu', '', individual, living, 'Senggi').
	language('snv', '', individual, living, 'Sa''ban').
	language('snw', '', individual, living, 'Selee').
	language('snx', '', individual, living, 'Sam').
	language('sny', '', individual, living, 'Saniyo-Hiyewe').
	language('snz', '', individual, living, 'Kou').
	language('soa', '', individual, living, 'Thai Song').
	language('sob', '', individual, living, 'Sobei').
	language('soc', '', individual, living, 'So (Democratic Republic of Congo)').
	language('sod', '', individual, living, 'Songoora').
	language('soe', '', individual, living, 'Songomeno').
	language('sog', '', individual, special, 'Sogdian').
	language('soh', '', individual, living, 'Aka').
	language('soi', '', individual, living, 'Sonha').
	language('soj', '', individual, living, 'Soi').
	language('sok', '', individual, living, 'Sokoro').
	language('sol', '', individual, living, 'Solos').
	language('som', 'so', individual, living, 'Somali').
	language('soo', '', individual, living, 'Songo').
	language('sop', '', individual, living, 'Songe').
	language('soq', '', individual, living, 'Kanasi').
	language('sor', '', individual, living, 'Somrai').
	language('sos', '', individual, living, 'Seeku').
	language('sot', 'st', individual, living, 'Southern Sotho').
	language('sou', '', individual, living, 'Southern Thai').
	language('sov', '', individual, living, 'Sonsorol').
	language('sow', '', individual, living, 'Sowanda').
	language('sox', '', individual, living, 'Swo').
	language('soy', '', individual, living, 'Miyobe').
	language('soz', '', individual, living, 'Temi').
	language('spa', 'es', individual, living, 'Spanish').
	language('spb', '', individual, living, 'Sepa (Indonesia)').
	language('spc', '', individual, living, 'Sapé').
	language('spd', '', individual, living, 'Saep').
	language('spe', '', individual, living, 'Sepa (Papua New Guinea)').
	language('spg', '', individual, living, 'Sian').
	language('spi', '', individual, living, 'Saponi').
	language('spk', '', individual, living, 'Sengo').
	language('spl', '', individual, living, 'Selepet').
	language('spm', '', individual, living, 'Akukem').
	language('spn', '', individual, living, 'Sanapaná').
	language('spo', '', individual, living, 'Spokane').
	language('spp', '', individual, living, 'Supyire Senoufo').
	language('spq', '', individual, living, 'Loreto-Ucayali Spanish').
	language('spr', '', individual, living, 'Saparua').
	language('sps', '', individual, living, 'Saposa').
	language('spt', '', individual, living, 'Spiti Bhoti').
	language('spu', '', individual, living, 'Sapuan').
	language('spv', '', individual, living, 'Sambalpuri').
	language('spx', '', individual, special, 'South Picene').
	language('spy', '', individual, living, 'Sabaot').
	language('sqa', '', individual, living, 'Shama-Sambuga').
	language('sqh', '', individual, living, 'Shau').
	language('sqi', 'sq', macrolanguage, living, 'Albanian').
	language('sqk', '', individual, living, 'Albanian Sign Language').
	language('sqm', '', individual, living, 'Suma').
	language('sqn', '', individual, extinct, 'Susquehannock').
	language('sqo', '', individual, living, 'Sorkhei').
	language('sqq', '', individual, living, 'Sou').
	language('sqr', '', individual, special, 'Siculo Arabic').
	language('sqs', '', individual, living, 'Sri Lankan Sign Language').
	language('sqt', '', individual, living, 'Soqotri').
	language('squ', '', individual, living, 'Squamish').
	language('sqx', '', individual, living, 'Kufr Qassem Sign Language (KQSL)').
	language('sra', '', individual, living, 'Saruga').
	language('srb', '', individual, living, 'Sora').
	language('src', '', individual, living, 'Logudorese Sardinian').
	language('srd', 'sc', macrolanguage, living, 'Sardinian').
	language('sre', '', individual, living, 'Sara').
	language('srf', '', individual, living, 'Nafi').
	language('srg', '', individual, living, 'Sulod').
	language('srh', '', individual, living, 'Sarikoli').
	language('sri', '', individual, living, 'Siriano').
	language('srk', '', individual, living, 'Serudung Murut').
	language('srl', '', individual, living, 'Isirawa').
	language('srm', '', individual, living, 'Saramaccan').
	language('srn', '', individual, living, 'Sranan Tongo').
	language('sro', '', individual, living, 'Campidanese Sardinian').
	language('srp', 'sr', individual, living, 'Serbian').
	language('srq', '', individual, living, 'Sirionó').
	language('srr', '', individual, living, 'Serer').
	language('srs', '', individual, living, 'Tsuut''ina').
	language('srt', '', individual, living, 'Sauri').
	language('sru', '', individual, living, 'Suruí').
	language('srv', '', individual, living, 'Southern Sorsoganon').
	language('srw', '', individual, living, 'Serua').
	language('srx', '', individual, living, 'Sirmauri').
	language('sry', '', individual, living, 'Sera').
	language('srz', '', individual, living, 'Shahmirzadi').
	language('ssb', '', individual, living, 'Southern Sama').
	language('ssc', '', individual, living, 'Suba-Simbiti').
	language('ssd', '', individual, living, 'Siroi').
	language('sse', '', individual, living, 'Balangingi').
	language('ssf', '', individual, living, 'Thao').
	language('ssg', '', individual, living, 'Seimat').
	language('ssh', '', individual, living, 'Shihhi Arabic').
	language('ssi', '', individual, living, 'Sansi').
	language('ssj', '', individual, living, 'Sausi').
	language('ssk', '', individual, living, 'Sunam').
	language('ssl', '', individual, living, 'Western Sisaala').
	language('ssm', '', individual, living, 'Semnam').
	language('ssn', '', individual, living, 'Waata').
	language('sso', '', individual, living, 'Sissano').
	language('ssp', '', individual, living, 'Spanish Sign Language').
	language('ssq', '', individual, living, 'So''a').
	language('ssr', '', individual, living, 'Swiss-French Sign Language').
	language('sss', '', individual, living, 'Sô').
	language('sst', '', individual, living, 'Sinasina').
	language('ssu', '', individual, living, 'Susuami').
	language('ssv', '', individual, living, 'Shark Bay').
	language('ssw', 'ss', individual, living, 'Swati').
	language('ssx', '', individual, living, 'Samberigi').
	language('ssy', '', individual, living, 'Saho').
	language('ssz', '', individual, living, 'Sengseng').
	language('sta', '', individual, living, 'Settla').
	language('stb', '', individual, living, 'Northern Subanen').
	language('std', '', individual, living, 'Sentinel').
	language('ste', '', individual, living, 'Liana-Seti').
	language('stf', '', individual, living, 'Seta').
	language('stg', '', individual, living, 'Trieng').
	language('sth', '', individual, living, 'Shelta').
	language('sti', '', individual, living, 'Bulo Stieng').
	language('stj', '', individual, living, 'Matya Samo').
	language('stk', '', individual, living, 'Arammba').
	language('stl', '', individual, living, 'Stellingwerfs').
	language('stm', '', individual, living, 'Setaman').
	language('stn', '', individual, living, 'Owa').
	language('sto', '', individual, living, 'Stoney').
	language('stp', '', individual, living, 'Southeastern Tepehuan').
	language('stq', '', individual, living, 'Saterfriesisch').
	language('str', '', individual, living, 'Straits Salish').
	language('sts', '', individual, living, 'Shumashti').
	language('stt', '', individual, living, 'Budeh Stieng').
	language('stu', '', individual, living, 'Samtao').
	language('stv', '', individual, living, 'Silt''e').
	language('stw', '', individual, living, 'Satawalese').
	language('sty', '', individual, living, 'Siberian Tatar').
	language('sua', '', individual, living, 'Sulka').
	language('sub', '', individual, living, 'Suku').
	language('suc', '', individual, living, 'Western Subanon').
	language('sue', '', individual, living, 'Suena').
	language('sug', '', individual, living, 'Suganga').
	language('sui', '', individual, living, 'Suki').
	language('suj', '', individual, living, 'Shubi').
	language('suk', '', individual, living, 'Sukuma').
	language('sun', 'su', individual, living, 'Sundanese').
	language('suo', '', individual, living, 'Bouni').
	language('suq', '', individual, living, 'Tirmaga-Chai Suri').
	language('sur', '', individual, living, 'Mwaghavul').
	language('sus', '', individual, living, 'Susu').
	language('sut', '', individual, extinct, 'Subtiaba').
	language('suv', '', individual, living, 'Puroik').
	language('suw', '', individual, living, 'Sumbwa').
	language('sux', '', individual, special, 'Sumerian').
	language('suy', '', individual, living, 'Suyá').
	language('suz', '', individual, living, 'Sunwar').
	language('sva', '', individual, living, 'Svan').
	language('svb', '', individual, living, 'Ulau-Suain').
	language('svc', '', individual, living, 'Vincentian Creole English').
	language('sve', '', individual, living, 'Serili').
	language('svk', '', individual, living, 'Slovakian Sign Language').
	language('svm', '', individual, living, 'Slavomolisano').
	language('svs', '', individual, living, 'Savosavo').
	language('svx', '', individual, special, 'Skalvian').
	language('swa', 'sw', macrolanguage, living, 'Swahili (macrolanguage)').
	language('swb', '', individual, living, 'Maore Comorian').
	language('swc', '', individual, living, 'Congo Swahili').
	language('swe', 'sv', individual, living, 'Swedish').
	language('swf', '', individual, living, 'Sere').
	language('swg', '', individual, living, 'Swabian').
	language('swh', '', individual, living, 'Swahili (individual language)').
	language('swi', '', individual, living, 'Sui').
	language('swj', '', individual, living, 'Sira').
	language('swk', '', individual, living, 'Malawi Sena').
	language('swl', '', individual, living, 'Swedish Sign Language').
	language('swm', '', individual, living, 'Samosa').
	language('swn', '', individual, living, 'Sawknah').
	language('swo', '', individual, living, 'Shanenawa').
	language('swp', '', individual, living, 'Suau').
	language('swq', '', individual, living, 'Sharwa').
	language('swr', '', individual, living, 'Saweru').
	language('sws', '', individual, living, 'Seluwasan').
	language('swt', '', individual, living, 'Sawila').
	language('swu', '', individual, living, 'Suwawa').
	language('swv', '', individual, living, 'Shekhawati').
	language('sww', '', individual, extinct, 'Sowa').
	language('swx', '', individual, living, 'Suruahá').
	language('swy', '', individual, living, 'Sarua').
	language('sxb', '', individual, living, 'Suba').
	language('sxc', '', individual, special, 'Sicanian').
	language('sxe', '', individual, living, 'Sighu').
	language('sxg', '', individual, living, 'Shuhi').
	language('sxk', '', individual, extinct, 'Southern Kalapuya').
	language('sxl', '', individual, extinct, 'Selian').
	language('sxm', '', individual, living, 'Samre').
	language('sxn', '', individual, living, 'Sangir').
	language('sxo', '', individual, special, 'Sorothaptic').
	language('sxr', '', individual, living, 'Saaroa').
	language('sxs', '', individual, living, 'Sasaru').
	language('sxu', '', individual, living, 'Upper Saxon').
	language('sxw', '', individual, living, 'Saxwe Gbe').
	language('sya', '', individual, living, 'Siang').
	language('syb', '', individual, living, 'Central Subanen').
	language('syc', '', individual, special, 'Classical Syriac').
	language('syi', '', individual, living, 'Seki').
	language('syk', '', individual, living, 'Sukur').
	language('syl', '', individual, living, 'Sylheti').
	language('sym', '', individual, living, 'Maya Samo').
	language('syn', '', individual, living, 'Senaya').
	language('syo', '', individual, living, 'Suoy').
	language('syr', '', macrolanguage, living, 'Syriac').
	language('sys', '', individual, living, 'Sinyar').
	language('syw', '', individual, living, 'Kagate').
	language('syx', '', individual, living, 'Samay').
	language('syy', '', individual, living, 'Al-Sayyid Bedouin Sign Language').
	language('sza', '', individual, living, 'Semelai').
	language('szb', '', individual, living, 'Ngalum').
	language('szc', '', individual, living, 'Semaq Beri').
	language('sze', '', individual, living, 'Seze').
	language('szg', '', individual, living, 'Sengele').
	language('szl', '', individual, living, 'Silesian').
	language('szn', '', individual, living, 'Sula').
	language('szp', '', individual, living, 'Suabo').
	language('szs', '', individual, living, 'Solomon Islands Sign Language').
	language('szv', '', individual, living, 'Isubu').
	language('szw', '', individual, living, 'Sawai').
	language('szy', '', individual, living, 'Sakizaya').
	language('taa', '', individual, living, 'Lower Tanana').
	language('tab', '', individual, living, 'Tabassaran').
	language('tac', '', individual, living, 'Lowland Tarahumara').
	language('tad', '', individual, living, 'Tause').
	language('tae', '', individual, living, 'Tariana').
	language('taf', '', individual, living, 'Tapirapé').
	language('tag', '', individual, living, 'Tagoi').
	language('tah', 'ty', individual, living, 'Tahitian').
	language('taj', '', individual, living, 'Eastern Tamang').
	language('tak', '', individual, living, 'Tala').
	language('tal', '', individual, living, 'Tal').
	language('tam', 'ta', individual, living, 'Tamil').
	language('tan', '', individual, living, 'Tangale').
	language('tao', '', individual, living, 'Yami').
	language('tap', '', individual, living, 'Taabwa').
	language('taq', '', individual, living, 'Tamasheq').
	language('tar', '', individual, living, 'Central Tarahumara').
	language('tas', '', individual, extinct, 'Tay Boi').
	language('tat', 'tt', individual, living, 'Tatar').
	language('tau', '', individual, living, 'Upper Tanana').
	language('tav', '', individual, living, 'Tatuyo').
	language('taw', '', individual, living, 'Tai').
	language('tax', '', individual, living, 'Tamki').
	language('tay', '', individual, living, 'Atayal').
	language('taz', '', individual, living, 'Tocho').
	language('tba', '', individual, living, 'Aikanã').
	language('tbc', '', individual, living, 'Takia').
	language('tbd', '', individual, living, 'Kaki Ae').
	language('tbe', '', individual, living, 'Tanimbili').
	language('tbf', '', individual, living, 'Mandara').
	language('tbg', '', individual, living, 'North Tairora').
	language('tbh', '', individual, extinct, 'Dharawal').
	language('tbi', '', individual, living, 'Gaam').
	language('tbj', '', individual, living, 'Tiang').
	language('tbk', '', individual, living, 'Calamian Tagbanwa').
	language('tbl', '', individual, living, 'Tboli').
	language('tbm', '', individual, living, 'Tagbu').
	language('tbn', '', individual, living, 'Barro Negro Tunebo').
	language('tbo', '', individual, living, 'Tawala').
	language('tbp', '', individual, living, 'Taworta').
	language('tbr', '', individual, living, 'Tumtum').
	language('tbs', '', individual, living, 'Tanguat').
	language('tbt', '', individual, living, 'Tembo (Kitembo)').
	language('tbu', '', individual, extinct, 'Tubar').
	language('tbv', '', individual, living, 'Tobo').
	language('tbw', '', individual, living, 'Tagbanwa').
	language('tbx', '', individual, living, 'Kapin').
	language('tby', '', individual, living, 'Tabaru').
	language('tbz', '', individual, living, 'Ditammari').
	language('tca', '', individual, living, 'Ticuna').
	language('tcb', '', individual, living, 'Tanacross').
	language('tcc', '', individual, living, 'Datooga').
	language('tcd', '', individual, living, 'Tafi').
	language('tce', '', individual, living, 'Southern Tutchone').
	language('tcf', '', individual, living, 'Malinaltepec Me''phaa').
	language('tcg', '', individual, living, 'Tamagario').
	language('tch', '', individual, living, 'Turks And Caicos Creole English').
	language('tci', '', individual, living, 'Wára').
	language('tck', '', individual, living, 'Tchitchege').
	language('tcl', '', individual, extinct, 'Taman (Myanmar)').
	language('tcm', '', individual, living, 'Tanahmerah').
	language('tcn', '', individual, living, 'Tichurong').
	language('tco', '', individual, living, 'Taungyo').
	language('tcp', '', individual, living, 'Tawr Chin').
	language('tcq', '', individual, living, 'Kaiy').
	language('tcs', '', individual, living, 'Torres Strait Creole').
	language('tct', '', individual, living, 'T''en').
	language('tcu', '', individual, living, 'Southeastern Tarahumara').
	language('tcw', '', individual, living, 'Tecpatlán Totonac').
	language('tcx', '', individual, living, 'Toda').
	language('tcy', '', individual, living, 'Tulu').
	language('tcz', '', individual, living, 'Thado Chin').
	language('tda', '', individual, living, 'Tagdal').
	language('tdb', '', individual, living, 'Panchpargania').
	language('tdc', '', individual, living, 'Emberá-Tadó').
	language('tdd', '', individual, living, 'Tai Nüa').
	language('tde', '', individual, living, 'Tiranige Diga Dogon').
	language('tdf', '', individual, living, 'Talieng').
	language('tdg', '', individual, living, 'Western Tamang').
	language('tdh', '', individual, living, 'Thulung').
	language('tdi', '', individual, living, 'Tomadino').
	language('tdj', '', individual, living, 'Tajio').
	language('tdk', '', individual, living, 'Tambas').
	language('tdl', '', individual, living, 'Sur').
	language('tdm', '', individual, living, 'Taruma').
	language('tdn', '', individual, living, 'Tondano').
	language('tdo', '', individual, living, 'Teme').
	language('tdq', '', individual, living, 'Tita').
	language('tdr', '', individual, living, 'Todrah').
	language('tds', '', individual, living, 'Doutai').
	language('tdt', '', individual, living, 'Tetun Dili').
	language('tdv', '', individual, living, 'Toro').
	language('tdx', '', individual, living, 'Tandroy-Mahafaly Malagasy').
	language('tdy', '', individual, living, 'Tadyawan').
	language('tea', '', individual, living, 'Temiar').
	language('teb', '', individual, extinct, 'Tetete').
	language('tec', '', individual, living, 'Terik').
	language('ted', '', individual, living, 'Tepo Krumen').
	language('tee', '', individual, living, 'Huehuetla Tepehua').
	language('tef', '', individual, living, 'Teressa').
	language('teg', '', individual, living, 'Teke-Tege').
	language('teh', '', individual, living, 'Tehuelche').
	language('tei', '', individual, living, 'Torricelli').
	language('tek', '', individual, living, 'Ibali Teke').
	language('tel', 'te', individual, living, 'Telugu').
	language('tem', '', individual, living, 'Timne').
	language('ten', '', individual, extinct, 'Tama (Colombia)').
	language('teo', '', individual, living, 'Teso').
	language('tep', '', individual, extinct, 'Tepecano').
	language('teq', '', individual, living, 'Temein').
	language('ter', '', individual, living, 'Tereno').
	language('tes', '', individual, living, 'Tengger').
	language('tet', '', individual, living, 'Tetum').
	language('teu', '', individual, living, 'Soo').
	language('tev', '', individual, living, 'Teor').
	language('tew', '', individual, living, 'Tewa (USA)').
	language('tex', '', individual, living, 'Tennet').
	language('tey', '', individual, living, 'Tulishi').
	language('tez', '', individual, living, 'Tetserret').
	language('tfi', '', individual, living, 'Tofin Gbe').
	language('tfn', '', individual, living, 'Tanaina').
	language('tfo', '', individual, living, 'Tefaro').
	language('tfr', '', individual, living, 'Teribe').
	language('tft', '', individual, living, 'Ternate').
	language('tga', '', individual, living, 'Sagalla').
	language('tgb', '', individual, living, 'Tobilung').
	language('tgc', '', individual, living, 'Tigak').
	language('tgd', '', individual, living, 'Ciwogai').
	language('tge', '', individual, living, 'Eastern Gorkha Tamang').
	language('tgf', '', individual, living, 'Chalikha').
	language('tgh', '', individual, living, 'Tobagonian Creole English').
	language('tgi', '', individual, living, 'Lawunuia').
	language('tgj', '', individual, living, 'Tagin').
	language('tgk', 'tg', individual, living, 'Tajik').
	language('tgl', 'tl', individual, living, 'Tagalog').
	language('tgn', '', individual, living, 'Tandaganon').
	language('tgo', '', individual, living, 'Sudest').
	language('tgp', '', individual, living, 'Tangoa').
	language('tgq', '', individual, living, 'Tring').
	language('tgr', '', individual, living, 'Tareng').
	language('tgs', '', individual, living, 'Nume').
	language('tgt', '', individual, living, 'Central Tagbanwa').
	language('tgu', '', individual, living, 'Tanggu').
	language('tgv', '', individual, extinct, 'Tingui-Boto').
	language('tgw', '', individual, living, 'Tagwana Senoufo').
	language('tgx', '', individual, living, 'Tagish').
	language('tgy', '', individual, extinct, 'Togoyo').
	language('tgz', '', individual, extinct, 'Tagalaka').
	language('tha', 'th', individual, living, 'Thai').
	language('thd', '', individual, living, 'Kuuk Thaayorre').
	language('the', '', individual, living, 'Chitwania Tharu').
	language('thf', '', individual, living, 'Thangmi').
	language('thh', '', individual, living, 'Northern Tarahumara').
	language('thi', '', individual, living, 'Tai Long').
	language('thk', '', individual, living, 'Tharaka').
	language('thl', '', individual, living, 'Dangaura Tharu').
	language('thm', '', individual, living, 'Aheu').
	language('thn', '', individual, living, 'Thachanadan').
	language('thp', '', individual, living, 'Thompson').
	language('thq', '', individual, living, 'Kochila Tharu').
	language('thr', '', individual, living, 'Rana Tharu').
	language('ths', '', individual, living, 'Thakali').
	language('tht', '', individual, living, 'Tahltan').
	language('thu', '', individual, living, 'Thuri').
	language('thv', '', individual, living, 'Tahaggart Tamahaq').
	language('thy', '', individual, living, 'Tha').
	language('thz', '', individual, living, 'Tayart Tamajeq').
	language('tia', '', individual, living, 'Tidikelt Tamazight').
	language('tic', '', individual, living, 'Tira').
	language('tif', '', individual, living, 'Tifal').
	language('tig', '', individual, living, 'Tigre').
	language('tih', '', individual, living, 'Timugon Murut').
	language('tii', '', individual, living, 'Tiene').
	language('tij', '', individual, living, 'Tilung').
	language('tik', '', individual, living, 'Tikar').
	language('til', '', individual, extinct, 'Tillamook').
	language('tim', '', individual, living, 'Timbe').
	language('tin', '', individual, living, 'Tindi').
	language('tio', '', individual, living, 'Teop').
	language('tip', '', individual, living, 'Trimuris').
	language('tiq', '', individual, living, 'Tiéfo').
	language('tir', 'ti', individual, living, 'Tigrinya').
	language('tis', '', individual, living, 'Masadiit Itneg').
	language('tit', '', individual, living, 'Tinigua').
	language('tiu', '', individual, living, 'Adasen').
	language('tiv', '', individual, living, 'Tiv').
	language('tiw', '', individual, living, 'Tiwi').
	language('tix', '', individual, living, 'Southern Tiwa').
	language('tiy', '', individual, living, 'Tiruray').
	language('tiz', '', individual, living, 'Tai Hongjin').
	language('tja', '', individual, living, 'Tajuasohn').
	language('tjg', '', individual, living, 'Tunjung').
	language('tji', '', individual, living, 'Northern Tujia').
	language('tjj', '', individual, living, 'Tjungundji').
	language('tjl', '', individual, living, 'Tai Laing').
	language('tjm', '', individual, extinct, 'Timucua').
	language('tjn', '', individual, extinct, 'Tonjon').
	language('tjo', '', individual, living, 'Temacine Tamazight').
	language('tjp', '', individual, living, 'Tjupany').
	language('tjs', '', individual, living, 'Southern Tujia').
	language('tju', '', individual, extinct, 'Tjurruru').
	language('tjw', '', individual, living, 'Djabwurrung').
	language('tka', '', individual, extinct, 'Truká').
	language('tkb', '', individual, living, 'Buksa').
	language('tkd', '', individual, living, 'Tukudede').
	language('tke', '', individual, living, 'Takwane').
	language('tkf', '', individual, extinct, 'Tukumanféd').
	language('tkg', '', individual, living, 'Tesaka Malagasy').
	language('tkl', '', individual, living, 'Tokelau').
	language('tkm', '', individual, extinct, 'Takelma').
	language('tkn', '', individual, living, 'Toku-No-Shima').
	language('tkp', '', individual, living, 'Tikopia').
	language('tkq', '', individual, living, 'Tee').
	language('tkr', '', individual, living, 'Tsakhur').
	language('tks', '', individual, living, 'Takestani').
	language('tkt', '', individual, living, 'Kathoriya Tharu').
	language('tku', '', individual, living, 'Upper Necaxa Totonac').
	language('tkv', '', individual, living, 'Mur Pano').
	language('tkw', '', individual, living, 'Teanu').
	language('tkx', '', individual, living, 'Tangko').
	language('tkz', '', individual, living, 'Takua').
	language('tla', '', individual, living, 'Southwestern Tepehuan').
	language('tlb', '', individual, living, 'Tobelo').
	language('tlc', '', individual, living, 'Yecuatla Totonac').
	language('tld', '', individual, living, 'Talaud').
	language('tlf', '', individual, living, 'Telefol').
	language('tlg', '', individual, living, 'Tofanma').
	language('tlh', '', individual, constructed, 'Klingon').
	language('tli', '', individual, living, 'Tlingit').
	language('tlj', '', individual, living, 'Talinga-Bwisi').
	language('tlk', '', individual, living, 'Taloki').
	language('tll', '', individual, living, 'Tetela').
	language('tlm', '', individual, living, 'Tolomako').
	language('tln', '', individual, living, 'Talondo''').
	language('tlo', '', individual, living, 'Talodi').
	language('tlp', '', individual, living, 'Filomena Mata-Coahuitlán Totonac').
	language('tlq', '', individual, living, 'Tai Loi').
	language('tlr', '', individual, living, 'Talise').
	language('tls', '', individual, living, 'Tambotalo').
	language('tlt', '', individual, living, 'Sou Nama').
	language('tlu', '', individual, living, 'Tulehu').
	language('tlv', '', individual, living, 'Taliabu').
	language('tlx', '', individual, living, 'Khehek').
	language('tly', '', individual, living, 'Talysh').
	language('tma', '', individual, living, 'Tama (Chad)').
	language('tmb', '', individual, living, 'Katbol').
	language('tmc', '', individual, living, 'Tumak').
	language('tmd', '', individual, living, 'Haruai').
	language('tme', '', individual, extinct, 'Tremembé').
	language('tmf', '', individual, living, 'Toba-Maskoy').
	language('tmg', '', individual, extinct, 'Ternateño').
	language('tmh', '', macrolanguage, living, 'Tamashek').
	language('tmi', '', individual, living, 'Tutuba').
	language('tmj', '', individual, living, 'Samarokena').
	language('tml', '', individual, living, 'Tamnim Citak').
	language('tmm', '', individual, living, 'Tai Thanh').
	language('tmn', '', individual, living, 'Taman (Indonesia)').
	language('tmo', '', individual, living, 'Temoq').
	language('tmq', '', individual, living, 'Tumleo').
	language('tmr', '', individual, extinct, 'Jewish Babylonian Aramaic (ca. 200-1200 CE)').
	language('tms', '', individual, living, 'Tima').
	language('tmt', '', individual, living, 'Tasmate').
	language('tmu', '', individual, living, 'Iau').
	language('tmv', '', individual, living, 'Tembo (Motembo)').
	language('tmw', '', individual, living, 'Temuan').
	language('tmy', '', individual, living, 'Tami').
	language('tmz', '', individual, extinct, 'Tamanaku').
	language('tna', '', individual, living, 'Tacana').
	language('tnb', '', individual, living, 'Western Tunebo').
	language('tnc', '', individual, living, 'Tanimuca-Retuarã').
	language('tnd', '', individual, living, 'Angosturas Tunebo').
	language('tng', '', individual, living, 'Tobanga').
	language('tnh', '', individual, living, 'Maiani').
	language('tni', '', individual, living, 'Tandia').
	language('tnk', '', individual, living, 'Kwamera').
	language('tnl', '', individual, living, 'Lenakel').
	language('tnm', '', individual, living, 'Tabla').
	language('tnn', '', individual, living, 'North Tanna').
	language('tno', '', individual, living, 'Toromono').
	language('tnp', '', individual, living, 'Whitesands').
	language('tnq', '', individual, extinct, 'Taino').
	language('tnr', '', individual, living, 'Ménik').
	language('tns', '', individual, living, 'Tenis').
	language('tnt', '', individual, living, 'Tontemboan').
	language('tnu', '', individual, living, 'Tay Khang').
	language('tnv', '', individual, living, 'Tangchangya').
	language('tnw', '', individual, living, 'Tonsawang').
	language('tnx', '', individual, living, 'Tanema').
	language('tny', '', individual, living, 'Tongwe').
	language('tnz', '', individual, living, 'Ten''edn').
	language('tob', '', individual, living, 'Toba').
	language('toc', '', individual, living, 'Coyutla Totonac').
	language('tod', '', individual, living, 'Toma').
	language('tof', '', individual, living, 'Gizrra').
	language('tog', '', individual, living, 'Tonga (Nyasa)').
	language('toh', '', individual, living, 'Gitonga').
	language('toi', '', individual, living, 'Tonga (Zambia)').
	language('toj', '', individual, living, 'Tojolabal').
	language('tok', '', individual, constructed, 'Toki Pona').
	language('tol', '', individual, extinct, 'Tolowa').
	language('tom', '', individual, living, 'Tombulu').
	language('ton', 'to', individual, living, 'Tonga (Tonga Islands)').
	language('too', '', individual, living, 'Xicotepec De Juárez Totonac').
	language('top', '', individual, living, 'Papantla Totonac').
	language('toq', '', individual, living, 'Toposa').
	language('tor', '', individual, living, 'Togbo-Vara Banda').
	language('tos', '', individual, living, 'Highland Totonac').
	language('tou', '', individual, living, 'Tho').
	language('tov', '', individual, living, 'Upper Taromi').
	language('tow', '', individual, living, 'Jemez').
	language('tox', '', individual, living, 'Tobian').
	language('toy', '', individual, living, 'Topoiyo').
	language('toz', '', individual, living, 'To').
	language('tpa', '', individual, living, 'Taupota').
	language('tpc', '', individual, living, 'Azoyú Me''phaa').
	language('tpe', '', individual, living, 'Tippera').
	language('tpf', '', individual, living, 'Tarpia').
	language('tpg', '', individual, living, 'Kula').
	language('tpi', '', individual, living, 'Tok Pisin').
	language('tpj', '', individual, living, 'Tapieté').
	language('tpk', '', individual, extinct, 'Tupinikin').
	language('tpl', '', individual, living, 'Tlacoapa Me''phaa').
	language('tpm', '', individual, living, 'Tampulma').
	language('tpn', '', individual, extinct, 'Tupinambá').
	language('tpo', '', individual, living, 'Tai Pao').
	language('tpp', '', individual, living, 'Pisaflores Tepehua').
	language('tpq', '', individual, living, 'Tukpa').
	language('tpr', '', individual, living, 'Tuparí').
	language('tpt', '', individual, living, 'Tlachichilco Tepehua').
	language('tpu', '', individual, living, 'Tampuan').
	language('tpv', '', individual, living, 'Tanapag').
	language('tpx', '', individual, living, 'Acatepec Me''phaa').
	language('tpy', '', individual, living, 'Trumai').
	language('tpz', '', individual, living, 'Tinputz').
	language('tqb', '', individual, living, 'Tembé').
	language('tql', '', individual, living, 'Lehali').
	language('tqm', '', individual, living, 'Turumsa').
	language('tqn', '', individual, living, 'Tenino').
	language('tqo', '', individual, living, 'Toaripi').
	language('tqp', '', individual, living, 'Tomoip').
	language('tqq', '', individual, living, 'Tunni').
	language('tqr', '', individual, extinct, 'Torona').
	language('tqt', '', individual, living, 'Western Totonac').
	language('tqu', '', individual, living, 'Touo').
	language('tqw', '', individual, extinct, 'Tonkawa').
	language('tra', '', individual, living, 'Tirahi').
	language('trb', '', individual, living, 'Terebu').
	language('trc', '', individual, living, 'Copala Triqui').
	language('trd', '', individual, living, 'Turi').
	language('tre', '', individual, living, 'East Tarangan').
	language('trf', '', individual, living, 'Trinidadian Creole English').
	language('trg', '', individual, living, 'Lishán Didán').
	language('trh', '', individual, living, 'Turaka').
	language('tri', '', individual, living, 'Trió').
	language('trj', '', individual, living, 'Toram').
	language('trl', '', individual, living, 'Traveller Scottish').
	language('trm', '', individual, living, 'Tregami').
	language('trn', '', individual, living, 'Trinitario').
	language('tro', '', individual, living, 'Tarao Naga').
	language('trp', '', individual, living, 'Kok Borok').
	language('trq', '', individual, living, 'San Martín Itunyoso Triqui').
	language('trr', '', individual, living, 'Taushiro').
	language('trs', '', individual, living, 'Chicahuaxtla Triqui').
	language('trt', '', individual, living, 'Tunggare').
	language('tru', '', individual, living, 'Turoyo').
	language('trv', '', individual, living, 'Sediq').
	language('trw', '', individual, living, 'Torwali').
	language('trx', '', individual, living, 'Tringgus-Sembaan Bidayuh').
	language('try', '', individual, extinct, 'Turung').
	language('trz', '', individual, extinct, 'Torá').
	language('tsa', '', individual, living, 'Tsaangi').
	language('tsb', '', individual, living, 'Tsamai').
	language('tsc', '', individual, living, 'Tswa').
	language('tsd', '', individual, living, 'Tsakonian').
	language('tse', '', individual, living, 'Tunisian Sign Language').
	language('tsg', '', individual, living, 'Tausug').
	language('tsh', '', individual, living, 'Tsuvan').
	language('tsi', '', individual, living, 'Tsimshian').
	language('tsj', '', individual, living, 'Tshangla').
	language('tsk', '', individual, living, 'Tseku').
	language('tsl', '', individual, living, 'Ts''ün-Lao').
	language('tsm', '', individual, living, 'Turkish Sign Language').
	language('tsn', 'tn', individual, living, 'Tswana').
	language('tso', 'ts', individual, living, 'Tsonga').
	language('tsp', '', individual, living, 'Northern Toussian').
	language('tsq', '', individual, living, 'Thai Sign Language').
	language('tsr', '', individual, living, 'Akei').
	language('tss', '', individual, living, 'Taiwan Sign Language').
	language('tst', '', individual, living, 'Tondi Songway Kiini').
	language('tsu', '', individual, living, 'Tsou').
	language('tsv', '', individual, living, 'Tsogo').
	language('tsw', '', individual, living, 'Tsishingini').
	language('tsx', '', individual, living, 'Mubami').
	language('tsy', '', individual, living, 'Tebul Sign Language').
	language('tsz', '', individual, living, 'Purepecha').
	language('tta', '', individual, extinct, 'Tutelo').
	language('ttb', '', individual, living, 'Gaa').
	language('ttc', '', individual, living, 'Tektiteko').
	language('ttd', '', individual, living, 'Tauade').
	language('tte', '', individual, living, 'Bwanabwana').
	language('ttf', '', individual, living, 'Tuotomb').
	language('ttg', '', individual, living, 'Tutong').
	language('tth', '', individual, living, 'Upper Ta''oih').
	language('tti', '', individual, living, 'Tobati').
	language('ttj', '', individual, living, 'Tooro').
	language('ttk', '', individual, living, 'Totoro').
	language('ttl', '', individual, living, 'Totela').
	language('ttm', '', individual, living, 'Northern Tutchone').
	language('ttn', '', individual, living, 'Towei').
	language('tto', '', individual, living, 'Lower Ta''oih').
	language('ttp', '', individual, living, 'Tombelala').
	language('ttq', '', individual, living, 'Tawallammat Tamajaq').
	language('ttr', '', individual, living, 'Tera').
	language('tts', '', individual, living, 'Northeastern Thai').
	language('ttt', '', individual, living, 'Muslim Tat').
	language('ttu', '', individual, living, 'Torau').
	language('ttv', '', individual, living, 'Titan').
	language('ttw', '', individual, living, 'Long Wat').
	language('tty', '', individual, living, 'Sikaritai').
	language('ttz', '', individual, living, 'Tsum').
	language('tua', '', individual, living, 'Wiarumus').
	language('tub', '', individual, extinct, 'Tübatulabal').
	language('tuc', '', individual, living, 'Mutu').
	language('tud', '', individual, extinct, 'Tuxá').
	language('tue', '', individual, living, 'Tuyuca').
	language('tuf', '', individual, living, 'Central Tunebo').
	language('tug', '', individual, living, 'Tunia').
	language('tuh', '', individual, living, 'Taulil').
	language('tui', '', individual, living, 'Tupuri').
	language('tuj', '', individual, living, 'Tugutil').
	language('tuk', 'tk', individual, living, 'Turkmen').
	language('tul', '', individual, living, 'Tula').
	language('tum', '', individual, living, 'Tumbuka').
	language('tun', '', individual, living, 'Tunica').
	language('tuo', '', individual, living, 'Tucano').
	language('tuq', '', individual, living, 'Tedaga').
	language('tur', 'tr', individual, living, 'Turkish').
	language('tus', '', individual, living, 'Tuscarora').
	language('tuu', '', individual, living, 'Tututni').
	language('tuv', '', individual, living, 'Turkana').
	language('tux', '', individual, extinct, 'Tuxináwa').
	language('tuy', '', individual, living, 'Tugen').
	language('tuz', '', individual, living, 'Turka').
	language('tva', '', individual, living, 'Vaghua').
	language('tvd', '', individual, living, 'Tsuvadi').
	language('tve', '', individual, living, 'Te''un').
	language('tvg', '', individual, extinct, 'Tugunese').
	language('tvi', '', individual, living, 'Tulai').
	language('tvk', '', individual, living, 'Southeast Ambrym').
	language('tvl', '', individual, living, 'Tuvalu').
	language('tvm', '', individual, living, 'Tela-Masbuar').
	language('tvn', '', individual, living, 'Tavoyan').
	language('tvo', '', individual, living, 'Tidore').
	language('tvs', '', individual, living, 'Taveta').
	language('tvt', '', individual, living, 'Tutsa Naga').
	language('tvu', '', individual, living, 'Tunen').
	language('tvw', '', individual, living, 'Sedoa').
	language('tvx', '', individual, extinct, 'Taivoan').
	language('tvy', '', individual, extinct, 'Timor Pidgin').
	language('twa', '', individual, extinct, 'Twana').
	language('twb', '', individual, living, 'Western Tawbuid').
	language('twc', '', individual, extinct, 'Teshenawa').
	language('twd', '', individual, living, 'Twents').
	language('twe', '', individual, living, 'Tewa (Indonesia)').
	language('twf', '', individual, living, 'Northern Tiwa').
	language('twg', '', individual, living, 'Tereweng').
	language('twh', '', individual, living, 'Tai Dón').
	language('twi', 'tw', individual, living, 'Twi').
	language('twl', '', individual, living, 'Tawara').
	language('twm', '', individual, living, 'Tawang Monpa').
	language('twn', '', individual, living, 'Twendi').
	language('two', '', individual, living, 'Tswapong').
	language('twp', '', individual, living, 'Ere').
	language('twq', '', individual, living, 'Tasawaq').
	language('twr', '', individual, living, 'Southwestern Tarahumara').
	language('twt', '', individual, extinct, 'Turiwára').
	language('twu', '', individual, living, 'Termanu').
	language('tww', '', individual, living, 'Tuwari').
	language('twx', '', individual, living, 'Tewe').
	language('twy', '', individual, living, 'Tawoyan').
	language('txa', '', individual, living, 'Tombonuo').
	language('txb', '', individual, special, 'Tokharian B').
	language('txc', '', individual, extinct, 'Tsetsaut').
	language('txe', '', individual, living, 'Totoli').
	language('txg', '', individual, special, 'Tangut').
	language('txh', '', individual, special, 'Thracian').
	language('txi', '', individual, living, 'Ikpeng').
	language('txj', '', individual, living, 'Tarjumo').
	language('txm', '', individual, living, 'Tomini').
	language('txn', '', individual, living, 'West Tarangan').
	language('txo', '', individual, living, 'Toto').
	language('txq', '', individual, living, 'Tii').
	language('txr', '', individual, special, 'Tartessian').
	language('txs', '', individual, living, 'Tonsea').
	language('txt', '', individual, living, 'Citak').
	language('txu', '', individual, living, 'Kayapó').
	language('txx', '', individual, living, 'Tatana').
	language('txy', '', individual, living, 'Tanosy Malagasy').
	language('tya', '', individual, living, 'Tauya').
	language('tye', '', individual, living, 'Kyanga').
	language('tyh', '', individual, living, 'O''du').
	language('tyi', '', individual, living, 'Teke-Tsaayi').
	language('tyj', '', individual, living, 'Tai Do').
	language('tyl', '', individual, living, 'Thu Lao').
	language('tyn', '', individual, living, 'Kombai').
	language('typ', '', individual, extinct, 'Thaypan').
	language('tyr', '', individual, living, 'Tai Daeng').
	language('tys', '', individual, living, 'Tày Sa Pa').
	language('tyt', '', individual, living, 'Tày Tac').
	language('tyu', '', individual, living, 'Kua').
	language('tyv', '', individual, living, 'Tuvinian').
	language('tyx', '', individual, living, 'Teke-Tyee').
	language('tyy', '', individual, living, 'Tiyaa').
	language('tyz', '', individual, living, 'Tày').
	language('tza', '', individual, living, 'Tanzanian Sign Language').
	language('tzh', '', individual, living, 'Tzeltal').
	language('tzj', '', individual, living, 'Tz''utujil').
	language('tzl', '', individual, constructed, 'Talossan').
	language('tzm', '', individual, living, 'Central Atlas Tamazight').
	language('tzn', '', individual, living, 'Tugun').
	language('tzo', '', individual, living, 'Tzotzil').
	language('tzx', '', individual, living, 'Tabriak').
	language('uam', '', individual, extinct, 'Uamué').
	language('uan', '', individual, living, 'Kuan').
	language('uar', '', individual, living, 'Tairuma').
	language('uba', '', individual, living, 'Ubang').
	language('ubi', '', individual, living, 'Ubi').
	language('ubl', '', individual, living, 'Buhi''non Bikol').
	language('ubr', '', individual, living, 'Ubir').
	language('ubu', '', individual, living, 'Umbu-Ungu').
	language('uby', '', individual, extinct, 'Ubykh').
	language('uda', '', individual, living, 'Uda').
	language('ude', '', individual, living, 'Udihe').
	language('udg', '', individual, living, 'Muduga').
	language('udi', '', individual, living, 'Udi').
	language('udj', '', individual, living, 'Ujir').
	language('udl', '', individual, living, 'Wuzlam').
	language('udm', '', individual, living, 'Udmurt').
	language('udu', '', individual, living, 'Uduk').
	language('ues', '', individual, living, 'Kioko').
	language('ufi', '', individual, living, 'Ufim').
	language('uga', '', individual, special, 'Ugaritic').
	language('ugb', '', individual, extinct, 'Kuku-Ugbanh').
	language('uge', '', individual, living, 'Ughele').
	language('ugh', '', individual, living, 'Kubachi').
	language('ugn', '', individual, living, 'Ugandan Sign Language').
	language('ugo', '', individual, living, 'Ugong').
	language('ugy', '', individual, living, 'Uruguayan Sign Language').
	language('uha', '', individual, living, 'Uhami').
	language('uhn', '', individual, living, 'Damal').
	language('uig', 'ug', individual, living, 'Uighur').
	language('uis', '', individual, living, 'Uisai').
	language('uiv', '', individual, living, 'Iyive').
	language('uji', '', individual, living, 'Tanjijili').
	language('uka', '', individual, living, 'Kaburi').
	language('ukg', '', individual, living, 'Ukuriguma').
	language('ukh', '', individual, living, 'Ukhwejo').
	language('uki', '', individual, living, 'Kui (India)').
	language('ukk', '', individual, living, 'Muak Sa-aak').
	language('ukl', '', individual, living, 'Ukrainian Sign Language').
	language('ukp', '', individual, living, 'Ukpe-Bayobiri').
	language('ukq', '', individual, living, 'Ukwa').
	language('ukr', 'uk', individual, living, 'Ukrainian').
	language('uks', '', individual, living, 'Urubú-Kaapor Sign Language').
	language('uku', '', individual, living, 'Ukue').
	language('ukv', '', individual, living, 'Kuku').
	language('ukw', '', individual, living, 'Ukwuani-Aboh-Ndoni').
	language('uky', '', individual, extinct, 'Kuuk-Yak').
	language('ula', '', individual, living, 'Fungwa').
	language('ulb', '', individual, living, 'Ulukwumi').
	language('ulc', '', individual, living, 'Ulch').
	language('ule', '', individual, extinct, 'Lule').
	language('ulf', '', individual, living, 'Usku').
	language('uli', '', individual, living, 'Ulithian').
	language('ulk', '', individual, living, 'Meriam Mir').
	language('ull', '', individual, living, 'Ullatan').
	language('ulm', '', individual, living, 'Ulumanda''').
	language('uln', '', individual, living, 'Unserdeutsch').
	language('ulu', '', individual, living, 'Uma'' Lung').
	language('ulw', '', individual, living, 'Ulwa').
	language('uly', '', individual, living, 'Buli').
	language('uma', '', individual, living, 'Umatilla').
	language('umb', '', individual, living, 'Umbundu').
	language('umc', '', individual, special, 'Marrucinian').
	language('umd', '', individual, extinct, 'Umbindhamu').
	language('umg', '', individual, extinct, 'Morrobalama').
	language('umi', '', individual, living, 'Ukit').
	language('umm', '', individual, living, 'Umon').
	language('umn', '', individual, living, 'Makyan Naga').
	language('umo', '', individual, extinct, 'Umotína').
	language('ump', '', individual, living, 'Umpila').
	language('umr', '', individual, extinct, 'Umbugarla').
	language('ums', '', individual, living, 'Pendau').
	language('umu', '', individual, living, 'Munsee').
	language('una', '', individual, living, 'North Watut').
	language('und', '', special, special, 'Undetermined').
	language('une', '', individual, living, 'Uneme').
	language('ung', '', individual, living, 'Ngarinyin').
	language('uni', '', individual, living, 'Uni').
	language('unk', '', individual, living, 'Enawené-Nawé').
	language('unm', '', individual, extinct, 'Unami').
	language('unn', '', individual, living, 'Kurnai').
	language('unr', '', individual, living, 'Mundari').
	language('unu', '', individual, living, 'Unubahe').
	language('unx', '', individual, living, 'Munda').
	language('unz', '', individual, living, 'Unde Kaili').
	language('uon', '', individual, extinct, 'Kulon').
	language('upi', '', individual, living, 'Umeda').
	language('upv', '', individual, living, 'Uripiv-Wala-Rano-Atchin').
	language('ura', '', individual, living, 'Urarina').
	language('urb', '', individual, living, 'Urubú-Kaapor').
	language('urc', '', individual, extinct, 'Urningangg').
	language('urd', 'ur', individual, living, 'Urdu').
	language('ure', '', individual, living, 'Uru').
	language('urf', '', individual, extinct, 'Uradhi').
	language('urg', '', individual, living, 'Urigina').
	language('urh', '', individual, living, 'Urhobo').
	language('uri', '', individual, living, 'Urim').
	language('urk', '', individual, living, 'Urak Lawoi''').
	language('url', '', individual, living, 'Urali').
	language('urm', '', individual, living, 'Urapmin').
	language('urn', '', individual, living, 'Uruangnirin').
	language('uro', '', individual, living, 'Ura (Papua New Guinea)').
	language('urp', '', individual, living, 'Uru-Pa-In').
	language('urr', '', individual, living, 'Lehalurup').
	language('urt', '', individual, living, 'Urat').
	language('uru', '', individual, extinct, 'Urumi').
	language('urv', '', individual, extinct, 'Uruava').
	language('urw', '', individual, living, 'Sop').
	language('urx', '', individual, living, 'Urimo').
	language('ury', '', individual, living, 'Orya').
	language('urz', '', individual, living, 'Uru-Eu-Wau-Wau').
	language('usa', '', individual, living, 'Usarufa').
	language('ush', '', individual, living, 'Ushojo').
	language('usi', '', individual, living, 'Usui').
	language('usk', '', individual, living, 'Usaghade').
	language('usp', '', individual, living, 'Uspanteco').
	language('uss', '', individual, living, 'us-Saare').
	language('usu', '', individual, living, 'Uya').
	language('uta', '', individual, living, 'Otank').
	language('ute', '', individual, living, 'Ute-Southern Paiute').
	language('uth', '', individual, living, 'ut-Hun').
	language('utp', '', individual, living, 'Amba (Solomon Islands)').
	language('utr', '', individual, living, 'Etulo').
	language('utu', '', individual, living, 'Utu').
	language('uum', '', individual, living, 'Urum').
	language('uur', '', individual, living, 'Ura (Vanuatu)').
	language('uuu', '', individual, living, 'U').
	language('uve', '', individual, living, 'West Uvean').
	language('uvh', '', individual, living, 'Uri').
	language('uvl', '', individual, living, 'Lote').
	language('uwa', '', individual, living, 'Kuku-Uwanh').
	language('uya', '', individual, living, 'Doko-Uyanga').
	language('uzb', 'uz', macrolanguage, living, 'Uzbek').
	language('uzn', '', individual, living, 'Northern Uzbek').
	language('uzs', '', individual, living, 'Southern Uzbek').
	language('vaa', '', individual, living, 'Vaagri Booli').
	language('vae', '', individual, living, 'Vale').
	language('vaf', '', individual, living, 'Vafsi').
	language('vag', '', individual, living, 'Vagla').
	language('vah', '', individual, living, 'Varhadi-Nagpuri').
	language('vai', '', individual, living, 'Vai').
	language('vaj', '', individual, living, 'Sekele').
	language('val', '', individual, living, 'Vehes').
	language('vam', '', individual, living, 'Vanimo').
	language('van', '', individual, living, 'Valman').
	language('vao', '', individual, living, 'Vao').
	language('vap', '', individual, living, 'Vaiphei').
	language('var', '', individual, living, 'Huarijio').
	language('vas', '', individual, living, 'Vasavi').
	language('vau', '', individual, living, 'Vanuma').
	language('vav', '', individual, living, 'Varli').
	language('vay', '', individual, living, 'Wayu').
	language('vbb', '', individual, living, 'Southeast Babar').
	language('vbk', '', individual, living, 'Southwestern Bontok').
	language('vec', '', individual, living, 'Venetian').
	language('ved', '', individual, living, 'Veddah').
	language('vel', '', individual, living, 'Veluws').
	language('vem', '', individual, living, 'Vemgo-Mabas').
	language('ven', 've', individual, living, 'Venda').
	language('veo', '', individual, extinct, 'Ventureño').
	language('vep', '', individual, living, 'Veps').
	language('ver', '', individual, living, 'Mom Jango').
	language('vgr', '', individual, living, 'Vaghri').
	language('vgt', '', individual, living, 'Vlaamse Gebarentaal').
	language('vic', '', individual, living, 'Virgin Islands Creole English').
	language('vid', '', individual, living, 'Vidunda').
	language('vie', 'vi', individual, living, 'Vietnamese').
	language('vif', '', individual, living, 'Vili').
	language('vig', '', individual, living, 'Viemo').
	language('vil', '', individual, living, 'Vilela').
	language('vin', '', individual, living, 'Vinza').
	language('vis', '', individual, living, 'Vishavan').
	language('vit', '', individual, living, 'Viti').
	language('viv', '', individual, living, 'Iduna').
	language('vjk', '', individual, living, 'Bajjika').
	language('vka', '', individual, extinct, 'Kariyarra').
	language('vkj', '', individual, living, 'Kujarge').
	language('vkk', '', individual, living, 'Kaur').
	language('vkl', '', individual, living, 'Kulisusu').
	language('vkm', '', individual, extinct, 'Kamakan').
	language('vkn', '', individual, living, 'Koro Nulu').
	language('vko', '', individual, living, 'Kodeoha').
	language('vkp', '', individual, living, 'Korlai Creole Portuguese').
	language('vkt', '', individual, living, 'Tenggarong Kutai Malay').
	language('vku', '', individual, living, 'Kurrama').
	language('vkz', '', individual, living, 'Koro Zuba').
	language('vlp', '', individual, living, 'Valpei').
	language('vls', '', individual, living, 'Vlaams').
	language('vma', '', individual, extinct, 'Martuyhunira').
	language('vmb', '', individual, extinct, 'Barbaram').
	language('vmc', '', individual, living, 'Juxtlahuaca Mixtec').
	language('vmd', '', individual, living, 'Mudu Koraga').
	language('vme', '', individual, living, 'East Masela').
	language('vmf', '', individual, living, 'Mainfränkisch').
	language('vmg', '', individual, living, 'Lungalunga').
	language('vmh', '', individual, living, 'Maraghei').
	language('vmi', '', individual, extinct, 'Miwa').
	language('vmj', '', individual, living, 'Ixtayutla Mixtec').
	language('vmk', '', individual, living, 'Makhuwa-Shirima').
	language('vml', '', individual, extinct, 'Malgana').
	language('vmm', '', individual, living, 'Mitlatongo Mixtec').
	language('vmp', '', individual, living, 'Soyaltepec Mazatec').
	language('vmq', '', individual, living, 'Soyaltepec Mixtec').
	language('vmr', '', individual, living, 'Marenje').
	language('vms', '', individual, extinct, 'Moksela').
	language('vmu', '', individual, extinct, 'Muluridyi').
	language('vmv', '', individual, extinct, 'Valley Maidu').
	language('vmw', '', individual, living, 'Makhuwa').
	language('vmx', '', individual, living, 'Tamazola Mixtec').
	language('vmy', '', individual, living, 'Ayautla Mazatec').
	language('vmz', '', individual, living, 'Mazatlán Mazatec').
	language('vnk', '', individual, living, 'Vano').
	language('vnm', '', individual, living, 'Vinmavis').
	language('vnp', '', individual, living, 'Vunapu').
	language('vol', 'vo', individual, constructed, 'Volapük').
	language('vor', '', individual, living, 'Voro').
	language('vot', '', individual, living, 'Votic').
	language('vra', '', individual, living, 'Vera''a').
	language('vro', '', individual, living, 'Võro').
	language('vrs', '', individual, living, 'Varisi').
	language('vrt', '', individual, living, 'Burmbar').
	language('vsi', '', individual, living, 'Moldova Sign Language').
	language('vsl', '', individual, living, 'Venezuelan Sign Language').
	language('vsn', '', individual, special, 'Vedic Sanskrit').
	language('vsv', '', individual, living, 'Valencian Sign Language').
	language('vto', '', individual, living, 'Vitou').
	language('vum', '', individual, living, 'Vumbu').
	language('vun', '', individual, living, 'Vunjo').
	language('vut', '', individual, living, 'Vute').
	language('vwa', '', individual, living, 'Awa (China)').
	language('waa', '', individual, living, 'Walla Walla').
	language('wab', '', individual, living, 'Yote').
	language('wac', '', individual, extinct, 'Wasco-Wishram').
	language('wad', '', individual, living, 'Wamesa').
	language('wae', '', individual, living, 'Walser').
	language('waf', '', individual, extinct, 'Wakoná').
	language('wag', '', individual, living, 'Wa''ema').
	language('wah', '', individual, living, 'Watubela').
	language('wai', '', individual, living, 'Wares').
	language('waj', '', individual, living, 'Waffa').
	language('wal', '', individual, living, 'Wolaytta').
	language('wam', '', individual, extinct, 'Wampanoag').
	language('wan', '', individual, living, 'Wan').
	language('wao', '', individual, extinct, 'Wappo').
	language('wap', '', individual, living, 'Wapishana').
	language('waq', '', individual, living, 'Wagiman').
	language('war', '', individual, living, 'Waray (Philippines)').
	language('was', '', individual, living, 'Washo').
	language('wat', '', individual, living, 'Kaninuwa').
	language('wau', '', individual, living, 'Waurá').
	language('wav', '', individual, living, 'Waka').
	language('waw', '', individual, living, 'Waiwai').
	language('wax', '', individual, living, 'Watam').
	language('way', '', individual, living, 'Wayana').
	language('waz', '', individual, living, 'Wampur').
	language('wba', '', individual, living, 'Warao').
	language('wbb', '', individual, living, 'Wabo').
	language('wbe', '', individual, living, 'Waritai').
	language('wbf', '', individual, living, 'Wara').
	language('wbh', '', individual, living, 'Wanda').
	language('wbi', '', individual, living, 'Vwanji').
	language('wbj', '', individual, living, 'Alagwa').
	language('wbk', '', individual, living, 'Waigali').
	language('wbl', '', individual, living, 'Wakhi').
	language('wbm', '', individual, living, 'Wa').
	language('wbp', '', individual, living, 'Warlpiri').
	language('wbq', '', individual, living, 'Waddar').
	language('wbr', '', individual, living, 'Wagdi').
	language('wbs', '', individual, living, 'West Bengal Sign Language').
	language('wbt', '', individual, living, 'Warnman').
	language('wbv', '', individual, living, 'Wajarri').
	language('wbw', '', individual, living, 'Woi').
	language('wca', '', individual, living, 'Yanomámi').
	language('wci', '', individual, living, 'Waci Gbe').
	language('wdd', '', individual, living, 'Wandji').
	language('wdg', '', individual, living, 'Wadaginam').
	language('wdj', '', individual, living, 'Wadjiginy').
	language('wdk', '', individual, extinct, 'Wadikali').
	language('wdt', '', individual, living, 'Wendat').
	language('wdu', '', individual, extinct, 'Wadjigu').
	language('wdy', '', individual, extinct, 'Wadjabangayi').
	language('wea', '', individual, extinct, 'Wewaw').
	language('wec', '', individual, living, 'Wè Western').
	language('wed', '', individual, living, 'Wedau').
	language('weg', '', individual, living, 'Wergaia').
	language('weh', '', individual, living, 'Weh').
	language('wei', '', individual, living, 'Kiunum').
	language('wem', '', individual, living, 'Weme Gbe').
	language('weo', '', individual, living, 'Wemale').
	language('wep', '', individual, living, 'Westphalien').
	language('wer', '', individual, living, 'Weri').
	language('wes', '', individual, living, 'Cameroon Pidgin').
	language('wet', '', individual, living, 'Perai').
	language('weu', '', individual, living, 'Rawngtu Chin').
	language('wew', '', individual, living, 'Wejewa').
	language('wfg', '', individual, living, 'Yafi').
	language('wga', '', individual, extinct, 'Wagaya').
	language('wgb', '', individual, living, 'Wagawaga').
	language('wgg', '', individual, extinct, 'Wangkangurru').
	language('wgi', '', individual, living, 'Wahgi').
	language('wgo', '', individual, living, 'Waigeo').
	language('wgu', '', individual, extinct, 'Wirangu').
	language('wgy', '', individual, living, 'Warrgamay').
	language('wha', '', individual, living, 'Sou Upaa').
	language('whg', '', individual, living, 'North Wahgi').
	language('whk', '', individual, living, 'Wahau Kenyah').
	language('whu', '', individual, living, 'Wahau Kayan').
	language('wib', '', individual, living, 'Southern Toussian').
	language('wic', '', individual, extinct, 'Wichita').
	language('wie', '', individual, extinct, 'Wik-Epa').
	language('wif', '', individual, extinct, 'Wik-Keyangan').
	language('wig', '', individual, living, 'Wik Ngathan').
	language('wih', '', individual, living, 'Wik-Me''anha').
	language('wii', '', individual, living, 'Minidien').
	language('wij', '', individual, living, 'Wik-Iiyanh').
	language('wik', '', individual, living, 'Wikalkan').
	language('wil', '', individual, extinct, 'Wilawila').
	language('wim', '', individual, living, 'Wik-Mungkan').
	language('win', '', individual, living, 'Ho-Chunk').
	language('wir', '', individual, extinct, 'Wiraféd').
	language('wiu', '', individual, living, 'Wiru').
	language('wiv', '', individual, living, 'Vitu').
	language('wiy', '', individual, extinct, 'Wiyot').
	language('wja', '', individual, living, 'Waja').
	language('wji', '', individual, living, 'Warji').
	language('wka', '', individual, extinct, 'Kw''adza').
	language('wkb', '', individual, living, 'Kumbaran').
	language('wkd', '', individual, living, 'Wakde').
	language('wkl', '', individual, living, 'Kalanadi').
	language('wkr', '', individual, living, 'Keerray-Woorroong').
	language('wku', '', individual, living, 'Kunduvadi').
	language('wkw', '', individual, extinct, 'Wakawaka').
	language('wky', '', individual, extinct, 'Wangkayutyuru').
	language('wla', '', individual, living, 'Walio').
	language('wlc', '', individual, living, 'Mwali Comorian').
	language('wle', '', individual, living, 'Wolane').
	language('wlg', '', individual, living, 'Kunbarlang').
	language('wlh', '', individual, living, 'Welaun').
	language('wli', '', individual, living, 'Waioli').
	language('wlk', '', individual, extinct, 'Wailaki').
	language('wll', '', individual, living, 'Wali (Sudan)').
	language('wlm', '', individual, special, 'Middle Welsh').
	language('wln', 'wa', individual, living, 'Walloon').
	language('wlo', '', individual, living, 'Wolio').
	language('wlr', '', individual, living, 'Wailapa').
	language('wls', '', individual, living, 'Wallisian').
	language('wlu', '', individual, extinct, 'Wuliwuli').
	language('wlv', '', individual, living, 'Wichí Lhamtés Vejoz').
	language('wlw', '', individual, living, 'Walak').
	language('wlx', '', individual, living, 'Wali (Ghana)').
	language('wly', '', individual, extinct, 'Waling').
	language('wma', '', individual, extinct, 'Mawa (Nigeria)').
	language('wmb', '', individual, living, 'Wambaya').
	language('wmc', '', individual, living, 'Wamas').
	language('wmd', '', individual, living, 'Mamaindé').
	language('wme', '', individual, living, 'Wambule').
	language('wmg', '', individual, living, 'Western Minyag').
	language('wmh', '', individual, living, 'Waima''a').
	language('wmi', '', individual, extinct, 'Wamin').
	language('wmm', '', individual, living, 'Maiwa (Indonesia)').
	language('wmn', '', individual, extinct, 'Waamwang').
	language('wmo', '', individual, living, 'Wom (Papua New Guinea)').
	language('wms', '', individual, living, 'Wambon').
	language('wmt', '', individual, living, 'Walmajarri').
	language('wmw', '', individual, living, 'Mwani').
	language('wmx', '', individual, living, 'Womo').
	language('wnb', '', individual, living, 'Mokati').
	language('wnc', '', individual, living, 'Wantoat').
	language('wnd', '', individual, extinct, 'Wandarang').
	language('wne', '', individual, living, 'Waneci').
	language('wng', '', individual, living, 'Wanggom').
	language('wni', '', individual, living, 'Ndzwani Comorian').
	language('wnk', '', individual, living, 'Wanukaka').
	language('wnm', '', individual, extinct, 'Wanggamala').
	language('wnn', '', individual, extinct, 'Wunumara').
	language('wno', '', individual, living, 'Wano').
	language('wnp', '', individual, living, 'Wanap').
	language('wnu', '', individual, living, 'Usan').
	language('wnw', '', individual, living, 'Wintu').
	language('wny', '', individual, living, 'Wanyi').
	language('woa', '', individual, living, 'Kuwema').
	language('wob', '', individual, living, 'Wè Northern').
	language('woc', '', individual, living, 'Wogeo').
	language('wod', '', individual, living, 'Wolani').
	language('woe', '', individual, living, 'Woleaian').
	language('wof', '', individual, living, 'Gambian Wolof').
	language('wog', '', individual, living, 'Wogamusin').
	language('woi', '', individual, living, 'Kamang').
	language('wok', '', individual, living, 'Longto').
	language('wol', 'wo', individual, living, 'Wolof').
	language('wom', '', individual, living, 'Wom (Nigeria)').
	language('won', '', individual, living, 'Wongo').
	language('woo', '', individual, living, 'Manombai').
	language('wor', '', individual, living, 'Woria').
	language('wos', '', individual, living, 'Hanga Hundi').
	language('wow', '', individual, living, 'Wawonii').
	language('woy', '', individual, extinct, 'Weyto').
	language('wpc', '', individual, living, 'Maco').
	language('wrb', '', individual, extinct, 'Waluwarra').
	language('wrg', '', individual, extinct, 'Warungu').
	language('wrh', '', individual, extinct, 'Wiradjuri').
	language('wri', '', individual, extinct, 'Wariyangga').
	language('wrk', '', individual, living, 'Garrwa').
	language('wrl', '', individual, living, 'Warlmanpa').
	language('wrm', '', individual, living, 'Warumungu').
	language('wrn', '', individual, living, 'Warnang').
	language('wro', '', individual, extinct, 'Worrorra').
	language('wrp', '', individual, living, 'Waropen').
	language('wrr', '', individual, living, 'Wardaman').
	language('wrs', '', individual, living, 'Waris').
	language('wru', '', individual, living, 'Waru').
	language('wrv', '', individual, living, 'Waruna').
	language('wrw', '', individual, extinct, 'Gugu Warra').
	language('wrx', '', individual, living, 'Wae Rana').
	language('wry', '', individual, living, 'Merwari').
	language('wrz', '', individual, extinct, 'Waray (Australia)').
	language('wsa', '', individual, living, 'Warembori').
	language('wsg', '', individual, living, 'Adilabad Gondi').
	language('wsi', '', individual, living, 'Wusi').
	language('wsk', '', individual, living, 'Waskia').
	language('wsr', '', individual, living, 'Owenia').
	language('wss', '', individual, living, 'Wasa').
	language('wsu', '', individual, extinct, 'Wasu').
	language('wsv', '', individual, extinct, 'Wotapuri-Katarqalai').
	language('wtb', '', individual, living, 'Matambwe').
	language('wtf', '', individual, living, 'Watiwa').
	language('wth', '', individual, extinct, 'Wathawurrung').
	language('wti', '', individual, living, 'Berta').
	language('wtk', '', individual, living, 'Watakataui').
	language('wtm', '', individual, living, 'Mewati').
	language('wtw', '', individual, living, 'Wotu').
	language('wua', '', individual, living, 'Wikngenchera').
	language('wub', '', individual, living, 'Wunambal').
	language('wud', '', individual, living, 'Wudu').
	language('wuh', '', individual, living, 'Wutunhua').
	language('wul', '', individual, living, 'Silimo').
	language('wum', '', individual, living, 'Wumbvu').
	language('wun', '', individual, living, 'Bungu').
	language('wur', '', individual, extinct, 'Wurrugu').
	language('wut', '', individual, living, 'Wutung').
	language('wuu', '', individual, living, 'Wu Chinese').
	language('wuv', '', individual, living, 'Wuvulu-Aua').
	language('wux', '', individual, living, 'Wulna').
	language('wuy', '', individual, living, 'Wauyai').
	language('wwa', '', individual, living, 'Waama').
	language('wwb', '', individual, extinct, 'Wakabunga').
	language('wwo', '', individual, living, 'Wetamut').
	language('wwr', '', individual, extinct, 'Warrwa').
	language('www', '', individual, living, 'Wawa').
	language('wxa', '', individual, living, 'Waxianghua').
	language('wxw', '', individual, extinct, 'Wardandi').
	language('wyb', '', individual, living, 'Wangaaybuwan-Ngiyambaa').
	language('wyi', '', individual, extinct, 'Woiwurrung').
	language('wym', '', individual, living, 'Wymysorys').
	language('wyn', '', individual, living, 'Wyandot').
	language('wyr', '', individual, living, 'Wayoró').
	language('wyy', '', individual, living, 'Western Fijian').
	language('xaa', '', individual, special, 'Andalusian Arabic').
	language('xab', '', individual, living, 'Sambe').
	language('xac', '', individual, living, 'Kachari').
	language('xad', '', individual, extinct, 'Adai').
	language('xae', '', individual, special, 'Aequian').
	language('xag', '', individual, special, 'Aghwan').
	language('xai', '', individual, extinct, 'Kaimbé').
	language('xaj', '', individual, extinct, 'Ararandewára').
	language('xak', '', individual, extinct, 'Máku').
	language('xal', '', individual, living, 'Kalmyk').
	language('xam', '', individual, extinct, 'ǀXam').
	language('xan', '', individual, living, 'Xamtanga').
	language('xao', '', individual, living, 'Khao').
	language('xap', '', individual, extinct, 'Apalachee').
	language('xaq', '', individual, special, 'Aquitanian').
	language('xar', '', individual, extinct, 'Karami').
	language('xas', '', individual, extinct, 'Kamas').
	language('xat', '', individual, living, 'Katawixi').
	language('xau', '', individual, living, 'Kauwera').
	language('xav', '', individual, living, 'Xavánte').
	language('xaw', '', individual, living, 'Kawaiisu').
	language('xay', '', individual, living, 'Kayan Mahakam').
	language('xbb', '', individual, extinct, 'Lower Burdekin').
	language('xbc', '', individual, special, 'Bactrian').
	language('xbd', '', individual, extinct, 'Bindal').
	language('xbe', '', individual, extinct, 'Bigambal').
	language('xbg', '', individual, extinct, 'Bunganditj').
	language('xbi', '', individual, living, 'Kombio').
	language('xbj', '', individual, extinct, 'Birrpayi').
	language('xbm', '', individual, special, 'Middle Breton').
	language('xbn', '', individual, extinct, 'Kenaboi').
	language('xbo', '', individual, special, 'Bolgarian').
	language('xbp', '', individual, extinct, 'Bibbulman').
	language('xbr', '', individual, living, 'Kambera').
	language('xbw', '', individual, extinct, 'Kambiwá').
	language('xby', '', individual, living, 'Batjala').
	language('xcb', '', individual, special, 'Cumbric').
	language('xcc', '', individual, special, 'Camunic').
	language('xce', '', individual, special, 'Celtiberian').
	language('xcg', '', individual, special, 'Cisalpine Gaulish').
	language('xch', '', individual, extinct, 'Chemakum').
	language('xcl', '', individual, special, 'Classical Armenian').
	language('xcm', '', individual, extinct, 'Comecrudo').
	language('xcn', '', individual, extinct, 'Cotoname').
	language('xco', '', individual, special, 'Chorasmian').
	language('xcr', '', individual, special, 'Carian').
	language('xct', '', individual, special, 'Classical Tibetan').
	language('xcu', '', individual, special, 'Curonian').
	language('xcv', '', individual, extinct, 'Chuvantsy').
	language('xcw', '', individual, extinct, 'Coahuilteco').
	language('xcy', '', individual, extinct, 'Cayuse').
	language('xda', '', individual, living, 'Darkinyung').
	language('xdc', '', individual, special, 'Dacian').
	language('xdk', '', individual, extinct, 'Dharuk').
	language('xdm', '', individual, special, 'Edomite').
	language('xdo', '', individual, living, 'Kwandu').
	language('xdq', '', individual, living, 'Kaitag').
	language('xdy', '', individual, living, 'Malayic Dayak').
	language('xeb', '', individual, special, 'Eblan').
	language('xed', '', individual, living, 'Hdi').
	language('xeg', '', individual, extinct, 'ǁXegwi').
	language('xel', '', individual, living, 'Kelo').
	language('xem', '', individual, living, 'Kembayan').
	language('xep', '', individual, special, 'Epi-Olmec').
	language('xer', '', individual, living, 'Xerénte').
	language('xes', '', individual, living, 'Kesawai').
	language('xet', '', individual, living, 'Xetá').
	language('xeu', '', individual, living, 'Keoru-Ahia').
	language('xfa', '', individual, special, 'Faliscan').
	language('xga', '', individual, special, 'Galatian').
	language('xgb', '', individual, extinct, 'Gbin').
	language('xgd', '', individual, extinct, 'Gudang').
	language('xgf', '', individual, extinct, 'Gabrielino-Fernandeño').
	language('xgg', '', individual, extinct, 'Goreng').
	language('xgi', '', individual, extinct, 'Garingbal').
	language('xgl', '', individual, special, 'Galindan').
	language('xgm', '', individual, extinct, 'Dharumbal').
	language('xgr', '', individual, extinct, 'Garza').
	language('xgu', '', individual, living, 'Unggumi').
	language('xgw', '', individual, extinct, 'Guwa').
	language('xha', '', individual, special, 'Harami').
	language('xhc', '', individual, special, 'Hunnic').
	language('xhd', '', individual, special, 'Hadrami').
	language('xhe', '', individual, living, 'Khetrani').
	language('xhm', '', individual, special, 'Middle Khmer (1400 to 1850 CE)').
	language('xho', 'xh', individual, living, 'Xhosa').
	language('xhr', '', individual, special, 'Hernican').
	language('xht', '', individual, special, 'Hattic').
	language('xhu', '', individual, special, 'Hurrian').
	language('xhv', '', individual, living, 'Khua').
	language('xib', '', individual, special, 'Iberian').
	language('xii', '', individual, living, 'Xiri').
	language('xil', '', individual, special, 'Illyrian').
	language('xin', '', individual, extinct, 'Xinca').
	language('xir', '', individual, extinct, 'Xiriâna').
	language('xis', '', individual, living, 'Kisan').
	language('xiv', '', individual, special, 'Indus Valley Language').
	language('xiy', '', individual, living, 'Xipaya').
	language('xjb', '', individual, extinct, 'Minjungbal').
	language('xjt', '', individual, extinct, 'Jaitmatang').
	language('xka', '', individual, living, 'Kalkoti').
	language('xkb', '', individual, living, 'Northern Nago').
	language('xkc', '', individual, living, 'Kho''ini').
	language('xkd', '', individual, living, 'Mendalam Kayan').
	language('xke', '', individual, living, 'Kereho').
	language('xkf', '', individual, living, 'Khengkha').
	language('xkg', '', individual, living, 'Kagoro').
	language('xki', '', individual, living, 'Kenyan Sign Language').
	language('xkj', '', individual, living, 'Kajali').
	language('xkk', '', individual, living, 'Kachok').
	language('xkl', '', individual, living, 'Mainstream Kenyah').
	language('xkn', '', individual, living, 'Kayan River Kayan').
	language('xko', '', individual, living, 'Kiorr').
	language('xkp', '', individual, living, 'Kabatei').
	language('xkq', '', individual, living, 'Koroni').
	language('xkr', '', individual, extinct, 'Xakriabá').
	language('xks', '', individual, living, 'Kumbewaha').
	language('xkt', '', individual, living, 'Kantosi').
	language('xku', '', individual, living, 'Kaamba').
	language('xkv', '', individual, living, 'Kgalagadi').
	language('xkw', '', individual, living, 'Kembra').
	language('xkx', '', individual, living, 'Karore').
	language('xky', '', individual, living, 'Uma'' Lasan').
	language('xkz', '', individual, living, 'Kurtokha').
	language('xla', '', individual, living, 'Kamula').
	language('xlb', '', individual, extinct, 'Loup B').
	language('xlc', '', individual, special, 'Lycian').
	language('xld', '', individual, special, 'Lydian').
	language('xle', '', individual, special, 'Lemnian').
	language('xlg', '', individual, special, 'Ligurian (Ancient)').
	language('xli', '', individual, special, 'Liburnian').
	language('xln', '', individual, special, 'Alanic').
	language('xlo', '', individual, extinct, 'Loup A').
	language('xlp', '', individual, special, 'Lepontic').
	language('xls', '', individual, special, 'Lusitanian').
	language('xlu', '', individual, special, 'Cuneiform Luwian').
	language('xly', '', individual, special, 'Elymian').
	language('xma', '', individual, living, 'Mushungulu').
	language('xmb', '', individual, living, 'Mbonga').
	language('xmc', '', individual, living, 'Makhuwa-Marrevone').
	language('xmd', '', individual, living, 'Mbudum').
	language('xme', '', individual, special, 'Median').
	language('xmf', '', individual, living, 'Mingrelian').
	language('xmg', '', individual, living, 'Mengaka').
	language('xmh', '', individual, living, 'Kugu-Muminh').
	language('xmj', '', individual, living, 'Majera').
	language('xmk', '', individual, special, 'Ancient Macedonian').
	language('xml', '', individual, living, 'Malaysian Sign Language').
	language('xmm', '', individual, living, 'Manado Malay').
	language('xmn', '', individual, special, 'Manichaean Middle Persian').
	language('xmo', '', individual, living, 'Morerebi').
	language('xmp', '', individual, extinct, 'Kuku-Mu''inh').
	language('xmq', '', individual, extinct, 'Kuku-Mangk').
	language('xmr', '', individual, special, 'Meroitic').
	language('xms', '', individual, living, 'Moroccan Sign Language').
	language('xmt', '', individual, living, 'Matbat').
	language('xmu', '', individual, extinct, 'Kamu').
	language('xmv', '', individual, living, 'Antankarana Malagasy').
	language('xmw', '', individual, living, 'Tsimihety Malagasy').
	language('xmx', '', individual, living, 'Salawati').
	language('xmy', '', individual, living, 'Mayaguduna').
	language('xmz', '', individual, living, 'Mori Bawah').
	language('xna', '', individual, special, 'Ancient North Arabian').
	language('xnb', '', individual, living, 'Kanakanabu').
	language('xng', '', individual, special, 'Middle Mongolian').
	language('xnh', '', individual, living, 'Kuanhua').
	language('xni', '', individual, extinct, 'Ngarigu').
	language('xnj', '', individual, living, 'Ngoni (Tanzania)').
	language('xnk', '', individual, extinct, 'Nganakarti').
	language('xnm', '', individual, extinct, 'Ngumbarl').
	language('xnn', '', individual, living, 'Northern Kankanay').
	language('xno', '', individual, special, 'Anglo-Norman').
	language('xnq', '', individual, living, 'Ngoni (Mozambique)').
	language('xnr', '', individual, living, 'Kangri').
	language('xns', '', individual, living, 'Kanashi').
	language('xnt', '', individual, extinct, 'Narragansett').
	language('xnu', '', individual, extinct, 'Nukunul').
	language('xny', '', individual, living, 'Nyiyaparli').
	language('xnz', '', individual, living, 'Kenzi').
	language('xoc', '', individual, extinct, 'O''chi''chi''').
	language('xod', '', individual, living, 'Kokoda').
	language('xog', '', individual, living, 'Soga').
	language('xoi', '', individual, living, 'Kominimung').
	language('xok', '', individual, living, 'Xokleng').
	language('xom', '', individual, living, 'Komo (Sudan)').
	language('xon', '', individual, living, 'Konkomba').
	language('xoo', '', individual, extinct, 'Xukurú').
	language('xop', '', individual, living, 'Kopar').
	language('xor', '', individual, living, 'Korubo').
	language('xow', '', individual, living, 'Kowaki').
	language('xpa', '', individual, extinct, 'Pirriya').
	language('xpb', '', individual, extinct, 'Northeastern Tasmanian').
	language('xpc', '', individual, special, 'Pecheneg').
	language('xpd', '', individual, extinct, 'Oyster Bay Tasmanian').
	language('xpe', '', individual, living, 'Liberia Kpelle').
	language('xpf', '', individual, extinct, 'Southeast Tasmanian').
	language('xpg', '', individual, special, 'Phrygian').
	language('xph', '', individual, extinct, 'North Midlands Tasmanian').
	language('xpi', '', individual, special, 'Pictish').
	language('xpj', '', individual, extinct, 'Mpalitjanh').
	language('xpk', '', individual, living, 'Kulina Pano').
	language('xpl', '', individual, extinct, 'Port Sorell Tasmanian').
	language('xpm', '', individual, extinct, 'Pumpokol').
	language('xpn', '', individual, extinct, 'Kapinawá').
	language('xpo', '', individual, extinct, 'Pochutec').
	language('xpp', '', individual, special, 'Puyo-Paekche').
	language('xpq', '', individual, extinct, 'Mohegan-Pequot').
	language('xpr', '', individual, special, 'Parthian').
	language('xps', '', individual, special, 'Pisidian').
	language('xpt', '', individual, extinct, 'Punthamara').
	language('xpu', '', individual, special, 'Punic').
	language('xpv', '', individual, extinct, 'Northern Tasmanian').
	language('xpw', '', individual, extinct, 'Northwestern Tasmanian').
	language('xpx', '', individual, extinct, 'Southwestern Tasmanian').
	language('xpy', '', individual, special, 'Puyo').
	language('xpz', '', individual, extinct, 'Bruny Island Tasmanian').
	language('xqa', '', individual, special, 'Karakhanid').
	language('xqt', '', individual, special, 'Qatabanian').
	language('xra', '', individual, living, 'Krahô').
	language('xrb', '', individual, living, 'Eastern Karaboro').
	language('xrd', '', individual, extinct, 'Gundungurra').
	language('xre', '', individual, living, 'Kreye').
	language('xrg', '', individual, extinct, 'Minang').
	language('xri', '', individual, living, 'Krikati-Timbira').
	language('xrm', '', individual, special, 'Armazic').
	language('xrn', '', individual, extinct, 'Arin').
	language('xrr', '', individual, special, 'Raetic').
	language('xrt', '', individual, extinct, 'Aranama-Tamique').
	language('xru', '', individual, living, 'Marriammu').
	language('xrw', '', individual, living, 'Karawa').
	language('xsa', '', individual, special, 'Sabaean').
	language('xsb', '', individual, living, 'Sambal').
	language('xsc', '', individual, special, 'Scythian').
	language('xsd', '', individual, special, 'Sidetic').
	language('xse', '', individual, living, 'Sempan').
	language('xsh', '', individual, living, 'Shamang').
	language('xsi', '', individual, living, 'Sio').
	language('xsj', '', individual, living, 'Subi').
	language('xsl', '', individual, living, 'South Slavey').
	language('xsm', '', individual, living, 'Kasem').
	language('xsn', '', individual, living, 'Sanga (Nigeria)').
	language('xso', '', individual, extinct, 'Solano').
	language('xsp', '', individual, living, 'Silopi').
	language('xsq', '', individual, living, 'Makhuwa-Saka').
	language('xsr', '', individual, living, 'Sherpa').
	language('xsu', '', individual, living, 'Sanumá').
	language('xsv', '', individual, extinct, 'Sudovian').
	language('xsy', '', individual, living, 'Saisiyat').
	language('xta', '', individual, living, 'Alcozauca Mixtec').
	language('xtb', '', individual, living, 'Chazumba Mixtec').
	language('xtc', '', individual, living, 'Katcha-Kadugli-Miri').
	language('xtd', '', individual, living, 'Diuxi-Tilantongo Mixtec').
	language('xte', '', individual, living, 'Ketengban').
	language('xtg', '', individual, special, 'Transalpine Gaulish').
	language('xth', '', individual, extinct, 'Yitha Yitha').
	language('xti', '', individual, living, 'Sinicahua Mixtec').
	language('xtj', '', individual, living, 'San Juan Teita Mixtec').
	language('xtl', '', individual, living, 'Tijaltepec Mixtec').
	language('xtm', '', individual, living, 'Magdalena Peñasco Mixtec').
	language('xtn', '', individual, living, 'Northern Tlaxiaco Mixtec').
	language('xto', '', individual, special, 'Tokharian A').
	language('xtp', '', individual, living, 'San Miguel Piedras Mixtec').
	language('xtq', '', individual, special, 'Tumshuqese').
	language('xtr', '', individual, special, 'Early Tripuri').
	language('xts', '', individual, living, 'Sindihui Mixtec').
	language('xtt', '', individual, living, 'Tacahua Mixtec').
	language('xtu', '', individual, living, 'Cuyamecalco Mixtec').
	language('xtv', '', individual, extinct, 'Thawa').
	language('xtw', '', individual, living, 'Tawandê').
	language('xty', '', individual, living, 'Yoloxochitl Mixtec').
	language('xua', '', individual, living, 'Alu Kurumba').
	language('xub', '', individual, living, 'Betta Kurumba').
	language('xud', '', individual, extinct, 'Umiida').
	language('xug', '', individual, living, 'Kunigami').
	language('xuj', '', individual, living, 'Jennu Kurumba').
	language('xul', '', individual, extinct, 'Ngunawal').
	language('xum', '', individual, special, 'Umbrian').
	language('xun', '', individual, extinct, 'Unggaranggu').
	language('xuo', '', individual, living, 'Kuo').
	language('xup', '', individual, extinct, 'Upper Umpqua').
	language('xur', '', individual, special, 'Urartian').
	language('xut', '', individual, extinct, 'Kuthant').
	language('xuu', '', individual, living, 'Kxoe').
	language('xve', '', individual, special, 'Venetic').
	language('xvi', '', individual, living, 'Kamviri').
	language('xvn', '', individual, special, 'Vandalic').
	language('xvo', '', individual, special, 'Volscian').
	language('xvs', '', individual, special, 'Vestinian').
	language('xwa', '', individual, living, 'Kwaza').
	language('xwc', '', individual, extinct, 'Woccon').
	language('xwd', '', individual, extinct, 'Wadi Wadi').
	language('xwe', '', individual, living, 'Xwela Gbe').
	language('xwg', '', individual, living, 'Kwegu').
	language('xwj', '', individual, extinct, 'Wajuk').
	language('xwk', '', individual, extinct, 'Wangkumara').
	language('xwl', '', individual, living, 'Western Xwla Gbe').
	language('xwo', '', individual, extinct, 'Written Oirat').
	language('xwr', '', individual, living, 'Kwerba Mamberamo').
	language('xwt', '', individual, extinct, 'Wotjobaluk').
	language('xww', '', individual, extinct, 'Wemba Wemba').
	language('xxb', '', individual, extinct, 'Boro (Ghana)').
	language('xxk', '', individual, living, 'Ke''o').
	language('xxm', '', individual, extinct, 'Minkin').
	language('xxr', '', individual, extinct, 'Koropó').
	language('xxt', '', individual, extinct, 'Tambora').
	language('xya', '', individual, extinct, 'Yaygir').
	language('xyb', '', individual, extinct, 'Yandjibara').
	language('xyj', '', individual, extinct, 'Mayi-Yapi').
	language('xyk', '', individual, extinct, 'Mayi-Kulan').
	language('xyl', '', individual, extinct, 'Yalakalore').
	language('xyt', '', individual, extinct, 'Mayi-Thakurti').
	language('xyy', '', individual, living, 'Yorta Yorta').
	language('xzh', '', individual, special, 'Zhang-Zhung').
	language('xzm', '', individual, extinct, 'Zemgalian').
	language('xzp', '', individual, special, 'Ancient Zapotec').
	language('yaa', '', individual, living, 'Yaminahua').
	language('yab', '', individual, living, 'Yuhup').
	language('yac', '', individual, living, 'Pass Valley Yali').
	language('yad', '', individual, living, 'Yagua').
	language('yae', '', individual, living, 'Pumé').
	language('yaf', '', individual, living, 'Yaka (Democratic Republic of Congo)').
	language('yag', '', individual, living, 'Yámana').
	language('yah', '', individual, living, 'Yazgulyam').
	language('yai', '', individual, living, 'Yagnobi').
	language('yaj', '', individual, living, 'Banda-Yangere').
	language('yak', '', individual, living, 'Yakama').
	language('yal', '', individual, living, 'Yalunka').
	language('yam', '', individual, living, 'Yamba').
	language('yan', '', individual, living, 'Mayangna').
	language('yao', '', individual, living, 'Yao').
	language('yap', '', individual, living, 'Yapese').
	language('yaq', '', individual, living, 'Yaqui').
	language('yar', '', individual, living, 'Yabarana').
	language('yas', '', individual, living, 'Nugunu (Cameroon)').
	language('yat', '', individual, living, 'Yambeta').
	language('yau', '', individual, living, 'Yuwana').
	language('yav', '', individual, living, 'Yangben').
	language('yaw', '', individual, living, 'Yawalapití').
	language('yax', '', individual, living, 'Yauma').
	language('yay', '', individual, living, 'Agwagwune').
	language('yaz', '', individual, living, 'Lokaa').
	language('yba', '', individual, living, 'Yala').
	language('ybb', '', individual, living, 'Yemba').
	language('ybe', '', individual, living, 'West Yugur').
	language('ybh', '', individual, living, 'Yakha').
	language('ybi', '', individual, living, 'Yamphu').
	language('ybj', '', individual, living, 'Hasha').
	language('ybk', '', individual, living, 'Bokha').
	language('ybl', '', individual, living, 'Yukuben').
	language('ybm', '', individual, living, 'Yaben').
	language('ybn', '', individual, extinct, 'Yabaâna').
	language('ybo', '', individual, living, 'Yabong').
	language('ybx', '', individual, living, 'Yawiyo').
	language('yby', '', individual, living, 'Yaweyuha').
	language('ych', '', individual, living, 'Chesu').
	language('ycl', '', individual, living, 'Lolopo').
	language('ycn', '', individual, living, 'Yucuna').
	language('ycp', '', individual, living, 'Chepya').
	language('ycr', '', individual, living, 'Yilan Creole').
	language('yda', '', individual, extinct, 'Yanda').
	language('ydd', '', individual, living, 'Eastern Yiddish').
	language('yde', '', individual, living, 'Yangum Dey').
	language('ydg', '', individual, living, 'Yidgha').
	language('ydk', '', individual, living, 'Yoidik').
	language('yea', '', individual, living, 'Ravula').
	language('yec', '', individual, living, 'Yeniche').
	language('yee', '', individual, living, 'Yimas').
	language('yei', '', individual, extinct, 'Yeni').
	language('yej', '', individual, living, 'Yevanic').
	language('yel', '', individual, living, 'Yela').
	language('yer', '', individual, living, 'Tarok').
	language('yes', '', individual, living, 'Nyankpa').
	language('yet', '', individual, living, 'Yetfa').
	language('yeu', '', individual, living, 'Yerukula').
	language('yev', '', individual, living, 'Yapunda').
	language('yey', '', individual, living, 'Yeyi').
	language('yga', '', individual, extinct, 'Malyangapa').
	language('ygi', '', individual, extinct, 'Yiningayi').
	language('ygl', '', individual, living, 'Yangum Gel').
	language('ygm', '', individual, living, 'Yagomi').
	language('ygp', '', individual, living, 'Gepo').
	language('ygr', '', individual, living, 'Yagaria').
	language('ygs', '', individual, living, 'Yolŋu Sign Language').
	language('ygu', '', individual, living, 'Yugul').
	language('ygw', '', individual, living, 'Yagwoia').
	language('yha', '', individual, living, 'Baha Buyang').
	language('yhd', '', individual, living, 'Judeo-Iraqi Arabic').
	language('yhl', '', individual, living, 'Hlepho Phowa').
	language('yhs', '', individual, living, 'Yan-nhaŋu Sign Language').
	language('yia', '', individual, living, 'Yinggarda').
	language('yid', 'yi', macrolanguage, living, 'Yiddish').
	language('yif', '', individual, living, 'Ache').
	language('yig', '', individual, living, 'Wusa Nasu').
	language('yih', '', individual, extinct, 'Western Yiddish').
	language('yii', '', individual, living, 'Yidiny').
	language('yij', '', individual, living, 'Yindjibarndi').
	language('yik', '', individual, living, 'Dongshanba Lalo').
	language('yil', '', individual, extinct, 'Yindjilandji').
	language('yim', '', individual, living, 'Yimchungru Naga').
	language('yin', '', individual, living, 'Riang Lai').
	language('yip', '', individual, living, 'Pholo').
	language('yiq', '', individual, living, 'Miqie').
	language('yir', '', individual, living, 'North Awyu').
	language('yis', '', individual, living, 'Yis').
	language('yit', '', individual, living, 'Eastern Lalu').
	language('yiu', '', individual, living, 'Awu').
	language('yiv', '', individual, living, 'Northern Nisu').
	language('yix', '', individual, living, 'Axi Yi').
	language('yiz', '', individual, living, 'Azhe').
	language('yka', '', individual, living, 'Yakan').
	language('ykg', '', individual, living, 'Northern Yukaghir').
	language('ykh', '', individual, living, 'Khamnigan Mongol').
	language('yki', '', individual, living, 'Yoke').
	language('ykk', '', individual, living, 'Yakaikeke').
	language('ykl', '', individual, living, 'Khlula').
	language('ykm', '', individual, living, 'Kap').
	language('ykn', '', individual, living, 'Kua-nsi').
	language('yko', '', individual, living, 'Iyasa').
	language('ykr', '', individual, living, 'Yekora').
	language('ykt', '', individual, living, 'Kathu').
	language('yku', '', individual, living, 'Kuamasi').
	language('yky', '', individual, living, 'Yakoma').
	language('yla', '', individual, living, 'Yaul').
	language('ylb', '', individual, living, 'Yaleba').
	language('yle', '', individual, living, 'Yele').
	language('ylg', '', individual, living, 'Yelogu').
	language('yli', '', individual, living, 'Angguruk Yali').
	language('yll', '', individual, living, 'Yil').
	language('ylm', '', individual, living, 'Limi').
	language('yln', '', individual, living, 'Langnian Buyang').
	language('ylo', '', individual, living, 'Naluo Yi').
	language('ylr', '', individual, extinct, 'Yalarnnga').
	language('ylu', '', individual, living, 'Aribwaung').
	language('yly', '', individual, living, 'Nyâlayu').
	language('ymb', '', individual, living, 'Yambes').
	language('ymc', '', individual, living, 'Southern Muji').
	language('ymd', '', individual, living, 'Muda').
	language('yme', '', individual, extinct, 'Yameo').
	language('ymg', '', individual, living, 'Yamongeri').
	language('ymh', '', individual, living, 'Mili').
	language('ymi', '', individual, living, 'Moji').
	language('ymk', '', individual, living, 'Makwe').
	language('yml', '', individual, living, 'Iamalele').
	language('ymm', '', individual, living, 'Maay').
	language('ymn', '', individual, living, 'Yamna').
	language('ymo', '', individual, living, 'Yangum Mon').
	language('ymp', '', individual, living, 'Yamap').
	language('ymq', '', individual, living, 'Qila Muji').
	language('ymr', '', individual, living, 'Malasar').
	language('yms', '', individual, special, 'Mysian').
	language('ymx', '', individual, living, 'Northern Muji').
	language('ymz', '', individual, living, 'Muzi').
	language('yna', '', individual, living, 'Aluo').
	language('ynb', '', individual, living, 'Yamben').
	language('ynd', '', individual, extinct, 'Yandruwandha').
	language('yne', '', individual, living, 'Lang''e').
	language('yng', '', individual, living, 'Yango').
	language('ynk', '', individual, living, 'Naukan Yupik').
	language('ynl', '', individual, living, 'Yangulam').
	language('ynn', '', individual, extinct, 'Yana').
	language('yno', '', individual, living, 'Yong').
	language('ynq', '', individual, living, 'Yendang').
	language('yns', '', individual, living, 'Yansi').
	language('ynu', '', individual, extinct, 'Yahuna').
	language('yob', '', individual, extinct, 'Yoba').
	language('yog', '', individual, living, 'Yogad').
	language('yoi', '', individual, living, 'Yonaguni').
	language('yok', '', individual, living, 'Yokuts').
	language('yom', '', individual, living, 'Yombe').
	language('yon', '', individual, living, 'Yongkom').
	language('yor', 'yo', individual, living, 'Yoruba').
	language('yot', '', individual, living, 'Yotti').
	language('yox', '', individual, living, 'Yoron').
	language('yoy', '', individual, living, 'Yoy').
	language('ypa', '', individual, living, 'Phala').
	language('ypb', '', individual, living, 'Labo Phowa').
	language('ypg', '', individual, living, 'Phola').
	language('yph', '', individual, living, 'Phupha').
	language('ypm', '', individual, living, 'Phuma').
	language('ypn', '', individual, living, 'Ani Phowa').
	language('ypo', '', individual, living, 'Alo Phola').
	language('ypp', '', individual, living, 'Phupa').
	language('ypz', '', individual, living, 'Phuza').
	language('yra', '', individual, living, 'Yerakai').
	language('yrb', '', individual, living, 'Yareba').
	language('yre', '', individual, living, 'Yaouré').
	language('yrk', '', individual, living, 'Nenets').
	language('yrl', '', individual, living, 'Nhengatu').
	language('yrm', '', individual, living, 'Yirrk-Mel').
	language('yrn', '', individual, living, 'Yerong').
	language('yro', '', individual, living, 'Yaroamë').
	language('yrs', '', individual, living, 'Yarsun').
	language('yrw', '', individual, living, 'Yarawata').
	language('yry', '', individual, living, 'Yarluyandi').
	language('ysc', '', individual, extinct, 'Yassic').
	language('ysd', '', individual, living, 'Samatao').
	language('ysg', '', individual, living, 'Sonaga').
	language('ysl', '', individual, living, 'Yugoslavian Sign Language').
	language('ysm', '', individual, living, 'Myanmar Sign Language').
	language('ysn', '', individual, living, 'Sani').
	language('yso', '', individual, living, 'Nisi (China)').
	language('ysp', '', individual, living, 'Southern Lolopo').
	language('ysr', '', individual, extinct, 'Sirenik Yupik').
	language('yss', '', individual, living, 'Yessan-Mayo').
	language('ysy', '', individual, living, 'Sanie').
	language('yta', '', individual, living, 'Talu').
	language('ytl', '', individual, living, 'Tanglang').
	language('ytp', '', individual, living, 'Thopho').
	language('ytw', '', individual, living, 'Yout Wam').
	language('yty', '', individual, extinct, 'Yatay').
	language('yua', '', individual, living, 'Yucateco').
	language('yub', '', individual, extinct, 'Yugambal').
	language('yuc', '', individual, living, 'Yuchi').
	language('yud', '', individual, living, 'Judeo-Tripolitanian Arabic').
	language('yue', '', individual, living, 'Yue Chinese').
	language('yuf', '', individual, living, 'Havasupai-Walapai-Yavapai').
	language('yug', '', individual, extinct, 'Yug').
	language('yui', '', individual, living, 'Yurutí').
	language('yuj', '', individual, living, 'Karkar-Yuri').
	language('yuk', '', individual, extinct, 'Yuki').
	language('yul', '', individual, living, 'Yulu').
	language('yum', '', individual, living, 'Quechan').
	language('yun', '', individual, living, 'Bena (Nigeria)').
	language('yup', '', individual, living, 'Yukpa').
	language('yuq', '', individual, living, 'Yuqui').
	language('yur', '', individual, extinct, 'Yurok').
	language('yut', '', individual, living, 'Yopno').
	language('yuw', '', individual, living, 'Yau (Morobe Province)').
	language('yux', '', individual, living, 'Southern Yukaghir').
	language('yuy', '', individual, living, 'East Yugur').
	language('yuz', '', individual, living, 'Yuracare').
	language('yva', '', individual, living, 'Yawa').
	language('yvt', '', individual, extinct, 'Yavitero').
	language('ywa', '', individual, living, 'Kalou').
	language('ywg', '', individual, living, 'Yinhawangka').
	language('ywl', '', individual, living, 'Western Lalu').
	language('ywn', '', individual, living, 'Yawanawa').
	language('ywq', '', individual, living, 'Wuding-Luquan Yi').
	language('ywr', '', individual, living, 'Yawuru').
	language('ywt', '', individual, living, 'Xishanba Lalo').
	language('ywu', '', individual, living, 'Wumeng Nasu').
	language('yww', '', individual, extinct, 'Yawarawarga').
	language('yxa', '', individual, extinct, 'Mayawali').
	language('yxg', '', individual, extinct, 'Yagara').
	language('yxl', '', individual, extinct, 'Yardliyawarra').
	language('yxm', '', individual, extinct, 'Yinwum').
	language('yxu', '', individual, extinct, 'Yuyu').
	language('yxy', '', individual, extinct, 'Yabula Yabula').
	language('yyr', '', individual, extinct, 'Yir Yoront').
	language('yyu', '', individual, living, 'Yau (Sandaun Province)').
	language('yyz', '', individual, living, 'Ayizi').
	language('yzg', '', individual, living, 'E''ma Buyang').
	language('yzk', '', individual, living, 'Zokhuo').
	language('zaa', '', individual, living, 'Sierra de Juárez Zapotec').
	language('zab', '', individual, living, 'Western Tlacolula Valley Zapotec').
	language('zac', '', individual, living, 'Ocotlán Zapotec').
	language('zad', '', individual, living, 'Cajonos Zapotec').
	language('zae', '', individual, living, 'Yareni Zapotec').
	language('zaf', '', individual, living, 'Ayoquesco Zapotec').
	language('zag', '', individual, living, 'Zaghawa').
	language('zah', '', individual, living, 'Zangwal').
	language('zai', '', individual, living, 'Isthmus Zapotec').
	language('zaj', '', individual, living, 'Zaramo').
	language('zak', '', individual, living, 'Zanaki').
	language('zal', '', individual, living, 'Zauzou').
	language('zam', '', individual, living, 'Miahuatlán Zapotec').
	language('zao', '', individual, living, 'Ozolotepec Zapotec').
	language('zap', '', macrolanguage, living, 'Zapotec').
	language('zaq', '', individual, living, 'Aloápam Zapotec').
	language('zar', '', individual, living, 'Rincón Zapotec').
	language('zas', '', individual, living, 'Santo Domingo Albarradas Zapotec').
	language('zat', '', individual, living, 'Tabaa Zapotec').
	language('zau', '', individual, living, 'Zangskari').
	language('zav', '', individual, living, 'Yatzachi Zapotec').
	language('zaw', '', individual, living, 'Mitla Zapotec').
	language('zax', '', individual, living, 'Xadani Zapotec').
	language('zay', '', individual, living, 'Zayse-Zergulla').
	language('zaz', '', individual, living, 'Zari').
	language('zba', '', individual, constructed, 'Balaibalan').
	language('zbc', '', individual, living, 'Central Berawan').
	language('zbe', '', individual, living, 'East Berawan').
	language('zbl', '', individual, constructed, 'Blissymbols').
	language('zbt', '', individual, living, 'Batui').
	language('zbu', '', individual, living, 'Bu (Bauchi State)').
	language('zbw', '', individual, living, 'West Berawan').
	language('zca', '', individual, living, 'Coatecas Altas Zapotec').
	language('zcd', '', individual, living, 'Las Delicias Zapotec').
	language('zch', '', individual, living, 'Central Hongshuihe Zhuang').
	language('zdj', '', individual, living, 'Ngazidja Comorian').
	language('zea', '', individual, living, 'Zeeuws').
	language('zeg', '', individual, living, 'Zenag').
	language('zeh', '', individual, living, 'Eastern Hongshuihe Zhuang').
	language('zem', '', individual, living, 'Zeem').
	language('zen', '', individual, living, 'Zenaga').
	language('zga', '', individual, living, 'Kinga').
	language('zgb', '', individual, living, 'Guibei Zhuang').
	language('zgh', '', individual, living, 'Standard Moroccan Tamazight').
	language('zgm', '', individual, living, 'Minz Zhuang').
	language('zgn', '', individual, living, 'Guibian Zhuang').
	language('zgr', '', individual, living, 'Magori').
	language('zha', 'za', macrolanguage, living, 'Zhuang').
	language('zhb', '', individual, living, 'Zhaba').
	language('zhd', '', individual, living, 'Dai Zhuang').
	language('zhi', '', individual, living, 'Zhire').
	language('zhk', '', individual, living, 'Kurdish Sign Language').
	language('zhn', '', individual, living, 'Nong Zhuang').
	language('zho', 'zh', macrolanguage, living, 'Chinese').
	language('zhw', '', individual, living, 'Zhoa').
	language('zia', '', individual, living, 'Zia').
	language('zib', '', individual, living, 'Zimbabwe Sign Language').
	language('zik', '', individual, living, 'Zimakani').
	language('zil', '', individual, living, 'Zialo').
	language('zim', '', individual, living, 'Mesme').
	language('zin', '', individual, living, 'Zinza').
	language('ziw', '', individual, living, 'Zigula').
	language('ziz', '', individual, living, 'Zizilivakan').
	language('zka', '', individual, living, 'Kaimbulawa').
	language('zkd', '', individual, living, 'Kadu').
	language('zkg', '', individual, special, 'Koguryo').
	language('zkh', '', individual, special, 'Khorezmian').
	language('zkk', '', individual, extinct, 'Karankawa').
	language('zkn', '', individual, living, 'Kanan').
	language('zko', '', individual, extinct, 'Kott').
	language('zkp', '', individual, extinct, 'São Paulo Kaingáng').
	language('zkr', '', individual, living, 'Zakhring').
	language('zkt', '', individual, special, 'Kitan').
	language('zku', '', individual, living, 'Kaurna').
	language('zkv', '', individual, extinct, 'Krevinian').
	language('zkz', '', individual, special, 'Khazar').
	language('zla', '', individual, living, 'Zula').
	language('zlj', '', individual, living, 'Liujiang Zhuang').
	language('zlm', '', individual, living, 'Malay (individual language)').
	language('zln', '', individual, living, 'Lianshan Zhuang').
	language('zlq', '', individual, living, 'Liuqian Zhuang').
	language('zlu', '', individual, living, 'Zul').
	language('zma', '', individual, living, 'Manda (Australia)').
	language('zmb', '', individual, living, 'Zimba').
	language('zmc', '', individual, extinct, 'Margany').
	language('zmd', '', individual, living, 'Maridan').
	language('zme', '', individual, extinct, 'Mangerr').
	language('zmf', '', individual, living, 'Mfinu').
	language('zmg', '', individual, living, 'Marti Ke').
	language('zmh', '', individual, extinct, 'Makolkol').
	language('zmi', '', individual, living, 'Negeri Sembilan Malay').
	language('zmj', '', individual, living, 'Maridjabin').
	language('zmk', '', individual, extinct, 'Mandandanyi').
	language('zml', '', individual, extinct, 'Matngala').
	language('zmm', '', individual, living, 'Marimanindji').
	language('zmn', '', individual, living, 'Mbangwe').
	language('zmo', '', individual, living, 'Molo').
	language('zmp', '', individual, living, 'Mbuun').
	language('zmq', '', individual, living, 'Mituku').
	language('zmr', '', individual, living, 'Maranunggu').
	language('zms', '', individual, living, 'Mbesa').
	language('zmt', '', individual, living, 'Maringarr').
	language('zmu', '', individual, extinct, 'Muruwari').
	language('zmv', '', individual, extinct, 'Mbariman-Gudhinma').
	language('zmw', '', individual, living, 'Mbo (Democratic Republic of Congo)').
	language('zmx', '', individual, living, 'Bomitaba').
	language('zmy', '', individual, living, 'Mariyedi').
	language('zmz', '', individual, living, 'Mbandja').
	language('zna', '', individual, living, 'Zan Gula').
	language('zne', '', individual, living, 'Zande (individual language)').
	language('zng', '', individual, living, 'Mang').
	language('znk', '', individual, extinct, 'Manangkari').
	language('zns', '', individual, living, 'Mangas').
	language('zoc', '', individual, living, 'Copainalá Zoque').
	language('zoh', '', individual, living, 'Chimalapa Zoque').
	language('zom', '', individual, living, 'Zou').
	language('zoo', '', individual, living, 'Asunción Mixtepec Zapotec').
	language('zoq', '', individual, living, 'Tabasco Zoque').
	language('zor', '', individual, living, 'Rayón Zoque').
	language('zos', '', individual, living, 'Francisco León Zoque').
	language('zpa', '', individual, living, 'Lachiguiri Zapotec').
	language('zpb', '', individual, living, 'Yautepec Zapotec').
	language('zpc', '', individual, living, 'Choapan Zapotec').
	language('zpd', '', individual, living, 'Southeastern Ixtlán Zapotec').
	language('zpe', '', individual, living, 'Petapa Zapotec').
	language('zpf', '', individual, living, 'San Pedro Quiatoni Zapotec').
	language('zpg', '', individual, living, 'Guevea De Humboldt Zapotec').
	language('zph', '', individual, living, 'Totomachapan Zapotec').
	language('zpi', '', individual, living, 'Santa María Quiegolani Zapotec').
	language('zpj', '', individual, living, 'Quiavicuzas Zapotec').
	language('zpk', '', individual, living, 'Tlacolulita Zapotec').
	language('zpl', '', individual, living, 'Lachixío Zapotec').
	language('zpm', '', individual, living, 'Mixtepec Zapotec').
	language('zpn', '', individual, living, 'Santa Inés Yatzechi Zapotec').
	language('zpo', '', individual, living, 'Amatlán Zapotec').
	language('zpp', '', individual, living, 'El Alto Zapotec').
	language('zpq', '', individual, living, 'Zoogocho Zapotec').
	language('zpr', '', individual, living, 'Santiago Xanica Zapotec').
	language('zps', '', individual, living, 'Coatlán Zapotec').
	language('zpt', '', individual, living, 'San Vicente Coatlán Zapotec').
	language('zpu', '', individual, living, 'Yalálag Zapotec').
	language('zpv', '', individual, living, 'Chichicapan Zapotec').
	language('zpw', '', individual, living, 'Zaniza Zapotec').
	language('zpx', '', individual, living, 'San Baltazar Loxicha Zapotec').
	language('zpy', '', individual, living, 'Mazaltepec Zapotec').
	language('zpz', '', individual, living, 'Texmelucan Zapotec').
	language('zqe', '', individual, living, 'Qiubei Zhuang').
	language('zra', '', individual, special, 'Kara (Korea)').
	language('zrg', '', individual, living, 'Mirgan').
	language('zrn', '', individual, living, 'Zerenkel').
	language('zro', '', individual, living, 'Záparo').
	language('zrp', '', individual, extinct, 'Zarphatic').
	language('zrs', '', individual, living, 'Mairasi').
	language('zsa', '', individual, living, 'Sarasira').
	language('zsk', '', individual, special, 'Kaskean').
	language('zsl', '', individual, living, 'Zambian Sign Language').
	language('zsm', '', individual, living, 'Standard Malay').
	language('zsr', '', individual, living, 'Southern Rincon Zapotec').
	language('zsu', '', individual, living, 'Sukurum').
	language('zte', '', individual, living, 'Elotepec Zapotec').
	language('ztg', '', individual, living, 'Xanaguía Zapotec').
	language('ztl', '', individual, living, 'Lapaguía-Guivini Zapotec').
	language('ztm', '', individual, living, 'San Agustín Mixtepec Zapotec').
	language('ztn', '', individual, living, 'Santa Catarina Albarradas Zapotec').
	language('ztp', '', individual, living, 'Loxicha Zapotec').
	language('ztq', '', individual, living, 'Quioquitani-Quierí Zapotec').
	language('zts', '', individual, living, 'Tilquiapan Zapotec').
	language('ztt', '', individual, living, 'Tejalapan Zapotec').
	language('ztu', '', individual, living, 'Güilá Zapotec').
	language('ztx', '', individual, living, 'Zaachila Zapotec').
	language('zty', '', individual, living, 'Yatee Zapotec').
	language('zuh', '', individual, living, 'Tokano').
	language('zul', 'zu', individual, living, 'Zulu').
	language('zum', '', individual, living, 'Kumzari').
	language('zun', '', individual, living, 'Zuni').
	language('zuy', '', individual, living, 'Zumaya').
	language('zwa', '', individual, living, 'Zay').
	language('zxx', '', special, special, 'No linguistic content').
	language('zyb', '', individual, living, 'Yongbei Zhuang').
	language('zyg', '', individual, living, 'Yang Zhuang').
	language('zyj', '', individual, living, 'Youjiang Zhuang').
	language('zyn', '', individual, living, 'Yongnan Zhuang').
	language('zyp', '', individual, living, 'Zyphe Chin').
	language('zza', '', macrolanguage, living, 'Zaza').
	language('zzj', '', individual, living, 'Zuojiang Zhuang').

:- end_object.
