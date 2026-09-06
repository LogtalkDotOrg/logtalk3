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

% Generated from Unicode 17.0.0 ScriptExtensions.txt. Do not edit.

:- include(unicode_range_scripts).

unicode_script_extension(CodePoint, Extension) :-
	(	var(CodePoint) ->
		unicode_script_extension(Start, End, Extension),
		between(Start, End, CodePoint)
	;	unicode_script_extension(Start, End, SpecificExtension),
		CodePoint >= Start, CodePoint =< End ->
		Extension = SpecificExtension
	;	unicode_script(CodePoint, Script),
		Extension = [Script]
	).

unicode_script_extension(183, 183, ['Avst','Cari','Copt','Dupl','Elba','Geor','Glag','Gong','Goth','Grek','Hani','Latn','Lydi','Mahj','Perm','Shaw']).
unicode_script_extension(700, 700, ['Beng','Cyrl','Deva','Latn','Lisu','Thai','Toto']).
unicode_script_extension(711, 711, ['Bopo','Latn']).
unicode_script_extension(713, 715, ['Bopo','Latn']).
unicode_script_extension(717, 717, ['Latn','Lisu']).
unicode_script_extension(727, 727, ['Latn','Thai']).
unicode_script_extension(729, 729, ['Bopo','Latn']).
unicode_script_extension(768, 768, ['Cher','Copt','Cyrl','Grek','Latn','Perm','Sunu','Tale']).
unicode_script_extension(769, 769, ['Cher','Cyrl','Grek','Latn','Osge','Sunu','Tale','Todr']).
unicode_script_extension(770, 770, ['Cher','Cyrl','Latn','Tfng']).
unicode_script_extension(771, 771, ['Glag','Latn','Sunu','Syrc','Thai']).
unicode_script_extension(772, 772, ['Aghb','Cher','Copt','Cyrl','Goth','Grek','Latn','Osge','Syrc','Tfng','Todr']).
unicode_script_extension(773, 773, ['Copt','Elba','Glag','Goth','Kana','Latn']).
unicode_script_extension(774, 774, ['Cyrl','Grek','Latn','Perm','Tfng']).
unicode_script_extension(775, 775, ['Copt','Dupl','Hebr','Latn','Perm','Syrc','Tale','Tfng','Todr']).
unicode_script_extension(776, 776, ['Armn','Cyrl','Dupl','Goth','Grek','Hebr','Latn','Perm','Syrc','Tale','Tfng']).
unicode_script_extension(777, 777, ['Latn','Tfng']).
unicode_script_extension(778, 778, ['Dupl','Latn','Syrc']).
unicode_script_extension(779, 779, ['Cher','Cyrl','Latn','Osge']).
unicode_script_extension(780, 780, ['Cher','Latn','Tale']).
unicode_script_extension(781, 781, ['Latn','Sunu']).
unicode_script_extension(782, 782, ['Ethi','Latn']).
unicode_script_extension(784, 784, ['Latn','Sunu']).
unicode_script_extension(785, 785, ['Cyrl','Latn','Todr']).
unicode_script_extension(787, 787, ['Grek','Latn','Perm','Todr']).
unicode_script_extension(803, 803, ['Cher','Dupl','Kana','Latn','Syrc','Tfng']).
unicode_script_extension(804, 804, ['Cher','Dupl','Latn','Syrc']).
unicode_script_extension(805, 805, ['Latn','Syrc']).
unicode_script_extension(813, 813, ['Latn','Sunu','Syrc']).
unicode_script_extension(814, 814, ['Latn','Syrc']).
unicode_script_extension(816, 816, ['Cher','Latn','Syrc']).
unicode_script_extension(817, 817, ['Aghb','Cher','Goth','Latn','Sunu','Syrc','Thai']).
unicode_script_extension(834, 834, ['Grek']).
unicode_script_extension(837, 837, ['Grek']).
unicode_script_extension(856, 856, ['Latn','Osge']).
unicode_script_extension(862, 862, ['Aghb','Latn','Todr']).
unicode_script_extension(867, 879, ['Latn']).
unicode_script_extension(884, 884, ['Copt','Grek']).
unicode_script_extension(885, 885, ['Copt','Grek']).
unicode_script_extension(1155, 1155, ['Cyrl','Perm']).
unicode_script_extension(1156, 1156, ['Cyrl','Glag']).
unicode_script_extension(1157, 1158, ['Cyrl','Latn']).
unicode_script_extension(1159, 1159, ['Cyrl','Glag']).
unicode_script_extension(1417, 1417, ['Armn','Geor','Glag']).
unicode_script_extension(1548, 1548, ['Arab','Gara','Nkoo','Rohg','Syrc','Thaa','Yezi']).
unicode_script_extension(1563, 1563, ['Arab','Gara','Nkoo','Rohg','Syrc','Thaa','Yezi']).
unicode_script_extension(1564, 1564, ['Arab','Syrc','Thaa']).
unicode_script_extension(1567, 1567, ['Adlm','Arab','Gara','Nkoo','Rohg','Syrc','Thaa','Yezi']).
unicode_script_extension(1600, 1600, ['Adlm','Arab','Mand','Mani','Ougr','Phlp','Rohg','Sogd','Syrc']).
unicode_script_extension(1611, 1621, ['Arab','Syrc']).
unicode_script_extension(1632, 1641, ['Arab','Thaa','Yezi']).
unicode_script_extension(1648, 1648, ['Arab','Syrc']).
unicode_script_extension(1748, 1748, ['Arab','Rohg']).
unicode_script_extension(2385, 2385, ['Beng','Deva','Gran','Gujr','Guru','Knda','Latn','Mlym','Nand','Newa','Orya','Shrd','Taml','Telu','Tirh']).
unicode_script_extension(2386, 2386, ['Beng','Deva','Gran','Gujr','Guru','Knda','Latn','Mlym','Newa','Orya','Taml','Telu','Tirh']).
unicode_script_extension(2404, 2404, ['Beng','Deva','Dogr','Gong','Gonm','Gran','Gujr','Guru','Knda','Mahj','Mlym','Nand','Onao','Orya','Sind','Sinh','Sylo','Takr','Taml','Telu','Tirh']).
unicode_script_extension(2405, 2405, ['Beng','Deva','Dogr','Gong','Gonm','Gran','Gujr','Gukh','Guru','Knda','Limb','Mahj','Mlym','Nand','Onao','Orya','Sind','Sinh','Sylo','Takr','Taml','Telu','Tirh']).
unicode_script_extension(2406, 2415, ['Deva','Dogr','Kthi','Mahj']).
unicode_script_extension(2534, 2543, ['Beng','Cakm','Sylo']).
unicode_script_extension(2662, 2671, ['Guru','Mult']).
unicode_script_extension(2790, 2799, ['Gujr','Khoj']).
unicode_script_extension(3046, 3055, ['Gran','Taml']).
unicode_script_extension(3056, 3058, ['Gran','Taml']).
unicode_script_extension(3059, 3059, ['Gran','Taml']).
unicode_script_extension(3302, 3311, ['Knda','Nand','Tutg']).
unicode_script_extension(4160, 4169, ['Cakm','Mymr','Tale']).
unicode_script_extension(4347, 4347, ['Geor','Glag','Latn']).
unicode_script_extension(5867, 5869, ['Runr']).
unicode_script_extension(5941, 5942, ['Buhd','Hano','Tagb','Tglg']).
unicode_script_extension(6146, 6147, ['Mong','Phag']).
unicode_script_extension(6149, 6149, ['Mong','Phag']).
unicode_script_extension(7376, 7376, ['Beng','Deva','Gran','Knda']).
unicode_script_extension(7377, 7377, ['Deva']).
unicode_script_extension(7378, 7378, ['Beng','Deva','Gran','Knda']).
unicode_script_extension(7379, 7379, ['Deva','Gran','Knda']).
unicode_script_extension(7380, 7380, ['Deva']).
unicode_script_extension(7381, 7381, ['Beng','Deva','Newa','Telu','Tirh']).
unicode_script_extension(7382, 7382, ['Beng','Deva','Telu']).
unicode_script_extension(7383, 7383, ['Deva','Newa','Shrd']).
unicode_script_extension(7384, 7384, ['Beng','Deva','Newa','Telu']).
unicode_script_extension(7385, 7385, ['Deva','Shrd']).
unicode_script_extension(7386, 7386, ['Deva','Knda','Mlym','Orya','Taml','Telu']).
unicode_script_extension(7387, 7387, ['Deva']).
unicode_script_extension(7388, 7389, ['Deva','Shrd']).
unicode_script_extension(7390, 7391, ['Deva']).
unicode_script_extension(7392, 7392, ['Deva','Shrd']).
unicode_script_extension(7393, 7393, ['Beng','Deva']).
unicode_script_extension(7394, 7394, ['Deva','Newa','Tirh']).
unicode_script_extension(7395, 7400, ['Deva']).
unicode_script_extension(7401, 7401, ['Deva','Nand','Newa']).
unicode_script_extension(7402, 7402, ['Beng','Deva','Shrd']).
unicode_script_extension(7403, 7403, ['Deva','Newa']).
unicode_script_extension(7404, 7404, ['Deva']).
unicode_script_extension(7405, 7405, ['Beng','Deva','Newa','Shrd']).
unicode_script_extension(7406, 7409, ['Deva']).
unicode_script_extension(7410, 7410, ['Beng','Deva','Gran','Knda','Mlym','Nand','Orya','Sinh','Telu','Tirh','Tutg']).
unicode_script_extension(7411, 7411, ['Deva','Gran']).
unicode_script_extension(7412, 7412, ['Deva','Gran','Knda','Tutg']).
unicode_script_extension(7413, 7414, ['Beng','Deva']).
unicode_script_extension(7415, 7415, ['Beng']).
unicode_script_extension(7416, 7417, ['Deva','Gran']).
unicode_script_extension(7418, 7418, ['Nand']).
unicode_script_extension(7616, 7617, ['Grek']).
unicode_script_extension(7672, 7672, ['Cyrl','Latn','Syrc']).
unicode_script_extension(7674, 7674, ['Syrc']).
unicode_script_extension(8239, 8239, ['Latn','Mong','Phag']).
unicode_script_extension(8271, 8271, ['Adlm','Arab']).
unicode_script_extension(8282, 8282, ['Cari','Geor','Glag','Hung','Lyci','Orkh']).
unicode_script_extension(8285, 8285, ['Cari','Grek','Hung','Mero']).
unicode_script_extension(8432, 8432, ['Deva','Gran','Latn']).
unicode_script_extension(11799, 11799, ['Copt','Latn']).
unicode_script_extension(11824, 11824, ['Avst','Orkh']).
unicode_script_extension(11825, 11825, ['Avst','Cari','Geor','Hung','Kthi','Lydi','Samr']).
unicode_script_extension(11836, 11836, ['Dupl']).
unicode_script_extension(11841, 11841, ['Adlm','Arab','Hung']).
unicode_script_extension(11843, 11843, ['Cyrl','Glag']).
unicode_script_extension(12272, 12287, ['Hani','Tang']).
unicode_script_extension(12289, 12289, ['Bopo','Hang','Hani','Hira','Kana','Mong','Yiii']).
unicode_script_extension(12290, 12290, ['Bopo','Hang','Hani','Hira','Kana','Mong','Phag','Yiii']).
unicode_script_extension(12291, 12291, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12294, 12294, ['Hani']).
unicode_script_extension(12296, 12296, ['Bopo','Hang','Hani','Hira','Kana','Mong','Tibt','Yiii']).
unicode_script_extension(12297, 12297, ['Bopo','Hang','Hani','Hira','Kana','Mong','Tibt','Yiii']).
unicode_script_extension(12298, 12298, ['Bopo','Hang','Hani','Hira','Kana','Lisu','Mong','Tibt','Yiii']).
unicode_script_extension(12299, 12299, ['Bopo','Hang','Hani','Hira','Kana','Lisu','Mong','Tibt','Yiii']).
unicode_script_extension(12300, 12300, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12301, 12301, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12302, 12302, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12303, 12303, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12304, 12304, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12305, 12305, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12307, 12307, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12308, 12308, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12309, 12309, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12310, 12310, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12311, 12311, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12312, 12312, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12313, 12313, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12314, 12314, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12315, 12315, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12316, 12316, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12317, 12317, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12318, 12319, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12330, 12333, ['Bopo','Hani']).
unicode_script_extension(12336, 12336, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12337, 12341, ['Hira','Kana']).
unicode_script_extension(12343, 12343, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(12348, 12348, ['Hani','Hira','Kana']).
unicode_script_extension(12349, 12349, ['Hani','Hira','Kana']).
unicode_script_extension(12350, 12351, ['Hani']).
unicode_script_extension(12441, 12442, ['Hira','Kana']).
unicode_script_extension(12443, 12444, ['Hira','Kana']).
unicode_script_extension(12448, 12448, ['Hira','Kana']).
unicode_script_extension(12539, 12539, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(12540, 12540, ['Hira','Kana']).
unicode_script_extension(12688, 12689, ['Hani']).
unicode_script_extension(12690, 12693, ['Hani']).
unicode_script_extension(12694, 12703, ['Hani']).
unicode_script_extension(12736, 12773, ['Hani']).
unicode_script_extension(12783, 12783, ['Hani','Tang']).
unicode_script_extension(12832, 12841, ['Hani']).
unicode_script_extension(12842, 12871, ['Hani']).
unicode_script_extension(12928, 12937, ['Hani']).
unicode_script_extension(12938, 12976, ['Hani']).
unicode_script_extension(12992, 13003, ['Hani']).
unicode_script_extension(13055, 13055, ['Hani']).
unicode_script_extension(13144, 13168, ['Hani']).
unicode_script_extension(13179, 13183, ['Hani']).
unicode_script_extension(13280, 13310, ['Hani']).
unicode_script_extension(42607, 42607, ['Cyrl','Glag']).
unicode_script_extension(42752, 42759, ['Hani','Latn']).
unicode_script_extension(43056, 43058, ['Deva','Dogr','Gujr','Guru','Khoj','Knda','Kthi','Mahj','Mlym','Modi','Nand','Shrd','Sind','Takr','Tirh','Tutg']).
unicode_script_extension(43059, 43061, ['Deva','Dogr','Gujr','Guru','Khoj','Knda','Kthi','Mahj','Modi','Nand','Shrd','Sind','Takr','Tirh','Tutg']).
unicode_script_extension(43062, 43063, ['Deva','Dogr','Gujr','Guru','Khoj','Kthi','Mahj','Modi','Sind','Takr','Tirh']).
unicode_script_extension(43064, 43064, ['Deva','Dogr','Gujr','Guru','Khoj','Kthi','Mahj','Modi','Shrd','Sind','Takr','Tirh']).
unicode_script_extension(43065, 43065, ['Deva','Dogr','Gujr','Guru','Khoj','Kthi','Mahj','Modi','Sind','Takr','Tirh']).
unicode_script_extension(43249, 43249, ['Beng','Deva','Tutg']).
unicode_script_extension(43251, 43251, ['Deva','Taml']).
unicode_script_extension(43310, 43310, ['Kali','Latn','Mymr']).
unicode_script_extension(43471, 43471, ['Bugi','Java']).
unicode_script_extension(64830, 64830, ['Arab','Nkoo']).
unicode_script_extension(64831, 64831, ['Arab','Nkoo']).
unicode_script_extension(65010, 65010, ['Arab','Thaa']).
unicode_script_extension(65021, 65021, ['Arab','Thaa']).
unicode_script_extension(65093, 65094, ['Bopo','Hang','Hani','Hira','Kana']).
unicode_script_extension(65377, 65377, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(65378, 65378, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(65379, 65379, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(65380, 65381, ['Bopo','Hang','Hani','Hira','Kana','Yiii']).
unicode_script_extension(65392, 65392, ['Hira','Kana']).
unicode_script_extension(65438, 65439, ['Hira','Kana']).
unicode_script_extension(65792, 65793, ['Cpmn','Cprt','Linb']).
unicode_script_extension(65794, 65794, ['Cprt','Linb']).
unicode_script_extension(65799, 65843, ['Cprt','Lina','Linb']).
unicode_script_extension(65847, 65855, ['Cprt','Linb']).
unicode_script_extension(66272, 66272, ['Arab','Copt']).
unicode_script_extension(66273, 66299, ['Arab','Copt']).
unicode_script_extension(68338, 68338, ['Mani','Ougr']).
unicode_script_extension(70401, 70401, ['Gran','Taml']).
unicode_script_extension(70403, 70403, ['Gran','Taml']).
unicode_script_extension(70459, 70460, ['Gran','Taml']).
unicode_script_extension(73680, 73681, ['Gran','Taml']).
unicode_script_extension(73683, 73683, ['Gran','Taml']).
unicode_script_extension(113824, 113827, ['Dupl']).
unicode_script_extension(119648, 119665, ['Hani']).
unicode_script_extension(127568, 127569, ['Hani']).
