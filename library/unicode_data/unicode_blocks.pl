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

% Generated from Unicode 17.0.0 UCD data. Do not edit.

unicode_block(CodePoint, Value) :-
	(	var(CodePoint) ->
		unicode_block(Start, End, Value),
		between(Start, End, CodePoint)
	;	unicode_block(Start, End, SpecificValue),
		CodePoint >= Start, CodePoint =< End ->
		Value = SpecificValue
	;	between(0, 1114111, CodePoint),
		Value = 'No_Block'
	).

unicode_block(0, 127, 'Basic Latin').
unicode_block(128, 255, 'Latin-1 Supplement').
unicode_block(256, 383, 'Latin Extended-A').
unicode_block(384, 591, 'Latin Extended-B').
unicode_block(592, 687, 'IPA Extensions').
unicode_block(688, 767, 'Spacing Modifier Letters').
unicode_block(768, 879, 'Combining Diacritical Marks').
unicode_block(880, 1023, 'Greek and Coptic').
unicode_block(1024, 1279, 'Cyrillic').
unicode_block(1280, 1327, 'Cyrillic Supplement').
unicode_block(1328, 1423, 'Armenian').
unicode_block(1424, 1535, 'Hebrew').
unicode_block(1536, 1791, 'Arabic').
unicode_block(1792, 1871, 'Syriac').
unicode_block(1872, 1919, 'Arabic Supplement').
unicode_block(1920, 1983, 'Thaana').
unicode_block(1984, 2047, 'NKo').
unicode_block(2048, 2111, 'Samaritan').
unicode_block(2112, 2143, 'Mandaic').
unicode_block(2144, 2159, 'Syriac Supplement').
unicode_block(2160, 2207, 'Arabic Extended-B').
unicode_block(2208, 2303, 'Arabic Extended-A').
unicode_block(2304, 2431, 'Devanagari').
unicode_block(2432, 2559, 'Bengali').
unicode_block(2560, 2687, 'Gurmukhi').
unicode_block(2688, 2815, 'Gujarati').
unicode_block(2816, 2943, 'Oriya').
unicode_block(2944, 3071, 'Tamil').
unicode_block(3072, 3199, 'Telugu').
unicode_block(3200, 3327, 'Kannada').
unicode_block(3328, 3455, 'Malayalam').
unicode_block(3456, 3583, 'Sinhala').
unicode_block(3584, 3711, 'Thai').
unicode_block(3712, 3839, 'Lao').
unicode_block(3840, 4095, 'Tibetan').
unicode_block(4096, 4255, 'Myanmar').
unicode_block(4256, 4351, 'Georgian').
unicode_block(4352, 4607, 'Hangul Jamo').
unicode_block(4608, 4991, 'Ethiopic').
unicode_block(4992, 5023, 'Ethiopic Supplement').
unicode_block(5024, 5119, 'Cherokee').
unicode_block(5120, 5759, 'Unified Canadian Aboriginal Syllabics').
unicode_block(5760, 5791, 'Ogham').
unicode_block(5792, 5887, 'Runic').
unicode_block(5888, 5919, 'Tagalog').
unicode_block(5920, 5951, 'Hanunoo').
unicode_block(5952, 5983, 'Buhid').
unicode_block(5984, 6015, 'Tagbanwa').
unicode_block(6016, 6143, 'Khmer').
unicode_block(6144, 6319, 'Mongolian').
unicode_block(6320, 6399, 'Unified Canadian Aboriginal Syllabics Extended').
unicode_block(6400, 6479, 'Limbu').
unicode_block(6480, 6527, 'Tai Le').
unicode_block(6528, 6623, 'New Tai Lue').
unicode_block(6624, 6655, 'Khmer Symbols').
unicode_block(6656, 6687, 'Buginese').
unicode_block(6688, 6831, 'Tai Tham').
unicode_block(6832, 6911, 'Combining Diacritical Marks Extended').
unicode_block(6912, 7039, 'Balinese').
unicode_block(7040, 7103, 'Sundanese').
unicode_block(7104, 7167, 'Batak').
unicode_block(7168, 7247, 'Lepcha').
unicode_block(7248, 7295, 'Ol Chiki').
unicode_block(7296, 7311, 'Cyrillic Extended-C').
unicode_block(7312, 7359, 'Georgian Extended').
unicode_block(7360, 7375, 'Sundanese Supplement').
unicode_block(7376, 7423, 'Vedic Extensions').
unicode_block(7424, 7551, 'Phonetic Extensions').
unicode_block(7552, 7615, 'Phonetic Extensions Supplement').
unicode_block(7616, 7679, 'Combining Diacritical Marks Supplement').
unicode_block(7680, 7935, 'Latin Extended Additional').
unicode_block(7936, 8191, 'Greek Extended').
unicode_block(8192, 8303, 'General Punctuation').
unicode_block(8304, 8351, 'Superscripts and Subscripts').
unicode_block(8352, 8399, 'Currency Symbols').
unicode_block(8400, 8447, 'Combining Diacritical Marks for Symbols').
unicode_block(8448, 8527, 'Letterlike Symbols').
unicode_block(8528, 8591, 'Number Forms').
unicode_block(8592, 8703, 'Arrows').
unicode_block(8704, 8959, 'Mathematical Operators').
unicode_block(8960, 9215, 'Miscellaneous Technical').
unicode_block(9216, 9279, 'Control Pictures').
unicode_block(9280, 9311, 'Optical Character Recognition').
unicode_block(9312, 9471, 'Enclosed Alphanumerics').
unicode_block(9472, 9599, 'Box Drawing').
unicode_block(9600, 9631, 'Block Elements').
unicode_block(9632, 9727, 'Geometric Shapes').
unicode_block(9728, 9983, 'Miscellaneous Symbols').
unicode_block(9984, 10175, 'Dingbats').
unicode_block(10176, 10223, 'Miscellaneous Mathematical Symbols-A').
unicode_block(10224, 10239, 'Supplemental Arrows-A').
unicode_block(10240, 10495, 'Braille Patterns').
unicode_block(10496, 10623, 'Supplemental Arrows-B').
unicode_block(10624, 10751, 'Miscellaneous Mathematical Symbols-B').
unicode_block(10752, 11007, 'Supplemental Mathematical Operators').
unicode_block(11008, 11263, 'Miscellaneous Symbols and Arrows').
unicode_block(11264, 11359, 'Glagolitic').
unicode_block(11360, 11391, 'Latin Extended-C').
unicode_block(11392, 11519, 'Coptic').
unicode_block(11520, 11567, 'Georgian Supplement').
unicode_block(11568, 11647, 'Tifinagh').
unicode_block(11648, 11743, 'Ethiopic Extended').
unicode_block(11744, 11775, 'Cyrillic Extended-A').
unicode_block(11776, 11903, 'Supplemental Punctuation').
unicode_block(11904, 12031, 'CJK Radicals Supplement').
unicode_block(12032, 12255, 'Kangxi Radicals').
unicode_block(12272, 12287, 'Ideographic Description Characters').
unicode_block(12288, 12351, 'CJK Symbols and Punctuation').
unicode_block(12352, 12447, 'Hiragana').
unicode_block(12448, 12543, 'Katakana').
unicode_block(12544, 12591, 'Bopomofo').
unicode_block(12592, 12687, 'Hangul Compatibility Jamo').
unicode_block(12688, 12703, 'Kanbun').
unicode_block(12704, 12735, 'Bopomofo Extended').
unicode_block(12736, 12783, 'CJK Strokes').
unicode_block(12784, 12799, 'Katakana Phonetic Extensions').
unicode_block(12800, 13055, 'Enclosed CJK Letters and Months').
unicode_block(13056, 13311, 'CJK Compatibility').
unicode_block(13312, 19903, 'CJK Unified Ideographs Extension A').
unicode_block(19904, 19967, 'Yijing Hexagram Symbols').
unicode_block(19968, 40959, 'CJK Unified Ideographs').
unicode_block(40960, 42127, 'Yi Syllables').
unicode_block(42128, 42191, 'Yi Radicals').
unicode_block(42192, 42239, 'Lisu').
unicode_block(42240, 42559, 'Vai').
unicode_block(42560, 42655, 'Cyrillic Extended-B').
unicode_block(42656, 42751, 'Bamum').
unicode_block(42752, 42783, 'Modifier Tone Letters').
unicode_block(42784, 43007, 'Latin Extended-D').
unicode_block(43008, 43055, 'Syloti Nagri').
unicode_block(43056, 43071, 'Common Indic Number Forms').
unicode_block(43072, 43135, 'Phags-pa').
unicode_block(43136, 43231, 'Saurashtra').
unicode_block(43232, 43263, 'Devanagari Extended').
unicode_block(43264, 43311, 'Kayah Li').
unicode_block(43312, 43359, 'Rejang').
unicode_block(43360, 43391, 'Hangul Jamo Extended-A').
unicode_block(43392, 43487, 'Javanese').
unicode_block(43488, 43519, 'Myanmar Extended-B').
unicode_block(43520, 43615, 'Cham').
unicode_block(43616, 43647, 'Myanmar Extended-A').
unicode_block(43648, 43743, 'Tai Viet').
unicode_block(43744, 43775, 'Meetei Mayek Extensions').
unicode_block(43776, 43823, 'Ethiopic Extended-A').
unicode_block(43824, 43887, 'Latin Extended-E').
unicode_block(43888, 43967, 'Cherokee Supplement').
unicode_block(43968, 44031, 'Meetei Mayek').
unicode_block(44032, 55215, 'Hangul Syllables').
unicode_block(55216, 55295, 'Hangul Jamo Extended-B').
unicode_block(55296, 56191, 'High Surrogates').
unicode_block(56192, 56319, 'High Private Use Surrogates').
unicode_block(56320, 57343, 'Low Surrogates').
unicode_block(57344, 63743, 'Private Use Area').
unicode_block(63744, 64255, 'CJK Compatibility Ideographs').
unicode_block(64256, 64335, 'Alphabetic Presentation Forms').
unicode_block(64336, 65023, 'Arabic Presentation Forms-A').
unicode_block(65024, 65039, 'Variation Selectors').
unicode_block(65040, 65055, 'Vertical Forms').
unicode_block(65056, 65071, 'Combining Half Marks').
unicode_block(65072, 65103, 'CJK Compatibility Forms').
unicode_block(65104, 65135, 'Small Form Variants').
unicode_block(65136, 65279, 'Arabic Presentation Forms-B').
unicode_block(65280, 65519, 'Halfwidth and Fullwidth Forms').
unicode_block(65520, 65535, 'Specials').
unicode_block(65536, 65663, 'Linear B Syllabary').
unicode_block(65664, 65791, 'Linear B Ideograms').
unicode_block(65792, 65855, 'Aegean Numbers').
unicode_block(65856, 65935, 'Ancient Greek Numbers').
unicode_block(65936, 65999, 'Ancient Symbols').
unicode_block(66000, 66047, 'Phaistos Disc').
unicode_block(66176, 66207, 'Lycian').
unicode_block(66208, 66271, 'Carian').
unicode_block(66272, 66303, 'Coptic Epact Numbers').
unicode_block(66304, 66351, 'Old Italic').
unicode_block(66352, 66383, 'Gothic').
unicode_block(66384, 66431, 'Old Permic').
unicode_block(66432, 66463, 'Ugaritic').
unicode_block(66464, 66527, 'Old Persian').
unicode_block(66560, 66639, 'Deseret').
unicode_block(66640, 66687, 'Shavian').
unicode_block(66688, 66735, 'Osmanya').
unicode_block(66736, 66815, 'Osage').
unicode_block(66816, 66863, 'Elbasan').
unicode_block(66864, 66927, 'Caucasian Albanian').
unicode_block(66928, 67007, 'Vithkuqi').
unicode_block(67008, 67071, 'Todhri').
unicode_block(67072, 67455, 'Linear A').
unicode_block(67456, 67519, 'Latin Extended-F').
unicode_block(67584, 67647, 'Cypriot Syllabary').
unicode_block(67648, 67679, 'Imperial Aramaic').
unicode_block(67680, 67711, 'Palmyrene').
unicode_block(67712, 67759, 'Nabataean').
unicode_block(67808, 67839, 'Hatran').
unicode_block(67840, 67871, 'Phoenician').
unicode_block(67872, 67903, 'Lydian').
unicode_block(67904, 67935, 'Sidetic').
unicode_block(67968, 67999, 'Meroitic Hieroglyphs').
unicode_block(68000, 68095, 'Meroitic Cursive').
unicode_block(68096, 68191, 'Kharoshthi').
unicode_block(68192, 68223, 'Old South Arabian').
unicode_block(68224, 68255, 'Old North Arabian').
unicode_block(68288, 68351, 'Manichaean').
unicode_block(68352, 68415, 'Avestan').
unicode_block(68416, 68447, 'Inscriptional Parthian').
unicode_block(68448, 68479, 'Inscriptional Pahlavi').
unicode_block(68480, 68527, 'Psalter Pahlavi').
unicode_block(68608, 68687, 'Old Turkic').
unicode_block(68736, 68863, 'Old Hungarian').
unicode_block(68864, 68927, 'Hanifi Rohingya').
unicode_block(68928, 69007, 'Garay').
unicode_block(69216, 69247, 'Rumi Numeral Symbols').
unicode_block(69248, 69311, 'Yezidi').
unicode_block(69312, 69375, 'Arabic Extended-C').
unicode_block(69376, 69423, 'Old Sogdian').
unicode_block(69424, 69487, 'Sogdian').
unicode_block(69488, 69551, 'Old Uyghur').
unicode_block(69552, 69599, 'Chorasmian').
unicode_block(69600, 69631, 'Elymaic').
unicode_block(69632, 69759, 'Brahmi').
unicode_block(69760, 69839, 'Kaithi').
unicode_block(69840, 69887, 'Sora Sompeng').
unicode_block(69888, 69967, 'Chakma').
unicode_block(69968, 70015, 'Mahajani').
unicode_block(70016, 70111, 'Sharada').
unicode_block(70112, 70143, 'Sinhala Archaic Numbers').
unicode_block(70144, 70223, 'Khojki').
unicode_block(70272, 70319, 'Multani').
unicode_block(70320, 70399, 'Khudawadi').
unicode_block(70400, 70527, 'Grantha').
unicode_block(70528, 70655, 'Tulu-Tigalari').
unicode_block(70656, 70783, 'Newa').
unicode_block(70784, 70879, 'Tirhuta').
unicode_block(71040, 71167, 'Siddham').
unicode_block(71168, 71263, 'Modi').
unicode_block(71264, 71295, 'Mongolian Supplement').
unicode_block(71296, 71375, 'Takri').
unicode_block(71376, 71423, 'Myanmar Extended-C').
unicode_block(71424, 71503, 'Ahom').
unicode_block(71680, 71759, 'Dogra').
unicode_block(71840, 71935, 'Warang Citi').
unicode_block(71936, 72031, 'Dives Akuru').
unicode_block(72096, 72191, 'Nandinagari').
unicode_block(72192, 72271, 'Zanabazar Square').
unicode_block(72272, 72367, 'Soyombo').
unicode_block(72368, 72383, 'Unified Canadian Aboriginal Syllabics Extended-A').
unicode_block(72384, 72447, 'Pau Cin Hau').
unicode_block(72448, 72543, 'Devanagari Extended-A').
unicode_block(72544, 72575, 'Sharada Supplement').
unicode_block(72640, 72703, 'Sunuwar').
unicode_block(72704, 72815, 'Bhaiksuki').
unicode_block(72816, 72895, 'Marchen').
unicode_block(72960, 73055, 'Masaram Gondi').
unicode_block(73056, 73135, 'Gunjala Gondi').
unicode_block(73136, 73199, 'Tolong Siki').
unicode_block(73440, 73471, 'Makasar').
unicode_block(73472, 73567, 'Kawi').
unicode_block(73648, 73663, 'Lisu Supplement').
unicode_block(73664, 73727, 'Tamil Supplement').
unicode_block(73728, 74751, 'Cuneiform').
unicode_block(74752, 74879, 'Cuneiform Numbers and Punctuation').
unicode_block(74880, 75087, 'Early Dynastic Cuneiform').
unicode_block(77712, 77823, 'Cypro-Minoan').
unicode_block(77824, 78895, 'Egyptian Hieroglyphs').
unicode_block(78896, 78943, 'Egyptian Hieroglyph Format Controls').
unicode_block(78944, 82943, 'Egyptian Hieroglyphs Extended-A').
unicode_block(82944, 83583, 'Anatolian Hieroglyphs').
unicode_block(90368, 90431, 'Gurung Khema').
unicode_block(92160, 92735, 'Bamum Supplement').
unicode_block(92736, 92783, 'Mro').
unicode_block(92784, 92879, 'Tangsa').
unicode_block(92880, 92927, 'Bassa Vah').
unicode_block(92928, 93071, 'Pahawh Hmong').
unicode_block(93504, 93567, 'Kirat Rai').
unicode_block(93760, 93855, 'Medefaidrin').
unicode_block(93856, 93919, 'Beria Erfe').
unicode_block(93952, 94111, 'Miao').
unicode_block(94176, 94207, 'Ideographic Symbols and Punctuation').
unicode_block(94208, 100351, 'Tangut').
unicode_block(100352, 101119, 'Tangut Components').
unicode_block(101120, 101631, 'Khitan Small Script').
unicode_block(101632, 101759, 'Tangut Supplement').
unicode_block(101760, 101887, 'Tangut Components Supplement').
unicode_block(110576, 110591, 'Kana Extended-B').
unicode_block(110592, 110847, 'Kana Supplement').
unicode_block(110848, 110895, 'Kana Extended-A').
unicode_block(110896, 110959, 'Small Kana Extension').
unicode_block(110960, 111359, 'Nushu').
unicode_block(113664, 113823, 'Duployan').
unicode_block(113824, 113839, 'Shorthand Format Controls').
unicode_block(117760, 118463, 'Symbols for Legacy Computing Supplement').
unicode_block(118464, 118527, 'Miscellaneous Symbols Supplement').
unicode_block(118528, 118735, 'Znamenny Musical Notation').
unicode_block(118784, 119039, 'Byzantine Musical Symbols').
unicode_block(119040, 119295, 'Musical Symbols').
unicode_block(119296, 119375, 'Ancient Greek Musical Notation').
unicode_block(119488, 119519, 'Kaktovik Numerals').
unicode_block(119520, 119551, 'Mayan Numerals').
unicode_block(119552, 119647, 'Tai Xuan Jing Symbols').
unicode_block(119648, 119679, 'Counting Rod Numerals').
unicode_block(119808, 120831, 'Mathematical Alphanumeric Symbols').
unicode_block(120832, 121519, 'Sutton SignWriting').
unicode_block(122624, 122879, 'Latin Extended-G').
unicode_block(122880, 122927, 'Glagolitic Supplement').
unicode_block(122928, 123023, 'Cyrillic Extended-D').
unicode_block(123136, 123215, 'Nyiakeng Puachue Hmong').
unicode_block(123536, 123583, 'Toto').
unicode_block(123584, 123647, 'Wancho').
unicode_block(124112, 124159, 'Nag Mundari').
unicode_block(124368, 124415, 'Ol Onal').
unicode_block(124608, 124671, 'Tai Yo').
unicode_block(124896, 124927, 'Ethiopic Extended-B').
unicode_block(124928, 125151, 'Mende Kikakui').
unicode_block(125184, 125279, 'Adlam').
unicode_block(126064, 126143, 'Indic Siyaq Numbers').
unicode_block(126208, 126287, 'Ottoman Siyaq Numbers').
unicode_block(126464, 126719, 'Arabic Mathematical Alphabetic Symbols').
unicode_block(126976, 127023, 'Mahjong Tiles').
unicode_block(127024, 127135, 'Domino Tiles').
unicode_block(127136, 127231, 'Playing Cards').
unicode_block(127232, 127487, 'Enclosed Alphanumeric Supplement').
unicode_block(127488, 127743, 'Enclosed Ideographic Supplement').
unicode_block(127744, 128511, 'Miscellaneous Symbols and Pictographs').
unicode_block(128512, 128591, 'Emoticons').
unicode_block(128592, 128639, 'Ornamental Dingbats').
unicode_block(128640, 128767, 'Transport and Map Symbols').
unicode_block(128768, 128895, 'Alchemical Symbols').
unicode_block(128896, 129023, 'Geometric Shapes Extended').
unicode_block(129024, 129279, 'Supplemental Arrows-C').
unicode_block(129280, 129535, 'Supplemental Symbols and Pictographs').
unicode_block(129536, 129647, 'Chess Symbols').
unicode_block(129648, 129791, 'Symbols and Pictographs Extended-A').
unicode_block(129792, 130047, 'Symbols for Legacy Computing').
unicode_block(131072, 173791, 'CJK Unified Ideographs Extension B').
unicode_block(173824, 177983, 'CJK Unified Ideographs Extension C').
unicode_block(177984, 178207, 'CJK Unified Ideographs Extension D').
unicode_block(178208, 183983, 'CJK Unified Ideographs Extension E').
unicode_block(183984, 191471, 'CJK Unified Ideographs Extension F').
unicode_block(191472, 192095, 'CJK Unified Ideographs Extension I').
unicode_block(194560, 195103, 'CJK Compatibility Ideographs Supplement').
unicode_block(196608, 201551, 'CJK Unified Ideographs Extension G').
unicode_block(201552, 205743, 'CJK Unified Ideographs Extension H').
unicode_block(205744, 210047, 'CJK Unified Ideographs Extension J').
unicode_block(917504, 917631, 'Tags').
unicode_block(917760, 917999, 'Variation Selectors Supplement').
unicode_block(983040, 1048575, 'Supplementary Private Use Area-A').
unicode_block(1048576, 1114111, 'Supplementary Private Use Area-B').
