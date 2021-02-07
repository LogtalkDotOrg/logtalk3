%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of VivoMind Prolog Unicode Resources
%  SPDX-License-Identifier: CC0-1.0
%
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 19, 2012
%
%  Original Unicode file header comments follow

/*
# HangulSyllableType-6.1.0.txt
# Date: 2011-08-25, 00:02:18 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

% ================================================

% Property:	Hangul_Syllable_Type

%  All code points not explicitly listed for Hangul_Syllable_Type
%  have the value Not_Applicable (NA).

% @missing: 0000..10FFFF; Not_Applicable

% ================================================

unicode_hangul_syllable_type(CodePoint, Type) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_hangul_syllable_type(CodePointStart, CodePointEnd, Type),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_hangul_syllable_type(CodePoint, _, CodePointType) ->
		Type = CodePointType
	;	% look for a code point range that includes the given code point
		unicode_hangul_syllable_type(CodePointStart, CodePointEnd, CodePointType),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Type = CodePointType
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Type = 'NA'
	).

% Hangul_Syllable_Type=Leading_Jamo

unicode_hangul_syllable_type(0x1100, 0x115F, 'L').	% Lo  [96] HANGUL CHOSEONG KIYEOK..HANGUL CHOSEONG FILLER
unicode_hangul_syllable_type(0xA960, 0xA97C, 'L').	% Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH

% Total code points: 125

% ================================================

% Hangul_Syllable_Type=Vowel_Jamo

unicode_hangul_syllable_type(0x1160, 0x11A7, 'V').	% Lo  [72] HANGUL JUNGSEONG FILLER..HANGUL JUNGSEONG O-YAE
unicode_hangul_syllable_type(0xD7B0, 0xD7C6, 'V').	% Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E

% Total code points: 95

% ================================================

% Hangul_Syllable_Type=Trailing_Jamo

unicode_hangul_syllable_type(0x11A8, 0x11FF, 'T').	% Lo  [88] HANGUL JONGSEONG KIYEOK..HANGUL JONGSEONG SSANGNIEUN
unicode_hangul_syllable_type(0xD7CB, 0xD7FB, 'T').	% Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH

% Total code points: 137

% ================================================

% Hangul_Syllable_Type=LV_Syllable

unicode_hangul_syllable_type(0xAC00, 0xAC00, 'LV').	% Lo       HANGUL SYLLABLE GA
unicode_hangul_syllable_type(0xAC1C, 0xAC1C, 'LV').	% Lo       HANGUL SYLLABLE GAE
unicode_hangul_syllable_type(0xAC38, 0xAC38, 'LV').	% Lo       HANGUL SYLLABLE GYA
unicode_hangul_syllable_type(0xAC54, 0xAC54, 'LV').	% Lo       HANGUL SYLLABLE GYAE
unicode_hangul_syllable_type(0xAC70, 0xAC70, 'LV').	% Lo       HANGUL SYLLABLE GEO
unicode_hangul_syllable_type(0xAC8C, 0xAC8C, 'LV').	% Lo       HANGUL SYLLABLE GE
unicode_hangul_syllable_type(0xACA8, 0xACA8, 'LV').	% Lo       HANGUL SYLLABLE GYEO
unicode_hangul_syllable_type(0xACC4, 0xACC4, 'LV').	% Lo       HANGUL SYLLABLE GYE
unicode_hangul_syllable_type(0xACE0, 0xACE0, 'LV').	% Lo       HANGUL SYLLABLE GO
unicode_hangul_syllable_type(0xACFC, 0xACFC, 'LV').	% Lo       HANGUL SYLLABLE GWA
unicode_hangul_syllable_type(0xAD18, 0xAD18, 'LV').	% Lo       HANGUL SYLLABLE GWAE
unicode_hangul_syllable_type(0xAD34, 0xAD34, 'LV').	% Lo       HANGUL SYLLABLE GOE
unicode_hangul_syllable_type(0xAD50, 0xAD50, 'LV').	% Lo       HANGUL SYLLABLE GYO
unicode_hangul_syllable_type(0xAD6C, 0xAD6C, 'LV').	% Lo       HANGUL SYLLABLE GU
unicode_hangul_syllable_type(0xAD88, 0xAD88, 'LV').	% Lo       HANGUL SYLLABLE GWEO
unicode_hangul_syllable_type(0xADA4, 0xADA4, 'LV').	% Lo       HANGUL SYLLABLE GWE
unicode_hangul_syllable_type(0xADC0, 0xADC0, 'LV').	% Lo       HANGUL SYLLABLE GWI
unicode_hangul_syllable_type(0xADDC, 0xADDC, 'LV').	% Lo       HANGUL SYLLABLE GYU
unicode_hangul_syllable_type(0xADF8, 0xADF8, 'LV').	% Lo       HANGUL SYLLABLE GEU
unicode_hangul_syllable_type(0xAE14, 0xAE14, 'LV').	% Lo       HANGUL SYLLABLE GYI
unicode_hangul_syllable_type(0xAE30, 0xAE30, 'LV').	% Lo       HANGUL SYLLABLE GI
unicode_hangul_syllable_type(0xAE4C, 0xAE4C, 'LV').	% Lo       HANGUL SYLLABLE GGA
unicode_hangul_syllable_type(0xAE68, 0xAE68, 'LV').	% Lo       HANGUL SYLLABLE GGAE
unicode_hangul_syllable_type(0xAE84, 0xAE84, 'LV').	% Lo       HANGUL SYLLABLE GGYA
unicode_hangul_syllable_type(0xAEA0, 0xAEA0, 'LV').	% Lo       HANGUL SYLLABLE GGYAE
unicode_hangul_syllable_type(0xAEBC, 0xAEBC, 'LV').	% Lo       HANGUL SYLLABLE GGEO
unicode_hangul_syllable_type(0xAED8, 0xAED8, 'LV').	% Lo       HANGUL SYLLABLE GGE
unicode_hangul_syllable_type(0xAEF4, 0xAEF4, 'LV').	% Lo       HANGUL SYLLABLE GGYEO
unicode_hangul_syllable_type(0xAF10, 0xAF10, 'LV').	% Lo       HANGUL SYLLABLE GGYE
unicode_hangul_syllable_type(0xAF2C, 0xAF2C, 'LV').	% Lo       HANGUL SYLLABLE GGO
unicode_hangul_syllable_type(0xAF48, 0xAF48, 'LV').	% Lo       HANGUL SYLLABLE GGWA
unicode_hangul_syllable_type(0xAF64, 0xAF64, 'LV').	% Lo       HANGUL SYLLABLE GGWAE
unicode_hangul_syllable_type(0xAF80, 0xAF80, 'LV').	% Lo       HANGUL SYLLABLE GGOE
unicode_hangul_syllable_type(0xAF9C, 0xAF9C, 'LV').	% Lo       HANGUL SYLLABLE GGYO
unicode_hangul_syllable_type(0xAFB8, 0xAFB8, 'LV').	% Lo       HANGUL SYLLABLE GGU
unicode_hangul_syllable_type(0xAFD4, 0xAFD4, 'LV').	% Lo       HANGUL SYLLABLE GGWEO
unicode_hangul_syllable_type(0xAFF0, 0xAFF0, 'LV').	% Lo       HANGUL SYLLABLE GGWE
unicode_hangul_syllable_type(0xB00C, 0xB00C, 'LV').	% Lo       HANGUL SYLLABLE GGWI
unicode_hangul_syllable_type(0xB028, 0xB028, 'LV').	% Lo       HANGUL SYLLABLE GGYU
unicode_hangul_syllable_type(0xB044, 0xB044, 'LV').	% Lo       HANGUL SYLLABLE GGEU
unicode_hangul_syllable_type(0xB060, 0xB060, 'LV').	% Lo       HANGUL SYLLABLE GGYI
unicode_hangul_syllable_type(0xB07C, 0xB07C, 'LV').	% Lo       HANGUL SYLLABLE GGI
unicode_hangul_syllable_type(0xB098, 0xB098, 'LV').	% Lo       HANGUL SYLLABLE NA
unicode_hangul_syllable_type(0xB0B4, 0xB0B4, 'LV').	% Lo       HANGUL SYLLABLE NAE
unicode_hangul_syllable_type(0xB0D0, 0xB0D0, 'LV').	% Lo       HANGUL SYLLABLE NYA
unicode_hangul_syllable_type(0xB0EC, 0xB0EC, 'LV').	% Lo       HANGUL SYLLABLE NYAE
unicode_hangul_syllable_type(0xB108, 0xB108, 'LV').	% Lo       HANGUL SYLLABLE NEO
unicode_hangul_syllable_type(0xB124, 0xB124, 'LV').	% Lo       HANGUL SYLLABLE NE
unicode_hangul_syllable_type(0xB140, 0xB140, 'LV').	% Lo       HANGUL SYLLABLE NYEO
unicode_hangul_syllable_type(0xB15C, 0xB15C, 'LV').	% Lo       HANGUL SYLLABLE NYE
unicode_hangul_syllable_type(0xB178, 0xB178, 'LV').	% Lo       HANGUL SYLLABLE NO
unicode_hangul_syllable_type(0xB194, 0xB194, 'LV').	% Lo       HANGUL SYLLABLE NWA
unicode_hangul_syllable_type(0xB1B0, 0xB1B0, 'LV').	% Lo       HANGUL SYLLABLE NWAE
unicode_hangul_syllable_type(0xB1CC, 0xB1CC, 'LV').	% Lo       HANGUL SYLLABLE NOE
unicode_hangul_syllable_type(0xB1E8, 0xB1E8, 'LV').	% Lo       HANGUL SYLLABLE NYO
unicode_hangul_syllable_type(0xB204, 0xB204, 'LV').	% Lo       HANGUL SYLLABLE NU
unicode_hangul_syllable_type(0xB220, 0xB220, 'LV').	% Lo       HANGUL SYLLABLE NWEO
unicode_hangul_syllable_type(0xB23C, 0xB23C, 'LV').	% Lo       HANGUL SYLLABLE NWE
unicode_hangul_syllable_type(0xB258, 0xB258, 'LV').	% Lo       HANGUL SYLLABLE NWI
unicode_hangul_syllable_type(0xB274, 0xB274, 'LV').	% Lo       HANGUL SYLLABLE NYU
unicode_hangul_syllable_type(0xB290, 0xB290, 'LV').	% Lo       HANGUL SYLLABLE NEU
unicode_hangul_syllable_type(0xB2AC, 0xB2AC, 'LV').	% Lo       HANGUL SYLLABLE NYI
unicode_hangul_syllable_type(0xB2C8, 0xB2C8, 'LV').	% Lo       HANGUL SYLLABLE NI
unicode_hangul_syllable_type(0xB2E4, 0xB2E4, 'LV').	% Lo       HANGUL SYLLABLE DA
unicode_hangul_syllable_type(0xB300, 0xB300, 'LV').	% Lo       HANGUL SYLLABLE DAE
unicode_hangul_syllable_type(0xB31C, 0xB31C, 'LV').	% Lo       HANGUL SYLLABLE DYA
unicode_hangul_syllable_type(0xB338, 0xB338, 'LV').	% Lo       HANGUL SYLLABLE DYAE
unicode_hangul_syllable_type(0xB354, 0xB354, 'LV').	% Lo       HANGUL SYLLABLE DEO
unicode_hangul_syllable_type(0xB370, 0xB370, 'LV').	% Lo       HANGUL SYLLABLE DE
unicode_hangul_syllable_type(0xB38C, 0xB38C, 'LV').	% Lo       HANGUL SYLLABLE DYEO
unicode_hangul_syllable_type(0xB3A8, 0xB3A8, 'LV').	% Lo       HANGUL SYLLABLE DYE
unicode_hangul_syllable_type(0xB3C4, 0xB3C4, 'LV').	% Lo       HANGUL SYLLABLE DO
unicode_hangul_syllable_type(0xB3E0, 0xB3E0, 'LV').	% Lo       HANGUL SYLLABLE DWA
unicode_hangul_syllable_type(0xB3FC, 0xB3FC, 'LV').	% Lo       HANGUL SYLLABLE DWAE
unicode_hangul_syllable_type(0xB418, 0xB418, 'LV').	% Lo       HANGUL SYLLABLE DOE
unicode_hangul_syllable_type(0xB434, 0xB434, 'LV').	% Lo       HANGUL SYLLABLE DYO
unicode_hangul_syllable_type(0xB450, 0xB450, 'LV').	% Lo       HANGUL SYLLABLE DU
unicode_hangul_syllable_type(0xB46C, 0xB46C, 'LV').	% Lo       HANGUL SYLLABLE DWEO
unicode_hangul_syllable_type(0xB488, 0xB488, 'LV').	% Lo       HANGUL SYLLABLE DWE
unicode_hangul_syllable_type(0xB4A4, 0xB4A4, 'LV').	% Lo       HANGUL SYLLABLE DWI
unicode_hangul_syllable_type(0xB4C0, 0xB4C0, 'LV').	% Lo       HANGUL SYLLABLE DYU
unicode_hangul_syllable_type(0xB4DC, 0xB4DC, 'LV').	% Lo       HANGUL SYLLABLE DEU
unicode_hangul_syllable_type(0xB4F8, 0xB4F8, 'LV').	% Lo       HANGUL SYLLABLE DYI
unicode_hangul_syllable_type(0xB514, 0xB514, 'LV').	% Lo       HANGUL SYLLABLE DI
unicode_hangul_syllable_type(0xB530, 0xB530, 'LV').	% Lo       HANGUL SYLLABLE DDA
unicode_hangul_syllable_type(0xB54C, 0xB54C, 'LV').	% Lo       HANGUL SYLLABLE DDAE
unicode_hangul_syllable_type(0xB568, 0xB568, 'LV').	% Lo       HANGUL SYLLABLE DDYA
unicode_hangul_syllable_type(0xB584, 0xB584, 'LV').	% Lo       HANGUL SYLLABLE DDYAE
unicode_hangul_syllable_type(0xB5A0, 0xB5A0, 'LV').	% Lo       HANGUL SYLLABLE DDEO
unicode_hangul_syllable_type(0xB5BC, 0xB5BC, 'LV').	% Lo       HANGUL SYLLABLE DDE
unicode_hangul_syllable_type(0xB5D8, 0xB5D8, 'LV').	% Lo       HANGUL SYLLABLE DDYEO
unicode_hangul_syllable_type(0xB5F4, 0xB5F4, 'LV').	% Lo       HANGUL SYLLABLE DDYE
unicode_hangul_syllable_type(0xB610, 0xB610, 'LV').	% Lo       HANGUL SYLLABLE DDO
unicode_hangul_syllable_type(0xB62C, 0xB62C, 'LV').	% Lo       HANGUL SYLLABLE DDWA
unicode_hangul_syllable_type(0xB648, 0xB648, 'LV').	% Lo       HANGUL SYLLABLE DDWAE
unicode_hangul_syllable_type(0xB664, 0xB664, 'LV').	% Lo       HANGUL SYLLABLE DDOE
unicode_hangul_syllable_type(0xB680, 0xB680, 'LV').	% Lo       HANGUL SYLLABLE DDYO
unicode_hangul_syllable_type(0xB69C, 0xB69C, 'LV').	% Lo       HANGUL SYLLABLE DDU
unicode_hangul_syllable_type(0xB6B8, 0xB6B8, 'LV').	% Lo       HANGUL SYLLABLE DDWEO
unicode_hangul_syllable_type(0xB6D4, 0xB6D4, 'LV').	% Lo       HANGUL SYLLABLE DDWE
unicode_hangul_syllable_type(0xB6F0, 0xB6F0, 'LV').	% Lo       HANGUL SYLLABLE DDWI
unicode_hangul_syllable_type(0xB70C, 0xB70C, 'LV').	% Lo       HANGUL SYLLABLE DDYU
unicode_hangul_syllable_type(0xB728, 0xB728, 'LV').	% Lo       HANGUL SYLLABLE DDEU
unicode_hangul_syllable_type(0xB744, 0xB744, 'LV').	% Lo       HANGUL SYLLABLE DDYI
unicode_hangul_syllable_type(0xB760, 0xB760, 'LV').	% Lo       HANGUL SYLLABLE DDI
unicode_hangul_syllable_type(0xB77C, 0xB77C, 'LV').	% Lo       HANGUL SYLLABLE RA
unicode_hangul_syllable_type(0xB798, 0xB798, 'LV').	% Lo       HANGUL SYLLABLE RAE
unicode_hangul_syllable_type(0xB7B4, 0xB7B4, 'LV').	% Lo       HANGUL SYLLABLE RYA
unicode_hangul_syllable_type(0xB7D0, 0xB7D0, 'LV').	% Lo       HANGUL SYLLABLE RYAE
unicode_hangul_syllable_type(0xB7EC, 0xB7EC, 'LV').	% Lo       HANGUL SYLLABLE REO
unicode_hangul_syllable_type(0xB808, 0xB808, 'LV').	% Lo       HANGUL SYLLABLE RE
unicode_hangul_syllable_type(0xB824, 0xB824, 'LV').	% Lo       HANGUL SYLLABLE RYEO
unicode_hangul_syllable_type(0xB840, 0xB840, 'LV').	% Lo       HANGUL SYLLABLE RYE
unicode_hangul_syllable_type(0xB85C, 0xB85C, 'LV').	% Lo       HANGUL SYLLABLE RO
unicode_hangul_syllable_type(0xB878, 0xB878, 'LV').	% Lo       HANGUL SYLLABLE RWA
unicode_hangul_syllable_type(0xB894, 0xB894, 'LV').	% Lo       HANGUL SYLLABLE RWAE
unicode_hangul_syllable_type(0xB8B0, 0xB8B0, 'LV').	% Lo       HANGUL SYLLABLE ROE
unicode_hangul_syllable_type(0xB8CC, 0xB8CC, 'LV').	% Lo       HANGUL SYLLABLE RYO
unicode_hangul_syllable_type(0xB8E8, 0xB8E8, 'LV').	% Lo       HANGUL SYLLABLE RU
unicode_hangul_syllable_type(0xB904, 0xB904, 'LV').	% Lo       HANGUL SYLLABLE RWEO
unicode_hangul_syllable_type(0xB920, 0xB920, 'LV').	% Lo       HANGUL SYLLABLE RWE
unicode_hangul_syllable_type(0xB93C, 0xB93C, 'LV').	% Lo       HANGUL SYLLABLE RWI
unicode_hangul_syllable_type(0xB958, 0xB958, 'LV').	% Lo       HANGUL SYLLABLE RYU
unicode_hangul_syllable_type(0xB974, 0xB974, 'LV').	% Lo       HANGUL SYLLABLE REU
unicode_hangul_syllable_type(0xB990, 0xB990, 'LV').	% Lo       HANGUL SYLLABLE RYI
unicode_hangul_syllable_type(0xB9AC, 0xB9AC, 'LV').	% Lo       HANGUL SYLLABLE RI
unicode_hangul_syllable_type(0xB9C8, 0xB9C8, 'LV').	% Lo       HANGUL SYLLABLE MA
unicode_hangul_syllable_type(0xB9E4, 0xB9E4, 'LV').	% Lo       HANGUL SYLLABLE MAE
unicode_hangul_syllable_type(0xBA00, 0xBA00, 'LV').	% Lo       HANGUL SYLLABLE MYA
unicode_hangul_syllable_type(0xBA1C, 0xBA1C, 'LV').	% Lo       HANGUL SYLLABLE MYAE
unicode_hangul_syllable_type(0xBA38, 0xBA38, 'LV').	% Lo       HANGUL SYLLABLE MEO
unicode_hangul_syllable_type(0xBA54, 0xBA54, 'LV').	% Lo       HANGUL SYLLABLE ME
unicode_hangul_syllable_type(0xBA70, 0xBA70, 'LV').	% Lo       HANGUL SYLLABLE MYEO
unicode_hangul_syllable_type(0xBA8C, 0xBA8C, 'LV').	% Lo       HANGUL SYLLABLE MYE
unicode_hangul_syllable_type(0xBAA8, 0xBAA8, 'LV').	% Lo       HANGUL SYLLABLE MO
unicode_hangul_syllable_type(0xBAC4, 0xBAC4, 'LV').	% Lo       HANGUL SYLLABLE MWA
unicode_hangul_syllable_type(0xBAE0, 0xBAE0, 'LV').	% Lo       HANGUL SYLLABLE MWAE
unicode_hangul_syllable_type(0xBAFC, 0xBAFC, 'LV').	% Lo       HANGUL SYLLABLE MOE
unicode_hangul_syllable_type(0xBB18, 0xBB18, 'LV').	% Lo       HANGUL SYLLABLE MYO
unicode_hangul_syllable_type(0xBB34, 0xBB34, 'LV').	% Lo       HANGUL SYLLABLE MU
unicode_hangul_syllable_type(0xBB50, 0xBB50, 'LV').	% Lo       HANGUL SYLLABLE MWEO
unicode_hangul_syllable_type(0xBB6C, 0xBB6C, 'LV').	% Lo       HANGUL SYLLABLE MWE
unicode_hangul_syllable_type(0xBB88, 0xBB88, 'LV').	% Lo       HANGUL SYLLABLE MWI
unicode_hangul_syllable_type(0xBBA4, 0xBBA4, 'LV').	% Lo       HANGUL SYLLABLE MYU
unicode_hangul_syllable_type(0xBBC0, 0xBBC0, 'LV').	% Lo       HANGUL SYLLABLE MEU
unicode_hangul_syllable_type(0xBBDC, 0xBBDC, 'LV').	% Lo       HANGUL SYLLABLE MYI
unicode_hangul_syllable_type(0xBBF8, 0xBBF8, 'LV').	% Lo       HANGUL SYLLABLE MI
unicode_hangul_syllable_type(0xBC14, 0xBC14, 'LV').	% Lo       HANGUL SYLLABLE BA
unicode_hangul_syllable_type(0xBC30, 0xBC30, 'LV').	% Lo       HANGUL SYLLABLE BAE
unicode_hangul_syllable_type(0xBC4C, 0xBC4C, 'LV').	% Lo       HANGUL SYLLABLE BYA
unicode_hangul_syllable_type(0xBC68, 0xBC68, 'LV').	% Lo       HANGUL SYLLABLE BYAE
unicode_hangul_syllable_type(0xBC84, 0xBC84, 'LV').	% Lo       HANGUL SYLLABLE BEO
unicode_hangul_syllable_type(0xBCA0, 0xBCA0, 'LV').	% Lo       HANGUL SYLLABLE BE
unicode_hangul_syllable_type(0xBCBC, 0xBCBC, 'LV').	% Lo       HANGUL SYLLABLE BYEO
unicode_hangul_syllable_type(0xBCD8, 0xBCD8, 'LV').	% Lo       HANGUL SYLLABLE BYE
unicode_hangul_syllable_type(0xBCF4, 0xBCF4, 'LV').	% Lo       HANGUL SYLLABLE BO
unicode_hangul_syllable_type(0xBD10, 0xBD10, 'LV').	% Lo       HANGUL SYLLABLE BWA
unicode_hangul_syllable_type(0xBD2C, 0xBD2C, 'LV').	% Lo       HANGUL SYLLABLE BWAE
unicode_hangul_syllable_type(0xBD48, 0xBD48, 'LV').	% Lo       HANGUL SYLLABLE BOE
unicode_hangul_syllable_type(0xBD64, 0xBD64, 'LV').	% Lo       HANGUL SYLLABLE BYO
unicode_hangul_syllable_type(0xBD80, 0xBD80, 'LV').	% Lo       HANGUL SYLLABLE BU
unicode_hangul_syllable_type(0xBD9C, 0xBD9C, 'LV').	% Lo       HANGUL SYLLABLE BWEO
unicode_hangul_syllable_type(0xBDB8, 0xBDB8, 'LV').	% Lo       HANGUL SYLLABLE BWE
unicode_hangul_syllable_type(0xBDD4, 0xBDD4, 'LV').	% Lo       HANGUL SYLLABLE BWI
unicode_hangul_syllable_type(0xBDF0, 0xBDF0, 'LV').	% Lo       HANGUL SYLLABLE BYU
unicode_hangul_syllable_type(0xBE0C, 0xBE0C, 'LV').	% Lo       HANGUL SYLLABLE BEU
unicode_hangul_syllable_type(0xBE28, 0xBE28, 'LV').	% Lo       HANGUL SYLLABLE BYI
unicode_hangul_syllable_type(0xBE44, 0xBE44, 'LV').	% Lo       HANGUL SYLLABLE BI
unicode_hangul_syllable_type(0xBE60, 0xBE60, 'LV').	% Lo       HANGUL SYLLABLE BBA
unicode_hangul_syllable_type(0xBE7C, 0xBE7C, 'LV').	% Lo       HANGUL SYLLABLE BBAE
unicode_hangul_syllable_type(0xBE98, 0xBE98, 'LV').	% Lo       HANGUL SYLLABLE BBYA
unicode_hangul_syllable_type(0xBEB4, 0xBEB4, 'LV').	% Lo       HANGUL SYLLABLE BBYAE
unicode_hangul_syllable_type(0xBED0, 0xBED0, 'LV').	% Lo       HANGUL SYLLABLE BBEO
unicode_hangul_syllable_type(0xBEEC, 0xBEEC, 'LV').	% Lo       HANGUL SYLLABLE BBE
unicode_hangul_syllable_type(0xBF08, 0xBF08, 'LV').	% Lo       HANGUL SYLLABLE BBYEO
unicode_hangul_syllable_type(0xBF24, 0xBF24, 'LV').	% Lo       HANGUL SYLLABLE BBYE
unicode_hangul_syllable_type(0xBF40, 0xBF40, 'LV').	% Lo       HANGUL SYLLABLE BBO
unicode_hangul_syllable_type(0xBF5C, 0xBF5C, 'LV').	% Lo       HANGUL SYLLABLE BBWA
unicode_hangul_syllable_type(0xBF78, 0xBF78, 'LV').	% Lo       HANGUL SYLLABLE BBWAE
unicode_hangul_syllable_type(0xBF94, 0xBF94, 'LV').	% Lo       HANGUL SYLLABLE BBOE
unicode_hangul_syllable_type(0xBFB0, 0xBFB0, 'LV').	% Lo       HANGUL SYLLABLE BBYO
unicode_hangul_syllable_type(0xBFCC, 0xBFCC, 'LV').	% Lo       HANGUL SYLLABLE BBU
unicode_hangul_syllable_type(0xBFE8, 0xBFE8, 'LV').	% Lo       HANGUL SYLLABLE BBWEO
unicode_hangul_syllable_type(0xC004, 0xC004, 'LV').	% Lo       HANGUL SYLLABLE BBWE
unicode_hangul_syllable_type(0xC020, 0xC020, 'LV').	% Lo       HANGUL SYLLABLE BBWI
unicode_hangul_syllable_type(0xC03C, 0xC03C, 'LV').	% Lo       HANGUL SYLLABLE BBYU
unicode_hangul_syllable_type(0xC058, 0xC058, 'LV').	% Lo       HANGUL SYLLABLE BBEU
unicode_hangul_syllable_type(0xC074, 0xC074, 'LV').	% Lo       HANGUL SYLLABLE BBYI
unicode_hangul_syllable_type(0xC090, 0xC090, 'LV').	% Lo       HANGUL SYLLABLE BBI
unicode_hangul_syllable_type(0xC0AC, 0xC0AC, 'LV').	% Lo       HANGUL SYLLABLE SA
unicode_hangul_syllable_type(0xC0C8, 0xC0C8, 'LV').	% Lo       HANGUL SYLLABLE SAE
unicode_hangul_syllable_type(0xC0E4, 0xC0E4, 'LV').	% Lo       HANGUL SYLLABLE SYA
unicode_hangul_syllable_type(0xC100, 0xC100, 'LV').	% Lo       HANGUL SYLLABLE SYAE
unicode_hangul_syllable_type(0xC11C, 0xC11C, 'LV').	% Lo       HANGUL SYLLABLE SEO
unicode_hangul_syllable_type(0xC138, 0xC138, 'LV').	% Lo       HANGUL SYLLABLE SE
unicode_hangul_syllable_type(0xC154, 0xC154, 'LV').	% Lo       HANGUL SYLLABLE SYEO
unicode_hangul_syllable_type(0xC170, 0xC170, 'LV').	% Lo       HANGUL SYLLABLE SYE
unicode_hangul_syllable_type(0xC18C, 0xC18C, 'LV').	% Lo       HANGUL SYLLABLE SO
unicode_hangul_syllable_type(0xC1A8, 0xC1A8, 'LV').	% Lo       HANGUL SYLLABLE SWA
unicode_hangul_syllable_type(0xC1C4, 0xC1C4, 'LV').	% Lo       HANGUL SYLLABLE SWAE
unicode_hangul_syllable_type(0xC1E0, 0xC1E0, 'LV').	% Lo       HANGUL SYLLABLE SOE
unicode_hangul_syllable_type(0xC1FC, 0xC1FC, 'LV').	% Lo       HANGUL SYLLABLE SYO
unicode_hangul_syllable_type(0xC218, 0xC218, 'LV').	% Lo       HANGUL SYLLABLE SU
unicode_hangul_syllable_type(0xC234, 0xC234, 'LV').	% Lo       HANGUL SYLLABLE SWEO
unicode_hangul_syllable_type(0xC250, 0xC250, 'LV').	% Lo       HANGUL SYLLABLE SWE
unicode_hangul_syllable_type(0xC26C, 0xC26C, 'LV').	% Lo       HANGUL SYLLABLE SWI
unicode_hangul_syllable_type(0xC288, 0xC288, 'LV').	% Lo       HANGUL SYLLABLE SYU
unicode_hangul_syllable_type(0xC2A4, 0xC2A4, 'LV').	% Lo       HANGUL SYLLABLE SEU
unicode_hangul_syllable_type(0xC2C0, 0xC2C0, 'LV').	% Lo       HANGUL SYLLABLE SYI
unicode_hangul_syllable_type(0xC2DC, 0xC2DC, 'LV').	% Lo       HANGUL SYLLABLE SI
unicode_hangul_syllable_type(0xC2F8, 0xC2F8, 'LV').	% Lo       HANGUL SYLLABLE SSA
unicode_hangul_syllable_type(0xC314, 0xC314, 'LV').	% Lo       HANGUL SYLLABLE SSAE
unicode_hangul_syllable_type(0xC330, 0xC330, 'LV').	% Lo       HANGUL SYLLABLE SSYA
unicode_hangul_syllable_type(0xC34C, 0xC34C, 'LV').	% Lo       HANGUL SYLLABLE SSYAE
unicode_hangul_syllable_type(0xC368, 0xC368, 'LV').	% Lo       HANGUL SYLLABLE SSEO
unicode_hangul_syllable_type(0xC384, 0xC384, 'LV').	% Lo       HANGUL SYLLABLE SSE
unicode_hangul_syllable_type(0xC3A0, 0xC3A0, 'LV').	% Lo       HANGUL SYLLABLE SSYEO
unicode_hangul_syllable_type(0xC3BC, 0xC3BC, 'LV').	% Lo       HANGUL SYLLABLE SSYE
unicode_hangul_syllable_type(0xC3D8, 0xC3D8, 'LV').	% Lo       HANGUL SYLLABLE SSO
unicode_hangul_syllable_type(0xC3F4, 0xC3F4, 'LV').	% Lo       HANGUL SYLLABLE SSWA
unicode_hangul_syllable_type(0xC410, 0xC410, 'LV').	% Lo       HANGUL SYLLABLE SSWAE
unicode_hangul_syllable_type(0xC42C, 0xC42C, 'LV').	% Lo       HANGUL SYLLABLE SSOE
unicode_hangul_syllable_type(0xC448, 0xC448, 'LV').	% Lo       HANGUL SYLLABLE SSYO
unicode_hangul_syllable_type(0xC464, 0xC464, 'LV').	% Lo       HANGUL SYLLABLE SSU
unicode_hangul_syllable_type(0xC480, 0xC480, 'LV').	% Lo       HANGUL SYLLABLE SSWEO
unicode_hangul_syllable_type(0xC49C, 0xC49C, 'LV').	% Lo       HANGUL SYLLABLE SSWE
unicode_hangul_syllable_type(0xC4B8, 0xC4B8, 'LV').	% Lo       HANGUL SYLLABLE SSWI
unicode_hangul_syllable_type(0xC4D4, 0xC4D4, 'LV').	% Lo       HANGUL SYLLABLE SSYU
unicode_hangul_syllable_type(0xC4F0, 0xC4F0, 'LV').	% Lo       HANGUL SYLLABLE SSEU
unicode_hangul_syllable_type(0xC50C, 0xC50C, 'LV').	% Lo       HANGUL SYLLABLE SSYI
unicode_hangul_syllable_type(0xC528, 0xC528, 'LV').	% Lo       HANGUL SYLLABLE SSI
unicode_hangul_syllable_type(0xC544, 0xC544, 'LV').	% Lo       HANGUL SYLLABLE A
unicode_hangul_syllable_type(0xC560, 0xC560, 'LV').	% Lo       HANGUL SYLLABLE AE
unicode_hangul_syllable_type(0xC57C, 0xC57C, 'LV').	% Lo       HANGUL SYLLABLE YA
unicode_hangul_syllable_type(0xC598, 0xC598, 'LV').	% Lo       HANGUL SYLLABLE YAE
unicode_hangul_syllable_type(0xC5B4, 0xC5B4, 'LV').	% Lo       HANGUL SYLLABLE EO
unicode_hangul_syllable_type(0xC5D0, 0xC5D0, 'LV').	% Lo       HANGUL SYLLABLE E
unicode_hangul_syllable_type(0xC5EC, 0xC5EC, 'LV').	% Lo       HANGUL SYLLABLE YEO
unicode_hangul_syllable_type(0xC608, 0xC608, 'LV').	% Lo       HANGUL SYLLABLE YE
unicode_hangul_syllable_type(0xC624, 0xC624, 'LV').	% Lo       HANGUL SYLLABLE O
unicode_hangul_syllable_type(0xC640, 0xC640, 'LV').	% Lo       HANGUL SYLLABLE WA
unicode_hangul_syllable_type(0xC65C, 0xC65C, 'LV').	% Lo       HANGUL SYLLABLE WAE
unicode_hangul_syllable_type(0xC678, 0xC678, 'LV').	% Lo       HANGUL SYLLABLE OE
unicode_hangul_syllable_type(0xC694, 0xC694, 'LV').	% Lo       HANGUL SYLLABLE YO
unicode_hangul_syllable_type(0xC6B0, 0xC6B0, 'LV').	% Lo       HANGUL SYLLABLE U
unicode_hangul_syllable_type(0xC6CC, 0xC6CC, 'LV').	% Lo       HANGUL SYLLABLE WEO
unicode_hangul_syllable_type(0xC6E8, 0xC6E8, 'LV').	% Lo       HANGUL SYLLABLE WE
unicode_hangul_syllable_type(0xC704, 0xC704, 'LV').	% Lo       HANGUL SYLLABLE WI
unicode_hangul_syllable_type(0xC720, 0xC720, 'LV').	% Lo       HANGUL SYLLABLE YU
unicode_hangul_syllable_type(0xC73C, 0xC73C, 'LV').	% Lo       HANGUL SYLLABLE EU
unicode_hangul_syllable_type(0xC758, 0xC758, 'LV').	% Lo       HANGUL SYLLABLE YI
unicode_hangul_syllable_type(0xC774, 0xC774, 'LV').	% Lo       HANGUL SYLLABLE I
unicode_hangul_syllable_type(0xC790, 0xC790, 'LV').	% Lo       HANGUL SYLLABLE JA
unicode_hangul_syllable_type(0xC7AC, 0xC7AC, 'LV').	% Lo       HANGUL SYLLABLE JAE
unicode_hangul_syllable_type(0xC7C8, 0xC7C8, 'LV').	% Lo       HANGUL SYLLABLE JYA
unicode_hangul_syllable_type(0xC7E4, 0xC7E4, 'LV').	% Lo       HANGUL SYLLABLE JYAE
unicode_hangul_syllable_type(0xC800, 0xC800, 'LV').	% Lo       HANGUL SYLLABLE JEO
unicode_hangul_syllable_type(0xC81C, 0xC81C, 'LV').	% Lo       HANGUL SYLLABLE JE
unicode_hangul_syllable_type(0xC838, 0xC838, 'LV').	% Lo       HANGUL SYLLABLE JYEO
unicode_hangul_syllable_type(0xC854, 0xC854, 'LV').	% Lo       HANGUL SYLLABLE JYE
unicode_hangul_syllable_type(0xC870, 0xC870, 'LV').	% Lo       HANGUL SYLLABLE JO
unicode_hangul_syllable_type(0xC88C, 0xC88C, 'LV').	% Lo       HANGUL SYLLABLE JWA
unicode_hangul_syllable_type(0xC8A8, 0xC8A8, 'LV').	% Lo       HANGUL SYLLABLE JWAE
unicode_hangul_syllable_type(0xC8C4, 0xC8C4, 'LV').	% Lo       HANGUL SYLLABLE JOE
unicode_hangul_syllable_type(0xC8E0, 0xC8E0, 'LV').	% Lo       HANGUL SYLLABLE JYO
unicode_hangul_syllable_type(0xC8FC, 0xC8FC, 'LV').	% Lo       HANGUL SYLLABLE JU
unicode_hangul_syllable_type(0xC918, 0xC918, 'LV').	% Lo       HANGUL SYLLABLE JWEO
unicode_hangul_syllable_type(0xC934, 0xC934, 'LV').	% Lo       HANGUL SYLLABLE JWE
unicode_hangul_syllable_type(0xC950, 0xC950, 'LV').	% Lo       HANGUL SYLLABLE JWI
unicode_hangul_syllable_type(0xC96C, 0xC96C, 'LV').	% Lo       HANGUL SYLLABLE JYU
unicode_hangul_syllable_type(0xC988, 0xC988, 'LV').	% Lo       HANGUL SYLLABLE JEU
unicode_hangul_syllable_type(0xC9A4, 0xC9A4, 'LV').	% Lo       HANGUL SYLLABLE JYI
unicode_hangul_syllable_type(0xC9C0, 0xC9C0, 'LV').	% Lo       HANGUL SYLLABLE JI
unicode_hangul_syllable_type(0xC9DC, 0xC9DC, 'LV').	% Lo       HANGUL SYLLABLE JJA
unicode_hangul_syllable_type(0xC9F8, 0xC9F8, 'LV').	% Lo       HANGUL SYLLABLE JJAE
unicode_hangul_syllable_type(0xCA14, 0xCA14, 'LV').	% Lo       HANGUL SYLLABLE JJYA
unicode_hangul_syllable_type(0xCA30, 0xCA30, 'LV').	% Lo       HANGUL SYLLABLE JJYAE
unicode_hangul_syllable_type(0xCA4C, 0xCA4C, 'LV').	% Lo       HANGUL SYLLABLE JJEO
unicode_hangul_syllable_type(0xCA68, 0xCA68, 'LV').	% Lo       HANGUL SYLLABLE JJE
unicode_hangul_syllable_type(0xCA84, 0xCA84, 'LV').	% Lo       HANGUL SYLLABLE JJYEO
unicode_hangul_syllable_type(0xCAA0, 0xCAA0, 'LV').	% Lo       HANGUL SYLLABLE JJYE
unicode_hangul_syllable_type(0xCABC, 0xCABC, 'LV').	% Lo       HANGUL SYLLABLE JJO
unicode_hangul_syllable_type(0xCAD8, 0xCAD8, 'LV').	% Lo       HANGUL SYLLABLE JJWA
unicode_hangul_syllable_type(0xCAF4, 0xCAF4, 'LV').	% Lo       HANGUL SYLLABLE JJWAE
unicode_hangul_syllable_type(0xCB10, 0xCB10, 'LV').	% Lo       HANGUL SYLLABLE JJOE
unicode_hangul_syllable_type(0xCB2C, 0xCB2C, 'LV').	% Lo       HANGUL SYLLABLE JJYO
unicode_hangul_syllable_type(0xCB48, 0xCB48, 'LV').	% Lo       HANGUL SYLLABLE JJU
unicode_hangul_syllable_type(0xCB64, 0xCB64, 'LV').	% Lo       HANGUL SYLLABLE JJWEO
unicode_hangul_syllable_type(0xCB80, 0xCB80, 'LV').	% Lo       HANGUL SYLLABLE JJWE
unicode_hangul_syllable_type(0xCB9C, 0xCB9C, 'LV').	% Lo       HANGUL SYLLABLE JJWI
unicode_hangul_syllable_type(0xCBB8, 0xCBB8, 'LV').	% Lo       HANGUL SYLLABLE JJYU
unicode_hangul_syllable_type(0xCBD4, 0xCBD4, 'LV').	% Lo       HANGUL SYLLABLE JJEU
unicode_hangul_syllable_type(0xCBF0, 0xCBF0, 'LV').	% Lo       HANGUL SYLLABLE JJYI
unicode_hangul_syllable_type(0xCC0C, 0xCC0C, 'LV').	% Lo       HANGUL SYLLABLE JJI
unicode_hangul_syllable_type(0xCC28, 0xCC28, 'LV').	% Lo       HANGUL SYLLABLE CA
unicode_hangul_syllable_type(0xCC44, 0xCC44, 'LV').	% Lo       HANGUL SYLLABLE CAE
unicode_hangul_syllable_type(0xCC60, 0xCC60, 'LV').	% Lo       HANGUL SYLLABLE CYA
unicode_hangul_syllable_type(0xCC7C, 0xCC7C, 'LV').	% Lo       HANGUL SYLLABLE CYAE
unicode_hangul_syllable_type(0xCC98, 0xCC98, 'LV').	% Lo       HANGUL SYLLABLE CEO
unicode_hangul_syllable_type(0xCCB4, 0xCCB4, 'LV').	% Lo       HANGUL SYLLABLE CE
unicode_hangul_syllable_type(0xCCD0, 0xCCD0, 'LV').	% Lo       HANGUL SYLLABLE CYEO
unicode_hangul_syllable_type(0xCCEC, 0xCCEC, 'LV').	% Lo       HANGUL SYLLABLE CYE
unicode_hangul_syllable_type(0xCD08, 0xCD08, 'LV').	% Lo       HANGUL SYLLABLE CO
unicode_hangul_syllable_type(0xCD24, 0xCD24, 'LV').	% Lo       HANGUL SYLLABLE CWA
unicode_hangul_syllable_type(0xCD40, 0xCD40, 'LV').	% Lo       HANGUL SYLLABLE CWAE
unicode_hangul_syllable_type(0xCD5C, 0xCD5C, 'LV').	% Lo       HANGUL SYLLABLE COE
unicode_hangul_syllable_type(0xCD78, 0xCD78, 'LV').	% Lo       HANGUL SYLLABLE CYO
unicode_hangul_syllable_type(0xCD94, 0xCD94, 'LV').	% Lo       HANGUL SYLLABLE CU
unicode_hangul_syllable_type(0xCDB0, 0xCDB0, 'LV').	% Lo       HANGUL SYLLABLE CWEO
unicode_hangul_syllable_type(0xCDCC, 0xCDCC, 'LV').	% Lo       HANGUL SYLLABLE CWE
unicode_hangul_syllable_type(0xCDE8, 0xCDE8, 'LV').	% Lo       HANGUL SYLLABLE CWI
unicode_hangul_syllable_type(0xCE04, 0xCE04, 'LV').	% Lo       HANGUL SYLLABLE CYU
unicode_hangul_syllable_type(0xCE20, 0xCE20, 'LV').	% Lo       HANGUL SYLLABLE CEU
unicode_hangul_syllable_type(0xCE3C, 0xCE3C, 'LV').	% Lo       HANGUL SYLLABLE CYI
unicode_hangul_syllable_type(0xCE58, 0xCE58, 'LV').	% Lo       HANGUL SYLLABLE CI
unicode_hangul_syllable_type(0xCE74, 0xCE74, 'LV').	% Lo       HANGUL SYLLABLE KA
unicode_hangul_syllable_type(0xCE90, 0xCE90, 'LV').	% Lo       HANGUL SYLLABLE KAE
unicode_hangul_syllable_type(0xCEAC, 0xCEAC, 'LV').	% Lo       HANGUL SYLLABLE KYA
unicode_hangul_syllable_type(0xCEC8, 0xCEC8, 'LV').	% Lo       HANGUL SYLLABLE KYAE
unicode_hangul_syllable_type(0xCEE4, 0xCEE4, 'LV').	% Lo       HANGUL SYLLABLE KEO
unicode_hangul_syllable_type(0xCF00, 0xCF00, 'LV').	% Lo       HANGUL SYLLABLE KE
unicode_hangul_syllable_type(0xCF1C, 0xCF1C, 'LV').	% Lo       HANGUL SYLLABLE KYEO
unicode_hangul_syllable_type(0xCF38, 0xCF38, 'LV').	% Lo       HANGUL SYLLABLE KYE
unicode_hangul_syllable_type(0xCF54, 0xCF54, 'LV').	% Lo       HANGUL SYLLABLE KO
unicode_hangul_syllable_type(0xCF70, 0xCF70, 'LV').	% Lo       HANGUL SYLLABLE KWA
unicode_hangul_syllable_type(0xCF8C, 0xCF8C, 'LV').	% Lo       HANGUL SYLLABLE KWAE
unicode_hangul_syllable_type(0xCFA8, 0xCFA8, 'LV').	% Lo       HANGUL SYLLABLE KOE
unicode_hangul_syllable_type(0xCFC4, 0xCFC4, 'LV').	% Lo       HANGUL SYLLABLE KYO
unicode_hangul_syllable_type(0xCFE0, 0xCFE0, 'LV').	% Lo       HANGUL SYLLABLE KU
unicode_hangul_syllable_type(0xCFFC, 0xCFFC, 'LV').	% Lo       HANGUL SYLLABLE KWEO
unicode_hangul_syllable_type(0xD018, 0xD018, 'LV').	% Lo       HANGUL SYLLABLE KWE
unicode_hangul_syllable_type(0xD034, 0xD034, 'LV').	% Lo       HANGUL SYLLABLE KWI
unicode_hangul_syllable_type(0xD050, 0xD050, 'LV').	% Lo       HANGUL SYLLABLE KYU
unicode_hangul_syllable_type(0xD06C, 0xD06C, 'LV').	% Lo       HANGUL SYLLABLE KEU
unicode_hangul_syllable_type(0xD088, 0xD088, 'LV').	% Lo       HANGUL SYLLABLE KYI
unicode_hangul_syllable_type(0xD0A4, 0xD0A4, 'LV').	% Lo       HANGUL SYLLABLE KI
unicode_hangul_syllable_type(0xD0C0, 0xD0C0, 'LV').	% Lo       HANGUL SYLLABLE TA
unicode_hangul_syllable_type(0xD0DC, 0xD0DC, 'LV').	% Lo       HANGUL SYLLABLE TAE
unicode_hangul_syllable_type(0xD0F8, 0xD0F8, 'LV').	% Lo       HANGUL SYLLABLE TYA
unicode_hangul_syllable_type(0xD114, 0xD114, 'LV').	% Lo       HANGUL SYLLABLE TYAE
unicode_hangul_syllable_type(0xD130, 0xD130, 'LV').	% Lo       HANGUL SYLLABLE TEO
unicode_hangul_syllable_type(0xD14C, 0xD14C, 'LV').	% Lo       HANGUL SYLLABLE TE
unicode_hangul_syllable_type(0xD168, 0xD168, 'LV').	% Lo       HANGUL SYLLABLE TYEO
unicode_hangul_syllable_type(0xD184, 0xD184, 'LV').	% Lo       HANGUL SYLLABLE TYE
unicode_hangul_syllable_type(0xD1A0, 0xD1A0, 'LV').	% Lo       HANGUL SYLLABLE TO
unicode_hangul_syllable_type(0xD1BC, 0xD1BC, 'LV').	% Lo       HANGUL SYLLABLE TWA
unicode_hangul_syllable_type(0xD1D8, 0xD1D8, 'LV').	% Lo       HANGUL SYLLABLE TWAE
unicode_hangul_syllable_type(0xD1F4, 0xD1F4, 'LV').	% Lo       HANGUL SYLLABLE TOE
unicode_hangul_syllable_type(0xD210, 0xD210, 'LV').	% Lo       HANGUL SYLLABLE TYO
unicode_hangul_syllable_type(0xD22C, 0xD22C, 'LV').	% Lo       HANGUL SYLLABLE TU
unicode_hangul_syllable_type(0xD248, 0xD248, 'LV').	% Lo       HANGUL SYLLABLE TWEO
unicode_hangul_syllable_type(0xD264, 0xD264, 'LV').	% Lo       HANGUL SYLLABLE TWE
unicode_hangul_syllable_type(0xD280, 0xD280, 'LV').	% Lo       HANGUL SYLLABLE TWI
unicode_hangul_syllable_type(0xD29C, 0xD29C, 'LV').	% Lo       HANGUL SYLLABLE TYU
unicode_hangul_syllable_type(0xD2B8, 0xD2B8, 'LV').	% Lo       HANGUL SYLLABLE TEU
unicode_hangul_syllable_type(0xD2D4, 0xD2D4, 'LV').	% Lo       HANGUL SYLLABLE TYI
unicode_hangul_syllable_type(0xD2F0, 0xD2F0, 'LV').	% Lo       HANGUL SYLLABLE TI
unicode_hangul_syllable_type(0xD30C, 0xD30C, 'LV').	% Lo       HANGUL SYLLABLE PA
unicode_hangul_syllable_type(0xD328, 0xD328, 'LV').	% Lo       HANGUL SYLLABLE PAE
unicode_hangul_syllable_type(0xD344, 0xD344, 'LV').	% Lo       HANGUL SYLLABLE PYA
unicode_hangul_syllable_type(0xD360, 0xD360, 'LV').	% Lo       HANGUL SYLLABLE PYAE
unicode_hangul_syllable_type(0xD37C, 0xD37C, 'LV').	% Lo       HANGUL SYLLABLE PEO
unicode_hangul_syllable_type(0xD398, 0xD398, 'LV').	% Lo       HANGUL SYLLABLE PE
unicode_hangul_syllable_type(0xD3B4, 0xD3B4, 'LV').	% Lo       HANGUL SYLLABLE PYEO
unicode_hangul_syllable_type(0xD3D0, 0xD3D0, 'LV').	% Lo       HANGUL SYLLABLE PYE
unicode_hangul_syllable_type(0xD3EC, 0xD3EC, 'LV').	% Lo       HANGUL SYLLABLE PO
unicode_hangul_syllable_type(0xD408, 0xD408, 'LV').	% Lo       HANGUL SYLLABLE PWA
unicode_hangul_syllable_type(0xD424, 0xD424, 'LV').	% Lo       HANGUL SYLLABLE PWAE
unicode_hangul_syllable_type(0xD440, 0xD440, 'LV').	% Lo       HANGUL SYLLABLE POE
unicode_hangul_syllable_type(0xD45C, 0xD45C, 'LV').	% Lo       HANGUL SYLLABLE PYO
unicode_hangul_syllable_type(0xD478, 0xD478, 'LV').	% Lo       HANGUL SYLLABLE PU
unicode_hangul_syllable_type(0xD494, 0xD494, 'LV').	% Lo       HANGUL SYLLABLE PWEO
unicode_hangul_syllable_type(0xD4B0, 0xD4B0, 'LV').	% Lo       HANGUL SYLLABLE PWE
unicode_hangul_syllable_type(0xD4CC, 0xD4CC, 'LV').	% Lo       HANGUL SYLLABLE PWI
unicode_hangul_syllable_type(0xD4E8, 0xD4E8, 'LV').	% Lo       HANGUL SYLLABLE PYU
unicode_hangul_syllable_type(0xD504, 0xD504, 'LV').	% Lo       HANGUL SYLLABLE PEU
unicode_hangul_syllable_type(0xD520, 0xD520, 'LV').	% Lo       HANGUL SYLLABLE PYI
unicode_hangul_syllable_type(0xD53C, 0xD53C, 'LV').	% Lo       HANGUL SYLLABLE PI
unicode_hangul_syllable_type(0xD558, 0xD558, 'LV').	% Lo       HANGUL SYLLABLE HA
unicode_hangul_syllable_type(0xD574, 0xD574, 'LV').	% Lo       HANGUL SYLLABLE HAE
unicode_hangul_syllable_type(0xD590, 0xD590, 'LV').	% Lo       HANGUL SYLLABLE HYA
unicode_hangul_syllable_type(0xD5AC, 0xD5AC, 'LV').	% Lo       HANGUL SYLLABLE HYAE
unicode_hangul_syllable_type(0xD5C8, 0xD5C8, 'LV').	% Lo       HANGUL SYLLABLE HEO
unicode_hangul_syllable_type(0xD5E4, 0xD5E4, 'LV').	% Lo       HANGUL SYLLABLE HE
unicode_hangul_syllable_type(0xD600, 0xD600, 'LV').	% Lo       HANGUL SYLLABLE HYEO
unicode_hangul_syllable_type(0xD61C, 0xD61C, 'LV').	% Lo       HANGUL SYLLABLE HYE
unicode_hangul_syllable_type(0xD638, 0xD638, 'LV').	% Lo       HANGUL SYLLABLE HO
unicode_hangul_syllable_type(0xD654, 0xD654, 'LV').	% Lo       HANGUL SYLLABLE HWA
unicode_hangul_syllable_type(0xD670, 0xD670, 'LV').	% Lo       HANGUL SYLLABLE HWAE
unicode_hangul_syllable_type(0xD68C, 0xD68C, 'LV').	% Lo       HANGUL SYLLABLE HOE
unicode_hangul_syllable_type(0xD6A8, 0xD6A8, 'LV').	% Lo       HANGUL SYLLABLE HYO
unicode_hangul_syllable_type(0xD6C4, 0xD6C4, 'LV').	% Lo       HANGUL SYLLABLE HU
unicode_hangul_syllable_type(0xD6E0, 0xD6E0, 'LV').	% Lo       HANGUL SYLLABLE HWEO
unicode_hangul_syllable_type(0xD6FC, 0xD6FC, 'LV').	% Lo       HANGUL SYLLABLE HWE
unicode_hangul_syllable_type(0xD718, 0xD718, 'LV').	% Lo       HANGUL SYLLABLE HWI
unicode_hangul_syllable_type(0xD734, 0xD734, 'LV').	% Lo       HANGUL SYLLABLE HYU
unicode_hangul_syllable_type(0xD750, 0xD750, 'LV').	% Lo       HANGUL SYLLABLE HEU
unicode_hangul_syllable_type(0xD76C, 0xD76C, 'LV').	% Lo       HANGUL SYLLABLE HYI
unicode_hangul_syllable_type(0xD788, 0xD788, 'LV').	% Lo       HANGUL SYLLABLE HI

% Total code points: 399

% ================================================

% Hangul_Syllable_Type=LVT_Syllable

unicode_hangul_syllable_type(0xAC01, 0xAC1B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GAG..HANGUL SYLLABLE GAH
unicode_hangul_syllable_type(0xAC1D, 0xAC37, 'LVT').	% Lo  [27] HANGUL SYLLABLE GAEG..HANGUL SYLLABLE GAEH
unicode_hangul_syllable_type(0xAC39, 0xAC53, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYAG..HANGUL SYLLABLE GYAH
unicode_hangul_syllable_type(0xAC55, 0xAC6F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYAEG..HANGUL SYLLABLE GYAEH
unicode_hangul_syllable_type(0xAC71, 0xAC8B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GEOG..HANGUL SYLLABLE GEOH
unicode_hangul_syllable_type(0xAC8D, 0xACA7, 'LVT').	% Lo  [27] HANGUL SYLLABLE GEG..HANGUL SYLLABLE GEH
unicode_hangul_syllable_type(0xACA9, 0xACC3, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYEOG..HANGUL SYLLABLE GYEOH
unicode_hangul_syllable_type(0xACC5, 0xACDF, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYEG..HANGUL SYLLABLE GYEH
unicode_hangul_syllable_type(0xACE1, 0xACFB, 'LVT').	% Lo  [27] HANGUL SYLLABLE GOG..HANGUL SYLLABLE GOH
unicode_hangul_syllable_type(0xACFD, 0xAD17, 'LVT').	% Lo  [27] HANGUL SYLLABLE GWAG..HANGUL SYLLABLE GWAH
unicode_hangul_syllable_type(0xAD19, 0xAD33, 'LVT').	% Lo  [27] HANGUL SYLLABLE GWAEG..HANGUL SYLLABLE GWAEH
unicode_hangul_syllable_type(0xAD35, 0xAD4F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GOEG..HANGUL SYLLABLE GOEH
unicode_hangul_syllable_type(0xAD51, 0xAD6B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYOG..HANGUL SYLLABLE GYOH
unicode_hangul_syllable_type(0xAD6D, 0xAD87, 'LVT').	% Lo  [27] HANGUL SYLLABLE GUG..HANGUL SYLLABLE GUH
unicode_hangul_syllable_type(0xAD89, 0xADA3, 'LVT').	% Lo  [27] HANGUL SYLLABLE GWEOG..HANGUL SYLLABLE GWEOH
unicode_hangul_syllable_type(0xADA5, 0xADBF, 'LVT').	% Lo  [27] HANGUL SYLLABLE GWEG..HANGUL SYLLABLE GWEH
unicode_hangul_syllable_type(0xADC1, 0xADDB, 'LVT').	% Lo  [27] HANGUL SYLLABLE GWIG..HANGUL SYLLABLE GWIH
unicode_hangul_syllable_type(0xADDD, 0xADF7, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYUG..HANGUL SYLLABLE GYUH
unicode_hangul_syllable_type(0xADF9, 0xAE13, 'LVT').	% Lo  [27] HANGUL SYLLABLE GEUG..HANGUL SYLLABLE GEUH
unicode_hangul_syllable_type(0xAE15, 0xAE2F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GYIG..HANGUL SYLLABLE GYIH
unicode_hangul_syllable_type(0xAE31, 0xAE4B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GIG..HANGUL SYLLABLE GIH
unicode_hangul_syllable_type(0xAE4D, 0xAE67, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGAG..HANGUL SYLLABLE GGAH
unicode_hangul_syllable_type(0xAE69, 0xAE83, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGAEG..HANGUL SYLLABLE GGAEH
unicode_hangul_syllable_type(0xAE85, 0xAE9F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYAG..HANGUL SYLLABLE GGYAH
unicode_hangul_syllable_type(0xAEA1, 0xAEBB, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYAEG..HANGUL SYLLABLE GGYAEH
unicode_hangul_syllable_type(0xAEBD, 0xAED7, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGEOG..HANGUL SYLLABLE GGEOH
unicode_hangul_syllable_type(0xAED9, 0xAEF3, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGEG..HANGUL SYLLABLE GGEH
unicode_hangul_syllable_type(0xAEF5, 0xAF0F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYEOG..HANGUL SYLLABLE GGYEOH
unicode_hangul_syllable_type(0xAF11, 0xAF2B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYEG..HANGUL SYLLABLE GGYEH
unicode_hangul_syllable_type(0xAF2D, 0xAF47, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGOG..HANGUL SYLLABLE GGOH
unicode_hangul_syllable_type(0xAF49, 0xAF63, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGWAG..HANGUL SYLLABLE GGWAH
unicode_hangul_syllable_type(0xAF65, 0xAF7F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGWAEG..HANGUL SYLLABLE GGWAEH
unicode_hangul_syllable_type(0xAF81, 0xAF9B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGOEG..HANGUL SYLLABLE GGOEH
unicode_hangul_syllable_type(0xAF9D, 0xAFB7, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYOG..HANGUL SYLLABLE GGYOH
unicode_hangul_syllable_type(0xAFB9, 0xAFD3, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGUG..HANGUL SYLLABLE GGUH
unicode_hangul_syllable_type(0xAFD5, 0xAFEF, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGWEOG..HANGUL SYLLABLE GGWEOH
unicode_hangul_syllable_type(0xAFF1, 0xB00B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGWEG..HANGUL SYLLABLE GGWEH
unicode_hangul_syllable_type(0xB00D, 0xB027, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGWIG..HANGUL SYLLABLE GGWIH
unicode_hangul_syllable_type(0xB029, 0xB043, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYUG..HANGUL SYLLABLE GGYUH
unicode_hangul_syllable_type(0xB045, 0xB05F, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGEUG..HANGUL SYLLABLE GGEUH
unicode_hangul_syllable_type(0xB061, 0xB07B, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGYIG..HANGUL SYLLABLE GGYIH
unicode_hangul_syllable_type(0xB07D, 0xB097, 'LVT').	% Lo  [27] HANGUL SYLLABLE GGIG..HANGUL SYLLABLE GGIH
unicode_hangul_syllable_type(0xB099, 0xB0B3, 'LVT').	% Lo  [27] HANGUL SYLLABLE NAG..HANGUL SYLLABLE NAH
unicode_hangul_syllable_type(0xB0B5, 0xB0CF, 'LVT').	% Lo  [27] HANGUL SYLLABLE NAEG..HANGUL SYLLABLE NAEH
unicode_hangul_syllable_type(0xB0D1, 0xB0EB, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYAG..HANGUL SYLLABLE NYAH
unicode_hangul_syllable_type(0xB0ED, 0xB107, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYAEG..HANGUL SYLLABLE NYAEH
unicode_hangul_syllable_type(0xB109, 0xB123, 'LVT').	% Lo  [27] HANGUL SYLLABLE NEOG..HANGUL SYLLABLE NEOH
unicode_hangul_syllable_type(0xB125, 0xB13F, 'LVT').	% Lo  [27] HANGUL SYLLABLE NEG..HANGUL SYLLABLE NEH
unicode_hangul_syllable_type(0xB141, 0xB15B, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYEOG..HANGUL SYLLABLE NYEOH
unicode_hangul_syllable_type(0xB15D, 0xB177, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYEG..HANGUL SYLLABLE NYEH
unicode_hangul_syllable_type(0xB179, 0xB193, 'LVT').	% Lo  [27] HANGUL SYLLABLE NOG..HANGUL SYLLABLE NOH
unicode_hangul_syllable_type(0xB195, 0xB1AF, 'LVT').	% Lo  [27] HANGUL SYLLABLE NWAG..HANGUL SYLLABLE NWAH
unicode_hangul_syllable_type(0xB1B1, 0xB1CB, 'LVT').	% Lo  [27] HANGUL SYLLABLE NWAEG..HANGUL SYLLABLE NWAEH
unicode_hangul_syllable_type(0xB1CD, 0xB1E7, 'LVT').	% Lo  [27] HANGUL SYLLABLE NOEG..HANGUL SYLLABLE NOEH
unicode_hangul_syllable_type(0xB1E9, 0xB203, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYOG..HANGUL SYLLABLE NYOH
unicode_hangul_syllable_type(0xB205, 0xB21F, 'LVT').	% Lo  [27] HANGUL SYLLABLE NUG..HANGUL SYLLABLE NUH
unicode_hangul_syllable_type(0xB221, 0xB23B, 'LVT').	% Lo  [27] HANGUL SYLLABLE NWEOG..HANGUL SYLLABLE NWEOH
unicode_hangul_syllable_type(0xB23D, 0xB257, 'LVT').	% Lo  [27] HANGUL SYLLABLE NWEG..HANGUL SYLLABLE NWEH
unicode_hangul_syllable_type(0xB259, 0xB273, 'LVT').	% Lo  [27] HANGUL SYLLABLE NWIG..HANGUL SYLLABLE NWIH
unicode_hangul_syllable_type(0xB275, 0xB28F, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYUG..HANGUL SYLLABLE NYUH
unicode_hangul_syllable_type(0xB291, 0xB2AB, 'LVT').	% Lo  [27] HANGUL SYLLABLE NEUG..HANGUL SYLLABLE NEUH
unicode_hangul_syllable_type(0xB2AD, 0xB2C7, 'LVT').	% Lo  [27] HANGUL SYLLABLE NYIG..HANGUL SYLLABLE NYIH
unicode_hangul_syllable_type(0xB2C9, 0xB2E3, 'LVT').	% Lo  [27] HANGUL SYLLABLE NIG..HANGUL SYLLABLE NIH
unicode_hangul_syllable_type(0xB2E5, 0xB2FF, 'LVT').	% Lo  [27] HANGUL SYLLABLE DAG..HANGUL SYLLABLE DAH
unicode_hangul_syllable_type(0xB301, 0xB31B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DAEG..HANGUL SYLLABLE DAEH
unicode_hangul_syllable_type(0xB31D, 0xB337, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYAG..HANGUL SYLLABLE DYAH
unicode_hangul_syllable_type(0xB339, 0xB353, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYAEG..HANGUL SYLLABLE DYAEH
unicode_hangul_syllable_type(0xB355, 0xB36F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DEOG..HANGUL SYLLABLE DEOH
unicode_hangul_syllable_type(0xB371, 0xB38B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DEG..HANGUL SYLLABLE DEH
unicode_hangul_syllable_type(0xB38D, 0xB3A7, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYEOG..HANGUL SYLLABLE DYEOH
unicode_hangul_syllable_type(0xB3A9, 0xB3C3, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYEG..HANGUL SYLLABLE DYEH
unicode_hangul_syllable_type(0xB3C5, 0xB3DF, 'LVT').	% Lo  [27] HANGUL SYLLABLE DOG..HANGUL SYLLABLE DOH
unicode_hangul_syllable_type(0xB3E1, 0xB3FB, 'LVT').	% Lo  [27] HANGUL SYLLABLE DWAG..HANGUL SYLLABLE DWAH
unicode_hangul_syllable_type(0xB3FD, 0xB417, 'LVT').	% Lo  [27] HANGUL SYLLABLE DWAEG..HANGUL SYLLABLE DWAEH
unicode_hangul_syllable_type(0xB419, 0xB433, 'LVT').	% Lo  [27] HANGUL SYLLABLE DOEG..HANGUL SYLLABLE DOEH
unicode_hangul_syllable_type(0xB435, 0xB44F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYOG..HANGUL SYLLABLE DYOH
unicode_hangul_syllable_type(0xB451, 0xB46B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DUG..HANGUL SYLLABLE DUH
unicode_hangul_syllable_type(0xB46D, 0xB487, 'LVT').	% Lo  [27] HANGUL SYLLABLE DWEOG..HANGUL SYLLABLE DWEOH
unicode_hangul_syllable_type(0xB489, 0xB4A3, 'LVT').	% Lo  [27] HANGUL SYLLABLE DWEG..HANGUL SYLLABLE DWEH
unicode_hangul_syllable_type(0xB4A5, 0xB4BF, 'LVT').	% Lo  [27] HANGUL SYLLABLE DWIG..HANGUL SYLLABLE DWIH
unicode_hangul_syllable_type(0xB4C1, 0xB4DB, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYUG..HANGUL SYLLABLE DYUH
unicode_hangul_syllable_type(0xB4DD, 0xB4F7, 'LVT').	% Lo  [27] HANGUL SYLLABLE DEUG..HANGUL SYLLABLE DEUH
unicode_hangul_syllable_type(0xB4F9, 0xB513, 'LVT').	% Lo  [27] HANGUL SYLLABLE DYIG..HANGUL SYLLABLE DYIH
unicode_hangul_syllable_type(0xB515, 0xB52F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DIG..HANGUL SYLLABLE DIH
unicode_hangul_syllable_type(0xB531, 0xB54B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDAG..HANGUL SYLLABLE DDAH
unicode_hangul_syllable_type(0xB54D, 0xB567, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDAEG..HANGUL SYLLABLE DDAEH
unicode_hangul_syllable_type(0xB569, 0xB583, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYAG..HANGUL SYLLABLE DDYAH
unicode_hangul_syllable_type(0xB585, 0xB59F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYAEG..HANGUL SYLLABLE DDYAEH
unicode_hangul_syllable_type(0xB5A1, 0xB5BB, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDEOG..HANGUL SYLLABLE DDEOH
unicode_hangul_syllable_type(0xB5BD, 0xB5D7, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDEG..HANGUL SYLLABLE DDEH
unicode_hangul_syllable_type(0xB5D9, 0xB5F3, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYEOG..HANGUL SYLLABLE DDYEOH
unicode_hangul_syllable_type(0xB5F5, 0xB60F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYEG..HANGUL SYLLABLE DDYEH
unicode_hangul_syllable_type(0xB611, 0xB62B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDOG..HANGUL SYLLABLE DDOH
unicode_hangul_syllable_type(0xB62D, 0xB647, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDWAG..HANGUL SYLLABLE DDWAH
unicode_hangul_syllable_type(0xB649, 0xB663, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDWAEG..HANGUL SYLLABLE DDWAEH
unicode_hangul_syllable_type(0xB665, 0xB67F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDOEG..HANGUL SYLLABLE DDOEH
unicode_hangul_syllable_type(0xB681, 0xB69B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYOG..HANGUL SYLLABLE DDYOH
unicode_hangul_syllable_type(0xB69D, 0xB6B7, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDUG..HANGUL SYLLABLE DDUH
unicode_hangul_syllable_type(0xB6B9, 0xB6D3, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDWEOG..HANGUL SYLLABLE DDWEOH
unicode_hangul_syllable_type(0xB6D5, 0xB6EF, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDWEG..HANGUL SYLLABLE DDWEH
unicode_hangul_syllable_type(0xB6F1, 0xB70B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDWIG..HANGUL SYLLABLE DDWIH
unicode_hangul_syllable_type(0xB70D, 0xB727, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYUG..HANGUL SYLLABLE DDYUH
unicode_hangul_syllable_type(0xB729, 0xB743, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDEUG..HANGUL SYLLABLE DDEUH
unicode_hangul_syllable_type(0xB745, 0xB75F, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDYIG..HANGUL SYLLABLE DDYIH
unicode_hangul_syllable_type(0xB761, 0xB77B, 'LVT').	% Lo  [27] HANGUL SYLLABLE DDIG..HANGUL SYLLABLE DDIH
unicode_hangul_syllable_type(0xB77D, 0xB797, 'LVT').	% Lo  [27] HANGUL SYLLABLE RAG..HANGUL SYLLABLE RAH
unicode_hangul_syllable_type(0xB799, 0xB7B3, 'LVT').	% Lo  [27] HANGUL SYLLABLE RAEG..HANGUL SYLLABLE RAEH
unicode_hangul_syllable_type(0xB7B5, 0xB7CF, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYAG..HANGUL SYLLABLE RYAH
unicode_hangul_syllable_type(0xB7D1, 0xB7EB, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYAEG..HANGUL SYLLABLE RYAEH
unicode_hangul_syllable_type(0xB7ED, 0xB807, 'LVT').	% Lo  [27] HANGUL SYLLABLE REOG..HANGUL SYLLABLE REOH
unicode_hangul_syllable_type(0xB809, 0xB823, 'LVT').	% Lo  [27] HANGUL SYLLABLE REG..HANGUL SYLLABLE REH
unicode_hangul_syllable_type(0xB825, 0xB83F, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYEOG..HANGUL SYLLABLE RYEOH
unicode_hangul_syllable_type(0xB841, 0xB85B, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYEG..HANGUL SYLLABLE RYEH
unicode_hangul_syllable_type(0xB85D, 0xB877, 'LVT').	% Lo  [27] HANGUL SYLLABLE ROG..HANGUL SYLLABLE ROH
unicode_hangul_syllable_type(0xB879, 0xB893, 'LVT').	% Lo  [27] HANGUL SYLLABLE RWAG..HANGUL SYLLABLE RWAH
unicode_hangul_syllable_type(0xB895, 0xB8AF, 'LVT').	% Lo  [27] HANGUL SYLLABLE RWAEG..HANGUL SYLLABLE RWAEH
unicode_hangul_syllable_type(0xB8B1, 0xB8CB, 'LVT').	% Lo  [27] HANGUL SYLLABLE ROEG..HANGUL SYLLABLE ROEH
unicode_hangul_syllable_type(0xB8CD, 0xB8E7, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYOG..HANGUL SYLLABLE RYOH
unicode_hangul_syllable_type(0xB8E9, 0xB903, 'LVT').	% Lo  [27] HANGUL SYLLABLE RUG..HANGUL SYLLABLE RUH
unicode_hangul_syllable_type(0xB905, 0xB91F, 'LVT').	% Lo  [27] HANGUL SYLLABLE RWEOG..HANGUL SYLLABLE RWEOH
unicode_hangul_syllable_type(0xB921, 0xB93B, 'LVT').	% Lo  [27] HANGUL SYLLABLE RWEG..HANGUL SYLLABLE RWEH
unicode_hangul_syllable_type(0xB93D, 0xB957, 'LVT').	% Lo  [27] HANGUL SYLLABLE RWIG..HANGUL SYLLABLE RWIH
unicode_hangul_syllable_type(0xB959, 0xB973, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYUG..HANGUL SYLLABLE RYUH
unicode_hangul_syllable_type(0xB975, 0xB98F, 'LVT').	% Lo  [27] HANGUL SYLLABLE REUG..HANGUL SYLLABLE REUH
unicode_hangul_syllable_type(0xB991, 0xB9AB, 'LVT').	% Lo  [27] HANGUL SYLLABLE RYIG..HANGUL SYLLABLE RYIH
unicode_hangul_syllable_type(0xB9AD, 0xB9C7, 'LVT').	% Lo  [27] HANGUL SYLLABLE RIG..HANGUL SYLLABLE RIH
unicode_hangul_syllable_type(0xB9C9, 0xB9E3, 'LVT').	% Lo  [27] HANGUL SYLLABLE MAG..HANGUL SYLLABLE MAH
unicode_hangul_syllable_type(0xB9E5, 0xB9FF, 'LVT').	% Lo  [27] HANGUL SYLLABLE MAEG..HANGUL SYLLABLE MAEH
unicode_hangul_syllable_type(0xBA01, 0xBA1B, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYAG..HANGUL SYLLABLE MYAH
unicode_hangul_syllable_type(0xBA1D, 0xBA37, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYAEG..HANGUL SYLLABLE MYAEH
unicode_hangul_syllable_type(0xBA39, 0xBA53, 'LVT').	% Lo  [27] HANGUL SYLLABLE MEOG..HANGUL SYLLABLE MEOH
unicode_hangul_syllable_type(0xBA55, 0xBA6F, 'LVT').	% Lo  [27] HANGUL SYLLABLE MEG..HANGUL SYLLABLE MEH
unicode_hangul_syllable_type(0xBA71, 0xBA8B, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYEOG..HANGUL SYLLABLE MYEOH
unicode_hangul_syllable_type(0xBA8D, 0xBAA7, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYEG..HANGUL SYLLABLE MYEH
unicode_hangul_syllable_type(0xBAA9, 0xBAC3, 'LVT').	% Lo  [27] HANGUL SYLLABLE MOG..HANGUL SYLLABLE MOH
unicode_hangul_syllable_type(0xBAC5, 0xBADF, 'LVT').	% Lo  [27] HANGUL SYLLABLE MWAG..HANGUL SYLLABLE MWAH
unicode_hangul_syllable_type(0xBAE1, 0xBAFB, 'LVT').	% Lo  [27] HANGUL SYLLABLE MWAEG..HANGUL SYLLABLE MWAEH
unicode_hangul_syllable_type(0xBAFD, 0xBB17, 'LVT').	% Lo  [27] HANGUL SYLLABLE MOEG..HANGUL SYLLABLE MOEH
unicode_hangul_syllable_type(0xBB19, 0xBB33, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYOG..HANGUL SYLLABLE MYOH
unicode_hangul_syllable_type(0xBB35, 0xBB4F, 'LVT').	% Lo  [27] HANGUL SYLLABLE MUG..HANGUL SYLLABLE MUH
unicode_hangul_syllable_type(0xBB51, 0xBB6B, 'LVT').	% Lo  [27] HANGUL SYLLABLE MWEOG..HANGUL SYLLABLE MWEOH
unicode_hangul_syllable_type(0xBB6D, 0xBB87, 'LVT').	% Lo  [27] HANGUL SYLLABLE MWEG..HANGUL SYLLABLE MWEH
unicode_hangul_syllable_type(0xBB89, 0xBBA3, 'LVT').	% Lo  [27] HANGUL SYLLABLE MWIG..HANGUL SYLLABLE MWIH
unicode_hangul_syllable_type(0xBBA5, 0xBBBF, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYUG..HANGUL SYLLABLE MYUH
unicode_hangul_syllable_type(0xBBC1, 0xBBDB, 'LVT').	% Lo  [27] HANGUL SYLLABLE MEUG..HANGUL SYLLABLE MEUH
unicode_hangul_syllable_type(0xBBDD, 0xBBF7, 'LVT').	% Lo  [27] HANGUL SYLLABLE MYIG..HANGUL SYLLABLE MYIH
unicode_hangul_syllable_type(0xBBF9, 0xBC13, 'LVT').	% Lo  [27] HANGUL SYLLABLE MIG..HANGUL SYLLABLE MIH
unicode_hangul_syllable_type(0xBC15, 0xBC2F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BAG..HANGUL SYLLABLE BAH
unicode_hangul_syllable_type(0xBC31, 0xBC4B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BAEG..HANGUL SYLLABLE BAEH
unicode_hangul_syllable_type(0xBC4D, 0xBC67, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYAG..HANGUL SYLLABLE BYAH
unicode_hangul_syllable_type(0xBC69, 0xBC83, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYAEG..HANGUL SYLLABLE BYAEH
unicode_hangul_syllable_type(0xBC85, 0xBC9F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BEOG..HANGUL SYLLABLE BEOH
unicode_hangul_syllable_type(0xBCA1, 0xBCBB, 'LVT').	% Lo  [27] HANGUL SYLLABLE BEG..HANGUL SYLLABLE BEH
unicode_hangul_syllable_type(0xBCBD, 0xBCD7, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYEOG..HANGUL SYLLABLE BYEOH
unicode_hangul_syllable_type(0xBCD9, 0xBCF3, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYEG..HANGUL SYLLABLE BYEH
unicode_hangul_syllable_type(0xBCF5, 0xBD0F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BOG..HANGUL SYLLABLE BOH
unicode_hangul_syllable_type(0xBD11, 0xBD2B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BWAG..HANGUL SYLLABLE BWAH
unicode_hangul_syllable_type(0xBD2D, 0xBD47, 'LVT').	% Lo  [27] HANGUL SYLLABLE BWAEG..HANGUL SYLLABLE BWAEH
unicode_hangul_syllable_type(0xBD49, 0xBD63, 'LVT').	% Lo  [27] HANGUL SYLLABLE BOEG..HANGUL SYLLABLE BOEH
unicode_hangul_syllable_type(0xBD65, 0xBD7F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYOG..HANGUL SYLLABLE BYOH
unicode_hangul_syllable_type(0xBD81, 0xBD9B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BUG..HANGUL SYLLABLE BUH
unicode_hangul_syllable_type(0xBD9D, 0xBDB7, 'LVT').	% Lo  [27] HANGUL SYLLABLE BWEOG..HANGUL SYLLABLE BWEOH
unicode_hangul_syllable_type(0xBDB9, 0xBDD3, 'LVT').	% Lo  [27] HANGUL SYLLABLE BWEG..HANGUL SYLLABLE BWEH
unicode_hangul_syllable_type(0xBDD5, 0xBDEF, 'LVT').	% Lo  [27] HANGUL SYLLABLE BWIG..HANGUL SYLLABLE BWIH
unicode_hangul_syllable_type(0xBDF1, 0xBE0B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYUG..HANGUL SYLLABLE BYUH
unicode_hangul_syllable_type(0xBE0D, 0xBE27, 'LVT').	% Lo  [27] HANGUL SYLLABLE BEUG..HANGUL SYLLABLE BEUH
unicode_hangul_syllable_type(0xBE29, 0xBE43, 'LVT').	% Lo  [27] HANGUL SYLLABLE BYIG..HANGUL SYLLABLE BYIH
unicode_hangul_syllable_type(0xBE45, 0xBE5F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BIG..HANGUL SYLLABLE BIH
unicode_hangul_syllable_type(0xBE61, 0xBE7B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBAG..HANGUL SYLLABLE BBAH
unicode_hangul_syllable_type(0xBE7D, 0xBE97, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBAEG..HANGUL SYLLABLE BBAEH
unicode_hangul_syllable_type(0xBE99, 0xBEB3, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYAG..HANGUL SYLLABLE BBYAH
unicode_hangul_syllable_type(0xBEB5, 0xBECF, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYAEG..HANGUL SYLLABLE BBYAEH
unicode_hangul_syllable_type(0xBED1, 0xBEEB, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBEOG..HANGUL SYLLABLE BBEOH
unicode_hangul_syllable_type(0xBEED, 0xBF07, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBEG..HANGUL SYLLABLE BBEH
unicode_hangul_syllable_type(0xBF09, 0xBF23, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYEOG..HANGUL SYLLABLE BBYEOH
unicode_hangul_syllable_type(0xBF25, 0xBF3F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYEG..HANGUL SYLLABLE BBYEH
unicode_hangul_syllable_type(0xBF41, 0xBF5B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBOG..HANGUL SYLLABLE BBOH
unicode_hangul_syllable_type(0xBF5D, 0xBF77, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBWAG..HANGUL SYLLABLE BBWAH
unicode_hangul_syllable_type(0xBF79, 0xBF93, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBWAEG..HANGUL SYLLABLE BBWAEH
unicode_hangul_syllable_type(0xBF95, 0xBFAF, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBOEG..HANGUL SYLLABLE BBOEH
unicode_hangul_syllable_type(0xBFB1, 0xBFCB, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYOG..HANGUL SYLLABLE BBYOH
unicode_hangul_syllable_type(0xBFCD, 0xBFE7, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBUG..HANGUL SYLLABLE BBUH
unicode_hangul_syllable_type(0xBFE9, 0xC003, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBWEOG..HANGUL SYLLABLE BBWEOH
unicode_hangul_syllable_type(0xC005, 0xC01F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBWEG..HANGUL SYLLABLE BBWEH
unicode_hangul_syllable_type(0xC021, 0xC03B, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBWIG..HANGUL SYLLABLE BBWIH
unicode_hangul_syllable_type(0xC03D, 0xC057, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYUG..HANGUL SYLLABLE BBYUH
unicode_hangul_syllable_type(0xC059, 0xC073, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBEUG..HANGUL SYLLABLE BBEUH
unicode_hangul_syllable_type(0xC075, 0xC08F, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBYIG..HANGUL SYLLABLE BBYIH
unicode_hangul_syllable_type(0xC091, 0xC0AB, 'LVT').	% Lo  [27] HANGUL SYLLABLE BBIG..HANGUL SYLLABLE BBIH
unicode_hangul_syllable_type(0xC0AD, 0xC0C7, 'LVT').	% Lo  [27] HANGUL SYLLABLE SAG..HANGUL SYLLABLE SAH
unicode_hangul_syllable_type(0xC0C9, 0xC0E3, 'LVT').	% Lo  [27] HANGUL SYLLABLE SAEG..HANGUL SYLLABLE SAEH
unicode_hangul_syllable_type(0xC0E5, 0xC0FF, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYAG..HANGUL SYLLABLE SYAH
unicode_hangul_syllable_type(0xC101, 0xC11B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYAEG..HANGUL SYLLABLE SYAEH
unicode_hangul_syllable_type(0xC11D, 0xC137, 'LVT').	% Lo  [27] HANGUL SYLLABLE SEOG..HANGUL SYLLABLE SEOH
unicode_hangul_syllable_type(0xC139, 0xC153, 'LVT').	% Lo  [27] HANGUL SYLLABLE SEG..HANGUL SYLLABLE SEH
unicode_hangul_syllable_type(0xC155, 0xC16F, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYEOG..HANGUL SYLLABLE SYEOH
unicode_hangul_syllable_type(0xC171, 0xC18B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYEG..HANGUL SYLLABLE SYEH
unicode_hangul_syllable_type(0xC18D, 0xC1A7, 'LVT').	% Lo  [27] HANGUL SYLLABLE SOG..HANGUL SYLLABLE SOH
unicode_hangul_syllable_type(0xC1A9, 0xC1C3, 'LVT').	% Lo  [27] HANGUL SYLLABLE SWAG..HANGUL SYLLABLE SWAH
unicode_hangul_syllable_type(0xC1C5, 0xC1DF, 'LVT').	% Lo  [27] HANGUL SYLLABLE SWAEG..HANGUL SYLLABLE SWAEH
unicode_hangul_syllable_type(0xC1E1, 0xC1FB, 'LVT').	% Lo  [27] HANGUL SYLLABLE SOEG..HANGUL SYLLABLE SOEH
unicode_hangul_syllable_type(0xC1FD, 0xC217, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYOG..HANGUL SYLLABLE SYOH
unicode_hangul_syllable_type(0xC219, 0xC233, 'LVT').	% Lo  [27] HANGUL SYLLABLE SUG..HANGUL SYLLABLE SUH
unicode_hangul_syllable_type(0xC235, 0xC24F, 'LVT').	% Lo  [27] HANGUL SYLLABLE SWEOG..HANGUL SYLLABLE SWEOH
unicode_hangul_syllable_type(0xC251, 0xC26B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SWEG..HANGUL SYLLABLE SWEH
unicode_hangul_syllable_type(0xC26D, 0xC287, 'LVT').	% Lo  [27] HANGUL SYLLABLE SWIG..HANGUL SYLLABLE SWIH
unicode_hangul_syllable_type(0xC289, 0xC2A3, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYUG..HANGUL SYLLABLE SYUH
unicode_hangul_syllable_type(0xC2A5, 0xC2BF, 'LVT').	% Lo  [27] HANGUL SYLLABLE SEUG..HANGUL SYLLABLE SEUH
unicode_hangul_syllable_type(0xC2C1, 0xC2DB, 'LVT').	% Lo  [27] HANGUL SYLLABLE SYIG..HANGUL SYLLABLE SYIH
unicode_hangul_syllable_type(0xC2DD, 0xC2F7, 'LVT').	% Lo  [27] HANGUL SYLLABLE SIG..HANGUL SYLLABLE SIH
unicode_hangul_syllable_type(0xC2F9, 0xC313, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSAG..HANGUL SYLLABLE SSAH
unicode_hangul_syllable_type(0xC315, 0xC32F, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSAEG..HANGUL SYLLABLE SSAEH
unicode_hangul_syllable_type(0xC331, 0xC34B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYAG..HANGUL SYLLABLE SSYAH
unicode_hangul_syllable_type(0xC34D, 0xC367, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYAEG..HANGUL SYLLABLE SSYAEH
unicode_hangul_syllable_type(0xC369, 0xC383, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSEOG..HANGUL SYLLABLE SSEOH
unicode_hangul_syllable_type(0xC385, 0xC39F, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSEG..HANGUL SYLLABLE SSEH
unicode_hangul_syllable_type(0xC3A1, 0xC3BB, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYEOG..HANGUL SYLLABLE SSYEOH
unicode_hangul_syllable_type(0xC3BD, 0xC3D7, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYEG..HANGUL SYLLABLE SSYEH
unicode_hangul_syllable_type(0xC3D9, 0xC3F3, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSOG..HANGUL SYLLABLE SSOH
unicode_hangul_syllable_type(0xC3F5, 0xC40F, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSWAG..HANGUL SYLLABLE SSWAH
unicode_hangul_syllable_type(0xC411, 0xC42B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSWAEG..HANGUL SYLLABLE SSWAEH
unicode_hangul_syllable_type(0xC42D, 0xC447, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSOEG..HANGUL SYLLABLE SSOEH
unicode_hangul_syllable_type(0xC449, 0xC463, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYOG..HANGUL SYLLABLE SSYOH
unicode_hangul_syllable_type(0xC465, 0xC47F, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSUG..HANGUL SYLLABLE SSUH
unicode_hangul_syllable_type(0xC481, 0xC49B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSWEOG..HANGUL SYLLABLE SSWEOH
unicode_hangul_syllable_type(0xC49D, 0xC4B7, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSWEG..HANGUL SYLLABLE SSWEH
unicode_hangul_syllable_type(0xC4B9, 0xC4D3, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSWIG..HANGUL SYLLABLE SSWIH
unicode_hangul_syllable_type(0xC4D5, 0xC4EF, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYUG..HANGUL SYLLABLE SSYUH
unicode_hangul_syllable_type(0xC4F1, 0xC50B, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSEUG..HANGUL SYLLABLE SSEUH
unicode_hangul_syllable_type(0xC50D, 0xC527, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSYIG..HANGUL SYLLABLE SSYIH
unicode_hangul_syllable_type(0xC529, 0xC543, 'LVT').	% Lo  [27] HANGUL SYLLABLE SSIG..HANGUL SYLLABLE SSIH
unicode_hangul_syllable_type(0xC545, 0xC55F, 'LVT').	% Lo  [27] HANGUL SYLLABLE AG..HANGUL SYLLABLE AH
unicode_hangul_syllable_type(0xC561, 0xC57B, 'LVT').	% Lo  [27] HANGUL SYLLABLE AEG..HANGUL SYLLABLE AEH
unicode_hangul_syllable_type(0xC57D, 0xC597, 'LVT').	% Lo  [27] HANGUL SYLLABLE YAG..HANGUL SYLLABLE YAH
unicode_hangul_syllable_type(0xC599, 0xC5B3, 'LVT').	% Lo  [27] HANGUL SYLLABLE YAEG..HANGUL SYLLABLE YAEH
unicode_hangul_syllable_type(0xC5B5, 0xC5CF, 'LVT').	% Lo  [27] HANGUL SYLLABLE EOG..HANGUL SYLLABLE EOH
unicode_hangul_syllable_type(0xC5D1, 0xC5EB, 'LVT').	% Lo  [27] HANGUL SYLLABLE EG..HANGUL SYLLABLE EH
unicode_hangul_syllable_type(0xC5ED, 0xC607, 'LVT').	% Lo  [27] HANGUL SYLLABLE YEOG..HANGUL SYLLABLE YEOH
unicode_hangul_syllable_type(0xC609, 0xC623, 'LVT').	% Lo  [27] HANGUL SYLLABLE YEG..HANGUL SYLLABLE YEH
unicode_hangul_syllable_type(0xC625, 0xC63F, 'LVT').	% Lo  [27] HANGUL SYLLABLE OG..HANGUL SYLLABLE OH
unicode_hangul_syllable_type(0xC641, 0xC65B, 'LVT').	% Lo  [27] HANGUL SYLLABLE WAG..HANGUL SYLLABLE WAH
unicode_hangul_syllable_type(0xC65D, 0xC677, 'LVT').	% Lo  [27] HANGUL SYLLABLE WAEG..HANGUL SYLLABLE WAEH
unicode_hangul_syllable_type(0xC679, 0xC693, 'LVT').	% Lo  [27] HANGUL SYLLABLE OEG..HANGUL SYLLABLE OEH
unicode_hangul_syllable_type(0xC695, 0xC6AF, 'LVT').	% Lo  [27] HANGUL SYLLABLE YOG..HANGUL SYLLABLE YOH
unicode_hangul_syllable_type(0xC6B1, 0xC6CB, 'LVT').	% Lo  [27] HANGUL SYLLABLE UG..HANGUL SYLLABLE UH
unicode_hangul_syllable_type(0xC6CD, 0xC6E7, 'LVT').	% Lo  [27] HANGUL SYLLABLE WEOG..HANGUL SYLLABLE WEOH
unicode_hangul_syllable_type(0xC6E9, 0xC703, 'LVT').	% Lo  [27] HANGUL SYLLABLE WEG..HANGUL SYLLABLE WEH
unicode_hangul_syllable_type(0xC705, 0xC71F, 'LVT').	% Lo  [27] HANGUL SYLLABLE WIG..HANGUL SYLLABLE WIH
unicode_hangul_syllable_type(0xC721, 0xC73B, 'LVT').	% Lo  [27] HANGUL SYLLABLE YUG..HANGUL SYLLABLE YUH
unicode_hangul_syllable_type(0xC73D, 0xC757, 'LVT').	% Lo  [27] HANGUL SYLLABLE EUG..HANGUL SYLLABLE EUH
unicode_hangul_syllable_type(0xC759, 0xC773, 'LVT').	% Lo  [27] HANGUL SYLLABLE YIG..HANGUL SYLLABLE YIH
unicode_hangul_syllable_type(0xC775, 0xC78F, 'LVT').	% Lo  [27] HANGUL SYLLABLE IG..HANGUL SYLLABLE IH
unicode_hangul_syllable_type(0xC791, 0xC7AB, 'LVT').	% Lo  [27] HANGUL SYLLABLE JAG..HANGUL SYLLABLE JAH
unicode_hangul_syllable_type(0xC7AD, 0xC7C7, 'LVT').	% Lo  [27] HANGUL SYLLABLE JAEG..HANGUL SYLLABLE JAEH
unicode_hangul_syllable_type(0xC7C9, 0xC7E3, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYAG..HANGUL SYLLABLE JYAH
unicode_hangul_syllable_type(0xC7E5, 0xC7FF, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYAEG..HANGUL SYLLABLE JYAEH
unicode_hangul_syllable_type(0xC801, 0xC81B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JEOG..HANGUL SYLLABLE JEOH
unicode_hangul_syllable_type(0xC81D, 0xC837, 'LVT').	% Lo  [27] HANGUL SYLLABLE JEG..HANGUL SYLLABLE JEH
unicode_hangul_syllable_type(0xC839, 0xC853, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYEOG..HANGUL SYLLABLE JYEOH
unicode_hangul_syllable_type(0xC855, 0xC86F, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYEG..HANGUL SYLLABLE JYEH
unicode_hangul_syllable_type(0xC871, 0xC88B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JOG..HANGUL SYLLABLE JOH
unicode_hangul_syllable_type(0xC88D, 0xC8A7, 'LVT').	% Lo  [27] HANGUL SYLLABLE JWAG..HANGUL SYLLABLE JWAH
unicode_hangul_syllable_type(0xC8A9, 0xC8C3, 'LVT').	% Lo  [27] HANGUL SYLLABLE JWAEG..HANGUL SYLLABLE JWAEH
unicode_hangul_syllable_type(0xC8C5, 0xC8DF, 'LVT').	% Lo  [27] HANGUL SYLLABLE JOEG..HANGUL SYLLABLE JOEH
unicode_hangul_syllable_type(0xC8E1, 0xC8FB, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYOG..HANGUL SYLLABLE JYOH
unicode_hangul_syllable_type(0xC8FD, 0xC917, 'LVT').	% Lo  [27] HANGUL SYLLABLE JUG..HANGUL SYLLABLE JUH
unicode_hangul_syllable_type(0xC919, 0xC933, 'LVT').	% Lo  [27] HANGUL SYLLABLE JWEOG..HANGUL SYLLABLE JWEOH
unicode_hangul_syllable_type(0xC935, 0xC94F, 'LVT').	% Lo  [27] HANGUL SYLLABLE JWEG..HANGUL SYLLABLE JWEH
unicode_hangul_syllable_type(0xC951, 0xC96B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JWIG..HANGUL SYLLABLE JWIH
unicode_hangul_syllable_type(0xC96D, 0xC987, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYUG..HANGUL SYLLABLE JYUH
unicode_hangul_syllable_type(0xC989, 0xC9A3, 'LVT').	% Lo  [27] HANGUL SYLLABLE JEUG..HANGUL SYLLABLE JEUH
unicode_hangul_syllable_type(0xC9A5, 0xC9BF, 'LVT').	% Lo  [27] HANGUL SYLLABLE JYIG..HANGUL SYLLABLE JYIH
unicode_hangul_syllable_type(0xC9C1, 0xC9DB, 'LVT').	% Lo  [27] HANGUL SYLLABLE JIG..HANGUL SYLLABLE JIH
unicode_hangul_syllable_type(0xC9DD, 0xC9F7, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJAG..HANGUL SYLLABLE JJAH
unicode_hangul_syllable_type(0xC9F9, 0xCA13, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJAEG..HANGUL SYLLABLE JJAEH
unicode_hangul_syllable_type(0xCA15, 0xCA2F, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYAG..HANGUL SYLLABLE JJYAH
unicode_hangul_syllable_type(0xCA31, 0xCA4B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYAEG..HANGUL SYLLABLE JJYAEH
unicode_hangul_syllable_type(0xCA4D, 0xCA67, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJEOG..HANGUL SYLLABLE JJEOH
unicode_hangul_syllable_type(0xCA69, 0xCA83, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJEG..HANGUL SYLLABLE JJEH
unicode_hangul_syllable_type(0xCA85, 0xCA9F, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYEOG..HANGUL SYLLABLE JJYEOH
unicode_hangul_syllable_type(0xCAA1, 0xCABB, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYEG..HANGUL SYLLABLE JJYEH
unicode_hangul_syllable_type(0xCABD, 0xCAD7, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJOG..HANGUL SYLLABLE JJOH
unicode_hangul_syllable_type(0xCAD9, 0xCAF3, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJWAG..HANGUL SYLLABLE JJWAH
unicode_hangul_syllable_type(0xCAF5, 0xCB0F, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJWAEG..HANGUL SYLLABLE JJWAEH
unicode_hangul_syllable_type(0xCB11, 0xCB2B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJOEG..HANGUL SYLLABLE JJOEH
unicode_hangul_syllable_type(0xCB2D, 0xCB47, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYOG..HANGUL SYLLABLE JJYOH
unicode_hangul_syllable_type(0xCB49, 0xCB63, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJUG..HANGUL SYLLABLE JJUH
unicode_hangul_syllable_type(0xCB65, 0xCB7F, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJWEOG..HANGUL SYLLABLE JJWEOH
unicode_hangul_syllable_type(0xCB81, 0xCB9B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJWEG..HANGUL SYLLABLE JJWEH
unicode_hangul_syllable_type(0xCB9D, 0xCBB7, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJWIG..HANGUL SYLLABLE JJWIH
unicode_hangul_syllable_type(0xCBB9, 0xCBD3, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYUG..HANGUL SYLLABLE JJYUH
unicode_hangul_syllable_type(0xCBD5, 0xCBEF, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJEUG..HANGUL SYLLABLE JJEUH
unicode_hangul_syllable_type(0xCBF1, 0xCC0B, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJYIG..HANGUL SYLLABLE JJYIH
unicode_hangul_syllable_type(0xCC0D, 0xCC27, 'LVT').	% Lo  [27] HANGUL SYLLABLE JJIG..HANGUL SYLLABLE JJIH
unicode_hangul_syllable_type(0xCC29, 0xCC43, 'LVT').	% Lo  [27] HANGUL SYLLABLE CAG..HANGUL SYLLABLE CAH
unicode_hangul_syllable_type(0xCC45, 0xCC5F, 'LVT').	% Lo  [27] HANGUL SYLLABLE CAEG..HANGUL SYLLABLE CAEH
unicode_hangul_syllable_type(0xCC61, 0xCC7B, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYAG..HANGUL SYLLABLE CYAH
unicode_hangul_syllable_type(0xCC7D, 0xCC97, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYAEG..HANGUL SYLLABLE CYAEH
unicode_hangul_syllable_type(0xCC99, 0xCCB3, 'LVT').	% Lo  [27] HANGUL SYLLABLE CEOG..HANGUL SYLLABLE CEOH
unicode_hangul_syllable_type(0xCCB5, 0xCCCF, 'LVT').	% Lo  [27] HANGUL SYLLABLE CEG..HANGUL SYLLABLE CEH
unicode_hangul_syllable_type(0xCCD1, 0xCCEB, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYEOG..HANGUL SYLLABLE CYEOH
unicode_hangul_syllable_type(0xCCED, 0xCD07, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYEG..HANGUL SYLLABLE CYEH
unicode_hangul_syllable_type(0xCD09, 0xCD23, 'LVT').	% Lo  [27] HANGUL SYLLABLE COG..HANGUL SYLLABLE COH
unicode_hangul_syllable_type(0xCD25, 0xCD3F, 'LVT').	% Lo  [27] HANGUL SYLLABLE CWAG..HANGUL SYLLABLE CWAH
unicode_hangul_syllable_type(0xCD41, 0xCD5B, 'LVT').	% Lo  [27] HANGUL SYLLABLE CWAEG..HANGUL SYLLABLE CWAEH
unicode_hangul_syllable_type(0xCD5D, 0xCD77, 'LVT').	% Lo  [27] HANGUL SYLLABLE COEG..HANGUL SYLLABLE COEH
unicode_hangul_syllable_type(0xCD79, 0xCD93, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYOG..HANGUL SYLLABLE CYOH
unicode_hangul_syllable_type(0xCD95, 0xCDAF, 'LVT').	% Lo  [27] HANGUL SYLLABLE CUG..HANGUL SYLLABLE CUH
unicode_hangul_syllable_type(0xCDB1, 0xCDCB, 'LVT').	% Lo  [27] HANGUL SYLLABLE CWEOG..HANGUL SYLLABLE CWEOH
unicode_hangul_syllable_type(0xCDCD, 0xCDE7, 'LVT').	% Lo  [27] HANGUL SYLLABLE CWEG..HANGUL SYLLABLE CWEH
unicode_hangul_syllable_type(0xCDE9, 0xCE03, 'LVT').	% Lo  [27] HANGUL SYLLABLE CWIG..HANGUL SYLLABLE CWIH
unicode_hangul_syllable_type(0xCE05, 0xCE1F, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYUG..HANGUL SYLLABLE CYUH
unicode_hangul_syllable_type(0xCE21, 0xCE3B, 'LVT').	% Lo  [27] HANGUL SYLLABLE CEUG..HANGUL SYLLABLE CEUH
unicode_hangul_syllable_type(0xCE3D, 0xCE57, 'LVT').	% Lo  [27] HANGUL SYLLABLE CYIG..HANGUL SYLLABLE CYIH
unicode_hangul_syllable_type(0xCE59, 0xCE73, 'LVT').	% Lo  [27] HANGUL SYLLABLE CIG..HANGUL SYLLABLE CIH
unicode_hangul_syllable_type(0xCE75, 0xCE8F, 'LVT').	% Lo  [27] HANGUL SYLLABLE KAG..HANGUL SYLLABLE KAH
unicode_hangul_syllable_type(0xCE91, 0xCEAB, 'LVT').	% Lo  [27] HANGUL SYLLABLE KAEG..HANGUL SYLLABLE KAEH
unicode_hangul_syllable_type(0xCEAD, 0xCEC7, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYAG..HANGUL SYLLABLE KYAH
unicode_hangul_syllable_type(0xCEC9, 0xCEE3, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYAEG..HANGUL SYLLABLE KYAEH
unicode_hangul_syllable_type(0xCEE5, 0xCEFF, 'LVT').	% Lo  [27] HANGUL SYLLABLE KEOG..HANGUL SYLLABLE KEOH
unicode_hangul_syllable_type(0xCF01, 0xCF1B, 'LVT').	% Lo  [27] HANGUL SYLLABLE KEG..HANGUL SYLLABLE KEH
unicode_hangul_syllable_type(0xCF1D, 0xCF37, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYEOG..HANGUL SYLLABLE KYEOH
unicode_hangul_syllable_type(0xCF39, 0xCF53, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYEG..HANGUL SYLLABLE KYEH
unicode_hangul_syllable_type(0xCF55, 0xCF6F, 'LVT').	% Lo  [27] HANGUL SYLLABLE KOG..HANGUL SYLLABLE KOH
unicode_hangul_syllable_type(0xCF71, 0xCF8B, 'LVT').	% Lo  [27] HANGUL SYLLABLE KWAG..HANGUL SYLLABLE KWAH
unicode_hangul_syllable_type(0xCF8D, 0xCFA7, 'LVT').	% Lo  [27] HANGUL SYLLABLE KWAEG..HANGUL SYLLABLE KWAEH
unicode_hangul_syllable_type(0xCFA9, 0xCFC3, 'LVT').	% Lo  [27] HANGUL SYLLABLE KOEG..HANGUL SYLLABLE KOEH
unicode_hangul_syllable_type(0xCFC5, 0xCFDF, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYOG..HANGUL SYLLABLE KYOH
unicode_hangul_syllable_type(0xCFE1, 0xCFFB, 'LVT').	% Lo  [27] HANGUL SYLLABLE KUG..HANGUL SYLLABLE KUH
unicode_hangul_syllable_type(0xCFFD, 0xD017, 'LVT').	% Lo  [27] HANGUL SYLLABLE KWEOG..HANGUL SYLLABLE KWEOH
unicode_hangul_syllable_type(0xD019, 0xD033, 'LVT').	% Lo  [27] HANGUL SYLLABLE KWEG..HANGUL SYLLABLE KWEH
unicode_hangul_syllable_type(0xD035, 0xD04F, 'LVT').	% Lo  [27] HANGUL SYLLABLE KWIG..HANGUL SYLLABLE KWIH
unicode_hangul_syllable_type(0xD051, 0xD06B, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYUG..HANGUL SYLLABLE KYUH
unicode_hangul_syllable_type(0xD06D, 0xD087, 'LVT').	% Lo  [27] HANGUL SYLLABLE KEUG..HANGUL SYLLABLE KEUH
unicode_hangul_syllable_type(0xD089, 0xD0A3, 'LVT').	% Lo  [27] HANGUL SYLLABLE KYIG..HANGUL SYLLABLE KYIH
unicode_hangul_syllable_type(0xD0A5, 0xD0BF, 'LVT').	% Lo  [27] HANGUL SYLLABLE KIG..HANGUL SYLLABLE KIH
unicode_hangul_syllable_type(0xD0C1, 0xD0DB, 'LVT').	% Lo  [27] HANGUL SYLLABLE TAG..HANGUL SYLLABLE TAH
unicode_hangul_syllable_type(0xD0DD, 0xD0F7, 'LVT').	% Lo  [27] HANGUL SYLLABLE TAEG..HANGUL SYLLABLE TAEH
unicode_hangul_syllable_type(0xD0F9, 0xD113, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYAG..HANGUL SYLLABLE TYAH
unicode_hangul_syllable_type(0xD115, 0xD12F, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYAEG..HANGUL SYLLABLE TYAEH
unicode_hangul_syllable_type(0xD131, 0xD14B, 'LVT').	% Lo  [27] HANGUL SYLLABLE TEOG..HANGUL SYLLABLE TEOH
unicode_hangul_syllable_type(0xD14D, 0xD167, 'LVT').	% Lo  [27] HANGUL SYLLABLE TEG..HANGUL SYLLABLE TEH
unicode_hangul_syllable_type(0xD169, 0xD183, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYEOG..HANGUL SYLLABLE TYEOH
unicode_hangul_syllable_type(0xD185, 0xD19F, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYEG..HANGUL SYLLABLE TYEH
unicode_hangul_syllable_type(0xD1A1, 0xD1BB, 'LVT').	% Lo  [27] HANGUL SYLLABLE TOG..HANGUL SYLLABLE TOH
unicode_hangul_syllable_type(0xD1BD, 0xD1D7, 'LVT').	% Lo  [27] HANGUL SYLLABLE TWAG..HANGUL SYLLABLE TWAH
unicode_hangul_syllable_type(0xD1D9, 0xD1F3, 'LVT').	% Lo  [27] HANGUL SYLLABLE TWAEG..HANGUL SYLLABLE TWAEH
unicode_hangul_syllable_type(0xD1F5, 0xD20F, 'LVT').	% Lo  [27] HANGUL SYLLABLE TOEG..HANGUL SYLLABLE TOEH
unicode_hangul_syllable_type(0xD211, 0xD22B, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYOG..HANGUL SYLLABLE TYOH
unicode_hangul_syllable_type(0xD22D, 0xD247, 'LVT').	% Lo  [27] HANGUL SYLLABLE TUG..HANGUL SYLLABLE TUH
unicode_hangul_syllable_type(0xD249, 0xD263, 'LVT').	% Lo  [27] HANGUL SYLLABLE TWEOG..HANGUL SYLLABLE TWEOH
unicode_hangul_syllable_type(0xD265, 0xD27F, 'LVT').	% Lo  [27] HANGUL SYLLABLE TWEG..HANGUL SYLLABLE TWEH
unicode_hangul_syllable_type(0xD281, 0xD29B, 'LVT').	% Lo  [27] HANGUL SYLLABLE TWIG..HANGUL SYLLABLE TWIH
unicode_hangul_syllable_type(0xD29D, 0xD2B7, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYUG..HANGUL SYLLABLE TYUH
unicode_hangul_syllable_type(0xD2B9, 0xD2D3, 'LVT').	% Lo  [27] HANGUL SYLLABLE TEUG..HANGUL SYLLABLE TEUH
unicode_hangul_syllable_type(0xD2D5, 0xD2EF, 'LVT').	% Lo  [27] HANGUL SYLLABLE TYIG..HANGUL SYLLABLE TYIH
unicode_hangul_syllable_type(0xD2F1, 0xD30B, 'LVT').	% Lo  [27] HANGUL SYLLABLE TIG..HANGUL SYLLABLE TIH
unicode_hangul_syllable_type(0xD30D, 0xD327, 'LVT').	% Lo  [27] HANGUL SYLLABLE PAG..HANGUL SYLLABLE PAH
unicode_hangul_syllable_type(0xD329, 0xD343, 'LVT').	% Lo  [27] HANGUL SYLLABLE PAEG..HANGUL SYLLABLE PAEH
unicode_hangul_syllable_type(0xD345, 0xD35F, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYAG..HANGUL SYLLABLE PYAH
unicode_hangul_syllable_type(0xD361, 0xD37B, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYAEG..HANGUL SYLLABLE PYAEH
unicode_hangul_syllable_type(0xD37D, 0xD397, 'LVT').	% Lo  [27] HANGUL SYLLABLE PEOG..HANGUL SYLLABLE PEOH
unicode_hangul_syllable_type(0xD399, 0xD3B3, 'LVT').	% Lo  [27] HANGUL SYLLABLE PEG..HANGUL SYLLABLE PEH
unicode_hangul_syllable_type(0xD3B5, 0xD3CF, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYEOG..HANGUL SYLLABLE PYEOH
unicode_hangul_syllable_type(0xD3D1, 0xD3EB, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYEG..HANGUL SYLLABLE PYEH
unicode_hangul_syllable_type(0xD3ED, 0xD407, 'LVT').	% Lo  [27] HANGUL SYLLABLE POG..HANGUL SYLLABLE POH
unicode_hangul_syllable_type(0xD409, 0xD423, 'LVT').	% Lo  [27] HANGUL SYLLABLE PWAG..HANGUL SYLLABLE PWAH
unicode_hangul_syllable_type(0xD425, 0xD43F, 'LVT').	% Lo  [27] HANGUL SYLLABLE PWAEG..HANGUL SYLLABLE PWAEH
unicode_hangul_syllable_type(0xD441, 0xD45B, 'LVT').	% Lo  [27] HANGUL SYLLABLE POEG..HANGUL SYLLABLE POEH
unicode_hangul_syllable_type(0xD45D, 0xD477, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYOG..HANGUL SYLLABLE PYOH
unicode_hangul_syllable_type(0xD479, 0xD493, 'LVT').	% Lo  [27] HANGUL SYLLABLE PUG..HANGUL SYLLABLE PUH
unicode_hangul_syllable_type(0xD495, 0xD4AF, 'LVT').	% Lo  [27] HANGUL SYLLABLE PWEOG..HANGUL SYLLABLE PWEOH
unicode_hangul_syllable_type(0xD4B1, 0xD4CB, 'LVT').	% Lo  [27] HANGUL SYLLABLE PWEG..HANGUL SYLLABLE PWEH
unicode_hangul_syllable_type(0xD4CD, 0xD4E7, 'LVT').	% Lo  [27] HANGUL SYLLABLE PWIG..HANGUL SYLLABLE PWIH
unicode_hangul_syllable_type(0xD4E9, 0xD503, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYUG..HANGUL SYLLABLE PYUH
unicode_hangul_syllable_type(0xD505, 0xD51F, 'LVT').	% Lo  [27] HANGUL SYLLABLE PEUG..HANGUL SYLLABLE PEUH
unicode_hangul_syllable_type(0xD521, 0xD53B, 'LVT').	% Lo  [27] HANGUL SYLLABLE PYIG..HANGUL SYLLABLE PYIH
unicode_hangul_syllable_type(0xD53D, 0xD557, 'LVT').	% Lo  [27] HANGUL SYLLABLE PIG..HANGUL SYLLABLE PIH
unicode_hangul_syllable_type(0xD559, 0xD573, 'LVT').	% Lo  [27] HANGUL SYLLABLE HAG..HANGUL SYLLABLE HAH
unicode_hangul_syllable_type(0xD575, 0xD58F, 'LVT').	% Lo  [27] HANGUL SYLLABLE HAEG..HANGUL SYLLABLE HAEH
unicode_hangul_syllable_type(0xD591, 0xD5AB, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYAG..HANGUL SYLLABLE HYAH
unicode_hangul_syllable_type(0xD5AD, 0xD5C7, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYAEG..HANGUL SYLLABLE HYAEH
unicode_hangul_syllable_type(0xD5C9, 0xD5E3, 'LVT').	% Lo  [27] HANGUL SYLLABLE HEOG..HANGUL SYLLABLE HEOH
unicode_hangul_syllable_type(0xD5E5, 0xD5FF, 'LVT').	% Lo  [27] HANGUL SYLLABLE HEG..HANGUL SYLLABLE HEH
unicode_hangul_syllable_type(0xD601, 0xD61B, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYEOG..HANGUL SYLLABLE HYEOH
unicode_hangul_syllable_type(0xD61D, 0xD637, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYEG..HANGUL SYLLABLE HYEH
unicode_hangul_syllable_type(0xD639, 0xD653, 'LVT').	% Lo  [27] HANGUL SYLLABLE HOG..HANGUL SYLLABLE HOH
unicode_hangul_syllable_type(0xD655, 0xD66F, 'LVT').	% Lo  [27] HANGUL SYLLABLE HWAG..HANGUL SYLLABLE HWAH
unicode_hangul_syllable_type(0xD671, 0xD68B, 'LVT').	% Lo  [27] HANGUL SYLLABLE HWAEG..HANGUL SYLLABLE HWAEH
unicode_hangul_syllable_type(0xD68D, 0xD6A7, 'LVT').	% Lo  [27] HANGUL SYLLABLE HOEG..HANGUL SYLLABLE HOEH
unicode_hangul_syllable_type(0xD6A9, 0xD6C3, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYOG..HANGUL SYLLABLE HYOH
unicode_hangul_syllable_type(0xD6C5, 0xD6DF, 'LVT').	% Lo  [27] HANGUL SYLLABLE HUG..HANGUL SYLLABLE HUH
unicode_hangul_syllable_type(0xD6E1, 0xD6FB, 'LVT').	% Lo  [27] HANGUL SYLLABLE HWEOG..HANGUL SYLLABLE HWEOH
unicode_hangul_syllable_type(0xD6FD, 0xD717, 'LVT').	% Lo  [27] HANGUL SYLLABLE HWEG..HANGUL SYLLABLE HWEH
unicode_hangul_syllable_type(0xD719, 0xD733, 'LVT').	% Lo  [27] HANGUL SYLLABLE HWIG..HANGUL SYLLABLE HWIH
unicode_hangul_syllable_type(0xD735, 0xD74F, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYUG..HANGUL SYLLABLE HYUH
unicode_hangul_syllable_type(0xD751, 0xD76B, 'LVT').	% Lo  [27] HANGUL SYLLABLE HEUG..HANGUL SYLLABLE HEUH
unicode_hangul_syllable_type(0xD76D, 0xD787, 'LVT').	% Lo  [27] HANGUL SYLLABLE HYIG..HANGUL SYLLABLE HYIH
unicode_hangul_syllable_type(0xD789, 0xD7A3, 'LVT').	% Lo  [27] HANGUL SYLLABLE HIG..HANGUL SYLLABLE HIH

% Total code points: 10773

% EOF
