%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 15, 2012
%
%  Original Unicode file header comments follow

/*
# PropList-6.1.0.txt
# Date: 2011-11-30, 01:49:54 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
*/

unicode_unified_ideograph(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_unified_ideograph(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_unified_ideograph(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_unified_ideograph(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_unified_ideograph(0x3400, 0x4DB5).	% Unified_Ideograph # Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_unified_ideograph(0x4E00, 0x9FCC).	% Unified_Ideograph # Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_unified_ideograph(0xFA0E, 0xFA0F).	% Unified_Ideograph # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA0E..CJK COMPATIBILITY IDEOGRAPH-FA0F
unicode_unified_ideograph(0xFA11, 0xFA11).	% Unified_Ideograph # Lo       CJK COMPATIBILITY IDEOGRAPH-FA11
unicode_unified_ideograph(0xFA13, 0xFA14).	% Unified_Ideograph # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA13..CJK COMPATIBILITY IDEOGRAPH-FA14
unicode_unified_ideograph(0xFA1F, 0xFA1F).	% Unified_Ideograph # Lo       CJK COMPATIBILITY IDEOGRAPH-FA1F
unicode_unified_ideograph(0xFA21, 0xFA21).	% Unified_Ideograph # Lo       CJK COMPATIBILITY IDEOGRAPH-FA21
unicode_unified_ideograph(0xFA23, 0xFA24).	% Unified_Ideograph # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA23..CJK COMPATIBILITY IDEOGRAPH-FA24
unicode_unified_ideograph(0xFA27, 0xFA29).	% Unified_Ideograph # Lo   [3] CJK COMPATIBILITY IDEOGRAPH-FA27..CJK COMPATIBILITY IDEOGRAPH-FA29
unicode_unified_ideograph(0x20000, 0x2A6D6).	% Unified_Ideograph # Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_unified_ideograph(0x2A700, 0x2B734).	% Unified_Ideograph # Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_unified_ideograph(0x2B740, 0x2B81D).	% Unified_Ideograph # Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D

% Total code points: 74617
