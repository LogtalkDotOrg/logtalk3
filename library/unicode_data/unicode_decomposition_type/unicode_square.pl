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
# DerivedDecompositionType-6.1.0.txt
# Date: 2011-07-25, 00:54:13 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# Decomposition_Type (from UnicodeData.txt, field 5: see UAX #44: http://www.unicode.org/reports/tr44/)

#  All code points not explicitly listed for Decomposition_Type
#  have the value None.

# @missing: 0000..10FFFF; None
*/

unicode_square(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_square(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_square(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_square(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_square(0x3250, 0x3250).		% Square So       PARTNERSHIP SIGN
unicode_square(0x32CC, 0x32CF).		% Square So   [4] SQUARE HG..LIMITED LIABILITY SIGN
unicode_square(0x3300, 0x3357).		% Square So  [88] SQUARE APAATO..SQUARE WATTO
unicode_square(0x3371, 0x33DF).		% Square So [111] SQUARE HPA..SQUARE A OVER M
unicode_square(0x33FF, 0x33FF).		% Square So       SQUARE GAL
unicode_square(0x1F130, 0x1F14F).	% Square So  [32] SQUARED LATIN CAPITAL LETTER A..SQUARED WC
unicode_square(0x1F190, 0x1F190).	% Square So       SQUARE DJ
unicode_square(0x1F200, 0x1F202).	% Square So   [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
unicode_square(0x1F210, 0x1F23A).	% Square So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6

% Total code points: 284
