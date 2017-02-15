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

unicode_deprecated(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_deprecated(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_deprecated(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_deprecated(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_deprecated(0x0149, 0x0149).	% Deprecated # L&       LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
unicode_deprecated(0x0673, 0x0673).	% Deprecated # Lo       ARABIC LETTER ALEF WITH WAVY HAMZA BELOW
unicode_deprecated(0x0F77, 0x0F77).	% Deprecated # Mn       TIBETAN VOWEL SIGN VOCALIC RR
unicode_deprecated(0x0F79, 0x0F79).	% Deprecated # Mn       TIBETAN VOWEL SIGN VOCALIC LL
unicode_deprecated(0x17A3, 0x17A4).	% Deprecated # Lo   [2] KHMER INDEPENDENT VOWEL QAQ..KHMER INDEPENDENT VOWEL QAA
unicode_deprecated(0x206A, 0x206F).	% Deprecated # Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_deprecated(0x2329, 0x2329).	% Deprecated # Ps       LEFT-POINTING ANGLE BRACKET
unicode_deprecated(0x232A, 0x232A).	% Deprecated # Pe       RIGHT-POINTING ANGLE BRACKET
unicode_deprecated(0xE0001, 0xE0001).	% Deprecated # Cf       LANGUAGE TAG
unicode_deprecated(0xE0020, 0xE007F).	% Deprecated # Cf  [96] TAG SPACE..CANCEL TAG

% Total code points: 111
