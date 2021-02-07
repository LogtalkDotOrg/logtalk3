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

unicode_other_uppercase(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_other_uppercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_other_uppercase(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_other_uppercase(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_other_uppercase(0x2160, 0x216F).	% Other_Uppercase # Nl  [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND
unicode_other_uppercase(0x24B6, 0x24CF).	% Other_Uppercase # So  [26] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN CAPITAL LETTER Z

% Total code points: 42
