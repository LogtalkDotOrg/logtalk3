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

unicode_soft_dotted(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_soft_dotted(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_soft_dotted(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_soft_dotted(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_soft_dotted(0x0069, 0x006A).	% Soft_Dotted # L&   [2] LATIN SMALL LETTER I..LATIN SMALL LETTER J
unicode_soft_dotted(0x012F, 0x012F).	% Soft_Dotted # L&       LATIN SMALL LETTER I WITH OGONEK
unicode_soft_dotted(0x0249, 0x0249).	% Soft_Dotted # L&       LATIN SMALL LETTER J WITH STROKE
unicode_soft_dotted(0x0268, 0x0268).	% Soft_Dotted # L&       LATIN SMALL LETTER I WITH STROKE
unicode_soft_dotted(0x029D, 0x029D).	% Soft_Dotted # L&       LATIN SMALL LETTER J WITH CROSSED-TAIL
unicode_soft_dotted(0x02B2, 0x02B2).	% Soft_Dotted # Lm       MODIFIER LETTER SMALL J
unicode_soft_dotted(0x03F3, 0x03F3).	% Soft_Dotted # L&       GREEK LETTER YOT
unicode_soft_dotted(0x0456, 0x0456).	% Soft_Dotted # L&       CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
unicode_soft_dotted(0x0458, 0x0458).	% Soft_Dotted # L&       CYRILLIC SMALL LETTER JE
unicode_soft_dotted(0x1D62, 0x1D62).	% Soft_Dotted # Lm       LATIN SUBSCRIPT SMALL LETTER I
unicode_soft_dotted(0x1D96, 0x1D96).	% Soft_Dotted # L&       LATIN SMALL LETTER I WITH RETROFLEX HOOK
unicode_soft_dotted(0x1DA4, 0x1DA4).	% Soft_Dotted # Lm       MODIFIER LETTER SMALL I WITH STROKE
unicode_soft_dotted(0x1DA8, 0x1DA8).	% Soft_Dotted # Lm       MODIFIER LETTER SMALL J WITH CROSSED-TAIL
unicode_soft_dotted(0x1E2D, 0x1E2D).	% Soft_Dotted # L&       LATIN SMALL LETTER I WITH TILDE BELOW
unicode_soft_dotted(0x1ECB, 0x1ECB).	% Soft_Dotted # L&       LATIN SMALL LETTER I WITH DOT BELOW
unicode_soft_dotted(0x2071, 0x2071).	% Soft_Dotted # Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_soft_dotted(0x2148, 0x2149).	% Soft_Dotted # L&   [2] DOUBLE-STRUCK ITALIC SMALL I..DOUBLE-STRUCK ITALIC SMALL J
unicode_soft_dotted(0x2C7C, 0x2C7C).	% Soft_Dotted # Lm       LATIN SUBSCRIPT SMALL LETTER J
unicode_soft_dotted(0x1D422, 0x1D423).	% Soft_Dotted # L&   [2] MATHEMATICAL BOLD SMALL I..MATHEMATICAL BOLD SMALL J
unicode_soft_dotted(0x1D456, 0x1D457).	% Soft_Dotted # L&   [2] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL ITALIC SMALL J
unicode_soft_dotted(0x1D48A, 0x1D48B).	% Soft_Dotted # L&   [2] MATHEMATICAL BOLD ITALIC SMALL I..MATHEMATICAL BOLD ITALIC SMALL J
unicode_soft_dotted(0x1D4BE, 0x1D4BF).	% Soft_Dotted # L&   [2] MATHEMATICAL SCRIPT SMALL I..MATHEMATICAL SCRIPT SMALL J
unicode_soft_dotted(0x1D4F2, 0x1D4F3).	% Soft_Dotted # L&   [2] MATHEMATICAL BOLD SCRIPT SMALL I..MATHEMATICAL BOLD SCRIPT SMALL J
unicode_soft_dotted(0x1D526, 0x1D527).	% Soft_Dotted # L&   [2] MATHEMATICAL FRAKTUR SMALL I..MATHEMATICAL FRAKTUR SMALL J
unicode_soft_dotted(0x1D55A, 0x1D55B).	% Soft_Dotted # L&   [2] MATHEMATICAL DOUBLE-STRUCK SMALL I..MATHEMATICAL DOUBLE-STRUCK SMALL J
unicode_soft_dotted(0x1D58E, 0x1D58F).	% Soft_Dotted # L&   [2] MATHEMATICAL BOLD FRAKTUR SMALL I..MATHEMATICAL BOLD FRAKTUR SMALL J
unicode_soft_dotted(0x1D5C2, 0x1D5C3).	% Soft_Dotted # L&   [2] MATHEMATICAL SANS-SERIF SMALL I..MATHEMATICAL SANS-SERIF SMALL J
unicode_soft_dotted(0x1D5F6, 0x1D5F7).	% Soft_Dotted # L&   [2] MATHEMATICAL SANS-SERIF BOLD SMALL I..MATHEMATICAL SANS-SERIF BOLD SMALL J
unicode_soft_dotted(0x1D62A, 0x1D62B).	% Soft_Dotted # L&   [2] MATHEMATICAL SANS-SERIF ITALIC SMALL I..MATHEMATICAL SANS-SERIF ITALIC SMALL J
unicode_soft_dotted(0x1D65E, 0x1D65F).	% Soft_Dotted # L&   [2] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J
unicode_soft_dotted(0x1D692, 0x1D693).	% Soft_Dotted # L&   [2] MATHEMATICAL MONOSPACE SMALL I..MATHEMATICAL MONOSPACE SMALL J

% Total code points: 46
