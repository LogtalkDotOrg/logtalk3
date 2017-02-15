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

unicode_noncharacter_code_point(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_noncharacter_code_point(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_noncharacter_code_point(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_noncharacter_code_point(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_noncharacter_code_point(0xFDD0, 0xFDEF).		% Noncharacter_Code_Point # Cn  [32] <noncharacter-FDD0>..<noncharacter-FDEF>
unicode_noncharacter_code_point(0xFFFE, 0xFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-FFFE>..<noncharacter-FFFF>
unicode_noncharacter_code_point(0x1FFFE, 0x1FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-1FFFE>..<noncharacter-1FFFF>
unicode_noncharacter_code_point(0x2FFFE, 0x2FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-2FFFE>..<noncharacter-2FFFF>
unicode_noncharacter_code_point(0x3FFFE, 0x3FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-3FFFE>..<noncharacter-3FFFF>
unicode_noncharacter_code_point(0x4FFFE, 0x4FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-4FFFE>..<noncharacter-4FFFF>
unicode_noncharacter_code_point(0x5FFFE, 0x5FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-5FFFE>..<noncharacter-5FFFF>
unicode_noncharacter_code_point(0x6FFFE, 0x6FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-6FFFE>..<noncharacter-6FFFF>
unicode_noncharacter_code_point(0x7FFFE, 0x7FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-7FFFE>..<noncharacter-7FFFF>
unicode_noncharacter_code_point(0x8FFFE, 0x8FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-8FFFE>..<noncharacter-8FFFF>
unicode_noncharacter_code_point(0x9FFFE, 0x9FFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-9FFFE>..<noncharacter-9FFFF>
unicode_noncharacter_code_point(0xAFFFE, 0xAFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-AFFFE>..<noncharacter-AFFFF>
unicode_noncharacter_code_point(0xBFFFE, 0xBFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-BFFFE>..<noncharacter-BFFFF>
unicode_noncharacter_code_point(0xCFFFE, 0xCFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-CFFFE>..<noncharacter-CFFFF>
unicode_noncharacter_code_point(0xDFFFE, 0xDFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-DFFFE>..<noncharacter-DFFFF>
unicode_noncharacter_code_point(0xEFFFE, 0xEFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-EFFFE>..<noncharacter-EFFFF>
unicode_noncharacter_code_point(0xFFFFE, 0xFFFFF).		% Noncharacter_Code_Point # Cn   [2] <noncharacter-FFFFE>..<noncharacter-FFFFF>
unicode_noncharacter_code_point(0x10FFFE, 0x10FFFF).	% Noncharacter_Code_Point # Cn   [2] <noncharacter-10FFFE>..<noncharacter-10FFFF>

% Total code points: 66
