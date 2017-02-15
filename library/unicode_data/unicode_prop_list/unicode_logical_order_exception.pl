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

unicode_logical_order_exception(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_logical_order_exception(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_logical_order_exception(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_logical_order_exception(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_logical_order_exception(0x0E40, 0x0E44).	% Logical_Order_Exception # Lo   [5] THAI CHARACTER SARA E..THAI CHARACTER SARA AI MAIMALAI
unicode_logical_order_exception(0x0EC0, 0x0EC4).	% Logical_Order_Exception # Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_logical_order_exception(0xAAB5, 0xAAB6).	% Logical_Order_Exception # Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_logical_order_exception(0xAAB9, 0xAAB9).	% Logical_Order_Exception # Lo       TAI VIET VOWEL UEA
unicode_logical_order_exception(0xAABB, 0xAABC).	% Logical_Order_Exception # Lo   [2] TAI VIET VOWEL AUE..TAI VIET VOWEL AY

% Total code points: 15
