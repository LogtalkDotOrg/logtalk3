%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: September 30, 2012
%
%  Original Unicode file header comments follow

/*
# DerivedCoreProperties-6.1.0.txt
# Date: 2011-12-11, 18:26:55 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

% ================================================
*/

unicode_grapheme_link(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_grapheme_link(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_grapheme_link(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_grapheme_link(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

% Derived Property: Grapheme_Link (deprecated)
%  Generated from: Canonical_Combining_Class=Virama
%  Use Canonical_Combining_Class=Virama directly instead

unicode_grapheme_link(0x094D, 0x094D).		% Grapheme_Link Mn       DEVANAGARI SIGN VIRAMA
unicode_grapheme_link(0x09CD, 0x09CD).		% Grapheme_Link Mn       BENGALI SIGN VIRAMA
unicode_grapheme_link(0x0A4D, 0x0A4D).		% Grapheme_Link Mn       GURMUKHI SIGN VIRAMA
unicode_grapheme_link(0x0ACD, 0x0ACD).		% Grapheme_Link Mn       GUJARATI SIGN VIRAMA
unicode_grapheme_link(0x0B4D, 0x0B4D).		% Grapheme_Link Mn       ORIYA SIGN VIRAMA
unicode_grapheme_link(0x0BCD, 0x0BCD).		% Grapheme_Link Mn       TAMIL SIGN VIRAMA
unicode_grapheme_link(0x0C4D, 0x0C4D).		% Grapheme_Link Mn       TELUGU SIGN VIRAMA
unicode_grapheme_link(0x0CCD, 0x0CCD).		% Grapheme_Link Mn       KANNADA SIGN VIRAMA
unicode_grapheme_link(0x0D4D, 0x0D4D).		% Grapheme_Link Mn       MALAYALAM SIGN VIRAMA
unicode_grapheme_link(0x0DCA, 0x0DCA).		% Grapheme_Link Mn       SINHALA SIGN AL-LAKUNA
unicode_grapheme_link(0x0E3A, 0x0E3A).		% Grapheme_Link Mn       THAI CHARACTER PHINTHU
unicode_grapheme_link(0x0F84, 0x0F84).		% Grapheme_Link Mn       TIBETAN MARK HALANTA
unicode_grapheme_link(0x1039, 0x103A).		% Grapheme_Link Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_grapheme_link(0x1714, 0x1714).		% Grapheme_Link Mn       TAGALOG SIGN VIRAMA
unicode_grapheme_link(0x1734, 0x1734).		% Grapheme_Link Mn       HANUNOO SIGN PAMUDPOD
unicode_grapheme_link(0x17D2, 0x17D2).		% Grapheme_Link Mn       KHMER SIGN COENG
unicode_grapheme_link(0x1A60, 0x1A60).		% Grapheme_Link Mn       TAI THAM SIGN SAKOT
unicode_grapheme_link(0x1B44, 0x1B44).		% Grapheme_Link Mc       BALINESE ADEG ADEG
unicode_grapheme_link(0x1BAA, 0x1BAA).		% Grapheme_Link Mc       SUNDANESE SIGN PAMAAEH
unicode_grapheme_link(0x1BAB, 0x1BAB).		% Grapheme_Link Mn       SUNDANESE SIGN VIRAMA
unicode_grapheme_link(0x1BF2, 0x1BF3).		% Grapheme_Link Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_grapheme_link(0x2D7F, 0x2D7F).		% Grapheme_Link Mn       TIFINAGH CONSONANT JOINER
unicode_grapheme_link(0xA806, 0xA806).		% Grapheme_Link Mn       SYLOTI NAGRI SIGN HASANTA
unicode_grapheme_link(0xA8C4, 0xA8C4).		% Grapheme_Link Mn       SAURASHTRA SIGN VIRAMA
unicode_grapheme_link(0xA953, 0xA953).		% Grapheme_Link Mc       REJANG VIRAMA
unicode_grapheme_link(0xA9C0, 0xA9C0).		% Grapheme_Link Mc       JAVANESE PANGKON
unicode_grapheme_link(0xAAF6, 0xAAF6).		% Grapheme_Link Mn       MEETEI MAYEK VIRAMA
unicode_grapheme_link(0xABED, 0xABED).		% Grapheme_Link Mn       MEETEI MAYEK APUN IYEK
unicode_grapheme_link(0x10A3F, 0x10A3F).	% Grapheme_Link Mn       KHAROSHTHI VIRAMA
unicode_grapheme_link(0x11046, 0x11046).	% Grapheme_Link Mn       BRAHMI VIRAMA
unicode_grapheme_link(0x110B9, 0x110B9).	% Grapheme_Link Mn       KAITHI SIGN VIRAMA
unicode_grapheme_link(0x11133, 0x11134).	% Grapheme_Link Mn   [2] CHAKMA VIRAMA..CHAKMA MAAYYAA
unicode_grapheme_link(0x111C0, 0x111C0).	% Grapheme_Link Mc       SHARADA SIGN VIRAMA
unicode_grapheme_link(0x116B6, 0x116B6).	% Grapheme_Link Mc       TAKRI SIGN VIRAMA

% Total code points: 37
