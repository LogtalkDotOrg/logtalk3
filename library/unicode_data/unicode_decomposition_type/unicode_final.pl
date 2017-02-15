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

unicode_final(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_final(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_final(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_final(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_final(0xFB51, 0xFB51).	% Final Lo       ARABIC LETTER ALEF WASLA FINAL FORM
unicode_final(0xFB53, 0xFB53).	% Final Lo       ARABIC LETTER BEEH FINAL FORM
unicode_final(0xFB57, 0xFB57).	% Final Lo       ARABIC LETTER PEH FINAL FORM
unicode_final(0xFB5B, 0xFB5B).	% Final Lo       ARABIC LETTER BEHEH FINAL FORM
unicode_final(0xFB5F, 0xFB5F).	% Final Lo       ARABIC LETTER TTEHEH FINAL FORM
unicode_final(0xFB63, 0xFB63).	% Final Lo       ARABIC LETTER TEHEH FINAL FORM
unicode_final(0xFB67, 0xFB67).	% Final Lo       ARABIC LETTER TTEH FINAL FORM
unicode_final(0xFB6B, 0xFB6B).	% Final Lo       ARABIC LETTER VEH FINAL FORM
unicode_final(0xFB6F, 0xFB6F).	% Final Lo       ARABIC LETTER PEHEH FINAL FORM
unicode_final(0xFB73, 0xFB73).	% Final Lo       ARABIC LETTER DYEH FINAL FORM
unicode_final(0xFB77, 0xFB77).	% Final Lo       ARABIC LETTER NYEH FINAL FORM
unicode_final(0xFB7B, 0xFB7B).	% Final Lo       ARABIC LETTER TCHEH FINAL FORM
unicode_final(0xFB7F, 0xFB7F).	% Final Lo       ARABIC LETTER TCHEHEH FINAL FORM
unicode_final(0xFB83, 0xFB83).	% Final Lo       ARABIC LETTER DDAHAL FINAL FORM
unicode_final(0xFB85, 0xFB85).	% Final Lo       ARABIC LETTER DAHAL FINAL FORM
unicode_final(0xFB87, 0xFB87).	% Final Lo       ARABIC LETTER DUL FINAL FORM
unicode_final(0xFB89, 0xFB89).	% Final Lo       ARABIC LETTER DDAL FINAL FORM
unicode_final(0xFB8B, 0xFB8B).	% Final Lo       ARABIC LETTER JEH FINAL FORM
unicode_final(0xFB8D, 0xFB8D).	% Final Lo       ARABIC LETTER RREH FINAL FORM
unicode_final(0xFB8F, 0xFB8F).	% Final Lo       ARABIC LETTER KEHEH FINAL FORM
unicode_final(0xFB93, 0xFB93).	% Final Lo       ARABIC LETTER GAF FINAL FORM
unicode_final(0xFB97, 0xFB97).	% Final Lo       ARABIC LETTER GUEH FINAL FORM
unicode_final(0xFB9B, 0xFB9B).	% Final Lo       ARABIC LETTER NGOEH FINAL FORM
unicode_final(0xFB9F, 0xFB9F).	% Final Lo       ARABIC LETTER NOON GHUNNA FINAL FORM
unicode_final(0xFBA1, 0xFBA1).	% Final Lo       ARABIC LETTER RNOON FINAL FORM
unicode_final(0xFBA5, 0xFBA5).	% Final Lo       ARABIC LETTER HEH WITH YEH ABOVE FINAL FORM
unicode_final(0xFBA7, 0xFBA7).	% Final Lo       ARABIC LETTER HEH GOAL FINAL FORM
unicode_final(0xFBAB, 0xFBAB).	% Final Lo       ARABIC LETTER HEH DOACHASHMEE FINAL FORM
unicode_final(0xFBAF, 0xFBAF).	% Final Lo       ARABIC LETTER YEH BARREE FINAL FORM
unicode_final(0xFBB1, 0xFBB1).	% Final Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_final(0xFBD4, 0xFBD4).	% Final Lo       ARABIC LETTER NG FINAL FORM
unicode_final(0xFBD8, 0xFBD8).	% Final Lo       ARABIC LETTER U FINAL FORM
unicode_final(0xFBDA, 0xFBDA).	% Final Lo       ARABIC LETTER OE FINAL FORM
unicode_final(0xFBDC, 0xFBDC).	% Final Lo       ARABIC LETTER YU FINAL FORM
unicode_final(0xFBDF, 0xFBDF).	% Final Lo       ARABIC LETTER VE FINAL FORM
unicode_final(0xFBE1, 0xFBE1).	% Final Lo       ARABIC LETTER KIRGHIZ OE FINAL FORM
unicode_final(0xFBE3, 0xFBE3).	% Final Lo       ARABIC LETTER KIRGHIZ YU FINAL FORM
unicode_final(0xFBE5, 0xFBE5).	% Final Lo       ARABIC LETTER E FINAL FORM
unicode_final(0xFBEB, 0xFBEB).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF FINAL FORM
unicode_final(0xFBED, 0xFBED).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE FINAL FORM
unicode_final(0xFBEF, 0xFBEF).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW FINAL FORM
unicode_final(0xFBF1, 0xFBF1).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U FINAL FORM
unicode_final(0xFBF3, 0xFBF3).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE FINAL FORM
unicode_final(0xFBF5, 0xFBF5).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU FINAL FORM
unicode_final(0xFBF7, 0xFBF7).	% Final Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E FINAL FORM
unicode_final(0xFBFA, 0xFBFA).	% Final Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
unicode_final(0xFBFD, 0xFBFD).	% Final Lo       ARABIC LETTER FARSI YEH FINAL FORM
unicode_final(0xFC64, 0xFC96).	% Final Lo  [51] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH REH FINAL FORM..ARABIC LIGATURE YEH WITH YEH FINAL FORM
unicode_final(0xFD11, 0xFD2C).	% Final Lo  [28] ARABIC LIGATURE TAH WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE DAD WITH REH FINAL FORM
unicode_final(0xFD3C, 0xFD3C).	% Final Lo       ARABIC LIGATURE ALEF WITH FATHATAN FINAL FORM
unicode_final(0xFD51, 0xFD51).	% Final Lo       ARABIC LIGATURE TEH WITH HAH WITH JEEM FINAL FORM
unicode_final(0xFD58, 0xFD58).	% Final Lo       ARABIC LIGATURE JEEM WITH MEEM WITH HAH FINAL FORM
unicode_final(0xFD5A, 0xFD5B).	% Final Lo   [2] ARABIC LIGATURE HAH WITH MEEM WITH YEH FINAL FORM..ARABIC LIGATURE HAH WITH MEEM WITH ALEF MAKSURA FINAL FORM
unicode_final(0xFD5E, 0xFD5F).	% Final Lo   [2] ARABIC LIGATURE SEEN WITH JEEM WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE SEEN WITH MEEM WITH HAH FINAL FORM
unicode_final(0xFD62, 0xFD62).	% Final Lo       ARABIC LIGATURE SEEN WITH MEEM WITH MEEM FINAL FORM
unicode_final(0xFD64, 0xFD64).	% Final Lo       ARABIC LIGATURE SAD WITH HAH WITH HAH FINAL FORM
unicode_final(0xFD66, 0xFD67).	% Final Lo   [2] ARABIC LIGATURE SAD WITH MEEM WITH MEEM FINAL FORM..ARABIC LIGATURE SHEEN WITH HAH WITH MEEM FINAL FORM
unicode_final(0xFD69, 0xFD6A).	% Final Lo   [2] ARABIC LIGATURE SHEEN WITH JEEM WITH YEH FINAL FORM..ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH FINAL FORM
unicode_final(0xFD6C, 0xFD6C).	% Final Lo       ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM FINAL FORM
unicode_final(0xFD6E, 0xFD6F).	% Final Lo   [2] ARABIC LIGATURE DAD WITH HAH WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE DAD WITH KHAH WITH MEEM FINAL FORM
unicode_final(0xFD71, 0xFD71).	% Final Lo       ARABIC LIGATURE TAH WITH MEEM WITH HAH FINAL FORM
unicode_final(0xFD74, 0xFD76).	% Final Lo   [3] ARABIC LIGATURE TAH WITH MEEM WITH YEH FINAL FORM..ARABIC LIGATURE AIN WITH MEEM WITH MEEM FINAL FORM
unicode_final(0xFD78, 0xFD7C).	% Final Lo   [5] ARABIC LIGATURE AIN WITH MEEM WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE FEH WITH KHAH WITH MEEM FINAL FORM
unicode_final(0xFD7E, 0xFD82).	% Final Lo   [5] ARABIC LIGATURE QAF WITH MEEM WITH HAH FINAL FORM..ARABIC LIGATURE LAM WITH HAH WITH ALEF MAKSURA FINAL FORM
unicode_final(0xFD84, 0xFD85).	% Final Lo   [2] ARABIC LIGATURE LAM WITH JEEM WITH JEEM FINAL FORM..ARABIC LIGATURE LAM WITH KHAH WITH MEEM FINAL FORM
unicode_final(0xFD87, 0xFD87).	% Final Lo       ARABIC LIGATURE LAM WITH MEEM WITH HAH FINAL FORM
unicode_final(0xFD8B, 0xFD8B).	% Final Lo       ARABIC LIGATURE MEEM WITH HAH WITH YEH FINAL FORM
unicode_final(0xFD96, 0xFD97).	% Final Lo   [2] ARABIC LIGATURE NOON WITH HAH WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH MEEM FINAL FORM
unicode_final(0xFD99, 0xFD9C).	% Final Lo   [4] ARABIC LIGATURE NOON WITH JEEM WITH ALEF MAKSURA FINAL FORM..ARABIC LIGATURE YEH WITH MEEM WITH MEEM FINAL FORM
unicode_final(0xFD9E, 0xFDB3).	% Final Lo  [22] ARABIC LIGATURE BEH WITH KHAH WITH YEH FINAL FORM..ARABIC LIGATURE NOON WITH HAH WITH YEH FINAL FORM
unicode_final(0xFDB6, 0xFDB7).	% Final Lo   [2] ARABIC LIGATURE AIN WITH MEEM WITH YEH FINAL FORM..ARABIC LIGATURE KAF WITH MEEM WITH YEH FINAL FORM
unicode_final(0xFDB9, 0xFDB9).	% Final Lo       ARABIC LIGATURE MEEM WITH KHAH WITH YEH FINAL FORM
unicode_final(0xFDBB, 0xFDC2).	% Final Lo   [8] ARABIC LIGATURE KAF WITH MEEM WITH MEEM FINAL FORM..ARABIC LIGATURE BEH WITH HAH WITH YEH FINAL FORM
unicode_final(0xFDC6, 0xFDC7).	% Final Lo   [2] ARABIC LIGATURE SEEN WITH KHAH WITH YEH FINAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_final(0xFE82, 0xFE82).	% Final Lo       ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
unicode_final(0xFE84, 0xFE84).	% Final Lo       ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
unicode_final(0xFE86, 0xFE86).	% Final Lo       ARABIC LETTER WAW WITH HAMZA ABOVE FINAL FORM
unicode_final(0xFE88, 0xFE88).	% Final Lo       ARABIC LETTER ALEF WITH HAMZA BELOW FINAL FORM
unicode_final(0xFE8A, 0xFE8A).	% Final Lo       ARABIC LETTER YEH WITH HAMZA ABOVE FINAL FORM
unicode_final(0xFE8E, 0xFE8E).	% Final Lo       ARABIC LETTER ALEF FINAL FORM
unicode_final(0xFE90, 0xFE90).	% Final Lo       ARABIC LETTER BEH FINAL FORM
unicode_final(0xFE94, 0xFE94).	% Final Lo       ARABIC LETTER TEH MARBUTA FINAL FORM
unicode_final(0xFE96, 0xFE96).	% Final Lo       ARABIC LETTER TEH FINAL FORM
unicode_final(0xFE9A, 0xFE9A).	% Final Lo       ARABIC LETTER THEH FINAL FORM
unicode_final(0xFE9E, 0xFE9E).	% Final Lo       ARABIC LETTER JEEM FINAL FORM
unicode_final(0xFEA2, 0xFEA2).	% Final Lo       ARABIC LETTER HAH FINAL FORM
unicode_final(0xFEA6, 0xFEA6).	% Final Lo       ARABIC LETTER KHAH FINAL FORM
unicode_final(0xFEAA, 0xFEAA).	% Final Lo       ARABIC LETTER DAL FINAL FORM
unicode_final(0xFEAC, 0xFEAC).	% Final Lo       ARABIC LETTER THAL FINAL FORM
unicode_final(0xFEAE, 0xFEAE).	% Final Lo       ARABIC LETTER REH FINAL FORM
unicode_final(0xFEB0, 0xFEB0).	% Final Lo       ARABIC LETTER ZAIN FINAL FORM
unicode_final(0xFEB2, 0xFEB2).	% Final Lo       ARABIC LETTER SEEN FINAL FORM
unicode_final(0xFEB6, 0xFEB6).	% Final Lo       ARABIC LETTER SHEEN FINAL FORM
unicode_final(0xFEBA, 0xFEBA).	% Final Lo       ARABIC LETTER SAD FINAL FORM
unicode_final(0xFEBE, 0xFEBE).	% Final Lo       ARABIC LETTER DAD FINAL FORM
unicode_final(0xFEC2, 0xFEC2).	% Final Lo       ARABIC LETTER TAH FINAL FORM
unicode_final(0xFEC6, 0xFEC6).	% Final Lo       ARABIC LETTER ZAH FINAL FORM
unicode_final(0xFECA, 0xFECA).	% Final Lo       ARABIC LETTER AIN FINAL FORM
unicode_final(0xFECE, 0xFECE).	% Final Lo       ARABIC LETTER GHAIN FINAL FORM
unicode_final(0xFED2, 0xFED2).	% Final Lo       ARABIC LETTER FEH FINAL FORM
unicode_final(0xFED6, 0xFED6).	% Final Lo       ARABIC LETTER QAF FINAL FORM
unicode_final(0xFEDA, 0xFEDA).	% Final Lo       ARABIC LETTER KAF FINAL FORM
unicode_final(0xFEDE, 0xFEDE).	% Final Lo       ARABIC LETTER LAM FINAL FORM
unicode_final(0xFEE2, 0xFEE2).	% Final Lo       ARABIC LETTER MEEM FINAL FORM
unicode_final(0xFEE6, 0xFEE6).	% Final Lo       ARABIC LETTER NOON FINAL FORM
unicode_final(0xFEEA, 0xFEEA).	% Final Lo       ARABIC LETTER HEH FINAL FORM
unicode_final(0xFEEE, 0xFEEE).	% Final Lo       ARABIC LETTER WAW FINAL FORM
unicode_final(0xFEF0, 0xFEF0).	% Final Lo       ARABIC LETTER ALEF MAKSURA FINAL FORM
unicode_final(0xFEF2, 0xFEF2).	% Final Lo       ARABIC LETTER YEH FINAL FORM
unicode_final(0xFEF6, 0xFEF6).	% Final Lo       ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
unicode_final(0xFEF8, 0xFEF8).	% Final Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
unicode_final(0xFEFA, 0xFEFA).	% Final Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW FINAL FORM
unicode_final(0xFEFC, 0xFEFC).	% Final Lo       ARABIC LIGATURE LAM WITH ALEF FINAL FORM

% Total code points: 240
