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

unicode_isolated(CodePoint) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_isolated(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_isolated(CodePoint, _) ->
		true
	;	% look for a code point range that includes the given code point
		unicode_isolated(CodePointStart, CodePointEnd),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	).

% ================================================

unicode_isolated(0xFB50, 0xFB50).	% Isolated Lo       ARABIC LETTER ALEF WASLA ISOLATED FORM
unicode_isolated(0xFB52, 0xFB52).	% Isolated Lo       ARABIC LETTER BEEH ISOLATED FORM
unicode_isolated(0xFB56, 0xFB56).	% Isolated Lo       ARABIC LETTER PEH ISOLATED FORM
unicode_isolated(0xFB5A, 0xFB5A).	% Isolated Lo       ARABIC LETTER BEHEH ISOLATED FORM
unicode_isolated(0xFB5E, 0xFB5E).	% Isolated Lo       ARABIC LETTER TTEHEH ISOLATED FORM
unicode_isolated(0xFB62, 0xFB62).	% Isolated Lo       ARABIC LETTER TEHEH ISOLATED FORM
unicode_isolated(0xFB66, 0xFB66).	% Isolated Lo       ARABIC LETTER TTEH ISOLATED FORM
unicode_isolated(0xFB6A, 0xFB6A).	% Isolated Lo       ARABIC LETTER VEH ISOLATED FORM
unicode_isolated(0xFB6E, 0xFB6E).	% Isolated Lo       ARABIC LETTER PEHEH ISOLATED FORM
unicode_isolated(0xFB72, 0xFB72).	% Isolated Lo       ARABIC LETTER DYEH ISOLATED FORM
unicode_isolated(0xFB76, 0xFB76).	% Isolated Lo       ARABIC LETTER NYEH ISOLATED FORM
unicode_isolated(0xFB7A, 0xFB7A).	% Isolated Lo       ARABIC LETTER TCHEH ISOLATED FORM
unicode_isolated(0xFB7E, 0xFB7E).	% Isolated Lo       ARABIC LETTER TCHEHEH ISOLATED FORM
unicode_isolated(0xFB82, 0xFB82).	% Isolated Lo       ARABIC LETTER DDAHAL ISOLATED FORM
unicode_isolated(0xFB84, 0xFB84).	% Isolated Lo       ARABIC LETTER DAHAL ISOLATED FORM
unicode_isolated(0xFB86, 0xFB86).	% Isolated Lo       ARABIC LETTER DUL ISOLATED FORM
unicode_isolated(0xFB88, 0xFB88).	% Isolated Lo       ARABIC LETTER DDAL ISOLATED FORM
unicode_isolated(0xFB8A, 0xFB8A).	% Isolated Lo       ARABIC LETTER JEH ISOLATED FORM
unicode_isolated(0xFB8C, 0xFB8C).	% Isolated Lo       ARABIC LETTER RREH ISOLATED FORM
unicode_isolated(0xFB8E, 0xFB8E).	% Isolated Lo       ARABIC LETTER KEHEH ISOLATED FORM
unicode_isolated(0xFB92, 0xFB92).	% Isolated Lo       ARABIC LETTER GAF ISOLATED FORM
unicode_isolated(0xFB96, 0xFB96).	% Isolated Lo       ARABIC LETTER GUEH ISOLATED FORM
unicode_isolated(0xFB9A, 0xFB9A).	% Isolated Lo       ARABIC LETTER NGOEH ISOLATED FORM
unicode_isolated(0xFB9E, 0xFB9E).	% Isolated Lo       ARABIC LETTER NOON GHUNNA ISOLATED FORM
unicode_isolated(0xFBA0, 0xFBA0).	% Isolated Lo       ARABIC LETTER RNOON ISOLATED FORM
unicode_isolated(0xFBA4, 0xFBA4).	% Isolated Lo       ARABIC LETTER HEH WITH YEH ABOVE ISOLATED FORM
unicode_isolated(0xFBA6, 0xFBA6).	% Isolated Lo       ARABIC LETTER HEH GOAL ISOLATED FORM
unicode_isolated(0xFBAA, 0xFBAA).	% Isolated Lo       ARABIC LETTER HEH DOACHASHMEE ISOLATED FORM
unicode_isolated(0xFBAE, 0xFBAE).	% Isolated Lo       ARABIC LETTER YEH BARREE ISOLATED FORM
unicode_isolated(0xFBB0, 0xFBB0).	% Isolated Lo       ARABIC LETTER YEH BARREE WITH HAMZA ABOVE ISOLATED FORM
unicode_isolated(0xFBD3, 0xFBD3).	% Isolated Lo       ARABIC LETTER NG ISOLATED FORM
unicode_isolated(0xFBD7, 0xFBD7).	% Isolated Lo       ARABIC LETTER U ISOLATED FORM
unicode_isolated(0xFBD9, 0xFBD9).	% Isolated Lo       ARABIC LETTER OE ISOLATED FORM
unicode_isolated(0xFBDB, 0xFBDB).	% Isolated Lo       ARABIC LETTER YU ISOLATED FORM
unicode_isolated(0xFBDD, 0xFBDE).	% Isolated Lo   [2] ARABIC LETTER U WITH HAMZA ABOVE ISOLATED FORM..ARABIC LETTER VE ISOLATED FORM
unicode_isolated(0xFBE0, 0xFBE0).	% Isolated Lo       ARABIC LETTER KIRGHIZ OE ISOLATED FORM
unicode_isolated(0xFBE2, 0xFBE2).	% Isolated Lo       ARABIC LETTER KIRGHIZ YU ISOLATED FORM
unicode_isolated(0xFBE4, 0xFBE4).	% Isolated Lo       ARABIC LETTER E ISOLATED FORM
unicode_isolated(0xFBEA, 0xFBEA).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF ISOLATED FORM
unicode_isolated(0xFBEC, 0xFBEC).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE ISOLATED FORM
unicode_isolated(0xFBEE, 0xFBEE).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW ISOLATED FORM
unicode_isolated(0xFBF0, 0xFBF0).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U ISOLATED FORM
unicode_isolated(0xFBF2, 0xFBF2).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE ISOLATED FORM
unicode_isolated(0xFBF4, 0xFBF4).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU ISOLATED FORM
unicode_isolated(0xFBF6, 0xFBF6).	% Isolated Lo       ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E ISOLATED FORM
unicode_isolated(0xFBF9, 0xFBF9).	% Isolated Lo       ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
unicode_isolated(0xFBFC, 0xFBFC).	% Isolated Lo       ARABIC LETTER FARSI YEH ISOLATED FORM
unicode_isolated(0xFC00, 0xFC63).	% Isolated Lo [100] ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM ISOLATED FORM..ARABIC LIGATURE SHADDA WITH SUPERSCRIPT ALEF ISOLATED FORM
unicode_isolated(0xFCF5, 0xFD10).	% Isolated Lo  [28] ARABIC LIGATURE TAH WITH ALEF MAKSURA ISOLATED FORM..ARABIC LIGATURE DAD WITH REH ISOLATED FORM
unicode_isolated(0xFD3D, 0xFD3D).	% Isolated Lo       ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_isolated(0xFDF0, 0xFDFB).	% Isolated Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_isolated(0xFDFC, 0xFDFC).	% Isolated Sc       RIAL SIGN
unicode_isolated(0xFE70, 0xFE70).	% Isolated Lo       ARABIC FATHATAN ISOLATED FORM
unicode_isolated(0xFE72, 0xFE72).	% Isolated Lo       ARABIC DAMMATAN ISOLATED FORM
unicode_isolated(0xFE74, 0xFE74).	% Isolated Lo       ARABIC KASRATAN ISOLATED FORM
unicode_isolated(0xFE76, 0xFE76).	% Isolated Lo       ARABIC FATHA ISOLATED FORM
unicode_isolated(0xFE78, 0xFE78).	% Isolated Lo       ARABIC DAMMA ISOLATED FORM
unicode_isolated(0xFE7A, 0xFE7A).	% Isolated Lo       ARABIC KASRA ISOLATED FORM
unicode_isolated(0xFE7C, 0xFE7C).	% Isolated Lo       ARABIC SHADDA ISOLATED FORM
unicode_isolated(0xFE7E, 0xFE7E).	% Isolated Lo       ARABIC SUKUN ISOLATED FORM
unicode_isolated(0xFE80, 0xFE81).	% Isolated Lo   [2] ARABIC LETTER HAMZA ISOLATED FORM..ARABIC LETTER ALEF WITH MADDA ABOVE ISOLATED FORM
unicode_isolated(0xFE83, 0xFE83).	% Isolated Lo       ARABIC LETTER ALEF WITH HAMZA ABOVE ISOLATED FORM
unicode_isolated(0xFE85, 0xFE85).	% Isolated Lo       ARABIC LETTER WAW WITH HAMZA ABOVE ISOLATED FORM
unicode_isolated(0xFE87, 0xFE87).	% Isolated Lo       ARABIC LETTER ALEF WITH HAMZA BELOW ISOLATED FORM
unicode_isolated(0xFE89, 0xFE89).	% Isolated Lo       ARABIC LETTER YEH WITH HAMZA ABOVE ISOLATED FORM
unicode_isolated(0xFE8D, 0xFE8D).	% Isolated Lo       ARABIC LETTER ALEF ISOLATED FORM
unicode_isolated(0xFE8F, 0xFE8F).	% Isolated Lo       ARABIC LETTER BEH ISOLATED FORM
unicode_isolated(0xFE93, 0xFE93).	% Isolated Lo       ARABIC LETTER TEH MARBUTA ISOLATED FORM
unicode_isolated(0xFE95, 0xFE95).	% Isolated Lo       ARABIC LETTER TEH ISOLATED FORM
unicode_isolated(0xFE99, 0xFE99).	% Isolated Lo       ARABIC LETTER THEH ISOLATED FORM
unicode_isolated(0xFE9D, 0xFE9D).	% Isolated Lo       ARABIC LETTER JEEM ISOLATED FORM
unicode_isolated(0xFEA1, 0xFEA1).	% Isolated Lo       ARABIC LETTER HAH ISOLATED FORM
unicode_isolated(0xFEA5, 0xFEA5).	% Isolated Lo       ARABIC LETTER KHAH ISOLATED FORM
unicode_isolated(0xFEA9, 0xFEA9).	% Isolated Lo       ARABIC LETTER DAL ISOLATED FORM
unicode_isolated(0xFEAB, 0xFEAB).	% Isolated Lo       ARABIC LETTER THAL ISOLATED FORM
unicode_isolated(0xFEAD, 0xFEAD).	% Isolated Lo       ARABIC LETTER REH ISOLATED FORM
unicode_isolated(0xFEAF, 0xFEAF).	% Isolated Lo       ARABIC LETTER ZAIN ISOLATED FORM
unicode_isolated(0xFEB1, 0xFEB1).	% Isolated Lo       ARABIC LETTER SEEN ISOLATED FORM
unicode_isolated(0xFEB5, 0xFEB5).	% Isolated Lo       ARABIC LETTER SHEEN ISOLATED FORM
unicode_isolated(0xFEB9, 0xFEB9).	% Isolated Lo       ARABIC LETTER SAD ISOLATED FORM
unicode_isolated(0xFEBD, 0xFEBD).	% Isolated Lo       ARABIC LETTER DAD ISOLATED FORM
unicode_isolated(0xFEC1, 0xFEC1).	% Isolated Lo       ARABIC LETTER TAH ISOLATED FORM
unicode_isolated(0xFEC5, 0xFEC5).	% Isolated Lo       ARABIC LETTER ZAH ISOLATED FORM
unicode_isolated(0xFEC9, 0xFEC9).	% Isolated Lo       ARABIC LETTER AIN ISOLATED FORM
unicode_isolated(0xFECD, 0xFECD).	% Isolated Lo       ARABIC LETTER GHAIN ISOLATED FORM
unicode_isolated(0xFED1, 0xFED1).	% Isolated Lo       ARABIC LETTER FEH ISOLATED FORM
unicode_isolated(0xFED5, 0xFED5).	% Isolated Lo       ARABIC LETTER QAF ISOLATED FORM
unicode_isolated(0xFED9, 0xFED9).	% Isolated Lo       ARABIC LETTER KAF ISOLATED FORM
unicode_isolated(0xFEDD, 0xFEDD).	% Isolated Lo       ARABIC LETTER LAM ISOLATED FORM
unicode_isolated(0xFEE1, 0xFEE1).	% Isolated Lo       ARABIC LETTER MEEM ISOLATED FORM
unicode_isolated(0xFEE5, 0xFEE5).	% Isolated Lo       ARABIC LETTER NOON ISOLATED FORM
unicode_isolated(0xFEE9, 0xFEE9).	% Isolated Lo       ARABIC LETTER HEH ISOLATED FORM
unicode_isolated(0xFEED, 0xFEED).	% Isolated Lo       ARABIC LETTER WAW ISOLATED FORM
unicode_isolated(0xFEEF, 0xFEEF).	% Isolated Lo       ARABIC LETTER ALEF MAKSURA ISOLATED FORM
unicode_isolated(0xFEF1, 0xFEF1).	% Isolated Lo       ARABIC LETTER YEH ISOLATED FORM
unicode_isolated(0xFEF5, 0xFEF5).	% Isolated Lo       ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM
unicode_isolated(0xFEF7, 0xFEF7).	% Isolated Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
unicode_isolated(0xFEF9, 0xFEF9).	% Isolated Lo       ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW ISOLATED FORM
unicode_isolated(0xFEFB, 0xFEFB).	% Isolated Lo       ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM

% Total code points: 238
