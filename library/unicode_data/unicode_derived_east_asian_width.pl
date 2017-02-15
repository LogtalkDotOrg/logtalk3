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
# DerivedEastAsianWidth-6.2.0.txt
# Date: 2012-05-20, 00:42:33 GMT [MD]
#
# Unicode Character Database
# Copyright (c) 1991-2012 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/

# ================================================

# East_Asian_Width (listing EastAsianWidth.txt, field 1)

#  All code points not explicitly listed for East_Asian_Width
#  have the value Neutral (N).

# @missing: 0000..10FFFF; Neutral

# ================================================
*/

unicode_east_asian_width(CodePoint, Width) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_east_asian_width(CodePointStart, CodePointEnd, Width),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_east_asian_width(CodePoint, _, CodePointWidth) ->
		Width = CodePointWidth
	;	% look for a code point range that includes the given code point
		unicode_east_asian_width(CodePointStart, CodePointEnd, CodePointWidth),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Width = CodePointWidth
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Width = 'N'
	).

% East_Asian_Width=Neutral

unicode_east_asian_width(0x0000, 0x001F, 'N').	% Cc  [32] <control-0000>..<control-001F>
unicode_east_asian_width(0x007F, 0x009F, 'N').	% Cc  [33] <control-007F>..<control-009F>
unicode_east_asian_width(0x00A0, 0x00A0, 'N').	% Zs       NO-BREAK SPACE
unicode_east_asian_width(0x00A9, 0x00A9, 'N').	% So       COPYRIGHT SIGN
unicode_east_asian_width(0x00AB, 0x00AB, 'N').	% Pi       LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_east_asian_width(0x00B5, 0x00B5, 'N').	% L&       MICRO SIGN
unicode_east_asian_width(0x00BB, 0x00BB, 'N').	% Pf       RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
unicode_east_asian_width(0x00C0, 0x00C5, 'N').	% L&   [6] LATIN CAPITAL LETTER A WITH GRAVE..LATIN CAPITAL LETTER A WITH RING ABOVE
unicode_east_asian_width(0x00C7, 0x00CF, 'N').	% L&   [9] LATIN CAPITAL LETTER C WITH CEDILLA..LATIN CAPITAL LETTER I WITH DIAERESIS
unicode_east_asian_width(0x00D1, 0x00D6, 'N').	% L&   [6] LATIN CAPITAL LETTER N WITH TILDE..LATIN CAPITAL LETTER O WITH DIAERESIS
unicode_east_asian_width(0x00D9, 0x00DD, 'N').	% L&   [5] LATIN CAPITAL LETTER U WITH GRAVE..LATIN CAPITAL LETTER Y WITH ACUTE
unicode_east_asian_width(0x00E2, 0x00E5, 'N').	% L&   [4] LATIN SMALL LETTER A WITH CIRCUMFLEX..LATIN SMALL LETTER A WITH RING ABOVE
unicode_east_asian_width(0x00E7, 0x00E7, 'N').	% L&       LATIN SMALL LETTER C WITH CEDILLA
unicode_east_asian_width(0x00EB, 0x00EB, 'N').	% L&       LATIN SMALL LETTER E WITH DIAERESIS
unicode_east_asian_width(0x00EE, 0x00EF, 'N').	% L&   [2] LATIN SMALL LETTER I WITH CIRCUMFLEX..LATIN SMALL LETTER I WITH DIAERESIS
unicode_east_asian_width(0x00F1, 0x00F1, 'N').	% L&       LATIN SMALL LETTER N WITH TILDE
unicode_east_asian_width(0x00F4, 0x00F6, 'N').	% L&   [3] LATIN SMALL LETTER O WITH CIRCUMFLEX..LATIN SMALL LETTER O WITH DIAERESIS
unicode_east_asian_width(0x00FB, 0x00FB, 'N').	% L&       LATIN SMALL LETTER U WITH CIRCUMFLEX
unicode_east_asian_width(0x00FD, 0x00FD, 'N').	% L&       LATIN SMALL LETTER Y WITH ACUTE
unicode_east_asian_width(0x00FF, 0x0100, 'N').	% L&   [2] LATIN SMALL LETTER Y WITH DIAERESIS..LATIN CAPITAL LETTER A WITH MACRON
unicode_east_asian_width(0x0102, 0x0110, 'N').	% L&  [15] LATIN CAPITAL LETTER A WITH BREVE..LATIN CAPITAL LETTER D WITH STROKE
unicode_east_asian_width(0x0112, 0x0112, 'N').	% L&       LATIN CAPITAL LETTER E WITH MACRON
unicode_east_asian_width(0x0114, 0x011A, 'N').	% L&   [7] LATIN CAPITAL LETTER E WITH BREVE..LATIN CAPITAL LETTER E WITH CARON
unicode_east_asian_width(0x011C, 0x0125, 'N').	% L&  [10] LATIN CAPITAL LETTER G WITH CIRCUMFLEX..LATIN SMALL LETTER H WITH CIRCUMFLEX
unicode_east_asian_width(0x0128, 0x012A, 'N').	% L&   [3] LATIN CAPITAL LETTER I WITH TILDE..LATIN CAPITAL LETTER I WITH MACRON
unicode_east_asian_width(0x012C, 0x0130, 'N').	% L&   [5] LATIN CAPITAL LETTER I WITH BREVE..LATIN CAPITAL LETTER I WITH DOT ABOVE
unicode_east_asian_width(0x0134, 0x0137, 'N').	% L&   [4] LATIN CAPITAL LETTER J WITH CIRCUMFLEX..LATIN SMALL LETTER K WITH CEDILLA
unicode_east_asian_width(0x0139, 0x013E, 'N').	% L&   [6] LATIN CAPITAL LETTER L WITH ACUTE..LATIN SMALL LETTER L WITH CARON
unicode_east_asian_width(0x0143, 0x0143, 'N').	% L&       LATIN CAPITAL LETTER N WITH ACUTE
unicode_east_asian_width(0x0145, 0x0147, 'N').	% L&   [3] LATIN CAPITAL LETTER N WITH CEDILLA..LATIN CAPITAL LETTER N WITH CARON
unicode_east_asian_width(0x014C, 0x014C, 'N').	% L&       LATIN CAPITAL LETTER O WITH MACRON
unicode_east_asian_width(0x014E, 0x0151, 'N').	% L&   [4] LATIN CAPITAL LETTER O WITH BREVE..LATIN SMALL LETTER O WITH DOUBLE ACUTE
unicode_east_asian_width(0x0154, 0x0165, 'N').	% L&  [18] LATIN CAPITAL LETTER R WITH ACUTE..LATIN SMALL LETTER T WITH CARON
unicode_east_asian_width(0x0168, 0x016A, 'N').	% L&   [3] LATIN CAPITAL LETTER U WITH TILDE..LATIN CAPITAL LETTER U WITH MACRON
unicode_east_asian_width(0x016C, 0x01BA, 'N').	% L&  [79] LATIN CAPITAL LETTER U WITH BREVE..LATIN SMALL LETTER EZH WITH TAIL
unicode_east_asian_width(0x01BB, 0x01BB, 'N').	% Lo       LATIN LETTER TWO WITH STROKE
unicode_east_asian_width(0x01BC, 0x01BF, 'N').	% L&   [4] LATIN CAPITAL LETTER TONE FIVE..LATIN LETTER WYNN
unicode_east_asian_width(0x01C0, 0x01C3, 'N').	% Lo   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
unicode_east_asian_width(0x01C4, 0x01CD, 'N').	% L&  [10] LATIN CAPITAL LETTER DZ WITH CARON..LATIN CAPITAL LETTER A WITH CARON
unicode_east_asian_width(0x01CF, 0x01CF, 'N').	% L&       LATIN CAPITAL LETTER I WITH CARON
unicode_east_asian_width(0x01D1, 0x01D1, 'N').	% L&       LATIN CAPITAL LETTER O WITH CARON
unicode_east_asian_width(0x01D3, 0x01D3, 'N').	% L&       LATIN CAPITAL LETTER U WITH CARON
unicode_east_asian_width(0x01D5, 0x01D5, 'N').	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
unicode_east_asian_width(0x01D7, 0x01D7, 'N').	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
unicode_east_asian_width(0x01D9, 0x01D9, 'N').	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
unicode_east_asian_width(0x01DB, 0x01DB, 'N').	% L&       LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
unicode_east_asian_width(0x01DD, 0x0250, 'N').	% L& [116] LATIN SMALL LETTER TURNED E..LATIN SMALL LETTER TURNED A
unicode_east_asian_width(0x0252, 0x0260, 'N').	% L&  [15] LATIN SMALL LETTER TURNED ALPHA..LATIN SMALL LETTER G WITH HOOK
unicode_east_asian_width(0x0262, 0x0293, 'N').	% L&  [50] LATIN LETTER SMALL CAPITAL G..LATIN SMALL LETTER EZH WITH CURL
unicode_east_asian_width(0x0294, 0x0294, 'N').	% Lo       LATIN LETTER GLOTTAL STOP
unicode_east_asian_width(0x0295, 0x02AF, 'N').	% L&  [27] LATIN LETTER PHARYNGEAL VOICED FRICATIVE..LATIN SMALL LETTER TURNED H WITH FISHHOOK AND TAIL
unicode_east_asian_width(0x02B0, 0x02C1, 'N').	% Lm  [18] MODIFIER LETTER SMALL H..MODIFIER LETTER REVERSED GLOTTAL STOP
unicode_east_asian_width(0x02C2, 0x02C3, 'N').	% Sk   [2] MODIFIER LETTER LEFT ARROWHEAD..MODIFIER LETTER RIGHT ARROWHEAD
unicode_east_asian_width(0x02C5, 0x02C5, 'N').	% Sk       MODIFIER LETTER DOWN ARROWHEAD
unicode_east_asian_width(0x02C6, 0x02C6, 'N').	% Lm       MODIFIER LETTER CIRCUMFLEX ACCENT
unicode_east_asian_width(0x02C8, 0x02C8, 'N').	% Lm       MODIFIER LETTER VERTICAL LINE
unicode_east_asian_width(0x02CC, 0x02CC, 'N').	% Lm       MODIFIER LETTER LOW VERTICAL LINE
unicode_east_asian_width(0x02CE, 0x02CF, 'N').	% Lm   [2] MODIFIER LETTER LOW GRAVE ACCENT..MODIFIER LETTER LOW ACUTE ACCENT
unicode_east_asian_width(0x02D1, 0x02D1, 'N').	% Lm       MODIFIER LETTER HALF TRIANGULAR COLON
unicode_east_asian_width(0x02D2, 0x02D7, 'N').	% Sk   [6] MODIFIER LETTER CENTRED RIGHT HALF RING..MODIFIER LETTER MINUS SIGN
unicode_east_asian_width(0x02DC, 0x02DC, 'N').	% Sk       SMALL TILDE
unicode_east_asian_width(0x02DE, 0x02DE, 'N').	% Sk       MODIFIER LETTER RHOTIC HOOK
unicode_east_asian_width(0x02E0, 0x02E4, 'N').	% Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
unicode_east_asian_width(0x02E5, 0x02EB, 'N').	% Sk   [7] MODIFIER LETTER EXTRA-HIGH TONE BAR..MODIFIER LETTER YANG DEPARTING TONE MARK
unicode_east_asian_width(0x02EC, 0x02EC, 'N').	% Lm       MODIFIER LETTER VOICING
unicode_east_asian_width(0x02ED, 0x02ED, 'N').	% Sk       MODIFIER LETTER UNASPIRATED
unicode_east_asian_width(0x02EE, 0x02EE, 'N').	% Lm       MODIFIER LETTER DOUBLE APOSTROPHE
unicode_east_asian_width(0x02EF, 0x02FF, 'N').	% Sk  [17] MODIFIER LETTER LOW DOWN ARROWHEAD..MODIFIER LETTER LOW LEFT ARROW
unicode_east_asian_width(0x0370, 0x0373, 'N').	% L&   [4] GREEK CAPITAL LETTER HETA..GREEK SMALL LETTER ARCHAIC SAMPI
unicode_east_asian_width(0x0374, 0x0374, 'N').	% Lm       GREEK NUMERAL SIGN
unicode_east_asian_width(0x0375, 0x0375, 'N').	% Sk       GREEK LOWER NUMERAL SIGN
unicode_east_asian_width(0x0376, 0x0377, 'N').	% L&   [2] GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA..GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
unicode_east_asian_width(0x037A, 0x037A, 'N').	% Lm       GREEK YPOGEGRAMMENI
unicode_east_asian_width(0x037B, 0x037D, 'N').	% L&   [3] GREEK SMALL REVERSED LUNATE SIGMA SYMBOL..GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
unicode_east_asian_width(0x037E, 0x037E, 'N').	% Po       GREEK QUESTION MARK
unicode_east_asian_width(0x0384, 0x0385, 'N').	% Sk   [2] GREEK TONOS..GREEK DIALYTIKA TONOS
unicode_east_asian_width(0x0386, 0x0386, 'N').	% L&       GREEK CAPITAL LETTER ALPHA WITH TONOS
unicode_east_asian_width(0x0387, 0x0387, 'N').	% Po       GREEK ANO TELEIA
unicode_east_asian_width(0x0388, 0x038A, 'N').	% L&   [3] GREEK CAPITAL LETTER EPSILON WITH TONOS..GREEK CAPITAL LETTER IOTA WITH TONOS
unicode_east_asian_width(0x038C, 0x038C, 'N').	% L&       GREEK CAPITAL LETTER OMICRON WITH TONOS
unicode_east_asian_width(0x038E, 0x0390, 'N').	% L&   [3] GREEK CAPITAL LETTER UPSILON WITH TONOS..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
unicode_east_asian_width(0x03AA, 0x03B0, 'N').	% L&   [7] GREEK CAPITAL LETTER IOTA WITH DIALYTIKA..GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
unicode_east_asian_width(0x03C2, 0x03C2, 'N').	% L&       GREEK SMALL LETTER FINAL SIGMA
unicode_east_asian_width(0x03CA, 0x03F5, 'N').	% L&  [44] GREEK SMALL LETTER IOTA WITH DIALYTIKA..GREEK LUNATE EPSILON SYMBOL
unicode_east_asian_width(0x03F6, 0x03F6, 'N').	% Sm       GREEK REVERSED LUNATE EPSILON SYMBOL
unicode_east_asian_width(0x03F7, 0x0400, 'N').	% L&  [10] GREEK CAPITAL LETTER SHO..CYRILLIC CAPITAL LETTER IE WITH GRAVE
unicode_east_asian_width(0x0402, 0x040F, 'N').	% L&  [14] CYRILLIC CAPITAL LETTER DJE..CYRILLIC CAPITAL LETTER DZHE
unicode_east_asian_width(0x0450, 0x0450, 'N').	% L&       CYRILLIC SMALL LETTER IE WITH GRAVE
unicode_east_asian_width(0x0452, 0x0481, 'N').	% L&  [48] CYRILLIC SMALL LETTER DJE..CYRILLIC SMALL LETTER KOPPA
unicode_east_asian_width(0x0482, 0x0482, 'N').	% So       CYRILLIC THOUSANDS SIGN
unicode_east_asian_width(0x0483, 0x0487, 'N').	% Mn   [5] COMBINING CYRILLIC TITLO..COMBINING CYRILLIC POKRYTIE
unicode_east_asian_width(0x0488, 0x0489, 'N').	% Me   [2] COMBINING CYRILLIC HUNDRED THOUSANDS SIGN..COMBINING CYRILLIC MILLIONS SIGN
unicode_east_asian_width(0x048A, 0x0527, 'N').	% L& [158] CYRILLIC CAPITAL LETTER SHORT I WITH TAIL..CYRILLIC SMALL LETTER SHHA WITH DESCENDER
unicode_east_asian_width(0x0531, 0x0556, 'N').	% L&  [38] ARMENIAN CAPITAL LETTER AYB..ARMENIAN CAPITAL LETTER FEH
unicode_east_asian_width(0x0559, 0x0559, 'N').	% Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
unicode_east_asian_width(0x055A, 0x055F, 'N').	% Po   [6] ARMENIAN APOSTROPHE..ARMENIAN ABBREVIATION MARK
unicode_east_asian_width(0x0561, 0x0587, 'N').	% L&  [39] ARMENIAN SMALL LETTER AYB..ARMENIAN SMALL LIGATURE ECH YIWN
unicode_east_asian_width(0x0589, 0x0589, 'N').	% Po       ARMENIAN FULL STOP
unicode_east_asian_width(0x058A, 0x058A, 'N').	% Pd       ARMENIAN HYPHEN
unicode_east_asian_width(0x058F, 0x058F, 'N').	% Sc       ARMENIAN DRAM SIGN
unicode_east_asian_width(0x0591, 0x05BD, 'N').	% Mn  [45] HEBREW ACCENT ETNAHTA..HEBREW POINT METEG
unicode_east_asian_width(0x05BE, 0x05BE, 'N').	% Pd       HEBREW PUNCTUATION MAQAF
unicode_east_asian_width(0x05BF, 0x05BF, 'N').	% Mn       HEBREW POINT RAFE
unicode_east_asian_width(0x05C0, 0x05C0, 'N').	% Po       HEBREW PUNCTUATION PASEQ
unicode_east_asian_width(0x05C1, 0x05C2, 'N').	% Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
unicode_east_asian_width(0x05C3, 0x05C3, 'N').	% Po       HEBREW PUNCTUATION SOF PASUQ
unicode_east_asian_width(0x05C4, 0x05C5, 'N').	% Mn   [2] HEBREW MARK UPPER DOT..HEBREW MARK LOWER DOT
unicode_east_asian_width(0x05C6, 0x05C6, 'N').	% Po       HEBREW PUNCTUATION NUN HAFUKHA
unicode_east_asian_width(0x05C7, 0x05C7, 'N').	% Mn       HEBREW POINT QAMATS QATAN
unicode_east_asian_width(0x05D0, 0x05EA, 'N').	% Lo  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
unicode_east_asian_width(0x05F0, 0x05F2, 'N').	% Lo   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
unicode_east_asian_width(0x05F3, 0x05F4, 'N').	% Po   [2] HEBREW PUNCTUATION GERESH..HEBREW PUNCTUATION GERSHAYIM
unicode_east_asian_width(0x0600, 0x0604, 'N').	% Cf   [5] ARABIC NUMBER SIGN..ARABIC SIGN SAMVAT
unicode_east_asian_width(0x0606, 0x0608, 'N').	% Sm   [3] ARABIC-INDIC CUBE ROOT..ARABIC RAY
unicode_east_asian_width(0x0609, 0x060A, 'N').	% Po   [2] ARABIC-INDIC PER MILLE SIGN..ARABIC-INDIC PER TEN THOUSAND SIGN
unicode_east_asian_width(0x060B, 0x060B, 'N').	% Sc       AFGHANI SIGN
unicode_east_asian_width(0x060C, 0x060D, 'N').	% Po   [2] ARABIC COMMA..ARABIC DATE SEPARATOR
unicode_east_asian_width(0x060E, 0x060F, 'N').	% So   [2] ARABIC POETIC VERSE SIGN..ARABIC SIGN MISRA
unicode_east_asian_width(0x0610, 0x061A, 'N').	% Mn  [11] ARABIC SIGN SALLALLAHOU ALAYHE WASSALLAM..ARABIC SMALL KASRA
unicode_east_asian_width(0x061B, 0x061B, 'N').	% Po       ARABIC SEMICOLON
unicode_east_asian_width(0x061E, 0x061F, 'N').	% Po   [2] ARABIC TRIPLE DOT PUNCTUATION MARK..ARABIC QUESTION MARK
unicode_east_asian_width(0x0620, 0x063F, 'N').	% Lo  [32] ARABIC LETTER KASHMIRI YEH..ARABIC LETTER FARSI YEH WITH THREE DOTS ABOVE
unicode_east_asian_width(0x0640, 0x0640, 'N').	% Lm       ARABIC TATWEEL
unicode_east_asian_width(0x0641, 0x064A, 'N').	% Lo  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
unicode_east_asian_width(0x064B, 0x065F, 'N').	% Mn  [21] ARABIC FATHATAN..ARABIC WAVY HAMZA BELOW
unicode_east_asian_width(0x0660, 0x0669, 'N').	% Nd  [10] ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
unicode_east_asian_width(0x066A, 0x066D, 'N').	% Po   [4] ARABIC PERCENT SIGN..ARABIC FIVE POINTED STAR
unicode_east_asian_width(0x066E, 0x066F, 'N').	% Lo   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
unicode_east_asian_width(0x0670, 0x0670, 'N').	% Mn       ARABIC LETTER SUPERSCRIPT ALEF
unicode_east_asian_width(0x0671, 0x06D3, 'N').	% Lo  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
unicode_east_asian_width(0x06D4, 0x06D4, 'N').	% Po       ARABIC FULL STOP
unicode_east_asian_width(0x06D5, 0x06D5, 'N').	% Lo       ARABIC LETTER AE
unicode_east_asian_width(0x06D6, 0x06DC, 'N').	% Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
unicode_east_asian_width(0x06DD, 0x06DD, 'N').	% Cf       ARABIC END OF AYAH
unicode_east_asian_width(0x06DE, 0x06DE, 'N').	% So       ARABIC START OF RUB EL HIZB
unicode_east_asian_width(0x06DF, 0x06E4, 'N').	% Mn   [6] ARABIC SMALL HIGH ROUNDED ZERO..ARABIC SMALL HIGH MADDA
unicode_east_asian_width(0x06E5, 0x06E6, 'N').	% Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
unicode_east_asian_width(0x06E7, 0x06E8, 'N').	% Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
unicode_east_asian_width(0x06E9, 0x06E9, 'N').	% So       ARABIC PLACE OF SAJDAH
unicode_east_asian_width(0x06EA, 0x06ED, 'N').	% Mn   [4] ARABIC EMPTY CENTRE LOW STOP..ARABIC SMALL LOW MEEM
unicode_east_asian_width(0x06EE, 0x06EF, 'N').	% Lo   [2] ARABIC LETTER DAL WITH INVERTED V..ARABIC LETTER REH WITH INVERTED V
unicode_east_asian_width(0x06F0, 0x06F9, 'N').	% Nd  [10] EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
unicode_east_asian_width(0x06FA, 0x06FC, 'N').	% Lo   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
unicode_east_asian_width(0x06FD, 0x06FE, 'N').	% So   [2] ARABIC SIGN SINDHI AMPERSAND..ARABIC SIGN SINDHI POSTPOSITION MEN
unicode_east_asian_width(0x06FF, 0x06FF, 'N').	% Lo       ARABIC LETTER HEH WITH INVERTED V
unicode_east_asian_width(0x0700, 0x070D, 'N').	% Po  [14] SYRIAC END OF PARAGRAPH..SYRIAC HARKLEAN ASTERISCUS
unicode_east_asian_width(0x070F, 0x070F, 'N').	% Cf       SYRIAC ABBREVIATION MARK
unicode_east_asian_width(0x0710, 0x0710, 'N').	% Lo       SYRIAC LETTER ALAPH
unicode_east_asian_width(0x0711, 0x0711, 'N').	% Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
unicode_east_asian_width(0x0712, 0x072F, 'N').	% Lo  [30] SYRIAC LETTER BETH..SYRIAC LETTER PERSIAN DHALATH
unicode_east_asian_width(0x0730, 0x074A, 'N').	% Mn  [27] SYRIAC PTHAHA ABOVE..SYRIAC BARREKH
unicode_east_asian_width(0x074D, 0x07A5, 'N').	% Lo  [89] SYRIAC LETTER SOGDIAN ZHAIN..THAANA LETTER WAAVU
unicode_east_asian_width(0x07A6, 0x07B0, 'N').	% Mn  [11] THAANA ABAFILI..THAANA SUKUN
unicode_east_asian_width(0x07B1, 0x07B1, 'N').	% Lo       THAANA LETTER NAA
unicode_east_asian_width(0x07C0, 0x07C9, 'N').	% Nd  [10] NKO DIGIT ZERO..NKO DIGIT NINE
unicode_east_asian_width(0x07CA, 0x07EA, 'N').	% Lo  [33] NKO LETTER A..NKO LETTER JONA RA
unicode_east_asian_width(0x07EB, 0x07F3, 'N').	% Mn   [9] NKO COMBINING SHORT HIGH TONE..NKO COMBINING DOUBLE DOT ABOVE
unicode_east_asian_width(0x07F4, 0x07F5, 'N').	% Lm   [2] NKO HIGH TONE APOSTROPHE..NKO LOW TONE APOSTROPHE
unicode_east_asian_width(0x07F6, 0x07F6, 'N').	% So       NKO SYMBOL OO DENNEN
unicode_east_asian_width(0x07F7, 0x07F9, 'N').	% Po   [3] NKO SYMBOL GBAKURUNEN..NKO EXCLAMATION MARK
unicode_east_asian_width(0x07FA, 0x07FA, 'N').	% Lm       NKO LAJANYALAN
unicode_east_asian_width(0x0800, 0x0815, 'N').	% Lo  [22] SAMARITAN LETTER ALAF..SAMARITAN LETTER TAAF
unicode_east_asian_width(0x0816, 0x0819, 'N').	% Mn   [4] SAMARITAN MARK IN..SAMARITAN MARK DAGESH
unicode_east_asian_width(0x081A, 0x081A, 'N').	% Lm       SAMARITAN MODIFIER LETTER EPENTHETIC YUT
unicode_east_asian_width(0x081B, 0x0823, 'N').	% Mn   [9] SAMARITAN MARK EPENTHETIC YUT..SAMARITAN VOWEL SIGN A
unicode_east_asian_width(0x0824, 0x0824, 'N').	% Lm       SAMARITAN MODIFIER LETTER SHORT A
unicode_east_asian_width(0x0825, 0x0827, 'N').	% Mn   [3] SAMARITAN VOWEL SIGN SHORT A..SAMARITAN VOWEL SIGN U
unicode_east_asian_width(0x0828, 0x0828, 'N').	% Lm       SAMARITAN MODIFIER LETTER I
unicode_east_asian_width(0x0829, 0x082D, 'N').	% Mn   [5] SAMARITAN VOWEL SIGN LONG I..SAMARITAN MARK NEQUDAA
unicode_east_asian_width(0x0830, 0x083E, 'N').	% Po  [15] SAMARITAN PUNCTUATION NEQUDAA..SAMARITAN PUNCTUATION ANNAAU
unicode_east_asian_width(0x0840, 0x0858, 'N').	% Lo  [25] MANDAIC LETTER HALQA..MANDAIC LETTER AIN
unicode_east_asian_width(0x0859, 0x085B, 'N').	% Mn   [3] MANDAIC AFFRICATION MARK..MANDAIC GEMINATION MARK
unicode_east_asian_width(0x085E, 0x085E, 'N').	% Po       MANDAIC PUNCTUATION
unicode_east_asian_width(0x08A0, 0x08A0, 'N').	% Lo       ARABIC LETTER BEH WITH SMALL V BELOW
unicode_east_asian_width(0x08A2, 0x08AC, 'N').	% Lo  [11] ARABIC LETTER JEEM WITH TWO DOTS ABOVE..ARABIC LETTER ROHINGYA YEH
unicode_east_asian_width(0x08E4, 0x08FE, 'N').	% Mn  [27] ARABIC CURLY FATHA..ARABIC DAMMA WITH DOT
unicode_east_asian_width(0x0900, 0x0902, 'N').	% Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_east_asian_width(0x0903, 0x0903, 'N').	% Mc       DEVANAGARI SIGN VISARGA
unicode_east_asian_width(0x0904, 0x0939, 'N').	% Lo  [54] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER HA
unicode_east_asian_width(0x093A, 0x093A, 'N').	% Mn       DEVANAGARI VOWEL SIGN OE
unicode_east_asian_width(0x093B, 0x093B, 'N').	% Mc       DEVANAGARI VOWEL SIGN OOE
unicode_east_asian_width(0x093C, 0x093C, 'N').	% Mn       DEVANAGARI SIGN NUKTA
unicode_east_asian_width(0x093D, 0x093D, 'N').	% Lo       DEVANAGARI SIGN AVAGRAHA
unicode_east_asian_width(0x093E, 0x0940, 'N').	% Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_east_asian_width(0x0941, 0x0948, 'N').	% Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_east_asian_width(0x0949, 0x094C, 'N').	% Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_east_asian_width(0x094D, 0x094D, 'N').	% Mn       DEVANAGARI SIGN VIRAMA
unicode_east_asian_width(0x094E, 0x094F, 'N').	% Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_east_asian_width(0x0950, 0x0950, 'N').	% Lo       DEVANAGARI OM
unicode_east_asian_width(0x0951, 0x0957, 'N').	% Mn   [7] DEVANAGARI STRESS SIGN UDATTA..DEVANAGARI VOWEL SIGN UUE
unicode_east_asian_width(0x0958, 0x0961, 'N').	% Lo  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
unicode_east_asian_width(0x0962, 0x0963, 'N').	% Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x0964, 0x0965, 'N').	% Po   [2] DEVANAGARI DANDA..DEVANAGARI DOUBLE DANDA
unicode_east_asian_width(0x0966, 0x096F, 'N').	% Nd  [10] DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
unicode_east_asian_width(0x0970, 0x0970, 'N').	% Po       DEVANAGARI ABBREVIATION SIGN
unicode_east_asian_width(0x0971, 0x0971, 'N').	% Lm       DEVANAGARI SIGN HIGH SPACING DOT
unicode_east_asian_width(0x0972, 0x0977, 'N').	% Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_east_asian_width(0x0979, 0x097F, 'N').	% Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_east_asian_width(0x0981, 0x0981, 'N').	% Mn       BENGALI SIGN CANDRABINDU
unicode_east_asian_width(0x0982, 0x0983, 'N').	% Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
unicode_east_asian_width(0x0985, 0x098C, 'N').	% Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_east_asian_width(0x098F, 0x0990, 'N').	% Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_east_asian_width(0x0993, 0x09A8, 'N').	% Lo  [22] BENGALI LETTER O..BENGALI LETTER NA
unicode_east_asian_width(0x09AA, 0x09B0, 'N').	% Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_east_asian_width(0x09B2, 0x09B2, 'N').	% Lo       BENGALI LETTER LA
unicode_east_asian_width(0x09B6, 0x09B9, 'N').	% Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_east_asian_width(0x09BC, 0x09BC, 'N').	% Mn       BENGALI SIGN NUKTA
unicode_east_asian_width(0x09BD, 0x09BD, 'N').	% Lo       BENGALI SIGN AVAGRAHA
unicode_east_asian_width(0x09BE, 0x09C0, 'N').	% Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_east_asian_width(0x09C1, 0x09C4, 'N').	% Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_east_asian_width(0x09C7, 0x09C8, 'N').	% Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_east_asian_width(0x09CB, 0x09CC, 'N').	% Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_east_asian_width(0x09CD, 0x09CD, 'N').	% Mn       BENGALI SIGN VIRAMA
unicode_east_asian_width(0x09CE, 0x09CE, 'N').	% Lo       BENGALI LETTER KHANDA TA
unicode_east_asian_width(0x09D7, 0x09D7, 'N').	% Mc       BENGALI AU LENGTH MARK
unicode_east_asian_width(0x09DC, 0x09DD, 'N').	% Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_east_asian_width(0x09DF, 0x09E1, 'N').	% Lo   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
unicode_east_asian_width(0x09E2, 0x09E3, 'N').	% Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x09E6, 0x09EF, 'N').	% Nd  [10] BENGALI DIGIT ZERO..BENGALI DIGIT NINE
unicode_east_asian_width(0x09F0, 0x09F1, 'N').	% Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_east_asian_width(0x09F2, 0x09F3, 'N').	% Sc   [2] BENGALI RUPEE MARK..BENGALI RUPEE SIGN
unicode_east_asian_width(0x09F4, 0x09F9, 'N').	% No   [6] BENGALI CURRENCY NUMERATOR ONE..BENGALI CURRENCY DENOMINATOR SIXTEEN
unicode_east_asian_width(0x09FA, 0x09FA, 'N').	% So       BENGALI ISSHAR
unicode_east_asian_width(0x09FB, 0x09FB, 'N').	% Sc       BENGALI GANDA MARK
unicode_east_asian_width(0x0A01, 0x0A02, 'N').	% Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_east_asian_width(0x0A03, 0x0A03, 'N').	% Mc       GURMUKHI SIGN VISARGA
unicode_east_asian_width(0x0A05, 0x0A0A, 'N').	% Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_east_asian_width(0x0A0F, 0x0A10, 'N').	% Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_east_asian_width(0x0A13, 0x0A28, 'N').	% Lo  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
unicode_east_asian_width(0x0A2A, 0x0A30, 'N').	% Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_east_asian_width(0x0A32, 0x0A33, 'N').	% Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_east_asian_width(0x0A35, 0x0A36, 'N').	% Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_east_asian_width(0x0A38, 0x0A39, 'N').	% Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_east_asian_width(0x0A3C, 0x0A3C, 'N').	% Mn       GURMUKHI SIGN NUKTA
unicode_east_asian_width(0x0A3E, 0x0A40, 'N').	% Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_east_asian_width(0x0A41, 0x0A42, 'N').	% Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_east_asian_width(0x0A47, 0x0A48, 'N').	% Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_east_asian_width(0x0A4B, 0x0A4D, 'N').	% Mn   [3] GURMUKHI VOWEL SIGN OO..GURMUKHI SIGN VIRAMA
unicode_east_asian_width(0x0A51, 0x0A51, 'N').	% Mn       GURMUKHI SIGN UDAAT
unicode_east_asian_width(0x0A59, 0x0A5C, 'N').	% Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_east_asian_width(0x0A5E, 0x0A5E, 'N').	% Lo       GURMUKHI LETTER FA
unicode_east_asian_width(0x0A66, 0x0A6F, 'N').	% Nd  [10] GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
unicode_east_asian_width(0x0A70, 0x0A71, 'N').	% Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
unicode_east_asian_width(0x0A72, 0x0A74, 'N').	% Lo   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
unicode_east_asian_width(0x0A75, 0x0A75, 'N').	% Mn       GURMUKHI SIGN YAKASH
unicode_east_asian_width(0x0A81, 0x0A82, 'N').	% Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_east_asian_width(0x0A83, 0x0A83, 'N').	% Mc       GUJARATI SIGN VISARGA
unicode_east_asian_width(0x0A85, 0x0A8D, 'N').	% Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_east_asian_width(0x0A8F, 0x0A91, 'N').	% Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_east_asian_width(0x0A93, 0x0AA8, 'N').	% Lo  [22] GUJARATI LETTER O..GUJARATI LETTER NA
unicode_east_asian_width(0x0AAA, 0x0AB0, 'N').	% Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_east_asian_width(0x0AB2, 0x0AB3, 'N').	% Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_east_asian_width(0x0AB5, 0x0AB9, 'N').	% Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_east_asian_width(0x0ABC, 0x0ABC, 'N').	% Mn       GUJARATI SIGN NUKTA
unicode_east_asian_width(0x0ABD, 0x0ABD, 'N').	% Lo       GUJARATI SIGN AVAGRAHA
unicode_east_asian_width(0x0ABE, 0x0AC0, 'N').	% Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_east_asian_width(0x0AC1, 0x0AC5, 'N').	% Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_east_asian_width(0x0AC7, 0x0AC8, 'N').	% Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_east_asian_width(0x0AC9, 0x0AC9, 'N').	% Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_east_asian_width(0x0ACB, 0x0ACC, 'N').	% Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_east_asian_width(0x0ACD, 0x0ACD, 'N').	% Mn       GUJARATI SIGN VIRAMA
unicode_east_asian_width(0x0AD0, 0x0AD0, 'N').	% Lo       GUJARATI OM
unicode_east_asian_width(0x0AE0, 0x0AE1, 'N').	% Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_east_asian_width(0x0AE2, 0x0AE3, 'N').	% Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x0AE6, 0x0AEF, 'N').	% Nd  [10] GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
unicode_east_asian_width(0x0AF0, 0x0AF0, 'N').	% Po       GUJARATI ABBREVIATION SIGN
unicode_east_asian_width(0x0AF1, 0x0AF1, 'N').	% Sc       GUJARATI RUPEE SIGN
unicode_east_asian_width(0x0B01, 0x0B01, 'N').	% Mn       ORIYA SIGN CANDRABINDU
unicode_east_asian_width(0x0B02, 0x0B03, 'N').	% Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
unicode_east_asian_width(0x0B05, 0x0B0C, 'N').	% Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_east_asian_width(0x0B0F, 0x0B10, 'N').	% Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_east_asian_width(0x0B13, 0x0B28, 'N').	% Lo  [22] ORIYA LETTER O..ORIYA LETTER NA
unicode_east_asian_width(0x0B2A, 0x0B30, 'N').	% Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_east_asian_width(0x0B32, 0x0B33, 'N').	% Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_east_asian_width(0x0B35, 0x0B39, 'N').	% Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_east_asian_width(0x0B3C, 0x0B3C, 'N').	% Mn       ORIYA SIGN NUKTA
unicode_east_asian_width(0x0B3D, 0x0B3D, 'N').	% Lo       ORIYA SIGN AVAGRAHA
unicode_east_asian_width(0x0B3E, 0x0B3E, 'N').	% Mc       ORIYA VOWEL SIGN AA
unicode_east_asian_width(0x0B3F, 0x0B3F, 'N').	% Mn       ORIYA VOWEL SIGN I
unicode_east_asian_width(0x0B40, 0x0B40, 'N').	% Mc       ORIYA VOWEL SIGN II
unicode_east_asian_width(0x0B41, 0x0B44, 'N').	% Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_east_asian_width(0x0B47, 0x0B48, 'N').	% Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_east_asian_width(0x0B4B, 0x0B4C, 'N').	% Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_east_asian_width(0x0B4D, 0x0B4D, 'N').	% Mn       ORIYA SIGN VIRAMA
unicode_east_asian_width(0x0B56, 0x0B56, 'N').	% Mn       ORIYA AI LENGTH MARK
unicode_east_asian_width(0x0B57, 0x0B57, 'N').	% Mc       ORIYA AU LENGTH MARK
unicode_east_asian_width(0x0B5C, 0x0B5D, 'N').	% Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_east_asian_width(0x0B5F, 0x0B61, 'N').	% Lo   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
unicode_east_asian_width(0x0B62, 0x0B63, 'N').	% Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x0B66, 0x0B6F, 'N').	% Nd  [10] ORIYA DIGIT ZERO..ORIYA DIGIT NINE
unicode_east_asian_width(0x0B70, 0x0B70, 'N').	% So       ORIYA ISSHAR
unicode_east_asian_width(0x0B71, 0x0B71, 'N').	% Lo       ORIYA LETTER WA
unicode_east_asian_width(0x0B72, 0x0B77, 'N').	% No   [6] ORIYA FRACTION ONE QUARTER..ORIYA FRACTION THREE SIXTEENTHS
unicode_east_asian_width(0x0B82, 0x0B82, 'N').	% Mn       TAMIL SIGN ANUSVARA
unicode_east_asian_width(0x0B83, 0x0B83, 'N').	% Lo       TAMIL SIGN VISARGA
unicode_east_asian_width(0x0B85, 0x0B8A, 'N').	% Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_east_asian_width(0x0B8E, 0x0B90, 'N').	% Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_east_asian_width(0x0B92, 0x0B95, 'N').	% Lo   [4] TAMIL LETTER O..TAMIL LETTER KA
unicode_east_asian_width(0x0B99, 0x0B9A, 'N').	% Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_east_asian_width(0x0B9C, 0x0B9C, 'N').	% Lo       TAMIL LETTER JA
unicode_east_asian_width(0x0B9E, 0x0B9F, 'N').	% Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_east_asian_width(0x0BA3, 0x0BA4, 'N').	% Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_east_asian_width(0x0BA8, 0x0BAA, 'N').	% Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_east_asian_width(0x0BAE, 0x0BB9, 'N').	% Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_east_asian_width(0x0BBE, 0x0BBF, 'N').	% Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_east_asian_width(0x0BC0, 0x0BC0, 'N').	% Mn       TAMIL VOWEL SIGN II
unicode_east_asian_width(0x0BC1, 0x0BC2, 'N').	% Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_east_asian_width(0x0BC6, 0x0BC8, 'N').	% Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_east_asian_width(0x0BCA, 0x0BCC, 'N').	% Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_east_asian_width(0x0BCD, 0x0BCD, 'N').	% Mn       TAMIL SIGN VIRAMA
unicode_east_asian_width(0x0BD0, 0x0BD0, 'N').	% Lo       TAMIL OM
unicode_east_asian_width(0x0BD7, 0x0BD7, 'N').	% Mc       TAMIL AU LENGTH MARK
unicode_east_asian_width(0x0BE6, 0x0BEF, 'N').	% Nd  [10] TAMIL DIGIT ZERO..TAMIL DIGIT NINE
unicode_east_asian_width(0x0BF0, 0x0BF2, 'N').	% No   [3] TAMIL NUMBER TEN..TAMIL NUMBER ONE THOUSAND
unicode_east_asian_width(0x0BF3, 0x0BF8, 'N').	% So   [6] TAMIL DAY SIGN..TAMIL AS ABOVE SIGN
unicode_east_asian_width(0x0BF9, 0x0BF9, 'N').	% Sc       TAMIL RUPEE SIGN
unicode_east_asian_width(0x0BFA, 0x0BFA, 'N').	% So       TAMIL NUMBER SIGN
unicode_east_asian_width(0x0C01, 0x0C03, 'N').	% Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
unicode_east_asian_width(0x0C05, 0x0C0C, 'N').	% Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_east_asian_width(0x0C0E, 0x0C10, 'N').	% Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_east_asian_width(0x0C12, 0x0C28, 'N').	% Lo  [23] TELUGU LETTER O..TELUGU LETTER NA
unicode_east_asian_width(0x0C2A, 0x0C33, 'N').	% Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_east_asian_width(0x0C35, 0x0C39, 'N').	% Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_east_asian_width(0x0C3D, 0x0C3D, 'N').	% Lo       TELUGU SIGN AVAGRAHA
unicode_east_asian_width(0x0C3E, 0x0C40, 'N').	% Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_east_asian_width(0x0C41, 0x0C44, 'N').	% Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_east_asian_width(0x0C46, 0x0C48, 'N').	% Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_east_asian_width(0x0C4A, 0x0C4D, 'N').	% Mn   [4] TELUGU VOWEL SIGN O..TELUGU SIGN VIRAMA
unicode_east_asian_width(0x0C55, 0x0C56, 'N').	% Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_east_asian_width(0x0C58, 0x0C59, 'N').	% Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_east_asian_width(0x0C60, 0x0C61, 'N').	% Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_east_asian_width(0x0C62, 0x0C63, 'N').	% Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x0C66, 0x0C6F, 'N').	% Nd  [10] TELUGU DIGIT ZERO..TELUGU DIGIT NINE
unicode_east_asian_width(0x0C78, 0x0C7E, 'N').	% No   [7] TELUGU FRACTION DIGIT ZERO FOR ODD POWERS OF FOUR..TELUGU FRACTION DIGIT THREE FOR EVEN POWERS OF FOUR
unicode_east_asian_width(0x0C7F, 0x0C7F, 'N').	% So       TELUGU SIGN TUUMU
unicode_east_asian_width(0x0C82, 0x0C83, 'N').	% Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
unicode_east_asian_width(0x0C85, 0x0C8C, 'N').	% Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_east_asian_width(0x0C8E, 0x0C90, 'N').	% Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_east_asian_width(0x0C92, 0x0CA8, 'N').	% Lo  [23] KANNADA LETTER O..KANNADA LETTER NA
unicode_east_asian_width(0x0CAA, 0x0CB3, 'N').	% Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_east_asian_width(0x0CB5, 0x0CB9, 'N').	% Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_east_asian_width(0x0CBC, 0x0CBC, 'N').	% Mn       KANNADA SIGN NUKTA
unicode_east_asian_width(0x0CBD, 0x0CBD, 'N').	% Lo       KANNADA SIGN AVAGRAHA
unicode_east_asian_width(0x0CBE, 0x0CBE, 'N').	% Mc       KANNADA VOWEL SIGN AA
unicode_east_asian_width(0x0CBF, 0x0CBF, 'N').	% Mn       KANNADA VOWEL SIGN I
unicode_east_asian_width(0x0CC0, 0x0CC4, 'N').	% Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_east_asian_width(0x0CC6, 0x0CC6, 'N').	% Mn       KANNADA VOWEL SIGN E
unicode_east_asian_width(0x0CC7, 0x0CC8, 'N').	% Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_east_asian_width(0x0CCA, 0x0CCB, 'N').	% Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_east_asian_width(0x0CCC, 0x0CCD, 'N').	% Mn   [2] KANNADA VOWEL SIGN AU..KANNADA SIGN VIRAMA
unicode_east_asian_width(0x0CD5, 0x0CD6, 'N').	% Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_east_asian_width(0x0CDE, 0x0CDE, 'N').	% Lo       KANNADA LETTER FA
unicode_east_asian_width(0x0CE0, 0x0CE1, 'N').	% Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_east_asian_width(0x0CE2, 0x0CE3, 'N').	% Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x0CE6, 0x0CEF, 'N').	% Nd  [10] KANNADA DIGIT ZERO..KANNADA DIGIT NINE
unicode_east_asian_width(0x0CF1, 0x0CF2, 'N').	% Lo   [2] KANNADA SIGN JIHVAMULIYA..KANNADA SIGN UPADHMANIYA
unicode_east_asian_width(0x0D02, 0x0D03, 'N').	% Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
unicode_east_asian_width(0x0D05, 0x0D0C, 'N').	% Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_east_asian_width(0x0D0E, 0x0D10, 'N').	% Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_east_asian_width(0x0D12, 0x0D3A, 'N').	% Lo  [41] MALAYALAM LETTER O..MALAYALAM LETTER TTTA
unicode_east_asian_width(0x0D3D, 0x0D3D, 'N').	% Lo       MALAYALAM SIGN AVAGRAHA
unicode_east_asian_width(0x0D3E, 0x0D40, 'N').	% Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_east_asian_width(0x0D41, 0x0D44, 'N').	% Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_east_asian_width(0x0D46, 0x0D48, 'N').	% Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_east_asian_width(0x0D4A, 0x0D4C, 'N').	% Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_east_asian_width(0x0D4D, 0x0D4D, 'N').	% Mn       MALAYALAM SIGN VIRAMA
unicode_east_asian_width(0x0D4E, 0x0D4E, 'N').	% Lo       MALAYALAM LETTER DOT REPH
unicode_east_asian_width(0x0D57, 0x0D57, 'N').	% Mc       MALAYALAM AU LENGTH MARK
unicode_east_asian_width(0x0D60, 0x0D61, 'N').	% Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_east_asian_width(0x0D62, 0x0D63, 'N').	% Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x0D66, 0x0D6F, 'N').	% Nd  [10] MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
unicode_east_asian_width(0x0D70, 0x0D75, 'N').	% No   [6] MALAYALAM NUMBER TEN..MALAYALAM FRACTION THREE QUARTERS
unicode_east_asian_width(0x0D79, 0x0D79, 'N').	% So       MALAYALAM DATE MARK
unicode_east_asian_width(0x0D7A, 0x0D7F, 'N').	% Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K
unicode_east_asian_width(0x0D82, 0x0D83, 'N').	% Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
unicode_east_asian_width(0x0D85, 0x0D96, 'N').	% Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_east_asian_width(0x0D9A, 0x0DB1, 'N').	% Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_east_asian_width(0x0DB3, 0x0DBB, 'N').	% Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_east_asian_width(0x0DBD, 0x0DBD, 'N').	% Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_east_asian_width(0x0DC0, 0x0DC6, 'N').	% Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_east_asian_width(0x0DCA, 0x0DCA, 'N').	% Mn       SINHALA SIGN AL-LAKUNA
unicode_east_asian_width(0x0DCF, 0x0DD1, 'N').	% Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_east_asian_width(0x0DD2, 0x0DD4, 'N').	% Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_east_asian_width(0x0DD6, 0x0DD6, 'N').	% Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_east_asian_width(0x0DD8, 0x0DDF, 'N').	% Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_east_asian_width(0x0DF2, 0x0DF3, 'N').	% Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_east_asian_width(0x0DF4, 0x0DF4, 'N').	% Po       SINHALA PUNCTUATION KUNDDALIYA
unicode_east_asian_width(0x0E01, 0x0E30, 'N').	% Lo  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
unicode_east_asian_width(0x0E31, 0x0E31, 'N').	% Mn       THAI CHARACTER MAI HAN-AKAT
unicode_east_asian_width(0x0E32, 0x0E33, 'N').	% Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_east_asian_width(0x0E34, 0x0E3A, 'N').	% Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
unicode_east_asian_width(0x0E3F, 0x0E3F, 'N').	% Sc       THAI CURRENCY SYMBOL BAHT
unicode_east_asian_width(0x0E40, 0x0E45, 'N').	% Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_east_asian_width(0x0E46, 0x0E46, 'N').	% Lm       THAI CHARACTER MAIYAMOK
unicode_east_asian_width(0x0E47, 0x0E4E, 'N').	% Mn   [8] THAI CHARACTER MAITAIKHU..THAI CHARACTER YAMAKKAN
unicode_east_asian_width(0x0E4F, 0x0E4F, 'N').	% Po       THAI CHARACTER FONGMAN
unicode_east_asian_width(0x0E50, 0x0E59, 'N').	% Nd  [10] THAI DIGIT ZERO..THAI DIGIT NINE
unicode_east_asian_width(0x0E5A, 0x0E5B, 'N').	% Po   [2] THAI CHARACTER ANGKHANKHU..THAI CHARACTER KHOMUT
unicode_east_asian_width(0x0E81, 0x0E82, 'N').	% Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_east_asian_width(0x0E84, 0x0E84, 'N').	% Lo       LAO LETTER KHO TAM
unicode_east_asian_width(0x0E87, 0x0E88, 'N').	% Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_east_asian_width(0x0E8A, 0x0E8A, 'N').	% Lo       LAO LETTER SO TAM
unicode_east_asian_width(0x0E8D, 0x0E8D, 'N').	% Lo       LAO LETTER NYO
unicode_east_asian_width(0x0E94, 0x0E97, 'N').	% Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_east_asian_width(0x0E99, 0x0E9F, 'N').	% Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_east_asian_width(0x0EA1, 0x0EA3, 'N').	% Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_east_asian_width(0x0EA5, 0x0EA5, 'N').	% Lo       LAO LETTER LO LOOT
unicode_east_asian_width(0x0EA7, 0x0EA7, 'N').	% Lo       LAO LETTER WO
unicode_east_asian_width(0x0EAA, 0x0EAB, 'N').	% Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_east_asian_width(0x0EAD, 0x0EB0, 'N').	% Lo   [4] LAO LETTER O..LAO VOWEL SIGN A
unicode_east_asian_width(0x0EB1, 0x0EB1, 'N').	% Mn       LAO VOWEL SIGN MAI KAN
unicode_east_asian_width(0x0EB2, 0x0EB3, 'N').	% Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_east_asian_width(0x0EB4, 0x0EB9, 'N').	% Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_east_asian_width(0x0EBB, 0x0EBC, 'N').	% Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
unicode_east_asian_width(0x0EBD, 0x0EBD, 'N').	% Lo       LAO SEMIVOWEL SIGN NYO
unicode_east_asian_width(0x0EC0, 0x0EC4, 'N').	% Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_east_asian_width(0x0EC6, 0x0EC6, 'N').	% Lm       LAO KO LA
unicode_east_asian_width(0x0EC8, 0x0ECD, 'N').	% Mn   [6] LAO TONE MAI EK..LAO NIGGAHITA
unicode_east_asian_width(0x0ED0, 0x0ED9, 'N').	% Nd  [10] LAO DIGIT ZERO..LAO DIGIT NINE
unicode_east_asian_width(0x0EDC, 0x0EDF, 'N').	% Lo   [4] LAO HO NO..LAO LETTER KHMU NYO
unicode_east_asian_width(0x0F00, 0x0F00, 'N').	% Lo       TIBETAN SYLLABLE OM
unicode_east_asian_width(0x0F01, 0x0F03, 'N').	% So   [3] TIBETAN MARK GTER YIG MGO TRUNCATED A..TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA
unicode_east_asian_width(0x0F04, 0x0F12, 'N').	% Po  [15] TIBETAN MARK INITIAL YIG MGO MDUN MA..TIBETAN MARK RGYA GRAM SHAD
unicode_east_asian_width(0x0F13, 0x0F13, 'N').	% So       TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN
unicode_east_asian_width(0x0F14, 0x0F14, 'N').	% Po       TIBETAN MARK GTER TSHEG
unicode_east_asian_width(0x0F15, 0x0F17, 'N').	% So   [3] TIBETAN LOGOTYPE SIGN CHAD RTAGS..TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS
unicode_east_asian_width(0x0F18, 0x0F19, 'N').	% Mn   [2] TIBETAN ASTROLOGICAL SIGN -KHYUD PA..TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
unicode_east_asian_width(0x0F1A, 0x0F1F, 'N').	% So   [6] TIBETAN SIGN RDEL DKAR GCIG..TIBETAN SIGN RDEL DKAR RDEL NAG
unicode_east_asian_width(0x0F20, 0x0F29, 'N').	% Nd  [10] TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
unicode_east_asian_width(0x0F2A, 0x0F33, 'N').	% No  [10] TIBETAN DIGIT HALF ONE..TIBETAN DIGIT HALF ZERO
unicode_east_asian_width(0x0F34, 0x0F34, 'N').	% So       TIBETAN MARK BSDUS RTAGS
unicode_east_asian_width(0x0F35, 0x0F35, 'N').	% Mn       TIBETAN MARK NGAS BZUNG NYI ZLA
unicode_east_asian_width(0x0F36, 0x0F36, 'N').	% So       TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN
unicode_east_asian_width(0x0F37, 0x0F37, 'N').	% Mn       TIBETAN MARK NGAS BZUNG SGOR RTAGS
unicode_east_asian_width(0x0F38, 0x0F38, 'N').	% So       TIBETAN MARK CHE MGO
unicode_east_asian_width(0x0F39, 0x0F39, 'N').	% Mn       TIBETAN MARK TSA -PHRU
unicode_east_asian_width(0x0F3A, 0x0F3A, 'N').	% Ps       TIBETAN MARK GUG RTAGS GYON
unicode_east_asian_width(0x0F3B, 0x0F3B, 'N').	% Pe       TIBETAN MARK GUG RTAGS GYAS
unicode_east_asian_width(0x0F3C, 0x0F3C, 'N').	% Ps       TIBETAN MARK ANG KHANG GYON
unicode_east_asian_width(0x0F3D, 0x0F3D, 'N').	% Pe       TIBETAN MARK ANG KHANG GYAS
unicode_east_asian_width(0x0F3E, 0x0F3F, 'N').	% Mc   [2] TIBETAN SIGN YAR TSHES..TIBETAN SIGN MAR TSHES
unicode_east_asian_width(0x0F40, 0x0F47, 'N').	% Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_east_asian_width(0x0F49, 0x0F6C, 'N').	% Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_east_asian_width(0x0F71, 0x0F7E, 'N').	% Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
unicode_east_asian_width(0x0F7F, 0x0F7F, 'N').	% Mc       TIBETAN SIGN RNAM BCAD
unicode_east_asian_width(0x0F80, 0x0F84, 'N').	% Mn   [5] TIBETAN VOWEL SIGN REVERSED I..TIBETAN MARK HALANTA
unicode_east_asian_width(0x0F85, 0x0F85, 'N').	% Po       TIBETAN MARK PALUTA
unicode_east_asian_width(0x0F86, 0x0F87, 'N').	% Mn   [2] TIBETAN SIGN LCI RTAGS..TIBETAN SIGN YANG RTAGS
unicode_east_asian_width(0x0F88, 0x0F8C, 'N').	% Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN
unicode_east_asian_width(0x0F8D, 0x0F97, 'N').	% Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_east_asian_width(0x0F99, 0x0FBC, 'N').	% Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_east_asian_width(0x0FBE, 0x0FC5, 'N').	% So   [8] TIBETAN KU RU KHA..TIBETAN SYMBOL RDO RJE
unicode_east_asian_width(0x0FC6, 0x0FC6, 'N').	% Mn       TIBETAN SYMBOL PADMA GDAN
unicode_east_asian_width(0x0FC7, 0x0FCC, 'N').	% So   [6] TIBETAN SYMBOL RDO RJE RGYA GRAM..TIBETAN SYMBOL NOR BU BZHI -KHYIL
unicode_east_asian_width(0x0FCE, 0x0FCF, 'N').	% So   [2] TIBETAN SIGN RDEL NAG RDEL DKAR..TIBETAN SIGN RDEL NAG GSUM
unicode_east_asian_width(0x0FD0, 0x0FD4, 'N').	% Po   [5] TIBETAN MARK BSKA- SHOG GI MGO RGYAN..TIBETAN MARK CLOSING BRDA RNYING YIG MGO SGAB MA
unicode_east_asian_width(0x0FD5, 0x0FD8, 'N').	% So   [4] RIGHT-FACING SVASTI SIGN..LEFT-FACING SVASTI SIGN WITH DOTS
unicode_east_asian_width(0x0FD9, 0x0FDA, 'N').	% Po   [2] TIBETAN MARK LEADING MCHAN RTAGS..TIBETAN MARK TRAILING MCHAN RTAGS
unicode_east_asian_width(0x1000, 0x102A, 'N').	% Lo  [43] MYANMAR LETTER KA..MYANMAR LETTER AU
unicode_east_asian_width(0x102B, 0x102C, 'N').	% Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_east_asian_width(0x102D, 0x1030, 'N').	% Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_east_asian_width(0x1031, 0x1031, 'N').	% Mc       MYANMAR VOWEL SIGN E
unicode_east_asian_width(0x1032, 0x1037, 'N').	% Mn   [6] MYANMAR VOWEL SIGN AI..MYANMAR SIGN DOT BELOW
unicode_east_asian_width(0x1038, 0x1038, 'N').	% Mc       MYANMAR SIGN VISARGA
unicode_east_asian_width(0x1039, 0x103A, 'N').	% Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_east_asian_width(0x103B, 0x103C, 'N').	% Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_east_asian_width(0x103D, 0x103E, 'N').	% Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_east_asian_width(0x103F, 0x103F, 'N').	% Lo       MYANMAR LETTER GREAT SA
unicode_east_asian_width(0x1040, 0x1049, 'N').	% Nd  [10] MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
unicode_east_asian_width(0x104A, 0x104F, 'N').	% Po   [6] MYANMAR SIGN LITTLE SECTION..MYANMAR SYMBOL GENITIVE
unicode_east_asian_width(0x1050, 0x1055, 'N').	% Lo   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
unicode_east_asian_width(0x1056, 0x1057, 'N').	% Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_east_asian_width(0x1058, 0x1059, 'N').	% Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_east_asian_width(0x105A, 0x105D, 'N').	% Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_east_asian_width(0x105E, 0x1060, 'N').	% Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_east_asian_width(0x1061, 0x1061, 'N').	% Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_east_asian_width(0x1062, 0x1064, 'N').	% Mc   [3] MYANMAR VOWEL SIGN SGAW KAREN EU..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_east_asian_width(0x1065, 0x1066, 'N').	% Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_east_asian_width(0x1067, 0x106D, 'N').	% Mc   [7] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_east_asian_width(0x106E, 0x1070, 'N').	% Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_east_asian_width(0x1071, 0x1074, 'N').	% Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_east_asian_width(0x1075, 0x1081, 'N').	% Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_east_asian_width(0x1082, 0x1082, 'N').	% Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_east_asian_width(0x1083, 0x1084, 'N').	% Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_east_asian_width(0x1085, 0x1086, 'N').	% Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_east_asian_width(0x1087, 0x108C, 'N').	% Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_east_asian_width(0x108D, 0x108D, 'N').	% Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_east_asian_width(0x108E, 0x108E, 'N').	% Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_east_asian_width(0x108F, 0x108F, 'N').	% Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_east_asian_width(0x1090, 0x1099, 'N').	% Nd  [10] MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
unicode_east_asian_width(0x109A, 0x109C, 'N').	% Mc   [3] MYANMAR SIGN KHAMTI TONE-1..MYANMAR VOWEL SIGN AITON A
unicode_east_asian_width(0x109D, 0x109D, 'N').	% Mn       MYANMAR VOWEL SIGN AITON AI
unicode_east_asian_width(0x109E, 0x109F, 'N').	% So   [2] MYANMAR SYMBOL SHAN ONE..MYANMAR SYMBOL SHAN EXCLAMATION
unicode_east_asian_width(0x10A0, 0x10C5, 'N').	% L&  [38] GEORGIAN CAPITAL LETTER AN..GEORGIAN CAPITAL LETTER HOE
unicode_east_asian_width(0x10C7, 0x10C7, 'N').	% L&       GEORGIAN CAPITAL LETTER YN
unicode_east_asian_width(0x10CD, 0x10CD, 'N').	% L&       GEORGIAN CAPITAL LETTER AEN
unicode_east_asian_width(0x10D0, 0x10FA, 'N').	% Lo  [43] GEORGIAN LETTER AN..GEORGIAN LETTER AIN
unicode_east_asian_width(0x10FB, 0x10FB, 'N').	% Po       GEORGIAN PARAGRAPH SEPARATOR
unicode_east_asian_width(0x10FC, 0x10FC, 'N').	% Lm       MODIFIER LETTER GEORGIAN NAR
unicode_east_asian_width(0x10FD, 0x10FF, 'N').	% Lo   [3] GEORGIAN LETTER AEN..GEORGIAN LETTER LABIAL SIGN
unicode_east_asian_width(0x1160, 0x11A2, 'N').	% Lo  [67] HANGUL JUNGSEONG FILLER..HANGUL JUNGSEONG SSANGARAEA
unicode_east_asian_width(0x11A8, 0x11F9, 'N').	% Lo  [82] HANGUL JONGSEONG KIYEOK..HANGUL JONGSEONG YEORINHIEUH
unicode_east_asian_width(0x1200, 0x1248, 'N').	% Lo  [73] ETHIOPIC SYLLABLE HA..ETHIOPIC SYLLABLE QWA
unicode_east_asian_width(0x124A, 0x124D, 'N').	% Lo   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
unicode_east_asian_width(0x1250, 0x1256, 'N').	% Lo   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
unicode_east_asian_width(0x1258, 0x1258, 'N').	% Lo       ETHIOPIC SYLLABLE QHWA
unicode_east_asian_width(0x125A, 0x125D, 'N').	% Lo   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
unicode_east_asian_width(0x1260, 0x1288, 'N').	% Lo  [41] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XWA
unicode_east_asian_width(0x128A, 0x128D, 'N').	% Lo   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
unicode_east_asian_width(0x1290, 0x12B0, 'N').	% Lo  [33] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KWA
unicode_east_asian_width(0x12B2, 0x12B5, 'N').	% Lo   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
unicode_east_asian_width(0x12B8, 0x12BE, 'N').	% Lo   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
unicode_east_asian_width(0x12C0, 0x12C0, 'N').	% Lo       ETHIOPIC SYLLABLE KXWA
unicode_east_asian_width(0x12C2, 0x12C5, 'N').	% Lo   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
unicode_east_asian_width(0x12C8, 0x12D6, 'N').	% Lo  [15] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE PHARYNGEAL O
unicode_east_asian_width(0x12D8, 0x1310, 'N').	% Lo  [57] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE GWA
unicode_east_asian_width(0x1312, 0x1315, 'N').	% Lo   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
unicode_east_asian_width(0x1318, 0x135A, 'N').	% Lo  [67] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE FYA
unicode_east_asian_width(0x135D, 0x135F, 'N').	% Mn   [3] ETHIOPIC COMBINING GEMINATION AND VOWEL LENGTH MARK..ETHIOPIC COMBINING GEMINATION MARK
unicode_east_asian_width(0x1360, 0x1368, 'N').	% Po   [9] ETHIOPIC SECTION MARK..ETHIOPIC PARAGRAPH SEPARATOR
unicode_east_asian_width(0x1369, 0x137C, 'N').	% No  [20] ETHIOPIC DIGIT ONE..ETHIOPIC NUMBER TEN THOUSAND
unicode_east_asian_width(0x1380, 0x138F, 'N').	% Lo  [16] ETHIOPIC SYLLABLE SEBATBEIT MWA..ETHIOPIC SYLLABLE PWE
unicode_east_asian_width(0x1390, 0x1399, 'N').	% So  [10] ETHIOPIC TONAL MARK YIZET..ETHIOPIC TONAL MARK KURT
unicode_east_asian_width(0x13A0, 0x13F4, 'N').	% Lo  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
unicode_east_asian_width(0x1400, 0x1400, 'N').	% Pd       CANADIAN SYLLABICS HYPHEN
unicode_east_asian_width(0x1401, 0x166C, 'N').	% Lo [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
unicode_east_asian_width(0x166D, 0x166E, 'N').	% Po   [2] CANADIAN SYLLABICS CHI SIGN..CANADIAN SYLLABICS FULL STOP
unicode_east_asian_width(0x166F, 0x167F, 'N').	% Lo  [17] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS BLACKFOOT W
unicode_east_asian_width(0x1680, 0x1680, 'N').	% Zs       OGHAM SPACE MARK
unicode_east_asian_width(0x1681, 0x169A, 'N').	% Lo  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
unicode_east_asian_width(0x169B, 0x169B, 'N').	% Ps       OGHAM FEATHER MARK
unicode_east_asian_width(0x169C, 0x169C, 'N').	% Pe       OGHAM REVERSED FEATHER MARK
unicode_east_asian_width(0x16A0, 0x16EA, 'N').	% Lo  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
unicode_east_asian_width(0x16EB, 0x16ED, 'N').	% Po   [3] RUNIC SINGLE PUNCTUATION..RUNIC CROSS PUNCTUATION
unicode_east_asian_width(0x16EE, 0x16F0, 'N').	% Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
unicode_east_asian_width(0x1700, 0x170C, 'N').	% Lo  [13] TAGALOG LETTER A..TAGALOG LETTER YA
unicode_east_asian_width(0x170E, 0x1711, 'N').	% Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_east_asian_width(0x1712, 0x1714, 'N').	% Mn   [3] TAGALOG VOWEL SIGN I..TAGALOG SIGN VIRAMA
unicode_east_asian_width(0x1720, 0x1731, 'N').	% Lo  [18] HANUNOO LETTER A..HANUNOO LETTER HA
unicode_east_asian_width(0x1732, 0x1734, 'N').	% Mn   [3] HANUNOO VOWEL SIGN I..HANUNOO SIGN PAMUDPOD
unicode_east_asian_width(0x1735, 0x1736, 'N').	% Po   [2] PHILIPPINE SINGLE PUNCTUATION..PHILIPPINE DOUBLE PUNCTUATION
unicode_east_asian_width(0x1740, 0x1751, 'N').	% Lo  [18] BUHID LETTER A..BUHID LETTER HA
unicode_east_asian_width(0x1752, 0x1753, 'N').	% Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_east_asian_width(0x1760, 0x176C, 'N').	% Lo  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
unicode_east_asian_width(0x176E, 0x1770, 'N').	% Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_east_asian_width(0x1772, 0x1773, 'N').	% Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_east_asian_width(0x1780, 0x17B3, 'N').	% Lo  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
unicode_east_asian_width(0x17B4, 0x17B5, 'N').	% Mn   [2] KHMER VOWEL INHERENT AQ..KHMER VOWEL INHERENT AA
unicode_east_asian_width(0x17B6, 0x17B6, 'N').	% Mc       KHMER VOWEL SIGN AA
unicode_east_asian_width(0x17B7, 0x17BD, 'N').	% Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_east_asian_width(0x17BE, 0x17C5, 'N').	% Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_east_asian_width(0x17C6, 0x17C6, 'N').	% Mn       KHMER SIGN NIKAHIT
unicode_east_asian_width(0x17C7, 0x17C8, 'N').	% Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
unicode_east_asian_width(0x17C9, 0x17D3, 'N').	% Mn  [11] KHMER SIGN MUUSIKATOAN..KHMER SIGN BATHAMASAT
unicode_east_asian_width(0x17D4, 0x17D6, 'N').	% Po   [3] KHMER SIGN KHAN..KHMER SIGN CAMNUC PII KUUH
unicode_east_asian_width(0x17D7, 0x17D7, 'N').	% Lm       KHMER SIGN LEK TOO
unicode_east_asian_width(0x17D8, 0x17DA, 'N').	% Po   [3] KHMER SIGN BEYYAL..KHMER SIGN KOOMUUT
unicode_east_asian_width(0x17DB, 0x17DB, 'N').	% Sc       KHMER CURRENCY SYMBOL RIEL
unicode_east_asian_width(0x17DC, 0x17DC, 'N').	% Lo       KHMER SIGN AVAKRAHASANYA
unicode_east_asian_width(0x17DD, 0x17DD, 'N').	% Mn       KHMER SIGN ATTHACAN
unicode_east_asian_width(0x17E0, 0x17E9, 'N').	% Nd  [10] KHMER DIGIT ZERO..KHMER DIGIT NINE
unicode_east_asian_width(0x17F0, 0x17F9, 'N').	% No  [10] KHMER SYMBOL LEK ATTAK SON..KHMER SYMBOL LEK ATTAK PRAM-BUON
unicode_east_asian_width(0x1800, 0x1805, 'N').	% Po   [6] MONGOLIAN BIRGA..MONGOLIAN FOUR DOTS
unicode_east_asian_width(0x1806, 0x1806, 'N').	% Pd       MONGOLIAN TODO SOFT HYPHEN
unicode_east_asian_width(0x1807, 0x180A, 'N').	% Po   [4] MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER..MONGOLIAN NIRUGU
unicode_east_asian_width(0x180B, 0x180D, 'N').	% Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
unicode_east_asian_width(0x180E, 0x180E, 'N').	% Zs       MONGOLIAN VOWEL SEPARATOR
unicode_east_asian_width(0x1810, 0x1819, 'N').	% Nd  [10] MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
unicode_east_asian_width(0x1820, 0x1842, 'N').	% Lo  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
unicode_east_asian_width(0x1843, 0x1843, 'N').	% Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
unicode_east_asian_width(0x1844, 0x1877, 'N').	% Lo  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
unicode_east_asian_width(0x1880, 0x18A8, 'N').	% Lo  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
unicode_east_asian_width(0x18A9, 0x18A9, 'N').	% Mn       MONGOLIAN LETTER ALI GALI DAGALGA
unicode_east_asian_width(0x18AA, 0x18AA, 'N').	% Lo       MONGOLIAN LETTER MANCHU ALI GALI LHA
unicode_east_asian_width(0x18B0, 0x18F5, 'N').	% Lo  [70] CANADIAN SYLLABICS OY..CANADIAN SYLLABICS CARRIER DENTAL S
unicode_east_asian_width(0x1900, 0x191C, 'N').	% Lo  [29] LIMBU VOWEL-CARRIER LETTER..LIMBU LETTER HA
unicode_east_asian_width(0x1920, 0x1922, 'N').	% Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_east_asian_width(0x1923, 0x1926, 'N').	% Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_east_asian_width(0x1927, 0x1928, 'N').	% Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_east_asian_width(0x1929, 0x192B, 'N').	% Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_east_asian_width(0x1930, 0x1931, 'N').	% Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_east_asian_width(0x1932, 0x1932, 'N').	% Mn       LIMBU SMALL LETTER ANUSVARA
unicode_east_asian_width(0x1933, 0x1938, 'N').	% Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_east_asian_width(0x1939, 0x193B, 'N').	% Mn   [3] LIMBU SIGN MUKPHRENG..LIMBU SIGN SA-I
unicode_east_asian_width(0x1940, 0x1940, 'N').	% So       LIMBU SIGN LOO
unicode_east_asian_width(0x1944, 0x1945, 'N').	% Po   [2] LIMBU EXCLAMATION MARK..LIMBU QUESTION MARK
unicode_east_asian_width(0x1946, 0x194F, 'N').	% Nd  [10] LIMBU DIGIT ZERO..LIMBU DIGIT NINE
unicode_east_asian_width(0x1950, 0x196D, 'N').	% Lo  [30] TAI LE LETTER KA..TAI LE LETTER AI
unicode_east_asian_width(0x1970, 0x1974, 'N').	% Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_east_asian_width(0x1980, 0x19AB, 'N').	% Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_east_asian_width(0x19B0, 0x19C0, 'N').	% Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_east_asian_width(0x19C1, 0x19C7, 'N').	% Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_east_asian_width(0x19C8, 0x19C9, 'N').	% Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_east_asian_width(0x19D0, 0x19D9, 'N').	% Nd  [10] NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
unicode_east_asian_width(0x19DA, 0x19DA, 'N').	% No       NEW TAI LUE THAM DIGIT ONE
unicode_east_asian_width(0x19DE, 0x19FF, 'N').	% So  [34] NEW TAI LUE SIGN LAE..KHMER SYMBOL DAP-PRAM ROC
unicode_east_asian_width(0x1A00, 0x1A16, 'N').	% Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_east_asian_width(0x1A17, 0x1A18, 'N').	% Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_east_asian_width(0x1A19, 0x1A1B, 'N').	% Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_east_asian_width(0x1A1E, 0x1A1F, 'N').	% Po   [2] BUGINESE PALLAWA..BUGINESE END OF SECTION
unicode_east_asian_width(0x1A20, 0x1A54, 'N').	% Lo  [53] TAI THAM LETTER HIGH KA..TAI THAM LETTER GREAT SA
unicode_east_asian_width(0x1A55, 0x1A55, 'N').	% Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_east_asian_width(0x1A56, 0x1A56, 'N').	% Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_east_asian_width(0x1A57, 0x1A57, 'N').	% Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_east_asian_width(0x1A58, 0x1A5E, 'N').	% Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_east_asian_width(0x1A60, 0x1A60, 'N').	% Mn       TAI THAM SIGN SAKOT
unicode_east_asian_width(0x1A61, 0x1A61, 'N').	% Mc       TAI THAM VOWEL SIGN A
unicode_east_asian_width(0x1A62, 0x1A62, 'N').	% Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_east_asian_width(0x1A63, 0x1A64, 'N').	% Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_east_asian_width(0x1A65, 0x1A6C, 'N').	% Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_east_asian_width(0x1A6D, 0x1A72, 'N').	% Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_east_asian_width(0x1A73, 0x1A7C, 'N').	% Mn  [10] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN KHUEN-LUE KARAN
unicode_east_asian_width(0x1A7F, 0x1A7F, 'N').	% Mn       TAI THAM COMBINING CRYPTOGRAMMIC DOT
unicode_east_asian_width(0x1A80, 0x1A89, 'N').	% Nd  [10] TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
unicode_east_asian_width(0x1A90, 0x1A99, 'N').	% Nd  [10] TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
unicode_east_asian_width(0x1AA0, 0x1AA6, 'N').	% Po   [7] TAI THAM SIGN WIANG..TAI THAM SIGN REVERSED ROTATED RANA
unicode_east_asian_width(0x1AA7, 0x1AA7, 'N').	% Lm       TAI THAM SIGN MAI YAMOK
unicode_east_asian_width(0x1AA8, 0x1AAD, 'N').	% Po   [6] TAI THAM SIGN KAAN..TAI THAM SIGN CAANG
unicode_east_asian_width(0x1B00, 0x1B03, 'N').	% Mn   [4] BALINESE SIGN ULU RICEM..BALINESE SIGN SURANG
unicode_east_asian_width(0x1B04, 0x1B04, 'N').	% Mc       BALINESE SIGN BISAH
unicode_east_asian_width(0x1B05, 0x1B33, 'N').	% Lo  [47] BALINESE LETTER AKARA..BALINESE LETTER HA
unicode_east_asian_width(0x1B34, 0x1B34, 'N').	% Mn       BALINESE SIGN REREKAN
unicode_east_asian_width(0x1B35, 0x1B35, 'N').	% Mc       BALINESE VOWEL SIGN TEDUNG
unicode_east_asian_width(0x1B36, 0x1B3A, 'N').	% Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_east_asian_width(0x1B3B, 0x1B3B, 'N').	% Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_east_asian_width(0x1B3C, 0x1B3C, 'N').	% Mn       BALINESE VOWEL SIGN LA LENGA
unicode_east_asian_width(0x1B3D, 0x1B41, 'N').	% Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_east_asian_width(0x1B42, 0x1B42, 'N').	% Mn       BALINESE VOWEL SIGN PEPET
unicode_east_asian_width(0x1B43, 0x1B44, 'N').	% Mc   [2] BALINESE VOWEL SIGN PEPET TEDUNG..BALINESE ADEG ADEG
unicode_east_asian_width(0x1B45, 0x1B4B, 'N').	% Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_east_asian_width(0x1B50, 0x1B59, 'N').	% Nd  [10] BALINESE DIGIT ZERO..BALINESE DIGIT NINE
unicode_east_asian_width(0x1B5A, 0x1B60, 'N').	% Po   [7] BALINESE PANTI..BALINESE PAMENENG
unicode_east_asian_width(0x1B61, 0x1B6A, 'N').	% So  [10] BALINESE MUSICAL SYMBOL DONG..BALINESE MUSICAL SYMBOL DANG GEDE
unicode_east_asian_width(0x1B6B, 0x1B73, 'N').	% Mn   [9] BALINESE MUSICAL SYMBOL COMBINING TEGEH..BALINESE MUSICAL SYMBOL COMBINING GONG
unicode_east_asian_width(0x1B74, 0x1B7C, 'N').	% So   [9] BALINESE MUSICAL SYMBOL RIGHT-HAND OPEN DUG..BALINESE MUSICAL SYMBOL LEFT-HAND OPEN PING
unicode_east_asian_width(0x1B80, 0x1B81, 'N').	% Mn   [2] SUNDANESE SIGN PANYECEK..SUNDANESE SIGN PANGLAYAR
unicode_east_asian_width(0x1B82, 0x1B82, 'N').	% Mc       SUNDANESE SIGN PANGWISAD
unicode_east_asian_width(0x1B83, 0x1BA0, 'N').	% Lo  [30] SUNDANESE LETTER A..SUNDANESE LETTER HA
unicode_east_asian_width(0x1BA1, 0x1BA1, 'N').	% Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_east_asian_width(0x1BA2, 0x1BA5, 'N').	% Mn   [4] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE VOWEL SIGN PANYUKU
unicode_east_asian_width(0x1BA6, 0x1BA7, 'N').	% Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_east_asian_width(0x1BA8, 0x1BA9, 'N').	% Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_east_asian_width(0x1BAA, 0x1BAA, 'N').	% Mc       SUNDANESE SIGN PAMAAEH
unicode_east_asian_width(0x1BAB, 0x1BAB, 'N').	% Mn       SUNDANESE SIGN VIRAMA
unicode_east_asian_width(0x1BAC, 0x1BAD, 'N').	% Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_east_asian_width(0x1BAE, 0x1BAF, 'N').	% Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_east_asian_width(0x1BB0, 0x1BB9, 'N').	% Nd  [10] SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
unicode_east_asian_width(0x1BBA, 0x1BE5, 'N').	% Lo  [44] SUNDANESE AVAGRAHA..BATAK LETTER U
unicode_east_asian_width(0x1BE6, 0x1BE6, 'N').	% Mn       BATAK SIGN TOMPI
unicode_east_asian_width(0x1BE7, 0x1BE7, 'N').	% Mc       BATAK VOWEL SIGN E
unicode_east_asian_width(0x1BE8, 0x1BE9, 'N').	% Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_east_asian_width(0x1BEA, 0x1BEC, 'N').	% Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_east_asian_width(0x1BED, 0x1BED, 'N').	% Mn       BATAK VOWEL SIGN KARO O
unicode_east_asian_width(0x1BEE, 0x1BEE, 'N').	% Mc       BATAK VOWEL SIGN U
unicode_east_asian_width(0x1BEF, 0x1BF1, 'N').	% Mn   [3] BATAK VOWEL SIGN U FOR SIMALUNGUN SA..BATAK CONSONANT SIGN H
unicode_east_asian_width(0x1BF2, 0x1BF3, 'N').	% Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_east_asian_width(0x1BFC, 0x1BFF, 'N').	% Po   [4] BATAK SYMBOL BINDU NA METEK..BATAK SYMBOL BINDU PANGOLAT
unicode_east_asian_width(0x1C00, 0x1C23, 'N').	% Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_east_asian_width(0x1C24, 0x1C2B, 'N').	% Mc   [8] LEPCHA SUBJOINED LETTER YA..LEPCHA VOWEL SIGN UU
unicode_east_asian_width(0x1C2C, 0x1C33, 'N').	% Mn   [8] LEPCHA VOWEL SIGN E..LEPCHA CONSONANT SIGN T
unicode_east_asian_width(0x1C34, 0x1C35, 'N').	% Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_east_asian_width(0x1C36, 0x1C37, 'N').	% Mn   [2] LEPCHA SIGN RAN..LEPCHA SIGN NUKTA
unicode_east_asian_width(0x1C3B, 0x1C3F, 'N').	% Po   [5] LEPCHA PUNCTUATION TA-ROL..LEPCHA PUNCTUATION TSHOOK
unicode_east_asian_width(0x1C40, 0x1C49, 'N').	% Nd  [10] LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
unicode_east_asian_width(0x1C4D, 0x1C4F, 'N').	% Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_east_asian_width(0x1C50, 0x1C59, 'N').	% Nd  [10] OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
unicode_east_asian_width(0x1C5A, 0x1C77, 'N').	% Lo  [30] OL CHIKI LETTER LA..OL CHIKI LETTER OH
unicode_east_asian_width(0x1C78, 0x1C7D, 'N').	% Lm   [6] OL CHIKI MU TTUDDAG..OL CHIKI AHAD
unicode_east_asian_width(0x1C7E, 0x1C7F, 'N').	% Po   [2] OL CHIKI PUNCTUATION MUCAAD..OL CHIKI PUNCTUATION DOUBLE MUCAAD
unicode_east_asian_width(0x1CC0, 0x1CC7, 'N').	% Po   [8] SUNDANESE PUNCTUATION BINDU SURYA..SUNDANESE PUNCTUATION BINDU BA SATANGA
unicode_east_asian_width(0x1CD0, 0x1CD2, 'N').	% Mn   [3] VEDIC TONE KARSHANA..VEDIC TONE PRENKHA
unicode_east_asian_width(0x1CD3, 0x1CD3, 'N').	% Po       VEDIC SIGN NIHSHVASA
unicode_east_asian_width(0x1CD4, 0x1CE0, 'N').	% Mn  [13] VEDIC SIGN YAJURVEDIC MIDLINE SVARITA..VEDIC TONE RIGVEDIC KASHMIRI INDEPENDENT SVARITA
unicode_east_asian_width(0x1CE1, 0x1CE1, 'N').	% Mc       VEDIC TONE ATHARVAVEDIC INDEPENDENT SVARITA
unicode_east_asian_width(0x1CE2, 0x1CE8, 'N').	% Mn   [7] VEDIC SIGN VISARGA SVARITA..VEDIC SIGN VISARGA ANUDATTA WITH TAIL
unicode_east_asian_width(0x1CE9, 0x1CEC, 'N').	% Lo   [4] VEDIC SIGN ANUSVARA ANTARGOMUKHA..VEDIC SIGN ANUSVARA VAMAGOMUKHA WITH TAIL
unicode_east_asian_width(0x1CED, 0x1CED, 'N').	% Mn       VEDIC SIGN TIRYAK
unicode_east_asian_width(0x1CEE, 0x1CF1, 'N').	% Lo   [4] VEDIC SIGN HEXIFORM LONG ANUSVARA..VEDIC SIGN ANUSVARA UBHAYATO MUKHA
unicode_east_asian_width(0x1CF2, 0x1CF3, 'N').	% Mc   [2] VEDIC SIGN ARDHAVISARGA..VEDIC SIGN ROTATED ARDHAVISARGA
unicode_east_asian_width(0x1CF4, 0x1CF4, 'N').	% Mn       VEDIC TONE CANDRA ABOVE
unicode_east_asian_width(0x1CF5, 0x1CF6, 'N').	% Lo   [2] VEDIC SIGN JIHVAMULIYA..VEDIC SIGN UPADHMANIYA
unicode_east_asian_width(0x1D00, 0x1D2B, 'N').	% L&  [44] LATIN LETTER SMALL CAPITAL A..CYRILLIC LETTER SMALL CAPITAL EL
unicode_east_asian_width(0x1D2C, 0x1D6A, 'N').	% Lm  [63] MODIFIER LETTER CAPITAL A..GREEK SUBSCRIPT SMALL LETTER CHI
unicode_east_asian_width(0x1D6B, 0x1D77, 'N').	% L&  [13] LATIN SMALL LETTER UE..LATIN SMALL LETTER TURNED G
unicode_east_asian_width(0x1D78, 0x1D78, 'N').	% Lm       MODIFIER LETTER CYRILLIC EN
unicode_east_asian_width(0x1D79, 0x1D9A, 'N').	% L&  [34] LATIN SMALL LETTER INSULAR G..LATIN SMALL LETTER EZH WITH RETROFLEX HOOK
unicode_east_asian_width(0x1D9B, 0x1DBF, 'N').	% Lm  [37] MODIFIER LETTER SMALL TURNED ALPHA..MODIFIER LETTER SMALL THETA
unicode_east_asian_width(0x1DC0, 0x1DE6, 'N').	% Mn  [39] COMBINING DOTTED GRAVE ACCENT..COMBINING LATIN SMALL LETTER Z
unicode_east_asian_width(0x1DFC, 0x1DFF, 'N').	% Mn   [4] COMBINING DOUBLE INVERTED BREVE BELOW..COMBINING RIGHT ARROWHEAD AND DOWN ARROWHEAD BELOW
unicode_east_asian_width(0x1E00, 0x1F15, 'N').	% L& [278] LATIN CAPITAL LETTER A WITH RING BELOW..GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
unicode_east_asian_width(0x1F18, 0x1F1D, 'N').	% L&   [6] GREEK CAPITAL LETTER EPSILON WITH PSILI..GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
unicode_east_asian_width(0x1F20, 0x1F45, 'N').	% L&  [38] GREEK SMALL LETTER ETA WITH PSILI..GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
unicode_east_asian_width(0x1F48, 0x1F4D, 'N').	% L&   [6] GREEK CAPITAL LETTER OMICRON WITH PSILI..GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
unicode_east_asian_width(0x1F50, 0x1F57, 'N').	% L&   [8] GREEK SMALL LETTER UPSILON WITH PSILI..GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
unicode_east_asian_width(0x1F59, 0x1F59, 'N').	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA
unicode_east_asian_width(0x1F5B, 0x1F5B, 'N').	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
unicode_east_asian_width(0x1F5D, 0x1F5D, 'N').	% L&       GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
unicode_east_asian_width(0x1F5F, 0x1F7D, 'N').	% L&  [31] GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI..GREEK SMALL LETTER OMEGA WITH OXIA
unicode_east_asian_width(0x1F80, 0x1FB4, 'N').	% L&  [53] GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI..GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
unicode_east_asian_width(0x1FB6, 0x1FBC, 'N').	% L&   [7] GREEK SMALL LETTER ALPHA WITH PERISPOMENI..GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
unicode_east_asian_width(0x1FBD, 0x1FBD, 'N').	% Sk       GREEK KORONIS
unicode_east_asian_width(0x1FBE, 0x1FBE, 'N').	% L&       GREEK PROSGEGRAMMENI
unicode_east_asian_width(0x1FBF, 0x1FC1, 'N').	% Sk   [3] GREEK PSILI..GREEK DIALYTIKA AND PERISPOMENI
unicode_east_asian_width(0x1FC2, 0x1FC4, 'N').	% L&   [3] GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
unicode_east_asian_width(0x1FC6, 0x1FCC, 'N').	% L&   [7] GREEK SMALL LETTER ETA WITH PERISPOMENI..GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
unicode_east_asian_width(0x1FCD, 0x1FCF, 'N').	% Sk   [3] GREEK PSILI AND VARIA..GREEK PSILI AND PERISPOMENI
unicode_east_asian_width(0x1FD0, 0x1FD3, 'N').	% L&   [4] GREEK SMALL LETTER IOTA WITH VRACHY..GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
unicode_east_asian_width(0x1FD6, 0x1FDB, 'N').	% L&   [6] GREEK SMALL LETTER IOTA WITH PERISPOMENI..GREEK CAPITAL LETTER IOTA WITH OXIA
unicode_east_asian_width(0x1FDD, 0x1FDF, 'N').	% Sk   [3] GREEK DASIA AND VARIA..GREEK DASIA AND PERISPOMENI
unicode_east_asian_width(0x1FE0, 0x1FEC, 'N').	% L&  [13] GREEK SMALL LETTER UPSILON WITH VRACHY..GREEK CAPITAL LETTER RHO WITH DASIA
unicode_east_asian_width(0x1FED, 0x1FEF, 'N').	% Sk   [3] GREEK DIALYTIKA AND VARIA..GREEK VARIA
unicode_east_asian_width(0x1FF2, 0x1FF4, 'N').	% L&   [3] GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI..GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
unicode_east_asian_width(0x1FF6, 0x1FFC, 'N').	% L&   [7] GREEK SMALL LETTER OMEGA WITH PERISPOMENI..GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
unicode_east_asian_width(0x1FFD, 0x1FFE, 'N').	% Sk   [2] GREEK OXIA..GREEK DASIA
unicode_east_asian_width(0x2000, 0x200A, 'N').	% Zs  [11] EN QUAD..HAIR SPACE
unicode_east_asian_width(0x200B, 0x200F, 'N').	% Cf   [5] ZERO WIDTH SPACE..RIGHT-TO-LEFT MARK
unicode_east_asian_width(0x2011, 0x2012, 'N').	% Pd   [2] NON-BREAKING HYPHEN..FIGURE DASH
unicode_east_asian_width(0x2017, 0x2017, 'N').	% Po       DOUBLE LOW LINE
unicode_east_asian_width(0x201A, 0x201A, 'N').	% Ps       SINGLE LOW-9 QUOTATION MARK
unicode_east_asian_width(0x201B, 0x201B, 'N').	% Pi       SINGLE HIGH-REVERSED-9 QUOTATION MARK
unicode_east_asian_width(0x201E, 0x201E, 'N').	% Ps       DOUBLE LOW-9 QUOTATION MARK
unicode_east_asian_width(0x201F, 0x201F, 'N').	% Pi       DOUBLE HIGH-REVERSED-9 QUOTATION MARK
unicode_east_asian_width(0x2023, 0x2023, 'N').	% Po       TRIANGULAR BULLET
unicode_east_asian_width(0x2028, 0x2028, 'N').	% Zl       LINE SEPARATOR
unicode_east_asian_width(0x2029, 0x2029, 'N').	% Zp       PARAGRAPH SEPARATOR
unicode_east_asian_width(0x202A, 0x202E, 'N').	% Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
unicode_east_asian_width(0x202F, 0x202F, 'N').	% Zs       NARROW NO-BREAK SPACE
unicode_east_asian_width(0x2031, 0x2031, 'N').	% Po       PER TEN THOUSAND SIGN
unicode_east_asian_width(0x2034, 0x2034, 'N').	% Po       TRIPLE PRIME
unicode_east_asian_width(0x2036, 0x2038, 'N').	% Po   [3] REVERSED DOUBLE PRIME..CARET
unicode_east_asian_width(0x2039, 0x2039, 'N').	% Pi       SINGLE LEFT-POINTING ANGLE QUOTATION MARK
unicode_east_asian_width(0x203A, 0x203A, 'N').	% Pf       SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
unicode_east_asian_width(0x203C, 0x203D, 'N').	% Po   [2] DOUBLE EXCLAMATION MARK..INTERROBANG
unicode_east_asian_width(0x203F, 0x2040, 'N').	% Pc   [2] UNDERTIE..CHARACTER TIE
unicode_east_asian_width(0x2041, 0x2043, 'N').	% Po   [3] CARET INSERTION POINT..HYPHEN BULLET
unicode_east_asian_width(0x2044, 0x2044, 'N').	% Sm       FRACTION SLASH
unicode_east_asian_width(0x2045, 0x2045, 'N').	% Ps       LEFT SQUARE BRACKET WITH QUILL
unicode_east_asian_width(0x2046, 0x2046, 'N').	% Pe       RIGHT SQUARE BRACKET WITH QUILL
unicode_east_asian_width(0x2047, 0x2051, 'N').	% Po  [11] DOUBLE QUESTION MARK..TWO ASTERISKS ALIGNED VERTICALLY
unicode_east_asian_width(0x2052, 0x2052, 'N').	% Sm       COMMERCIAL MINUS SIGN
unicode_east_asian_width(0x2053, 0x2053, 'N').	% Po       SWUNG DASH
unicode_east_asian_width(0x2054, 0x2054, 'N').	% Pc       INVERTED UNDERTIE
unicode_east_asian_width(0x2055, 0x205E, 'N').	% Po  [10] FLOWER PUNCTUATION MARK..VERTICAL FOUR DOTS
unicode_east_asian_width(0x205F, 0x205F, 'N').	% Zs       MEDIUM MATHEMATICAL SPACE
unicode_east_asian_width(0x2060, 0x2064, 'N').	% Cf   [5] WORD JOINER..INVISIBLE PLUS
unicode_east_asian_width(0x206A, 0x206F, 'N').	% Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
unicode_east_asian_width(0x2070, 0x2070, 'N').	% No       SUPERSCRIPT ZERO
unicode_east_asian_width(0x2071, 0x2071, 'N').	% Lm       SUPERSCRIPT LATIN SMALL LETTER I
unicode_east_asian_width(0x2075, 0x2079, 'N').	% No   [5] SUPERSCRIPT FIVE..SUPERSCRIPT NINE
unicode_east_asian_width(0x207A, 0x207C, 'N').	% Sm   [3] SUPERSCRIPT PLUS SIGN..SUPERSCRIPT EQUALS SIGN
unicode_east_asian_width(0x207D, 0x207D, 'N').	% Ps       SUPERSCRIPT LEFT PARENTHESIS
unicode_east_asian_width(0x207E, 0x207E, 'N').	% Pe       SUPERSCRIPT RIGHT PARENTHESIS
unicode_east_asian_width(0x2080, 0x2080, 'N').	% No       SUBSCRIPT ZERO
unicode_east_asian_width(0x2085, 0x2089, 'N').	% No   [5] SUBSCRIPT FIVE..SUBSCRIPT NINE
unicode_east_asian_width(0x208A, 0x208C, 'N').	% Sm   [3] SUBSCRIPT PLUS SIGN..SUBSCRIPT EQUALS SIGN
unicode_east_asian_width(0x208D, 0x208D, 'N').	% Ps       SUBSCRIPT LEFT PARENTHESIS
unicode_east_asian_width(0x208E, 0x208E, 'N').	% Pe       SUBSCRIPT RIGHT PARENTHESIS
unicode_east_asian_width(0x2090, 0x209C, 'N').	% Lm  [13] LATIN SUBSCRIPT SMALL LETTER A..LATIN SUBSCRIPT SMALL LETTER T
unicode_east_asian_width(0x20A0, 0x20A8, 'N').	% Sc   [9] EURO-CURRENCY SIGN..RUPEE SIGN
unicode_east_asian_width(0x20AA, 0x20AB, 'N').	% Sc   [2] NEW SHEQEL SIGN..DONG SIGN
unicode_east_asian_width(0x20AD, 0x20BA, 'N').	% Sc  [14] KIP SIGN..TURKISH LIRA SIGN
unicode_east_asian_width(0x20D0, 0x20DC, 'N').	% Mn  [13] COMBINING LEFT HARPOON ABOVE..COMBINING FOUR DOTS ABOVE
unicode_east_asian_width(0x20DD, 0x20E0, 'N').	% Me   [4] COMBINING ENCLOSING CIRCLE..COMBINING ENCLOSING CIRCLE BACKSLASH
unicode_east_asian_width(0x20E1, 0x20E1, 'N').	% Mn       COMBINING LEFT RIGHT ARROW ABOVE
unicode_east_asian_width(0x20E2, 0x20E4, 'N').	% Me   [3] COMBINING ENCLOSING SCREEN..COMBINING ENCLOSING UPWARD POINTING TRIANGLE
unicode_east_asian_width(0x20E5, 0x20F0, 'N').	% Mn  [12] COMBINING REVERSE SOLIDUS OVERLAY..COMBINING ASTERISK ABOVE
unicode_east_asian_width(0x2100, 0x2101, 'N').	% So   [2] ACCOUNT OF..ADDRESSED TO THE SUBJECT
unicode_east_asian_width(0x2102, 0x2102, 'N').	% L&       DOUBLE-STRUCK CAPITAL C
unicode_east_asian_width(0x2104, 0x2104, 'N').	% So       CENTRE LINE SYMBOL
unicode_east_asian_width(0x2106, 0x2106, 'N').	% So       CADA UNA
unicode_east_asian_width(0x2107, 0x2107, 'N').	% L&       EULER CONSTANT
unicode_east_asian_width(0x2108, 0x2108, 'N').	% So       SCRUPLE
unicode_east_asian_width(0x210A, 0x2112, 'N').	% L&   [9] SCRIPT SMALL G..SCRIPT CAPITAL L
unicode_east_asian_width(0x2114, 0x2114, 'N').	% So       L B BAR SYMBOL
unicode_east_asian_width(0x2115, 0x2115, 'N').	% L&       DOUBLE-STRUCK CAPITAL N
unicode_east_asian_width(0x2117, 0x2117, 'N').	% So       SOUND RECORDING COPYRIGHT
unicode_east_asian_width(0x2118, 0x2118, 'N').	% Sm       SCRIPT CAPITAL P
unicode_east_asian_width(0x2119, 0x211D, 'N').	% L&   [5] DOUBLE-STRUCK CAPITAL P..DOUBLE-STRUCK CAPITAL R
unicode_east_asian_width(0x211E, 0x2120, 'N').	% So   [3] PRESCRIPTION TAKE..SERVICE MARK
unicode_east_asian_width(0x2123, 0x2123, 'N').	% So       VERSICLE
unicode_east_asian_width(0x2124, 0x2124, 'N').	% L&       DOUBLE-STRUCK CAPITAL Z
unicode_east_asian_width(0x2125, 0x2125, 'N').	% So       OUNCE SIGN
unicode_east_asian_width(0x2127, 0x2127, 'N').	% So       INVERTED OHM SIGN
unicode_east_asian_width(0x2128, 0x2128, 'N').	% L&       BLACK-LETTER CAPITAL Z
unicode_east_asian_width(0x2129, 0x2129, 'N').	% So       TURNED GREEK SMALL LETTER IOTA
unicode_east_asian_width(0x212A, 0x212A, 'N').	% L&       KELVIN SIGN
unicode_east_asian_width(0x212C, 0x212D, 'N').	% L&   [2] SCRIPT CAPITAL B..BLACK-LETTER CAPITAL C
unicode_east_asian_width(0x212E, 0x212E, 'N').	% So       ESTIMATED SYMBOL
unicode_east_asian_width(0x212F, 0x2134, 'N').	% L&   [6] SCRIPT SMALL E..SCRIPT SMALL O
unicode_east_asian_width(0x2135, 0x2138, 'N').	% Lo   [4] ALEF SYMBOL..DALET SYMBOL
unicode_east_asian_width(0x2139, 0x2139, 'N').	% L&       INFORMATION SOURCE
unicode_east_asian_width(0x213A, 0x213B, 'N').	% So   [2] ROTATED CAPITAL Q..FACSIMILE SIGN
unicode_east_asian_width(0x213C, 0x213F, 'N').	% L&   [4] DOUBLE-STRUCK SMALL PI..DOUBLE-STRUCK CAPITAL PI
unicode_east_asian_width(0x2140, 0x2144, 'N').	% Sm   [5] DOUBLE-STRUCK N-ARY SUMMATION..TURNED SANS-SERIF CAPITAL Y
unicode_east_asian_width(0x2145, 0x2149, 'N').	% L&   [5] DOUBLE-STRUCK ITALIC CAPITAL D..DOUBLE-STRUCK ITALIC SMALL J
unicode_east_asian_width(0x214A, 0x214A, 'N').	% So       PROPERTY LINE
unicode_east_asian_width(0x214B, 0x214B, 'N').	% Sm       TURNED AMPERSAND
unicode_east_asian_width(0x214C, 0x214D, 'N').	% So   [2] PER SIGN..AKTIESELSKAB
unicode_east_asian_width(0x214E, 0x214E, 'N').	% L&       TURNED SMALL F
unicode_east_asian_width(0x214F, 0x214F, 'N').	% So       SYMBOL FOR SAMARITAN SOURCE
unicode_east_asian_width(0x2150, 0x2152, 'N').	% No   [3] VULGAR FRACTION ONE SEVENTH..VULGAR FRACTION ONE TENTH
unicode_east_asian_width(0x2155, 0x215A, 'N').	% No   [6] VULGAR FRACTION ONE FIFTH..VULGAR FRACTION FIVE SIXTHS
unicode_east_asian_width(0x215F, 0x215F, 'N').	% No       FRACTION NUMERATOR ONE
unicode_east_asian_width(0x216C, 0x216F, 'N').	% Nl   [4] ROMAN NUMERAL FIFTY..ROMAN NUMERAL ONE THOUSAND
unicode_east_asian_width(0x217A, 0x2182, 'N').	% Nl   [9] SMALL ROMAN NUMERAL ELEVEN..ROMAN NUMERAL TEN THOUSAND
unicode_east_asian_width(0x2183, 0x2184, 'N').	% L&   [2] ROMAN NUMERAL REVERSED ONE HUNDRED..LATIN SMALL LETTER REVERSED C
unicode_east_asian_width(0x2185, 0x2188, 'N').	% Nl   [4] ROMAN NUMERAL SIX LATE FORM..ROMAN NUMERAL ONE HUNDRED THOUSAND
unicode_east_asian_width(0x219A, 0x219B, 'N').	% Sm   [2] LEFTWARDS ARROW WITH STROKE..RIGHTWARDS ARROW WITH STROKE
unicode_east_asian_width(0x219C, 0x219F, 'N').	% So   [4] LEFTWARDS WAVE ARROW..UPWARDS TWO HEADED ARROW
unicode_east_asian_width(0x21A0, 0x21A0, 'N').	% Sm       RIGHTWARDS TWO HEADED ARROW
unicode_east_asian_width(0x21A1, 0x21A2, 'N').	% So   [2] DOWNWARDS TWO HEADED ARROW..LEFTWARDS ARROW WITH TAIL
unicode_east_asian_width(0x21A3, 0x21A3, 'N').	% Sm       RIGHTWARDS ARROW WITH TAIL
unicode_east_asian_width(0x21A4, 0x21A5, 'N').	% So   [2] LEFTWARDS ARROW FROM BAR..UPWARDS ARROW FROM BAR
unicode_east_asian_width(0x21A6, 0x21A6, 'N').	% Sm       RIGHTWARDS ARROW FROM BAR
unicode_east_asian_width(0x21A7, 0x21AD, 'N').	% So   [7] DOWNWARDS ARROW FROM BAR..LEFT RIGHT WAVE ARROW
unicode_east_asian_width(0x21AE, 0x21AE, 'N').	% Sm       LEFT RIGHT ARROW WITH STROKE
unicode_east_asian_width(0x21AF, 0x21B7, 'N').	% So   [9] DOWNWARDS ZIGZAG ARROW..CLOCKWISE TOP SEMICIRCLE ARROW
unicode_east_asian_width(0x21BA, 0x21CD, 'N').	% So  [20] ANTICLOCKWISE OPEN CIRCLE ARROW..LEFTWARDS DOUBLE ARROW WITH STROKE
unicode_east_asian_width(0x21CE, 0x21CF, 'N').	% Sm   [2] LEFT RIGHT DOUBLE ARROW WITH STROKE..RIGHTWARDS DOUBLE ARROW WITH STROKE
unicode_east_asian_width(0x21D0, 0x21D1, 'N').	% So   [2] LEFTWARDS DOUBLE ARROW..UPWARDS DOUBLE ARROW
unicode_east_asian_width(0x21D3, 0x21D3, 'N').	% So       DOWNWARDS DOUBLE ARROW
unicode_east_asian_width(0x21D5, 0x21E6, 'N').	% So  [18] UP DOWN DOUBLE ARROW..LEFTWARDS WHITE ARROW
unicode_east_asian_width(0x21E8, 0x21F3, 'N').	% So  [12] RIGHTWARDS WHITE ARROW..UP DOWN WHITE ARROW
unicode_east_asian_width(0x21F4, 0x21FF, 'N').	% Sm  [12] RIGHT ARROW WITH SMALL CIRCLE..LEFT RIGHT OPEN-HEADED ARROW
unicode_east_asian_width(0x2201, 0x2201, 'N').	% Sm       COMPLEMENT
unicode_east_asian_width(0x2204, 0x2206, 'N').	% Sm   [3] THERE DOES NOT EXIST..INCREMENT
unicode_east_asian_width(0x2209, 0x220A, 'N').	% Sm   [2] NOT AN ELEMENT OF..SMALL ELEMENT OF
unicode_east_asian_width(0x220C, 0x220E, 'N').	% Sm   [3] DOES NOT CONTAIN AS MEMBER..END OF PROOF
unicode_east_asian_width(0x2210, 0x2210, 'N').	% Sm       N-ARY COPRODUCT
unicode_east_asian_width(0x2212, 0x2214, 'N').	% Sm   [3] MINUS SIGN..DOT PLUS
unicode_east_asian_width(0x2216, 0x2219, 'N').	% Sm   [4] SET MINUS..BULLET OPERATOR
unicode_east_asian_width(0x221B, 0x221C, 'N').	% Sm   [2] CUBE ROOT..FOURTH ROOT
unicode_east_asian_width(0x2221, 0x2222, 'N').	% Sm   [2] MEASURED ANGLE..SPHERICAL ANGLE
unicode_east_asian_width(0x2224, 0x2224, 'N').	% Sm       DOES NOT DIVIDE
unicode_east_asian_width(0x2226, 0x2226, 'N').	% Sm       NOT PARALLEL TO
unicode_east_asian_width(0x222D, 0x222D, 'N').	% Sm       TRIPLE INTEGRAL
unicode_east_asian_width(0x222F, 0x2233, 'N').	% Sm   [5] SURFACE INTEGRAL..ANTICLOCKWISE CONTOUR INTEGRAL
unicode_east_asian_width(0x2238, 0x223B, 'N').	% Sm   [4] DOT MINUS..HOMOTHETIC
unicode_east_asian_width(0x223E, 0x2247, 'N').	% Sm  [10] INVERTED LAZY S..NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
unicode_east_asian_width(0x2249, 0x224B, 'N').	% Sm   [3] NOT ALMOST EQUAL TO..TRIPLE TILDE
unicode_east_asian_width(0x224D, 0x2251, 'N').	% Sm   [5] EQUIVALENT TO..GEOMETRICALLY EQUAL TO
unicode_east_asian_width(0x2253, 0x225F, 'N').	% Sm  [13] IMAGE OF OR APPROXIMATELY EQUAL TO..QUESTIONED EQUAL TO
unicode_east_asian_width(0x2262, 0x2263, 'N').	% Sm   [2] NOT IDENTICAL TO..STRICTLY EQUIVALENT TO
unicode_east_asian_width(0x2268, 0x2269, 'N').	% Sm   [2] LESS-THAN BUT NOT EQUAL TO..GREATER-THAN BUT NOT EQUAL TO
unicode_east_asian_width(0x226C, 0x226D, 'N').	% Sm   [2] BETWEEN..NOT EQUIVALENT TO
unicode_east_asian_width(0x2270, 0x2281, 'N').	% Sm  [18] NEITHER LESS-THAN NOR EQUAL TO..DOES NOT SUCCEED
unicode_east_asian_width(0x2284, 0x2285, 'N').	% Sm   [2] NOT A SUBSET OF..NOT A SUPERSET OF
unicode_east_asian_width(0x2288, 0x2294, 'N').	% Sm  [13] NEITHER A SUBSET OF NOR EQUAL TO..SQUARE CUP
unicode_east_asian_width(0x2296, 0x2298, 'N').	% Sm   [3] CIRCLED MINUS..CIRCLED DIVISION SLASH
unicode_east_asian_width(0x229A, 0x22A4, 'N').	% Sm  [11] CIRCLED RING OPERATOR..DOWN TACK
unicode_east_asian_width(0x22A6, 0x22BE, 'N').	% Sm  [25] ASSERTION..RIGHT ANGLE WITH ARC
unicode_east_asian_width(0x22C0, 0x22FF, 'N').	% Sm  [64] N-ARY LOGICAL AND..Z NOTATION BAG MEMBERSHIP
unicode_east_asian_width(0x2300, 0x2307, 'N').	% So   [8] DIAMETER SIGN..WAVY LINE
unicode_east_asian_width(0x2308, 0x230B, 'N').	% Sm   [4] LEFT CEILING..RIGHT FLOOR
unicode_east_asian_width(0x230C, 0x2311, 'N').	% So   [6] BOTTOM RIGHT CROP..SQUARE LOZENGE
unicode_east_asian_width(0x2313, 0x231F, 'N').	% So  [13] SEGMENT..BOTTOM RIGHT CORNER
unicode_east_asian_width(0x2320, 0x2321, 'N').	% Sm   [2] TOP HALF INTEGRAL..BOTTOM HALF INTEGRAL
unicode_east_asian_width(0x2322, 0x2328, 'N').	% So   [7] FROWN..KEYBOARD
unicode_east_asian_width(0x232B, 0x237B, 'N').	% So  [81] ERASE TO THE LEFT..NOT CHECK MARK
unicode_east_asian_width(0x237C, 0x237C, 'N').	% Sm       RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
unicode_east_asian_width(0x237D, 0x239A, 'N').	% So  [30] SHOULDERED OPEN BOX..CLEAR SCREEN SYMBOL
unicode_east_asian_width(0x239B, 0x23B3, 'N').	% Sm  [25] LEFT PARENTHESIS UPPER HOOK..SUMMATION BOTTOM
unicode_east_asian_width(0x23B4, 0x23DB, 'N').	% So  [40] TOP SQUARE BRACKET..FUSE
unicode_east_asian_width(0x23DC, 0x23E1, 'N').	% Sm   [6] TOP PARENTHESIS..BOTTOM TORTOISE SHELL BRACKET
unicode_east_asian_width(0x23E2, 0x23F3, 'N').	% So  [18] WHITE TRAPEZIUM..HOURGLASS WITH FLOWING SAND
unicode_east_asian_width(0x2400, 0x2426, 'N').	% So  [39] SYMBOL FOR NULL..SYMBOL FOR SUBSTITUTE FORM TWO
unicode_east_asian_width(0x2440, 0x244A, 'N').	% So  [11] OCR HOOK..OCR DOUBLE BACKSLASH
unicode_east_asian_width(0x24EA, 0x24EA, 'N').	% No       CIRCLED DIGIT ZERO
unicode_east_asian_width(0x254C, 0x254F, 'N').	% So   [4] BOX DRAWINGS LIGHT DOUBLE DASH HORIZONTAL..BOX DRAWINGS HEAVY DOUBLE DASH VERTICAL
unicode_east_asian_width(0x2574, 0x257F, 'N').	% So  [12] BOX DRAWINGS LIGHT LEFT..BOX DRAWINGS HEAVY UP AND LIGHT DOWN
unicode_east_asian_width(0x2590, 0x2591, 'N').	% So   [2] RIGHT HALF BLOCK..LIGHT SHADE
unicode_east_asian_width(0x2596, 0x259F, 'N').	% So  [10] QUADRANT LOWER LEFT..QUADRANT UPPER RIGHT AND LOWER LEFT AND LOWER RIGHT
unicode_east_asian_width(0x25A2, 0x25A2, 'N').	% So       WHITE SQUARE WITH ROUNDED CORNERS
unicode_east_asian_width(0x25AA, 0x25B1, 'N').	% So   [8] BLACK SMALL SQUARE..WHITE PARALLELOGRAM
unicode_east_asian_width(0x25B4, 0x25B5, 'N').	% So   [2] BLACK UP-POINTING SMALL TRIANGLE..WHITE UP-POINTING SMALL TRIANGLE
unicode_east_asian_width(0x25B8, 0x25BB, 'N').	% So   [4] BLACK RIGHT-POINTING SMALL TRIANGLE..WHITE RIGHT-POINTING POINTER
unicode_east_asian_width(0x25BE, 0x25BF, 'N').	% So   [2] BLACK DOWN-POINTING SMALL TRIANGLE..WHITE DOWN-POINTING SMALL TRIANGLE
unicode_east_asian_width(0x25C2, 0x25C5, 'N').	% So   [4] BLACK LEFT-POINTING SMALL TRIANGLE..WHITE LEFT-POINTING POINTER
unicode_east_asian_width(0x25C9, 0x25CA, 'N').	% So   [2] FISHEYE..LOZENGE
unicode_east_asian_width(0x25CC, 0x25CD, 'N').	% So   [2] DOTTED CIRCLE..CIRCLE WITH VERTICAL FILL
unicode_east_asian_width(0x25D2, 0x25E1, 'N').	% So  [16] CIRCLE WITH LOWER HALF BLACK..LOWER HALF CIRCLE
unicode_east_asian_width(0x25E6, 0x25EE, 'N').	% So   [9] WHITE BULLET..UP-POINTING TRIANGLE WITH RIGHT HALF BLACK
unicode_east_asian_width(0x25F0, 0x25F7, 'N').	% So   [8] WHITE SQUARE WITH UPPER LEFT QUADRANT..WHITE CIRCLE WITH UPPER RIGHT QUADRANT
unicode_east_asian_width(0x25F8, 0x25FF, 'N').	% Sm   [8] UPPER LEFT TRIANGLE..LOWER RIGHT TRIANGLE
unicode_east_asian_width(0x2600, 0x2604, 'N').	% So   [5] BLACK SUN WITH RAYS..COMET
unicode_east_asian_width(0x2607, 0x2608, 'N').	% So   [2] LIGHTNING..THUNDERSTORM
unicode_east_asian_width(0x260A, 0x260D, 'N').	% So   [4] ASCENDING NODE..OPPOSITION
unicode_east_asian_width(0x2610, 0x2613, 'N').	% So   [4] BALLOT BOX..SALTIRE
unicode_east_asian_width(0x2616, 0x261B, 'N').	% So   [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
unicode_east_asian_width(0x261D, 0x261D, 'N').	% So       WHITE UP POINTING INDEX
unicode_east_asian_width(0x261F, 0x263F, 'N').	% So  [33] WHITE DOWN POINTING INDEX..MERCURY
unicode_east_asian_width(0x2641, 0x2641, 'N').	% So       EARTH
unicode_east_asian_width(0x2643, 0x265F, 'N').	% So  [29] JUPITER..BLACK CHESS PAWN
unicode_east_asian_width(0x2662, 0x2662, 'N').	% So       WHITE DIAMOND SUIT
unicode_east_asian_width(0x2666, 0x2666, 'N').	% So       BLACK DIAMOND SUIT
unicode_east_asian_width(0x266B, 0x266B, 'N').	% So       BEAMED EIGHTH NOTES
unicode_east_asian_width(0x266E, 0x266E, 'N').	% So       MUSIC NATURAL SIGN
unicode_east_asian_width(0x2670, 0x269D, 'N').	% So  [46] WEST SYRIAC CROSS..OUTLINED WHITE STAR
unicode_east_asian_width(0x26A0, 0x26BD, 'N').	% So  [30] WARNING SIGN..SOCCER BALL
unicode_east_asian_width(0x26C0, 0x26C3, 'N').	% So   [4] WHITE DRAUGHTS MAN..BLACK DRAUGHTS KING
unicode_east_asian_width(0x26CE, 0x26CE, 'N').	% So       OPHIUCHUS
unicode_east_asian_width(0x26E2, 0x26E2, 'N').	% So       ASTRONOMICAL SYMBOL FOR URANUS
unicode_east_asian_width(0x26E4, 0x26E7, 'N').	% So   [4] PENTAGRAM..INVERTED PENTAGRAM
unicode_east_asian_width(0x2701, 0x273C, 'N').	% So  [60] UPPER BLADE SCISSORS..OPEN CENTRE TEARDROP-SPOKED ASTERISK
unicode_east_asian_width(0x273E, 0x2756, 'N').	% So  [25] SIX PETALLED BLACK AND WHITE FLORETTE..BLACK DIAMOND MINUS WHITE X
unicode_east_asian_width(0x2758, 0x2767, 'N').	% So  [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
unicode_east_asian_width(0x2768, 0x2768, 'N').	% Ps       MEDIUM LEFT PARENTHESIS ORNAMENT
unicode_east_asian_width(0x2769, 0x2769, 'N').	% Pe       MEDIUM RIGHT PARENTHESIS ORNAMENT
unicode_east_asian_width(0x276A, 0x276A, 'N').	% Ps       MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
unicode_east_asian_width(0x276B, 0x276B, 'N').	% Pe       MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
unicode_east_asian_width(0x276C, 0x276C, 'N').	% Ps       MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_east_asian_width(0x276D, 0x276D, 'N').	% Pe       MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_east_asian_width(0x276E, 0x276E, 'N').	% Ps       HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_east_asian_width(0x276F, 0x276F, 'N').	% Pe       HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
unicode_east_asian_width(0x2770, 0x2770, 'N').	% Ps       HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
unicode_east_asian_width(0x2771, 0x2771, 'N').	% Pe       HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
unicode_east_asian_width(0x2772, 0x2772, 'N').	% Ps       LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
unicode_east_asian_width(0x2773, 0x2773, 'N').	% Pe       LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
unicode_east_asian_width(0x2774, 0x2774, 'N').	% Ps       MEDIUM LEFT CURLY BRACKET ORNAMENT
unicode_east_asian_width(0x2775, 0x2775, 'N').	% Pe       MEDIUM RIGHT CURLY BRACKET ORNAMENT
unicode_east_asian_width(0x2780, 0x2793, 'N').	% No  [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
unicode_east_asian_width(0x2794, 0x27BF, 'N').	% So  [44] HEAVY WIDE-HEADED RIGHTWARDS ARROW..DOUBLE CURLY LOOP
unicode_east_asian_width(0x27C0, 0x27C4, 'N').	% Sm   [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
unicode_east_asian_width(0x27C5, 0x27C5, 'N').	% Ps       LEFT S-SHAPED BAG DELIMITER
unicode_east_asian_width(0x27C6, 0x27C6, 'N').	% Pe       RIGHT S-SHAPED BAG DELIMITER
unicode_east_asian_width(0x27C7, 0x27E5, 'N').	% Sm  [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
unicode_east_asian_width(0x27EE, 0x27EE, 'N').	% Ps       MATHEMATICAL LEFT FLATTENED PARENTHESIS
unicode_east_asian_width(0x27EF, 0x27EF, 'N').	% Pe       MATHEMATICAL RIGHT FLATTENED PARENTHESIS
unicode_east_asian_width(0x27F0, 0x27FF, 'N').	% Sm  [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
unicode_east_asian_width(0x2800, 0x28FF, 'N').	% So [256] BRAILLE PATTERN BLANK..BRAILLE PATTERN DOTS-12345678
unicode_east_asian_width(0x2900, 0x2982, 'N').	% Sm [131] RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE..Z NOTATION TYPE COLON
unicode_east_asian_width(0x2983, 0x2983, 'N').	% Ps       LEFT WHITE CURLY BRACKET
unicode_east_asian_width(0x2984, 0x2984, 'N').	% Pe       RIGHT WHITE CURLY BRACKET
unicode_east_asian_width(0x2987, 0x2987, 'N').	% Ps       Z NOTATION LEFT IMAGE BRACKET
unicode_east_asian_width(0x2988, 0x2988, 'N').	% Pe       Z NOTATION RIGHT IMAGE BRACKET
unicode_east_asian_width(0x2989, 0x2989, 'N').	% Ps       Z NOTATION LEFT BINDING BRACKET
unicode_east_asian_width(0x298A, 0x298A, 'N').	% Pe       Z NOTATION RIGHT BINDING BRACKET
unicode_east_asian_width(0x298B, 0x298B, 'N').	% Ps       LEFT SQUARE BRACKET WITH UNDERBAR
unicode_east_asian_width(0x298C, 0x298C, 'N').	% Pe       RIGHT SQUARE BRACKET WITH UNDERBAR
unicode_east_asian_width(0x298D, 0x298D, 'N').	% Ps       LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_east_asian_width(0x298E, 0x298E, 'N').	% Pe       RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_east_asian_width(0x298F, 0x298F, 'N').	% Ps       LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
unicode_east_asian_width(0x2990, 0x2990, 'N').	% Pe       RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
unicode_east_asian_width(0x2991, 0x2991, 'N').	% Ps       LEFT ANGLE BRACKET WITH DOT
unicode_east_asian_width(0x2992, 0x2992, 'N').	% Pe       RIGHT ANGLE BRACKET WITH DOT
unicode_east_asian_width(0x2993, 0x2993, 'N').	% Ps       LEFT ARC LESS-THAN BRACKET
unicode_east_asian_width(0x2994, 0x2994, 'N').	% Pe       RIGHT ARC GREATER-THAN BRACKET
unicode_east_asian_width(0x2995, 0x2995, 'N').	% Ps       DOUBLE LEFT ARC GREATER-THAN BRACKET
unicode_east_asian_width(0x2996, 0x2996, 'N').	% Pe       DOUBLE RIGHT ARC LESS-THAN BRACKET
unicode_east_asian_width(0x2997, 0x2997, 'N').	% Ps       LEFT BLACK TORTOISE SHELL BRACKET
unicode_east_asian_width(0x2998, 0x2998, 'N').	% Pe       RIGHT BLACK TORTOISE SHELL BRACKET
unicode_east_asian_width(0x2999, 0x29D7, 'N').	% Sm  [63] DOTTED FENCE..BLACK HOURGLASS
unicode_east_asian_width(0x29D8, 0x29D8, 'N').	% Ps       LEFT WIGGLY FENCE
unicode_east_asian_width(0x29D9, 0x29D9, 'N').	% Pe       RIGHT WIGGLY FENCE
unicode_east_asian_width(0x29DA, 0x29DA, 'N').	% Ps       LEFT DOUBLE WIGGLY FENCE
unicode_east_asian_width(0x29DB, 0x29DB, 'N').	% Pe       RIGHT DOUBLE WIGGLY FENCE
unicode_east_asian_width(0x29DC, 0x29FB, 'N').	% Sm  [32] INCOMPLETE INFINITY..TRIPLE PLUS
unicode_east_asian_width(0x29FC, 0x29FC, 'N').	% Ps       LEFT-POINTING CURVED ANGLE BRACKET
unicode_east_asian_width(0x29FD, 0x29FD, 'N').	% Pe       RIGHT-POINTING CURVED ANGLE BRACKET
unicode_east_asian_width(0x29FE, 0x2AFF, 'N').	% Sm [258] TINY..N-ARY WHITE VERTICAL BAR
unicode_east_asian_width(0x2B00, 0x2B2F, 'N').	% So  [48] NORTH EAST WHITE ARROW..WHITE VERTICAL ELLIPSE
unicode_east_asian_width(0x2B30, 0x2B44, 'N').	% Sm  [21] LEFT ARROW WITH SMALL CIRCLE..RIGHTWARDS ARROW THROUGH SUPERSET
unicode_east_asian_width(0x2B45, 0x2B46, 'N').	% So   [2] LEFTWARDS QUADRUPLE ARROW..RIGHTWARDS QUADRUPLE ARROW
unicode_east_asian_width(0x2B47, 0x2B4C, 'N').	% Sm   [6] REVERSE TILDE OPERATOR ABOVE RIGHTWARDS ARROW..RIGHTWARDS ARROW ABOVE REVERSE TILDE OPERATOR
unicode_east_asian_width(0x2B50, 0x2B54, 'N').	% So   [5] WHITE MEDIUM STAR..WHITE RIGHT-POINTING PENTAGON
unicode_east_asian_width(0x2C00, 0x2C2E, 'N').	% L&  [47] GLAGOLITIC CAPITAL LETTER AZU..GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
unicode_east_asian_width(0x2C30, 0x2C5E, 'N').	% L&  [47] GLAGOLITIC SMALL LETTER AZU..GLAGOLITIC SMALL LETTER LATINATE MYSLITE
unicode_east_asian_width(0x2C60, 0x2C7B, 'N').	% L&  [28] LATIN CAPITAL LETTER L WITH DOUBLE BAR..LATIN LETTER SMALL CAPITAL TURNED E
unicode_east_asian_width(0x2C7C, 0x2C7D, 'N').	% Lm   [2] LATIN SUBSCRIPT SMALL LETTER J..MODIFIER LETTER CAPITAL V
unicode_east_asian_width(0x2C7E, 0x2CE4, 'N').	% L& [103] LATIN CAPITAL LETTER S WITH SWASH TAIL..COPTIC SYMBOL KAI
unicode_east_asian_width(0x2CE5, 0x2CEA, 'N').	% So   [6] COPTIC SYMBOL MI RO..COPTIC SYMBOL SHIMA SIMA
unicode_east_asian_width(0x2CEB, 0x2CEE, 'N').	% L&   [4] COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI..COPTIC SMALL LETTER CRYPTOGRAMMIC GANGIA
unicode_east_asian_width(0x2CEF, 0x2CF1, 'N').	% Mn   [3] COPTIC COMBINING NI ABOVE..COPTIC COMBINING SPIRITUS LENIS
unicode_east_asian_width(0x2CF2, 0x2CF3, 'N').	% L&   [2] COPTIC CAPITAL LETTER BOHAIRIC KHEI..COPTIC SMALL LETTER BOHAIRIC KHEI
unicode_east_asian_width(0x2CF9, 0x2CFC, 'N').	% Po   [4] COPTIC OLD NUBIAN FULL STOP..COPTIC OLD NUBIAN VERSE DIVIDER
unicode_east_asian_width(0x2CFD, 0x2CFD, 'N').	% No       COPTIC FRACTION ONE HALF
unicode_east_asian_width(0x2CFE, 0x2CFF, 'N').	% Po   [2] COPTIC FULL STOP..COPTIC MORPHOLOGICAL DIVIDER
unicode_east_asian_width(0x2D00, 0x2D25, 'N').	% L&  [38] GEORGIAN SMALL LETTER AN..GEORGIAN SMALL LETTER HOE
unicode_east_asian_width(0x2D27, 0x2D27, 'N').	% L&       GEORGIAN SMALL LETTER YN
unicode_east_asian_width(0x2D2D, 0x2D2D, 'N').	% L&       GEORGIAN SMALL LETTER AEN
unicode_east_asian_width(0x2D30, 0x2D67, 'N').	% Lo  [56] TIFINAGH LETTER YA..TIFINAGH LETTER YO
unicode_east_asian_width(0x2D6F, 0x2D6F, 'N').	% Lm       TIFINAGH MODIFIER LETTER LABIALIZATION MARK
unicode_east_asian_width(0x2D70, 0x2D70, 'N').	% Po       TIFINAGH SEPARATOR MARK
unicode_east_asian_width(0x2D7F, 0x2D7F, 'N').	% Mn       TIFINAGH CONSONANT JOINER
unicode_east_asian_width(0x2D80, 0x2D96, 'N').	% Lo  [23] ETHIOPIC SYLLABLE LOA..ETHIOPIC SYLLABLE GGWE
unicode_east_asian_width(0x2DA0, 0x2DA6, 'N').	% Lo   [7] ETHIOPIC SYLLABLE SSA..ETHIOPIC SYLLABLE SSO
unicode_east_asian_width(0x2DA8, 0x2DAE, 'N').	% Lo   [7] ETHIOPIC SYLLABLE CCA..ETHIOPIC SYLLABLE CCO
unicode_east_asian_width(0x2DB0, 0x2DB6, 'N').	% Lo   [7] ETHIOPIC SYLLABLE ZZA..ETHIOPIC SYLLABLE ZZO
unicode_east_asian_width(0x2DB8, 0x2DBE, 'N').	% Lo   [7] ETHIOPIC SYLLABLE CCHA..ETHIOPIC SYLLABLE CCHO
unicode_east_asian_width(0x2DC0, 0x2DC6, 'N').	% Lo   [7] ETHIOPIC SYLLABLE QYA..ETHIOPIC SYLLABLE QYO
unicode_east_asian_width(0x2DC8, 0x2DCE, 'N').	% Lo   [7] ETHIOPIC SYLLABLE KYA..ETHIOPIC SYLLABLE KYO
unicode_east_asian_width(0x2DD0, 0x2DD6, 'N').	% Lo   [7] ETHIOPIC SYLLABLE XYA..ETHIOPIC SYLLABLE XYO
unicode_east_asian_width(0x2DD8, 0x2DDE, 'N').	% Lo   [7] ETHIOPIC SYLLABLE GYA..ETHIOPIC SYLLABLE GYO
unicode_east_asian_width(0x2DE0, 0x2DFF, 'N').	% Mn  [32] COMBINING CYRILLIC LETTER BE..COMBINING CYRILLIC LETTER IOTIFIED BIG YUS
unicode_east_asian_width(0x2E00, 0x2E01, 'N').	% Po   [2] RIGHT ANGLE SUBSTITUTION MARKER..RIGHT ANGLE DOTTED SUBSTITUTION MARKER
unicode_east_asian_width(0x2E02, 0x2E02, 'N').	% Pi       LEFT SUBSTITUTION BRACKET
unicode_east_asian_width(0x2E03, 0x2E03, 'N').	% Pf       RIGHT SUBSTITUTION BRACKET
unicode_east_asian_width(0x2E04, 0x2E04, 'N').	% Pi       LEFT DOTTED SUBSTITUTION BRACKET
unicode_east_asian_width(0x2E05, 0x2E05, 'N').	% Pf       RIGHT DOTTED SUBSTITUTION BRACKET
unicode_east_asian_width(0x2E06, 0x2E08, 'N').	% Po   [3] RAISED INTERPOLATION MARKER..DOTTED TRANSPOSITION MARKER
unicode_east_asian_width(0x2E09, 0x2E09, 'N').	% Pi       LEFT TRANSPOSITION BRACKET
unicode_east_asian_width(0x2E0A, 0x2E0A, 'N').	% Pf       RIGHT TRANSPOSITION BRACKET
unicode_east_asian_width(0x2E0B, 0x2E0B, 'N').	% Po       RAISED SQUARE
unicode_east_asian_width(0x2E0C, 0x2E0C, 'N').	% Pi       LEFT RAISED OMISSION BRACKET
unicode_east_asian_width(0x2E0D, 0x2E0D, 'N').	% Pf       RIGHT RAISED OMISSION BRACKET
unicode_east_asian_width(0x2E0E, 0x2E16, 'N').	% Po   [9] EDITORIAL CORONIS..DOTTED RIGHT-POINTING ANGLE
unicode_east_asian_width(0x2E17, 0x2E17, 'N').	% Pd       DOUBLE OBLIQUE HYPHEN
unicode_east_asian_width(0x2E18, 0x2E19, 'N').	% Po   [2] INVERTED INTERROBANG..PALM BRANCH
unicode_east_asian_width(0x2E1A, 0x2E1A, 'N').	% Pd       HYPHEN WITH DIAERESIS
unicode_east_asian_width(0x2E1B, 0x2E1B, 'N').	% Po       TILDE WITH RING ABOVE
unicode_east_asian_width(0x2E1C, 0x2E1C, 'N').	% Pi       LEFT LOW PARAPHRASE BRACKET
unicode_east_asian_width(0x2E1D, 0x2E1D, 'N').	% Pf       RIGHT LOW PARAPHRASE BRACKET
unicode_east_asian_width(0x2E1E, 0x2E1F, 'N').	% Po   [2] TILDE WITH DOT ABOVE..TILDE WITH DOT BELOW
unicode_east_asian_width(0x2E20, 0x2E20, 'N').	% Pi       LEFT VERTICAL BAR WITH QUILL
unicode_east_asian_width(0x2E21, 0x2E21, 'N').	% Pf       RIGHT VERTICAL BAR WITH QUILL
unicode_east_asian_width(0x2E22, 0x2E22, 'N').	% Ps       TOP LEFT HALF BRACKET
unicode_east_asian_width(0x2E23, 0x2E23, 'N').	% Pe       TOP RIGHT HALF BRACKET
unicode_east_asian_width(0x2E24, 0x2E24, 'N').	% Ps       BOTTOM LEFT HALF BRACKET
unicode_east_asian_width(0x2E25, 0x2E25, 'N').	% Pe       BOTTOM RIGHT HALF BRACKET
unicode_east_asian_width(0x2E26, 0x2E26, 'N').	% Ps       LEFT SIDEWAYS U BRACKET
unicode_east_asian_width(0x2E27, 0x2E27, 'N').	% Pe       RIGHT SIDEWAYS U BRACKET
unicode_east_asian_width(0x2E28, 0x2E28, 'N').	% Ps       LEFT DOUBLE PARENTHESIS
unicode_east_asian_width(0x2E29, 0x2E29, 'N').	% Pe       RIGHT DOUBLE PARENTHESIS
unicode_east_asian_width(0x2E2A, 0x2E2E, 'N').	% Po   [5] TWO DOTS OVER ONE DOT PUNCTUATION..REVERSED QUESTION MARK
unicode_east_asian_width(0x2E2F, 0x2E2F, 'N').	% Lm       VERTICAL TILDE
unicode_east_asian_width(0x2E30, 0x2E39, 'N').	% Po  [10] RING POINT..TOP HALF SECTION SIGN
unicode_east_asian_width(0x2E3A, 0x2E3B, 'N').	% Pd   [2] TWO-EM DASH..THREE-EM DASH
unicode_east_asian_width(0x303F, 0x303F, 'N').	% So       IDEOGRAPHIC HALF FILL SPACE
unicode_east_asian_width(0x4DC0, 0x4DFF, 'N').	% So  [64] HEXAGRAM FOR THE CREATIVE HEAVEN..HEXAGRAM FOR BEFORE COMPLETION
unicode_east_asian_width(0xA4D0, 0xA4F7, 'N').	% Lo  [40] LISU LETTER BA..LISU LETTER OE
unicode_east_asian_width(0xA4F8, 0xA4FD, 'N').	% Lm   [6] LISU LETTER TONE MYA TI..LISU LETTER TONE MYA JEU
unicode_east_asian_width(0xA4FE, 0xA4FF, 'N').	% Po   [2] LISU PUNCTUATION COMMA..LISU PUNCTUATION FULL STOP
unicode_east_asian_width(0xA500, 0xA60B, 'N').	% Lo [268] VAI SYLLABLE EE..VAI SYLLABLE NG
unicode_east_asian_width(0xA60C, 0xA60C, 'N').	% Lm       VAI SYLLABLE LENGTHENER
unicode_east_asian_width(0xA60D, 0xA60F, 'N').	% Po   [3] VAI COMMA..VAI QUESTION MARK
unicode_east_asian_width(0xA610, 0xA61F, 'N').	% Lo  [16] VAI SYLLABLE NDOLE FA..VAI SYMBOL JONG
unicode_east_asian_width(0xA620, 0xA629, 'N').	% Nd  [10] VAI DIGIT ZERO..VAI DIGIT NINE
unicode_east_asian_width(0xA62A, 0xA62B, 'N').	% Lo   [2] VAI SYLLABLE NDOLE MA..VAI SYLLABLE NDOLE DO
unicode_east_asian_width(0xA640, 0xA66D, 'N').	% L&  [46] CYRILLIC CAPITAL LETTER ZEMLYA..CYRILLIC SMALL LETTER DOUBLE MONOCULAR O
unicode_east_asian_width(0xA66E, 0xA66E, 'N').	% Lo       CYRILLIC LETTER MULTIOCULAR O
unicode_east_asian_width(0xA66F, 0xA66F, 'N').	% Mn       COMBINING CYRILLIC VZMET
unicode_east_asian_width(0xA670, 0xA672, 'N').	% Me   [3] COMBINING CYRILLIC TEN MILLIONS SIGN..COMBINING CYRILLIC THOUSAND MILLIONS SIGN
unicode_east_asian_width(0xA673, 0xA673, 'N').	% Po       SLAVONIC ASTERISK
unicode_east_asian_width(0xA674, 0xA67D, 'N').	% Mn  [10] COMBINING CYRILLIC LETTER UKRAINIAN IE..COMBINING CYRILLIC PAYEROK
unicode_east_asian_width(0xA67E, 0xA67E, 'N').	% Po       CYRILLIC KAVYKA
unicode_east_asian_width(0xA67F, 0xA67F, 'N').	% Lm       CYRILLIC PAYEROK
unicode_east_asian_width(0xA680, 0xA697, 'N').	% L&  [24] CYRILLIC CAPITAL LETTER DWE..CYRILLIC SMALL LETTER SHWE
unicode_east_asian_width(0xA69F, 0xA69F, 'N').	% Mn       COMBINING CYRILLIC LETTER IOTIFIED E
unicode_east_asian_width(0xA6A0, 0xA6E5, 'N').	% Lo  [70] BAMUM LETTER A..BAMUM LETTER KI
unicode_east_asian_width(0xA6E6, 0xA6EF, 'N').	% Nl  [10] BAMUM LETTER MO..BAMUM LETTER KOGHOM
unicode_east_asian_width(0xA6F0, 0xA6F1, 'N').	% Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
unicode_east_asian_width(0xA6F2, 0xA6F7, 'N').	% Po   [6] BAMUM NJAEMLI..BAMUM QUESTION MARK
unicode_east_asian_width(0xA700, 0xA716, 'N').	% Sk  [23] MODIFIER LETTER CHINESE TONE YIN PING..MODIFIER LETTER EXTRA-LOW LEFT-STEM TONE BAR
unicode_east_asian_width(0xA717, 0xA71F, 'N').	% Lm   [9] MODIFIER LETTER DOT VERTICAL BAR..MODIFIER LETTER LOW INVERTED EXCLAMATION MARK
unicode_east_asian_width(0xA720, 0xA721, 'N').	% Sk   [2] MODIFIER LETTER STRESS AND HIGH TONE..MODIFIER LETTER STRESS AND LOW TONE
unicode_east_asian_width(0xA722, 0xA76F, 'N').	% L&  [78] LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF..LATIN SMALL LETTER CON
unicode_east_asian_width(0xA770, 0xA770, 'N').	% Lm       MODIFIER LETTER US
unicode_east_asian_width(0xA771, 0xA787, 'N').	% L&  [23] LATIN SMALL LETTER DUM..LATIN SMALL LETTER INSULAR T
unicode_east_asian_width(0xA788, 0xA788, 'N').	% Lm       MODIFIER LETTER LOW CIRCUMFLEX ACCENT
unicode_east_asian_width(0xA789, 0xA78A, 'N').	% Sk   [2] MODIFIER LETTER COLON..MODIFIER LETTER SHORT EQUALS SIGN
unicode_east_asian_width(0xA78B, 0xA78E, 'N').	% L&   [4] LATIN CAPITAL LETTER SALTILLO..LATIN SMALL LETTER L WITH RETROFLEX HOOK AND BELT
unicode_east_asian_width(0xA790, 0xA793, 'N').	% L&   [4] LATIN CAPITAL LETTER N WITH DESCENDER..LATIN SMALL LETTER C WITH BAR
unicode_east_asian_width(0xA7A0, 0xA7AA, 'N').	% L&  [11] LATIN CAPITAL LETTER G WITH OBLIQUE STROKE..LATIN CAPITAL LETTER H WITH HOOK
unicode_east_asian_width(0xA7F8, 0xA7F9, 'N').	% Lm   [2] MODIFIER LETTER CAPITAL H WITH STROKE..MODIFIER LETTER SMALL LIGATURE OE
unicode_east_asian_width(0xA7FA, 0xA7FA, 'N').	% L&       LATIN LETTER SMALL CAPITAL TURNED M
unicode_east_asian_width(0xA7FB, 0xA801, 'N').	% Lo   [7] LATIN EPIGRAPHIC LETTER REVERSED F..SYLOTI NAGRI LETTER I
unicode_east_asian_width(0xA802, 0xA802, 'N').	% Mn       SYLOTI NAGRI SIGN DVISVARA
unicode_east_asian_width(0xA803, 0xA805, 'N').	% Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_east_asian_width(0xA806, 0xA806, 'N').	% Mn       SYLOTI NAGRI SIGN HASANTA
unicode_east_asian_width(0xA807, 0xA80A, 'N').	% Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_east_asian_width(0xA80B, 0xA80B, 'N').	% Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_east_asian_width(0xA80C, 0xA822, 'N').	% Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_east_asian_width(0xA823, 0xA824, 'N').	% Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_east_asian_width(0xA825, 0xA826, 'N').	% Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_east_asian_width(0xA827, 0xA827, 'N').	% Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_east_asian_width(0xA828, 0xA82B, 'N').	% So   [4] SYLOTI NAGRI POETRY MARK-1..SYLOTI NAGRI POETRY MARK-4
unicode_east_asian_width(0xA830, 0xA835, 'N').	% No   [6] NORTH INDIC FRACTION ONE QUARTER..NORTH INDIC FRACTION THREE SIXTEENTHS
unicode_east_asian_width(0xA836, 0xA837, 'N').	% So   [2] NORTH INDIC QUARTER MARK..NORTH INDIC PLACEHOLDER MARK
unicode_east_asian_width(0xA838, 0xA838, 'N').	% Sc       NORTH INDIC RUPEE MARK
unicode_east_asian_width(0xA839, 0xA839, 'N').	% So       NORTH INDIC QUANTITY MARK
unicode_east_asian_width(0xA840, 0xA873, 'N').	% Lo  [52] PHAGS-PA LETTER KA..PHAGS-PA LETTER CANDRABINDU
unicode_east_asian_width(0xA874, 0xA877, 'N').	% Po   [4] PHAGS-PA SINGLE HEAD MARK..PHAGS-PA MARK DOUBLE SHAD
unicode_east_asian_width(0xA880, 0xA881, 'N').	% Mc   [2] SAURASHTRA SIGN ANUSVARA..SAURASHTRA SIGN VISARGA
unicode_east_asian_width(0xA882, 0xA8B3, 'N').	% Lo  [50] SAURASHTRA LETTER A..SAURASHTRA LETTER LLA
unicode_east_asian_width(0xA8B4, 0xA8C3, 'N').	% Mc  [16] SAURASHTRA CONSONANT SIGN HAARU..SAURASHTRA VOWEL SIGN AU
unicode_east_asian_width(0xA8C4, 0xA8C4, 'N').	% Mn       SAURASHTRA SIGN VIRAMA
unicode_east_asian_width(0xA8CE, 0xA8CF, 'N').	% Po   [2] SAURASHTRA DANDA..SAURASHTRA DOUBLE DANDA
unicode_east_asian_width(0xA8D0, 0xA8D9, 'N').	% Nd  [10] SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
unicode_east_asian_width(0xA8E0, 0xA8F1, 'N').	% Mn  [18] COMBINING DEVANAGARI DIGIT ZERO..COMBINING DEVANAGARI SIGN AVAGRAHA
unicode_east_asian_width(0xA8F2, 0xA8F7, 'N').	% Lo   [6] DEVANAGARI SIGN SPACING CANDRABINDU..DEVANAGARI SIGN CANDRABINDU AVAGRAHA
unicode_east_asian_width(0xA8F8, 0xA8FA, 'N').	% Po   [3] DEVANAGARI SIGN PUSHPIKA..DEVANAGARI CARET
unicode_east_asian_width(0xA8FB, 0xA8FB, 'N').	% Lo       DEVANAGARI HEADSTROKE
unicode_east_asian_width(0xA900, 0xA909, 'N').	% Nd  [10] KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
unicode_east_asian_width(0xA90A, 0xA925, 'N').	% Lo  [28] KAYAH LI LETTER KA..KAYAH LI LETTER OO
unicode_east_asian_width(0xA926, 0xA92D, 'N').	% Mn   [8] KAYAH LI VOWEL UE..KAYAH LI TONE CALYA PLOPHU
unicode_east_asian_width(0xA92E, 0xA92F, 'N').	% Po   [2] KAYAH LI SIGN CWI..KAYAH LI SIGN SHYA
unicode_east_asian_width(0xA930, 0xA946, 'N').	% Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_east_asian_width(0xA947, 0xA951, 'N').	% Mn  [11] REJANG VOWEL SIGN I..REJANG CONSONANT SIGN R
unicode_east_asian_width(0xA952, 0xA953, 'N').	% Mc   [2] REJANG CONSONANT SIGN H..REJANG VIRAMA
unicode_east_asian_width(0xA95F, 0xA95F, 'N').	% Po       REJANG SECTION MARK
unicode_east_asian_width(0xA980, 0xA982, 'N').	% Mn   [3] JAVANESE SIGN PANYANGGA..JAVANESE SIGN LAYAR
unicode_east_asian_width(0xA983, 0xA983, 'N').	% Mc       JAVANESE SIGN WIGNYAN
unicode_east_asian_width(0xA984, 0xA9B2, 'N').	% Lo  [47] JAVANESE LETTER A..JAVANESE LETTER HA
unicode_east_asian_width(0xA9B3, 0xA9B3, 'N').	% Mn       JAVANESE SIGN CECAK TELU
unicode_east_asian_width(0xA9B4, 0xA9B5, 'N').	% Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_east_asian_width(0xA9B6, 0xA9B9, 'N').	% Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_east_asian_width(0xA9BA, 0xA9BB, 'N').	% Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_east_asian_width(0xA9BC, 0xA9BC, 'N').	% Mn       JAVANESE VOWEL SIGN PEPET
unicode_east_asian_width(0xA9BD, 0xA9C0, 'N').	% Mc   [4] JAVANESE CONSONANT SIGN KERET..JAVANESE PANGKON
unicode_east_asian_width(0xA9C1, 0xA9CD, 'N').	% Po  [13] JAVANESE LEFT RERENGGAN..JAVANESE TURNED PADA PISELEH
unicode_east_asian_width(0xA9CF, 0xA9CF, 'N').	% Lm       JAVANESE PANGRANGKEP
unicode_east_asian_width(0xA9D0, 0xA9D9, 'N').	% Nd  [10] JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
unicode_east_asian_width(0xA9DE, 0xA9DF, 'N').	% Po   [2] JAVANESE PADA TIRTA TUMETES..JAVANESE PADA ISEN-ISEN
unicode_east_asian_width(0xAA00, 0xAA28, 'N').	% Lo  [41] CHAM LETTER A..CHAM LETTER HA
unicode_east_asian_width(0xAA29, 0xAA2E, 'N').	% Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_east_asian_width(0xAA2F, 0xAA30, 'N').	% Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_east_asian_width(0xAA31, 0xAA32, 'N').	% Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_east_asian_width(0xAA33, 0xAA34, 'N').	% Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_east_asian_width(0xAA35, 0xAA36, 'N').	% Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA
unicode_east_asian_width(0xAA40, 0xAA42, 'N').	% Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_east_asian_width(0xAA43, 0xAA43, 'N').	% Mn       CHAM CONSONANT SIGN FINAL NG
unicode_east_asian_width(0xAA44, 0xAA4B, 'N').	% Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_east_asian_width(0xAA4C, 0xAA4C, 'N').	% Mn       CHAM CONSONANT SIGN FINAL M
unicode_east_asian_width(0xAA4D, 0xAA4D, 'N').	% Mc       CHAM CONSONANT SIGN FINAL H
unicode_east_asian_width(0xAA50, 0xAA59, 'N').	% Nd  [10] CHAM DIGIT ZERO..CHAM DIGIT NINE
unicode_east_asian_width(0xAA5C, 0xAA5F, 'N').	% Po   [4] CHAM PUNCTUATION SPIRAL..CHAM PUNCTUATION TRIPLE DANDA
unicode_east_asian_width(0xAA60, 0xAA6F, 'N').	% Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_east_asian_width(0xAA70, 0xAA70, 'N').	% Lm       MYANMAR MODIFIER LETTER KHAMTI REDUPLICATION
unicode_east_asian_width(0xAA71, 0xAA76, 'N').	% Lo   [6] MYANMAR LETTER KHAMTI XA..MYANMAR LOGOGRAM KHAMTI HM
unicode_east_asian_width(0xAA77, 0xAA79, 'N').	% So   [3] MYANMAR SYMBOL AITON EXCLAMATION..MYANMAR SYMBOL AITON TWO
unicode_east_asian_width(0xAA7A, 0xAA7A, 'N').	% Lo       MYANMAR LETTER AITON RA
unicode_east_asian_width(0xAA7B, 0xAA7B, 'N').	% Mc       MYANMAR SIGN PAO KAREN TONE
unicode_east_asian_width(0xAA80, 0xAAAF, 'N').	% Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_east_asian_width(0xAAB0, 0xAAB0, 'N').	% Mn       TAI VIET MAI KANG
unicode_east_asian_width(0xAAB1, 0xAAB1, 'N').	% Lo       TAI VIET VOWEL AA
unicode_east_asian_width(0xAAB2, 0xAAB4, 'N').	% Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_east_asian_width(0xAAB5, 0xAAB6, 'N').	% Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_east_asian_width(0xAAB7, 0xAAB8, 'N').	% Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_east_asian_width(0xAAB9, 0xAABD, 'N').	% Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_east_asian_width(0xAABE, 0xAABF, 'N').	% Mn   [2] TAI VIET VOWEL AM..TAI VIET TONE MAI EK
unicode_east_asian_width(0xAAC0, 0xAAC0, 'N').	% Lo       TAI VIET TONE MAI NUENG
unicode_east_asian_width(0xAAC1, 0xAAC1, 'N').	% Mn       TAI VIET TONE MAI THO
unicode_east_asian_width(0xAAC2, 0xAAC2, 'N').	% Lo       TAI VIET TONE MAI SONG
unicode_east_asian_width(0xAADB, 0xAADC, 'N').	% Lo   [2] TAI VIET SYMBOL KON..TAI VIET SYMBOL NUENG
unicode_east_asian_width(0xAADD, 0xAADD, 'N').	% Lm       TAI VIET SYMBOL SAM
unicode_east_asian_width(0xAADE, 0xAADF, 'N').	% Po   [2] TAI VIET SYMBOL HO HOI..TAI VIET SYMBOL KOI KOI
unicode_east_asian_width(0xAAE0, 0xAAEA, 'N').	% Lo  [11] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER SSA
unicode_east_asian_width(0xAAEB, 0xAAEB, 'N').	% Mc       MEETEI MAYEK VOWEL SIGN II
unicode_east_asian_width(0xAAEC, 0xAAED, 'N').	% Mn   [2] MEETEI MAYEK VOWEL SIGN UU..MEETEI MAYEK VOWEL SIGN AAI
unicode_east_asian_width(0xAAEE, 0xAAEF, 'N').	% Mc   [2] MEETEI MAYEK VOWEL SIGN AU..MEETEI MAYEK VOWEL SIGN AAU
unicode_east_asian_width(0xAAF0, 0xAAF1, 'N').	% Po   [2] MEETEI MAYEK CHEIKHAN..MEETEI MAYEK AHANG KHUDAM
unicode_east_asian_width(0xAAF2, 0xAAF2, 'N').	% Lo       MEETEI MAYEK ANJI
unicode_east_asian_width(0xAAF3, 0xAAF4, 'N').	% Lm   [2] MEETEI MAYEK SYLLABLE REPETITION MARK..MEETEI MAYEK WORD REPETITION MARK
unicode_east_asian_width(0xAAF5, 0xAAF5, 'N').	% Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_east_asian_width(0xAAF6, 0xAAF6, 'N').	% Mn       MEETEI MAYEK VIRAMA
unicode_east_asian_width(0xAB01, 0xAB06, 'N').	% Lo   [6] ETHIOPIC SYLLABLE TTHU..ETHIOPIC SYLLABLE TTHO
unicode_east_asian_width(0xAB09, 0xAB0E, 'N').	% Lo   [6] ETHIOPIC SYLLABLE DDHU..ETHIOPIC SYLLABLE DDHO
unicode_east_asian_width(0xAB11, 0xAB16, 'N').	% Lo   [6] ETHIOPIC SYLLABLE DZU..ETHIOPIC SYLLABLE DZO
unicode_east_asian_width(0xAB20, 0xAB26, 'N').	% Lo   [7] ETHIOPIC SYLLABLE CCHHA..ETHIOPIC SYLLABLE CCHHO
unicode_east_asian_width(0xAB28, 0xAB2E, 'N').	% Lo   [7] ETHIOPIC SYLLABLE BBA..ETHIOPIC SYLLABLE BBO
unicode_east_asian_width(0xABC0, 0xABE2, 'N').	% Lo  [35] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER I LONSUM
unicode_east_asian_width(0xABE3, 0xABE4, 'N').	% Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_east_asian_width(0xABE5, 0xABE5, 'N').	% Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_east_asian_width(0xABE6, 0xABE7, 'N').	% Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_east_asian_width(0xABE8, 0xABE8, 'N').	% Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_east_asian_width(0xABE9, 0xABEA, 'N').	% Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_east_asian_width(0xABEB, 0xABEB, 'N').	% Po       MEETEI MAYEK CHEIKHEI
unicode_east_asian_width(0xABEC, 0xABEC, 'N').	% Mc       MEETEI MAYEK LUM IYEK
unicode_east_asian_width(0xABED, 0xABED, 'N').	% Mn       MEETEI MAYEK APUN IYEK
unicode_east_asian_width(0xABF0, 0xABF9, 'N').	% Nd  [10] MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
unicode_east_asian_width(0xFB00, 0xFB06, 'N').	% L&   [7] LATIN SMALL LIGATURE FF..LATIN SMALL LIGATURE ST
unicode_east_asian_width(0xFB13, 0xFB17, 'N').	% L&   [5] ARMENIAN SMALL LIGATURE MEN NOW..ARMENIAN SMALL LIGATURE MEN XEH
unicode_east_asian_width(0xFB1D, 0xFB1D, 'N').	% Lo       HEBREW LETTER YOD WITH HIRIQ
unicode_east_asian_width(0xFB1E, 0xFB1E, 'N').	% Mn       HEBREW POINT JUDEO-SPANISH VARIKA
unicode_east_asian_width(0xFB1F, 0xFB28, 'N').	% Lo  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
unicode_east_asian_width(0xFB29, 0xFB29, 'N').	% Sm       HEBREW LETTER ALTERNATIVE PLUS SIGN
unicode_east_asian_width(0xFB2A, 0xFB36, 'N').	% Lo  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
unicode_east_asian_width(0xFB38, 0xFB3C, 'N').	% Lo   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
unicode_east_asian_width(0xFB3E, 0xFB3E, 'N').	% Lo       HEBREW LETTER MEM WITH DAGESH
unicode_east_asian_width(0xFB40, 0xFB41, 'N').	% Lo   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
unicode_east_asian_width(0xFB43, 0xFB44, 'N').	% Lo   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
unicode_east_asian_width(0xFB46, 0xFBB1, 'N').	% Lo [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
unicode_east_asian_width(0xFBB2, 0xFBC1, 'N').	% Sk  [16] ARABIC SYMBOL DOT ABOVE..ARABIC SYMBOL SMALL TAH BELOW
unicode_east_asian_width(0xFBD3, 0xFD3D, 'N').	% Lo [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
unicode_east_asian_width(0xFD3E, 0xFD3E, 'N').	% Ps       ORNATE LEFT PARENTHESIS
unicode_east_asian_width(0xFD3F, 0xFD3F, 'N').	% Pe       ORNATE RIGHT PARENTHESIS
unicode_east_asian_width(0xFD50, 0xFD8F, 'N').	% Lo  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
unicode_east_asian_width(0xFD92, 0xFDC7, 'N').	% Lo  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
unicode_east_asian_width(0xFDF0, 0xFDFB, 'N').	% Lo  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
unicode_east_asian_width(0xFDFC, 0xFDFC, 'N').	% Sc       RIAL SIGN
unicode_east_asian_width(0xFDFD, 0xFDFD, 'N').	% So       ARABIC LIGATURE BISMILLAH AR-RAHMAN AR-RAHEEM
unicode_east_asian_width(0xFE20, 0xFE26, 'N').	% Mn   [7] COMBINING LIGATURE LEFT HALF..COMBINING CONJOINING MACRON
unicode_east_asian_width(0xFE70, 0xFE74, 'N').	% Lo   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
unicode_east_asian_width(0xFE76, 0xFEFC, 'N').	% Lo [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
unicode_east_asian_width(0xFEFF, 0xFEFF, 'N').	% Cf       ZERO WIDTH NO-BREAK SPACE
unicode_east_asian_width(0xFFF9, 0xFFFB, 'N').	% Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
unicode_east_asian_width(0xFFFC, 0xFFFC, 'N').	% So       OBJECT REPLACEMENT CHARACTER
unicode_east_asian_width(0x10000, 0x1000B, 'N').	% Lo  [12] LINEAR B SYLLABLE B008 A..LINEAR B SYLLABLE B046 JE
unicode_east_asian_width(0x1000D, 0x10026, 'N').	% Lo  [26] LINEAR B SYLLABLE B036 JO..LINEAR B SYLLABLE B032 QO
unicode_east_asian_width(0x10028, 0x1003A, 'N').	% Lo  [19] LINEAR B SYLLABLE B060 RA..LINEAR B SYLLABLE B042 WO
unicode_east_asian_width(0x1003C, 0x1003D, 'N').	% Lo   [2] LINEAR B SYLLABLE B017 ZA..LINEAR B SYLLABLE B074 ZE
unicode_east_asian_width(0x1003F, 0x1004D, 'N').	% Lo  [15] LINEAR B SYLLABLE B020 ZO..LINEAR B SYLLABLE B091 TWO
unicode_east_asian_width(0x10050, 0x1005D, 'N').	% Lo  [14] LINEAR B SYMBOL B018..LINEAR B SYMBOL B089
unicode_east_asian_width(0x10080, 0x100FA, 'N').	% Lo [123] LINEAR B IDEOGRAM B100 MAN..LINEAR B IDEOGRAM VESSEL B305
unicode_east_asian_width(0x10100, 0x10102, 'N').	% Po   [3] AEGEAN WORD SEPARATOR LINE..AEGEAN CHECK MARK
unicode_east_asian_width(0x10107, 0x10133, 'N').	% No  [45] AEGEAN NUMBER ONE..AEGEAN NUMBER NINETY THOUSAND
unicode_east_asian_width(0x10137, 0x1013F, 'N').	% So   [9] AEGEAN WEIGHT BASE UNIT..AEGEAN MEASURE THIRD SUBUNIT
unicode_east_asian_width(0x10140, 0x10174, 'N').	% Nl  [53] GREEK ACROPHONIC ATTIC ONE QUARTER..GREEK ACROPHONIC STRATIAN FIFTY MNAS
unicode_east_asian_width(0x10175, 0x10178, 'N').	% No   [4] GREEK ONE HALF SIGN..GREEK THREE QUARTERS SIGN
unicode_east_asian_width(0x10179, 0x10189, 'N').	% So  [17] GREEK YEAR SIGN..GREEK TRYBLION BASE SIGN
unicode_east_asian_width(0x1018A, 0x1018A, 'N').	% No       GREEK ZERO SIGN
unicode_east_asian_width(0x10190, 0x1019B, 'N').	% So  [12] ROMAN SEXTANS SIGN..ROMAN CENTURIAL SIGN
unicode_east_asian_width(0x101D0, 0x101FC, 'N').	% So  [45] PHAISTOS DISC SIGN PEDESTRIAN..PHAISTOS DISC SIGN WAVY BAND
unicode_east_asian_width(0x101FD, 0x101FD, 'N').	% Mn       PHAISTOS DISC SIGN COMBINING OBLIQUE STROKE
unicode_east_asian_width(0x10280, 0x1029C, 'N').	% Lo  [29] LYCIAN LETTER A..LYCIAN LETTER X
unicode_east_asian_width(0x102A0, 0x102D0, 'N').	% Lo  [49] CARIAN LETTER A..CARIAN LETTER UUU3
unicode_east_asian_width(0x10300, 0x1031E, 'N').	% Lo  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
unicode_east_asian_width(0x10320, 0x10323, 'N').	% No   [4] OLD ITALIC NUMERAL ONE..OLD ITALIC NUMERAL FIFTY
unicode_east_asian_width(0x10330, 0x10340, 'N').	% Lo  [17] GOTHIC LETTER AHSA..GOTHIC LETTER PAIRTHRA
unicode_east_asian_width(0x10341, 0x10341, 'N').	% Nl       GOTHIC LETTER NINETY
unicode_east_asian_width(0x10342, 0x10349, 'N').	% Lo   [8] GOTHIC LETTER RAIDA..GOTHIC LETTER OTHAL
unicode_east_asian_width(0x1034A, 0x1034A, 'N').	% Nl       GOTHIC LETTER NINE HUNDRED
unicode_east_asian_width(0x10380, 0x1039D, 'N').	% Lo  [30] UGARITIC LETTER ALPA..UGARITIC LETTER SSU
unicode_east_asian_width(0x1039F, 0x1039F, 'N').	% Po       UGARITIC WORD DIVIDER
unicode_east_asian_width(0x103A0, 0x103C3, 'N').	% Lo  [36] OLD PERSIAN SIGN A..OLD PERSIAN SIGN HA
unicode_east_asian_width(0x103C8, 0x103CF, 'N').	% Lo   [8] OLD PERSIAN SIGN AURAMAZDAA..OLD PERSIAN SIGN BUUMISH
unicode_east_asian_width(0x103D0, 0x103D0, 'N').	% Po       OLD PERSIAN WORD DIVIDER
unicode_east_asian_width(0x103D1, 0x103D5, 'N').	% Nl   [5] OLD PERSIAN NUMBER ONE..OLD PERSIAN NUMBER HUNDRED
unicode_east_asian_width(0x10400, 0x1044F, 'N').	% L&  [80] DESERET CAPITAL LETTER LONG I..DESERET SMALL LETTER EW
unicode_east_asian_width(0x10450, 0x1049D, 'N').	% Lo  [78] SHAVIAN LETTER PEEP..OSMANYA LETTER OO
unicode_east_asian_width(0x104A0, 0x104A9, 'N').	% Nd  [10] OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
unicode_east_asian_width(0x10800, 0x10805, 'N').	% Lo   [6] CYPRIOT SYLLABLE A..CYPRIOT SYLLABLE JA
unicode_east_asian_width(0x10808, 0x10808, 'N').	% Lo       CYPRIOT SYLLABLE JO
unicode_east_asian_width(0x1080A, 0x10835, 'N').	% Lo  [44] CYPRIOT SYLLABLE KA..CYPRIOT SYLLABLE WO
unicode_east_asian_width(0x10837, 0x10838, 'N').	% Lo   [2] CYPRIOT SYLLABLE XA..CYPRIOT SYLLABLE XE
unicode_east_asian_width(0x1083C, 0x1083C, 'N').	% Lo       CYPRIOT SYLLABLE ZA
unicode_east_asian_width(0x1083F, 0x10855, 'N').	% Lo  [23] CYPRIOT SYLLABLE ZO..IMPERIAL ARAMAIC LETTER TAW
unicode_east_asian_width(0x10857, 0x10857, 'N').	% Po       IMPERIAL ARAMAIC SECTION SIGN
unicode_east_asian_width(0x10858, 0x1085F, 'N').	% No   [8] IMPERIAL ARAMAIC NUMBER ONE..IMPERIAL ARAMAIC NUMBER TEN THOUSAND
unicode_east_asian_width(0x10900, 0x10915, 'N').	% Lo  [22] PHOENICIAN LETTER ALF..PHOENICIAN LETTER TAU
unicode_east_asian_width(0x10916, 0x1091B, 'N').	% No   [6] PHOENICIAN NUMBER ONE..PHOENICIAN NUMBER THREE
unicode_east_asian_width(0x1091F, 0x1091F, 'N').	% Po       PHOENICIAN WORD SEPARATOR
unicode_east_asian_width(0x10920, 0x10939, 'N').	% Lo  [26] LYDIAN LETTER A..LYDIAN LETTER C
unicode_east_asian_width(0x1093F, 0x1093F, 'N').	% Po       LYDIAN TRIANGULAR MARK
unicode_east_asian_width(0x10980, 0x109B7, 'N').	% Lo  [56] MEROITIC HIEROGLYPHIC LETTER A..MEROITIC CURSIVE LETTER DA
unicode_east_asian_width(0x109BE, 0x109BF, 'N').	% Lo   [2] MEROITIC CURSIVE LOGOGRAM RMT..MEROITIC CURSIVE LOGOGRAM IMN
unicode_east_asian_width(0x10A00, 0x10A00, 'N').	% Lo       KHAROSHTHI LETTER A
unicode_east_asian_width(0x10A01, 0x10A03, 'N').	% Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_east_asian_width(0x10A05, 0x10A06, 'N').	% Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_east_asian_width(0x10A0C, 0x10A0F, 'N').	% Mn   [4] KHAROSHTHI VOWEL LENGTH MARK..KHAROSHTHI SIGN VISARGA
unicode_east_asian_width(0x10A10, 0x10A13, 'N').	% Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_east_asian_width(0x10A15, 0x10A17, 'N').	% Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_east_asian_width(0x10A19, 0x10A33, 'N').	% Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_east_asian_width(0x10A38, 0x10A3A, 'N').	% Mn   [3] KHAROSHTHI SIGN BAR ABOVE..KHAROSHTHI SIGN DOT BELOW
unicode_east_asian_width(0x10A3F, 0x10A3F, 'N').	% Mn       KHAROSHTHI VIRAMA
unicode_east_asian_width(0x10A40, 0x10A47, 'N').	% No   [8] KHAROSHTHI DIGIT ONE..KHAROSHTHI NUMBER ONE THOUSAND
unicode_east_asian_width(0x10A50, 0x10A58, 'N').	% Po   [9] KHAROSHTHI PUNCTUATION DOT..KHAROSHTHI PUNCTUATION LINES
unicode_east_asian_width(0x10A60, 0x10A7C, 'N').	% Lo  [29] OLD SOUTH ARABIAN LETTER HE..OLD SOUTH ARABIAN LETTER THETH
unicode_east_asian_width(0x10A7D, 0x10A7E, 'N').	% No   [2] OLD SOUTH ARABIAN NUMBER ONE..OLD SOUTH ARABIAN NUMBER FIFTY
unicode_east_asian_width(0x10A7F, 0x10A7F, 'N').	% Po       OLD SOUTH ARABIAN NUMERIC INDICATOR
unicode_east_asian_width(0x10B00, 0x10B35, 'N').	% Lo  [54] AVESTAN LETTER A..AVESTAN LETTER HE
unicode_east_asian_width(0x10B39, 0x10B3F, 'N').	% Po   [7] AVESTAN ABBREVIATION MARK..LARGE ONE RING OVER TWO RINGS PUNCTUATION
unicode_east_asian_width(0x10B40, 0x10B55, 'N').	% Lo  [22] INSCRIPTIONAL PARTHIAN LETTER ALEPH..INSCRIPTIONAL PARTHIAN LETTER TAW
unicode_east_asian_width(0x10B58, 0x10B5F, 'N').	% No   [8] INSCRIPTIONAL PARTHIAN NUMBER ONE..INSCRIPTIONAL PARTHIAN NUMBER ONE THOUSAND
unicode_east_asian_width(0x10B60, 0x10B72, 'N').	% Lo  [19] INSCRIPTIONAL PAHLAVI LETTER ALEPH..INSCRIPTIONAL PAHLAVI LETTER TAW
unicode_east_asian_width(0x10B78, 0x10B7F, 'N').	% No   [8] INSCRIPTIONAL PAHLAVI NUMBER ONE..INSCRIPTIONAL PAHLAVI NUMBER ONE THOUSAND
unicode_east_asian_width(0x10C00, 0x10C48, 'N').	% Lo  [73] OLD TURKIC LETTER ORKHON A..OLD TURKIC LETTER ORKHON BASH
unicode_east_asian_width(0x10E60, 0x10E7E, 'N').	% No  [31] RUMI DIGIT ONE..RUMI FRACTION TWO THIRDS
unicode_east_asian_width(0x11000, 0x11000, 'N').	% Mc       BRAHMI SIGN CANDRABINDU
unicode_east_asian_width(0x11001, 0x11001, 'N').	% Mn       BRAHMI SIGN ANUSVARA
unicode_east_asian_width(0x11002, 0x11002, 'N').	% Mc       BRAHMI SIGN VISARGA
unicode_east_asian_width(0x11003, 0x11037, 'N').	% Lo  [53] BRAHMI SIGN JIHVAMULIYA..BRAHMI LETTER OLD TAMIL NNNA
unicode_east_asian_width(0x11038, 0x11046, 'N').	% Mn  [15] BRAHMI VOWEL SIGN AA..BRAHMI VIRAMA
unicode_east_asian_width(0x11047, 0x1104D, 'N').	% Po   [7] BRAHMI DANDA..BRAHMI PUNCTUATION LOTUS
unicode_east_asian_width(0x11052, 0x11065, 'N').	% No  [20] BRAHMI NUMBER ONE..BRAHMI NUMBER ONE THOUSAND
unicode_east_asian_width(0x11066, 0x1106F, 'N').	% Nd  [10] BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
unicode_east_asian_width(0x11080, 0x11081, 'N').	% Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA
unicode_east_asian_width(0x11082, 0x11082, 'N').	% Mc       KAITHI SIGN VISARGA
unicode_east_asian_width(0x11083, 0x110AF, 'N').	% Lo  [45] KAITHI LETTER A..KAITHI LETTER HA
unicode_east_asian_width(0x110B0, 0x110B2, 'N').	% Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_east_asian_width(0x110B3, 0x110B6, 'N').	% Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_east_asian_width(0x110B7, 0x110B8, 'N').	% Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_east_asian_width(0x110B9, 0x110BA, 'N').	% Mn   [2] KAITHI SIGN VIRAMA..KAITHI SIGN NUKTA
unicode_east_asian_width(0x110BB, 0x110BC, 'N').	% Po   [2] KAITHI ABBREVIATION SIGN..KAITHI ENUMERATION SIGN
unicode_east_asian_width(0x110BD, 0x110BD, 'N').	% Cf       KAITHI NUMBER SIGN
unicode_east_asian_width(0x110BE, 0x110C1, 'N').	% Po   [4] KAITHI SECTION MARK..KAITHI DOUBLE DANDA
unicode_east_asian_width(0x110D0, 0x110E8, 'N').	% Lo  [25] SORA SOMPENG LETTER SAH..SORA SOMPENG LETTER MAE
unicode_east_asian_width(0x110F0, 0x110F9, 'N').	% Nd  [10] SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
unicode_east_asian_width(0x11100, 0x11102, 'N').	% Mn   [3] CHAKMA SIGN CANDRABINDU..CHAKMA SIGN VISARGA
unicode_east_asian_width(0x11103, 0x11126, 'N').	% Lo  [36] CHAKMA LETTER AA..CHAKMA LETTER HAA
unicode_east_asian_width(0x11127, 0x1112B, 'N').	% Mn   [5] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN UU
unicode_east_asian_width(0x1112C, 0x1112C, 'N').	% Mc       CHAKMA VOWEL SIGN E
unicode_east_asian_width(0x1112D, 0x11134, 'N').	% Mn   [8] CHAKMA VOWEL SIGN AI..CHAKMA MAAYYAA
unicode_east_asian_width(0x11136, 0x1113F, 'N').	% Nd  [10] CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
unicode_east_asian_width(0x11140, 0x11143, 'N').	% Po   [4] CHAKMA SECTION MARK..CHAKMA QUESTION MARK
unicode_east_asian_width(0x11180, 0x11181, 'N').	% Mn   [2] SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_east_asian_width(0x11182, 0x11182, 'N').	% Mc       SHARADA SIGN VISARGA
unicode_east_asian_width(0x11183, 0x111B2, 'N').	% Lo  [48] SHARADA LETTER A..SHARADA LETTER HA
unicode_east_asian_width(0x111B3, 0x111B5, 'N').	% Mc   [3] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN II
unicode_east_asian_width(0x111B6, 0x111BE, 'N').	% Mn   [9] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN O
unicode_east_asian_width(0x111BF, 0x111C0, 'N').	% Mc   [2] SHARADA VOWEL SIGN AU..SHARADA SIGN VIRAMA
unicode_east_asian_width(0x111C1, 0x111C4, 'N').	% Lo   [4] SHARADA SIGN AVAGRAHA..SHARADA OM
unicode_east_asian_width(0x111C5, 0x111C8, 'N').	% Po   [4] SHARADA DANDA..SHARADA SEPARATOR
unicode_east_asian_width(0x111D0, 0x111D9, 'N').	% Nd  [10] SHARADA DIGIT ZERO..SHARADA DIGIT NINE
unicode_east_asian_width(0x11680, 0x116AA, 'N').	% Lo  [43] TAKRI LETTER A..TAKRI LETTER RRA
unicode_east_asian_width(0x116AB, 0x116AB, 'N').	% Mn       TAKRI SIGN ANUSVARA
unicode_east_asian_width(0x116AC, 0x116AC, 'N').	% Mc       TAKRI SIGN VISARGA
unicode_east_asian_width(0x116AD, 0x116AD, 'N').	% Mn       TAKRI VOWEL SIGN AA
unicode_east_asian_width(0x116AE, 0x116AF, 'N').	% Mc   [2] TAKRI VOWEL SIGN I..TAKRI VOWEL SIGN II
unicode_east_asian_width(0x116B0, 0x116B5, 'N').	% Mn   [6] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN AU
unicode_east_asian_width(0x116B6, 0x116B6, 'N').	% Mc       TAKRI SIGN VIRAMA
unicode_east_asian_width(0x116B7, 0x116B7, 'N').	% Mn       TAKRI SIGN NUKTA
unicode_east_asian_width(0x116C0, 0x116C9, 'N').	% Nd  [10] TAKRI DIGIT ZERO..TAKRI DIGIT NINE
unicode_east_asian_width(0x12000, 0x1236E, 'N').	% Lo [879] CUNEIFORM SIGN A..CUNEIFORM SIGN ZUM
unicode_east_asian_width(0x12400, 0x12462, 'N').	% Nl  [99] CUNEIFORM NUMERIC SIGN TWO ASH..CUNEIFORM NUMERIC SIGN OLD ASSYRIAN ONE QUARTER
unicode_east_asian_width(0x12470, 0x12473, 'N').	% Po   [4] CUNEIFORM PUNCTUATION SIGN OLD ASSYRIAN WORD DIVIDER..CUNEIFORM PUNCTUATION SIGN DIAGONAL TRICOLON
unicode_east_asian_width(0x13000, 0x1342E, 'N').	% Lo [1071] EGYPTIAN HIEROGLYPH A001..EGYPTIAN HIEROGLYPH AA032
unicode_east_asian_width(0x16800, 0x16A38, 'N').	% Lo [569] BAMUM LETTER PHASE-A NGKUE MFON..BAMUM LETTER PHASE-F VUEQ
unicode_east_asian_width(0x16F00, 0x16F44, 'N').	% Lo  [69] MIAO LETTER PA..MIAO LETTER HHA
unicode_east_asian_width(0x16F50, 0x16F50, 'N').	% Lo       MIAO LETTER NASALIZATION
unicode_east_asian_width(0x16F51, 0x16F7E, 'N').	% Mc  [46] MIAO SIGN ASPIRATION..MIAO VOWEL SIGN NG
unicode_east_asian_width(0x16F8F, 0x16F92, 'N').	% Mn   [4] MIAO TONE RIGHT..MIAO TONE BELOW
unicode_east_asian_width(0x16F93, 0x16F9F, 'N').	% Lm  [13] MIAO LETTER TONE-2..MIAO LETTER REFORMED TONE-8
unicode_east_asian_width(0x1D000, 0x1D0F5, 'N').	% So [246] BYZANTINE MUSICAL SYMBOL PSILI..BYZANTINE MUSICAL SYMBOL GORGON NEO KATO
unicode_east_asian_width(0x1D100, 0x1D126, 'N').	% So  [39] MUSICAL SYMBOL SINGLE BARLINE..MUSICAL SYMBOL DRUM CLEF-2
unicode_east_asian_width(0x1D129, 0x1D164, 'N').	% So  [60] MUSICAL SYMBOL MULTIPLE MEASURE REST..MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
unicode_east_asian_width(0x1D165, 0x1D166, 'N').	% Mc   [2] MUSICAL SYMBOL COMBINING STEM..MUSICAL SYMBOL COMBINING SPRECHGESANG STEM
unicode_east_asian_width(0x1D167, 0x1D169, 'N').	% Mn   [3] MUSICAL SYMBOL COMBINING TREMOLO-1..MUSICAL SYMBOL COMBINING TREMOLO-3
unicode_east_asian_width(0x1D16A, 0x1D16C, 'N').	% So   [3] MUSICAL SYMBOL FINGERED TREMOLO-1..MUSICAL SYMBOL FINGERED TREMOLO-3
unicode_east_asian_width(0x1D16D, 0x1D172, 'N').	% Mc   [6] MUSICAL SYMBOL COMBINING AUGMENTATION DOT..MUSICAL SYMBOL COMBINING FLAG-5
unicode_east_asian_width(0x1D173, 0x1D17A, 'N').	% Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
unicode_east_asian_width(0x1D17B, 0x1D182, 'N').	% Mn   [8] MUSICAL SYMBOL COMBINING ACCENT..MUSICAL SYMBOL COMBINING LOURE
unicode_east_asian_width(0x1D183, 0x1D184, 'N').	% So   [2] MUSICAL SYMBOL ARPEGGIATO UP..MUSICAL SYMBOL ARPEGGIATO DOWN
unicode_east_asian_width(0x1D185, 0x1D18B, 'N').	% Mn   [7] MUSICAL SYMBOL COMBINING DOIT..MUSICAL SYMBOL COMBINING TRIPLE TONGUE
unicode_east_asian_width(0x1D18C, 0x1D1A9, 'N').	% So  [30] MUSICAL SYMBOL RINFORZANDO..MUSICAL SYMBOL DEGREE SLASH
unicode_east_asian_width(0x1D1AA, 0x1D1AD, 'N').	% Mn   [4] MUSICAL SYMBOL COMBINING DOWN BOW..MUSICAL SYMBOL COMBINING SNAP PIZZICATO
unicode_east_asian_width(0x1D1AE, 0x1D1DD, 'N').	% So  [48] MUSICAL SYMBOL PEDAL MARK..MUSICAL SYMBOL PES SUBPUNCTIS
unicode_east_asian_width(0x1D200, 0x1D241, 'N').	% So  [66] GREEK VOCAL NOTATION SYMBOL-1..GREEK INSTRUMENTAL NOTATION SYMBOL-54
unicode_east_asian_width(0x1D242, 0x1D244, 'N').	% Mn   [3] COMBINING GREEK MUSICAL TRISEME..COMBINING GREEK MUSICAL PENTASEME
unicode_east_asian_width(0x1D245, 0x1D245, 'N').	% So       GREEK MUSICAL LEIMMA
unicode_east_asian_width(0x1D300, 0x1D356, 'N').	% So  [87] MONOGRAM FOR EARTH..TETRAGRAM FOR FOSTERING
unicode_east_asian_width(0x1D360, 0x1D371, 'N').	% No  [18] COUNTING ROD UNIT DIGIT ONE..COUNTING ROD TENS DIGIT NINE
unicode_east_asian_width(0x1D400, 0x1D454, 'N').	% L&  [85] MATHEMATICAL BOLD CAPITAL A..MATHEMATICAL ITALIC SMALL G
unicode_east_asian_width(0x1D456, 0x1D49C, 'N').	% L&  [71] MATHEMATICAL ITALIC SMALL I..MATHEMATICAL SCRIPT CAPITAL A
unicode_east_asian_width(0x1D49E, 0x1D49F, 'N').	% L&   [2] MATHEMATICAL SCRIPT CAPITAL C..MATHEMATICAL SCRIPT CAPITAL D
unicode_east_asian_width(0x1D4A2, 0x1D4A2, 'N').	% L&       MATHEMATICAL SCRIPT CAPITAL G
unicode_east_asian_width(0x1D4A5, 0x1D4A6, 'N').	% L&   [2] MATHEMATICAL SCRIPT CAPITAL J..MATHEMATICAL SCRIPT CAPITAL K
unicode_east_asian_width(0x1D4A9, 0x1D4AC, 'N').	% L&   [4] MATHEMATICAL SCRIPT CAPITAL N..MATHEMATICAL SCRIPT CAPITAL Q
unicode_east_asian_width(0x1D4AE, 0x1D4B9, 'N').	% L&  [12] MATHEMATICAL SCRIPT CAPITAL S..MATHEMATICAL SCRIPT SMALL D
unicode_east_asian_width(0x1D4BB, 0x1D4BB, 'N').	% L&       MATHEMATICAL SCRIPT SMALL F
unicode_east_asian_width(0x1D4BD, 0x1D4C3, 'N').	% L&   [7] MATHEMATICAL SCRIPT SMALL H..MATHEMATICAL SCRIPT SMALL N
unicode_east_asian_width(0x1D4C5, 0x1D505, 'N').	% L&  [65] MATHEMATICAL SCRIPT SMALL P..MATHEMATICAL FRAKTUR CAPITAL B
unicode_east_asian_width(0x1D507, 0x1D50A, 'N').	% L&   [4] MATHEMATICAL FRAKTUR CAPITAL D..MATHEMATICAL FRAKTUR CAPITAL G
unicode_east_asian_width(0x1D50D, 0x1D514, 'N').	% L&   [8] MATHEMATICAL FRAKTUR CAPITAL J..MATHEMATICAL FRAKTUR CAPITAL Q
unicode_east_asian_width(0x1D516, 0x1D51C, 'N').	% L&   [7] MATHEMATICAL FRAKTUR CAPITAL S..MATHEMATICAL FRAKTUR CAPITAL Y
unicode_east_asian_width(0x1D51E, 0x1D539, 'N').	% L&  [28] MATHEMATICAL FRAKTUR SMALL A..MATHEMATICAL DOUBLE-STRUCK CAPITAL B
unicode_east_asian_width(0x1D53B, 0x1D53E, 'N').	% L&   [4] MATHEMATICAL DOUBLE-STRUCK CAPITAL D..MATHEMATICAL DOUBLE-STRUCK CAPITAL G
unicode_east_asian_width(0x1D540, 0x1D544, 'N').	% L&   [5] MATHEMATICAL DOUBLE-STRUCK CAPITAL I..MATHEMATICAL DOUBLE-STRUCK CAPITAL M
unicode_east_asian_width(0x1D546, 0x1D546, 'N').	% L&       MATHEMATICAL DOUBLE-STRUCK CAPITAL O
unicode_east_asian_width(0x1D54A, 0x1D550, 'N').	% L&   [7] MATHEMATICAL DOUBLE-STRUCK CAPITAL S..MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
unicode_east_asian_width(0x1D552, 0x1D6A5, 'N').	% L& [340] MATHEMATICAL DOUBLE-STRUCK SMALL A..MATHEMATICAL ITALIC SMALL DOTLESS J
unicode_east_asian_width(0x1D6A8, 0x1D6C0, 'N').	% L&  [25] MATHEMATICAL BOLD CAPITAL ALPHA..MATHEMATICAL BOLD CAPITAL OMEGA
unicode_east_asian_width(0x1D6C1, 0x1D6C1, 'N').	% Sm       MATHEMATICAL BOLD NABLA
unicode_east_asian_width(0x1D6C2, 0x1D6DA, 'N').	% L&  [25] MATHEMATICAL BOLD SMALL ALPHA..MATHEMATICAL BOLD SMALL OMEGA
unicode_east_asian_width(0x1D6DB, 0x1D6DB, 'N').	% Sm       MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
unicode_east_asian_width(0x1D6DC, 0x1D6FA, 'N').	% L&  [31] MATHEMATICAL BOLD EPSILON SYMBOL..MATHEMATICAL ITALIC CAPITAL OMEGA
unicode_east_asian_width(0x1D6FB, 0x1D6FB, 'N').	% Sm       MATHEMATICAL ITALIC NABLA
unicode_east_asian_width(0x1D6FC, 0x1D714, 'N').	% L&  [25] MATHEMATICAL ITALIC SMALL ALPHA..MATHEMATICAL ITALIC SMALL OMEGA
unicode_east_asian_width(0x1D715, 0x1D715, 'N').	% Sm       MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
unicode_east_asian_width(0x1D716, 0x1D734, 'N').	% L&  [31] MATHEMATICAL ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
unicode_east_asian_width(0x1D735, 0x1D735, 'N').	% Sm       MATHEMATICAL BOLD ITALIC NABLA
unicode_east_asian_width(0x1D736, 0x1D74E, 'N').	% L&  [25] MATHEMATICAL BOLD ITALIC SMALL ALPHA..MATHEMATICAL BOLD ITALIC SMALL OMEGA
unicode_east_asian_width(0x1D74F, 0x1D74F, 'N').	% Sm       MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_east_asian_width(0x1D750, 0x1D76E, 'N').	% L&  [31] MATHEMATICAL BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
unicode_east_asian_width(0x1D76F, 0x1D76F, 'N').	% Sm       MATHEMATICAL SANS-SERIF BOLD NABLA
unicode_east_asian_width(0x1D770, 0x1D788, 'N').	% L&  [25] MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
unicode_east_asian_width(0x1D789, 0x1D789, 'N').	% Sm       MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
unicode_east_asian_width(0x1D78A, 0x1D7A8, 'N').	% L&  [31] MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL..MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
unicode_east_asian_width(0x1D7A9, 0x1D7A9, 'N').	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
unicode_east_asian_width(0x1D7AA, 0x1D7C2, 'N').	% L&  [25] MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA..MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
unicode_east_asian_width(0x1D7C3, 0x1D7C3, 'N').	% Sm       MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
unicode_east_asian_width(0x1D7C4, 0x1D7CB, 'N').	% L&   [8] MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL..MATHEMATICAL BOLD SMALL DIGAMMA
unicode_east_asian_width(0x1D7CE, 0x1D7FF, 'N').	% Nd  [50] MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
unicode_east_asian_width(0x1EE00, 0x1EE03, 'N').	% Lo   [4] ARABIC MATHEMATICAL ALEF..ARABIC MATHEMATICAL DAL
unicode_east_asian_width(0x1EE05, 0x1EE1F, 'N').	% Lo  [27] ARABIC MATHEMATICAL WAW..ARABIC MATHEMATICAL DOTLESS QAF
unicode_east_asian_width(0x1EE21, 0x1EE22, 'N').	% Lo   [2] ARABIC MATHEMATICAL INITIAL BEH..ARABIC MATHEMATICAL INITIAL JEEM
unicode_east_asian_width(0x1EE24, 0x1EE24, 'N').	% Lo       ARABIC MATHEMATICAL INITIAL HEH
unicode_east_asian_width(0x1EE27, 0x1EE27, 'N').	% Lo       ARABIC MATHEMATICAL INITIAL HAH
unicode_east_asian_width(0x1EE29, 0x1EE32, 'N').	% Lo  [10] ARABIC MATHEMATICAL INITIAL YEH..ARABIC MATHEMATICAL INITIAL QAF
unicode_east_asian_width(0x1EE34, 0x1EE37, 'N').	% Lo   [4] ARABIC MATHEMATICAL INITIAL SHEEN..ARABIC MATHEMATICAL INITIAL KHAH
unicode_east_asian_width(0x1EE39, 0x1EE39, 'N').	% Lo       ARABIC MATHEMATICAL INITIAL DAD
unicode_east_asian_width(0x1EE3B, 0x1EE3B, 'N').	% Lo       ARABIC MATHEMATICAL INITIAL GHAIN
unicode_east_asian_width(0x1EE42, 0x1EE42, 'N').	% Lo       ARABIC MATHEMATICAL TAILED JEEM
unicode_east_asian_width(0x1EE47, 0x1EE47, 'N').	% Lo       ARABIC MATHEMATICAL TAILED HAH
unicode_east_asian_width(0x1EE49, 0x1EE49, 'N').	% Lo       ARABIC MATHEMATICAL TAILED YEH
unicode_east_asian_width(0x1EE4B, 0x1EE4B, 'N').	% Lo       ARABIC MATHEMATICAL TAILED LAM
unicode_east_asian_width(0x1EE4D, 0x1EE4F, 'N').	% Lo   [3] ARABIC MATHEMATICAL TAILED NOON..ARABIC MATHEMATICAL TAILED AIN
unicode_east_asian_width(0x1EE51, 0x1EE52, 'N').	% Lo   [2] ARABIC MATHEMATICAL TAILED SAD..ARABIC MATHEMATICAL TAILED QAF
unicode_east_asian_width(0x1EE54, 0x1EE54, 'N').	% Lo       ARABIC MATHEMATICAL TAILED SHEEN
unicode_east_asian_width(0x1EE57, 0x1EE57, 'N').	% Lo       ARABIC MATHEMATICAL TAILED KHAH
unicode_east_asian_width(0x1EE59, 0x1EE59, 'N').	% Lo       ARABIC MATHEMATICAL TAILED DAD
unicode_east_asian_width(0x1EE5B, 0x1EE5B, 'N').	% Lo       ARABIC MATHEMATICAL TAILED GHAIN
unicode_east_asian_width(0x1EE5D, 0x1EE5D, 'N').	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS NOON
unicode_east_asian_width(0x1EE5F, 0x1EE5F, 'N').	% Lo       ARABIC MATHEMATICAL TAILED DOTLESS QAF
unicode_east_asian_width(0x1EE61, 0x1EE62, 'N').	% Lo   [2] ARABIC MATHEMATICAL STRETCHED BEH..ARABIC MATHEMATICAL STRETCHED JEEM
unicode_east_asian_width(0x1EE64, 0x1EE64, 'N').	% Lo       ARABIC MATHEMATICAL STRETCHED HEH
unicode_east_asian_width(0x1EE67, 0x1EE6A, 'N').	% Lo   [4] ARABIC MATHEMATICAL STRETCHED HAH..ARABIC MATHEMATICAL STRETCHED KAF
unicode_east_asian_width(0x1EE6C, 0x1EE72, 'N').	% Lo   [7] ARABIC MATHEMATICAL STRETCHED MEEM..ARABIC MATHEMATICAL STRETCHED QAF
unicode_east_asian_width(0x1EE74, 0x1EE77, 'N').	% Lo   [4] ARABIC MATHEMATICAL STRETCHED SHEEN..ARABIC MATHEMATICAL STRETCHED KHAH
unicode_east_asian_width(0x1EE79, 0x1EE7C, 'N').	% Lo   [4] ARABIC MATHEMATICAL STRETCHED DAD..ARABIC MATHEMATICAL STRETCHED DOTLESS BEH
unicode_east_asian_width(0x1EE7E, 0x1EE7E, 'N').	% Lo       ARABIC MATHEMATICAL STRETCHED DOTLESS FEH
unicode_east_asian_width(0x1EE80, 0x1EE89, 'N').	% Lo  [10] ARABIC MATHEMATICAL LOOPED ALEF..ARABIC MATHEMATICAL LOOPED YEH
unicode_east_asian_width(0x1EE8B, 0x1EE9B, 'N').	% Lo  [17] ARABIC MATHEMATICAL LOOPED LAM..ARABIC MATHEMATICAL LOOPED GHAIN
unicode_east_asian_width(0x1EEA1, 0x1EEA3, 'N').	% Lo   [3] ARABIC MATHEMATICAL DOUBLE-STRUCK BEH..ARABIC MATHEMATICAL DOUBLE-STRUCK DAL
unicode_east_asian_width(0x1EEA5, 0x1EEA9, 'N').	% Lo   [5] ARABIC MATHEMATICAL DOUBLE-STRUCK WAW..ARABIC MATHEMATICAL DOUBLE-STRUCK YEH
unicode_east_asian_width(0x1EEAB, 0x1EEBB, 'N').	% Lo  [17] ARABIC MATHEMATICAL DOUBLE-STRUCK LAM..ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN
unicode_east_asian_width(0x1EEF0, 0x1EEF1, 'N').	% Sm   [2] ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL..ARABIC MATHEMATICAL OPERATOR HAH WITH DAL
unicode_east_asian_width(0x1F000, 0x1F02B, 'N').	% So  [44] MAHJONG TILE EAST WIND..MAHJONG TILE BACK
unicode_east_asian_width(0x1F030, 0x1F093, 'N').	% So [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
unicode_east_asian_width(0x1F0A0, 0x1F0AE, 'N').	% So  [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
unicode_east_asian_width(0x1F0B1, 0x1F0BE, 'N').	% So  [14] PLAYING CARD ACE OF HEARTS..PLAYING CARD KING OF HEARTS
unicode_east_asian_width(0x1F0C1, 0x1F0CF, 'N').	% So  [15] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD BLACK JOKER
unicode_east_asian_width(0x1F0D1, 0x1F0DF, 'N').	% So  [15] PLAYING CARD ACE OF CLUBS..PLAYING CARD WHITE JOKER
unicode_east_asian_width(0x1F12E, 0x1F12E, 'N').	% So       CIRCLED WZ
unicode_east_asian_width(0x1F16A, 0x1F16B, 'N').	% So   [2] RAISED MC SIGN..RAISED MD SIGN
unicode_east_asian_width(0x1F1E6, 0x1F1FF, 'N').	% So  [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
unicode_east_asian_width(0x1F300, 0x1F320, 'N').	% So  [33] CYCLONE..SHOOTING STAR
unicode_east_asian_width(0x1F330, 0x1F335, 'N').	% So   [6] CHESTNUT..CACTUS
unicode_east_asian_width(0x1F337, 0x1F37C, 'N').	% So  [70] TULIP..BABY BOTTLE
unicode_east_asian_width(0x1F380, 0x1F393, 'N').	% So  [20] RIBBON..GRADUATION CAP
unicode_east_asian_width(0x1F3A0, 0x1F3C4, 'N').	% So  [37] CAROUSEL HORSE..SURFER
unicode_east_asian_width(0x1F3C6, 0x1F3CA, 'N').	% So   [5] TROPHY..SWIMMER
unicode_east_asian_width(0x1F3E0, 0x1F3F0, 'N').	% So  [17] HOUSE BUILDING..EUROPEAN CASTLE
unicode_east_asian_width(0x1F400, 0x1F43E, 'N').	% So  [63] RAT..PAW PRINTS
unicode_east_asian_width(0x1F440, 0x1F440, 'N').	% So       EYES
unicode_east_asian_width(0x1F442, 0x1F4F7, 'N').	% So [182] EAR..CAMERA
unicode_east_asian_width(0x1F4F9, 0x1F4FC, 'N').	% So   [4] VIDEO CAMERA..VIDEOCASSETTE
unicode_east_asian_width(0x1F500, 0x1F53D, 'N').	% So  [62] TWISTED RIGHTWARDS ARROWS..DOWN-POINTING SMALL RED TRIANGLE
unicode_east_asian_width(0x1F540, 0x1F543, 'N').	% So   [4] CIRCLED CROSS POMMEE..NOTCHED LEFT SEMICIRCLE WITH THREE DOTS
unicode_east_asian_width(0x1F550, 0x1F567, 'N').	% So  [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
unicode_east_asian_width(0x1F5FB, 0x1F640, 'N').	% So  [70] MOUNT FUJI..WEARY CAT FACE
unicode_east_asian_width(0x1F645, 0x1F64F, 'N').	% So  [11] FACE WITH NO GOOD GESTURE..PERSON WITH FOLDED HANDS
unicode_east_asian_width(0x1F680, 0x1F6C5, 'N').	% So  [70] ROCKET..LEFT LUGGAGE
unicode_east_asian_width(0x1F700, 0x1F773, 'N').	% So [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
unicode_east_asian_width(0xE0001, 0xE0001, 'N').	% Cf       LANGUAGE TAG
unicode_east_asian_width(0xE0020, 0xE007F, 'N').	% Cf  [96] TAG SPACE..CANCEL TAG

% The above property value applies to 782918 code points not listed here.
% Total code points: 801811

% ================================================

% East_Asian_Width=Ambiguous

unicode_east_asian_width(0x00A1, 0x00A1, 'A').	% Po       INVERTED EXCLAMATION MARK
unicode_east_asian_width(0x00A4, 0x00A4, 'A').	% Sc       CURRENCY SIGN
unicode_east_asian_width(0x00A7, 0x00A7, 'A').	% Po       SECTION SIGN
unicode_east_asian_width(0x00A8, 0x00A8, 'A').	% Sk       DIAERESIS
unicode_east_asian_width(0x00AA, 0x00AA, 'A').	% Lo       FEMININE ORDINAL INDICATOR
unicode_east_asian_width(0x00AD, 0x00AD, 'A').	% Cf       SOFT HYPHEN
unicode_east_asian_width(0x00AE, 0x00AE, 'A').	% So       REGISTERED SIGN
unicode_east_asian_width(0x00B0, 0x00B0, 'A').	% So       DEGREE SIGN
unicode_east_asian_width(0x00B1, 0x00B1, 'A').	% Sm       PLUS-MINUS SIGN
unicode_east_asian_width(0x00B2, 0x00B3, 'A').	% No   [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
unicode_east_asian_width(0x00B4, 0x00B4, 'A').	% Sk       ACUTE ACCENT
unicode_east_asian_width(0x00B6, 0x00B7, 'A').	% Po   [2] PILCROW SIGN..MIDDLE DOT
unicode_east_asian_width(0x00B8, 0x00B8, 'A').	% Sk       CEDILLA
unicode_east_asian_width(0x00B9, 0x00B9, 'A').	% No       SUPERSCRIPT ONE
unicode_east_asian_width(0x00BA, 0x00BA, 'A').	% Lo       MASCULINE ORDINAL INDICATOR
unicode_east_asian_width(0x00BC, 0x00BE, 'A').	% No   [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
unicode_east_asian_width(0x00BF, 0x00BF, 'A').	% Po       INVERTED QUESTION MARK
unicode_east_asian_width(0x00C6, 0x00C6, 'A').	% L&       LATIN CAPITAL LETTER AE
unicode_east_asian_width(0x00D0, 0x00D0, 'A').	% L&       LATIN CAPITAL LETTER ETH
unicode_east_asian_width(0x00D7, 0x00D7, 'A').	% Sm       MULTIPLICATION SIGN
unicode_east_asian_width(0x00D8, 0x00D8, 'A').	% L&       LATIN CAPITAL LETTER O WITH STROKE
unicode_east_asian_width(0x00DE, 0x00E1, 'A').	% L&   [4] LATIN CAPITAL LETTER THORN..LATIN SMALL LETTER A WITH ACUTE
unicode_east_asian_width(0x00E6, 0x00E6, 'A').	% L&       LATIN SMALL LETTER AE
unicode_east_asian_width(0x00E8, 0x00EA, 'A').	% L&   [3] LATIN SMALL LETTER E WITH GRAVE..LATIN SMALL LETTER E WITH CIRCUMFLEX
unicode_east_asian_width(0x00EC, 0x00ED, 'A').	% L&   [2] LATIN SMALL LETTER I WITH GRAVE..LATIN SMALL LETTER I WITH ACUTE
unicode_east_asian_width(0x00F0, 0x00F0, 'A').	% L&       LATIN SMALL LETTER ETH
unicode_east_asian_width(0x00F2, 0x00F3, 'A').	% L&   [2] LATIN SMALL LETTER O WITH GRAVE..LATIN SMALL LETTER O WITH ACUTE
unicode_east_asian_width(0x00F7, 0x00F7, 'A').	% Sm       DIVISION SIGN
unicode_east_asian_width(0x00F8, 0x00FA, 'A').	% L&   [3] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER U WITH ACUTE
unicode_east_asian_width(0x00FC, 0x00FC, 'A').	% L&       LATIN SMALL LETTER U WITH DIAERESIS
unicode_east_asian_width(0x00FE, 0x00FE, 'A').	% L&       LATIN SMALL LETTER THORN
unicode_east_asian_width(0x0101, 0x0101, 'A').	% L&       LATIN SMALL LETTER A WITH MACRON
unicode_east_asian_width(0x0111, 0x0111, 'A').	% L&       LATIN SMALL LETTER D WITH STROKE
unicode_east_asian_width(0x0113, 0x0113, 'A').	% L&       LATIN SMALL LETTER E WITH MACRON
unicode_east_asian_width(0x011B, 0x011B, 'A').	% L&       LATIN SMALL LETTER E WITH CARON
unicode_east_asian_width(0x0126, 0x0127, 'A').	% L&   [2] LATIN CAPITAL LETTER H WITH STROKE..LATIN SMALL LETTER H WITH STROKE
unicode_east_asian_width(0x012B, 0x012B, 'A').	% L&       LATIN SMALL LETTER I WITH MACRON
unicode_east_asian_width(0x0131, 0x0133, 'A').	% L&   [3] LATIN SMALL LETTER DOTLESS I..LATIN SMALL LIGATURE IJ
unicode_east_asian_width(0x0138, 0x0138, 'A').	% L&       LATIN SMALL LETTER KRA
unicode_east_asian_width(0x013F, 0x0142, 'A').	% L&   [4] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH STROKE
unicode_east_asian_width(0x0144, 0x0144, 'A').	% L&       LATIN SMALL LETTER N WITH ACUTE
unicode_east_asian_width(0x0148, 0x014B, 'A').	% L&   [4] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER ENG
unicode_east_asian_width(0x014D, 0x014D, 'A').	% L&       LATIN SMALL LETTER O WITH MACRON
unicode_east_asian_width(0x0152, 0x0153, 'A').	% L&   [2] LATIN CAPITAL LIGATURE OE..LATIN SMALL LIGATURE OE
unicode_east_asian_width(0x0166, 0x0167, 'A').	% L&   [2] LATIN CAPITAL LETTER T WITH STROKE..LATIN SMALL LETTER T WITH STROKE
unicode_east_asian_width(0x016B, 0x016B, 'A').	% L&       LATIN SMALL LETTER U WITH MACRON
unicode_east_asian_width(0x01CE, 0x01CE, 'A').	% L&       LATIN SMALL LETTER A WITH CARON
unicode_east_asian_width(0x01D0, 0x01D0, 'A').	% L&       LATIN SMALL LETTER I WITH CARON
unicode_east_asian_width(0x01D2, 0x01D2, 'A').	% L&       LATIN SMALL LETTER O WITH CARON
unicode_east_asian_width(0x01D4, 0x01D4, 'A').	% L&       LATIN SMALL LETTER U WITH CARON
unicode_east_asian_width(0x01D6, 0x01D6, 'A').	% L&       LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
unicode_east_asian_width(0x01D8, 0x01D8, 'A').	% L&       LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
unicode_east_asian_width(0x01DA, 0x01DA, 'A').	% L&       LATIN SMALL LETTER U WITH DIAERESIS AND CARON
unicode_east_asian_width(0x01DC, 0x01DC, 'A').	% L&       LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
unicode_east_asian_width(0x0251, 0x0251, 'A').	% L&       LATIN SMALL LETTER ALPHA
unicode_east_asian_width(0x0261, 0x0261, 'A').	% L&       LATIN SMALL LETTER SCRIPT G
unicode_east_asian_width(0x02C4, 0x02C4, 'A').	% Sk       MODIFIER LETTER UP ARROWHEAD
unicode_east_asian_width(0x02C7, 0x02C7, 'A').	% Lm       CARON
unicode_east_asian_width(0x02C9, 0x02CB, 'A').	% Lm   [3] MODIFIER LETTER MACRON..MODIFIER LETTER GRAVE ACCENT
unicode_east_asian_width(0x02CD, 0x02CD, 'A').	% Lm       MODIFIER LETTER LOW MACRON
unicode_east_asian_width(0x02D0, 0x02D0, 'A').	% Lm       MODIFIER LETTER TRIANGULAR COLON
unicode_east_asian_width(0x02D8, 0x02DB, 'A').	% Sk   [4] BREVE..OGONEK
unicode_east_asian_width(0x02DD, 0x02DD, 'A').	% Sk       DOUBLE ACUTE ACCENT
unicode_east_asian_width(0x02DF, 0x02DF, 'A').	% Sk       MODIFIER LETTER CROSS ACCENT
unicode_east_asian_width(0x0300, 0x036F, 'A').	% Mn [112] COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
unicode_east_asian_width(0x0391, 0x03A1, 'A').	% L&  [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
unicode_east_asian_width(0x03A3, 0x03A9, 'A').	% L&   [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
unicode_east_asian_width(0x03B1, 0x03C1, 'A').	% L&  [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
unicode_east_asian_width(0x03C3, 0x03C9, 'A').	% L&   [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
unicode_east_asian_width(0x0401, 0x0401, 'A').	% L&       CYRILLIC CAPITAL LETTER IO
unicode_east_asian_width(0x0410, 0x044F, 'A').	% L&  [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
unicode_east_asian_width(0x0451, 0x0451, 'A').	% L&       CYRILLIC SMALL LETTER IO
unicode_east_asian_width(0x2010, 0x2010, 'A').	% Pd       HYPHEN
unicode_east_asian_width(0x2013, 0x2015, 'A').	% Pd   [3] EN DASH..HORIZONTAL BAR
unicode_east_asian_width(0x2016, 0x2016, 'A').	% Po       DOUBLE VERTICAL LINE
unicode_east_asian_width(0x2018, 0x2018, 'A').	% Pi       LEFT SINGLE QUOTATION MARK
unicode_east_asian_width(0x2019, 0x2019, 'A').	% Pf       RIGHT SINGLE QUOTATION MARK
unicode_east_asian_width(0x201C, 0x201C, 'A').	% Pi       LEFT DOUBLE QUOTATION MARK
unicode_east_asian_width(0x201D, 0x201D, 'A').	% Pf       RIGHT DOUBLE QUOTATION MARK
unicode_east_asian_width(0x2020, 0x2022, 'A').	% Po   [3] DAGGER..BULLET
unicode_east_asian_width(0x2024, 0x2027, 'A').	% Po   [4] ONE DOT LEADER..HYPHENATION POINT
unicode_east_asian_width(0x2030, 0x2030, 'A').	% Po       PER MILLE SIGN
unicode_east_asian_width(0x2032, 0x2033, 'A').	% Po   [2] PRIME..DOUBLE PRIME
unicode_east_asian_width(0x2035, 0x2035, 'A').	% Po       REVERSED PRIME
unicode_east_asian_width(0x203B, 0x203B, 'A').	% Po       REFERENCE MARK
unicode_east_asian_width(0x203E, 0x203E, 'A').	% Po       OVERLINE
unicode_east_asian_width(0x2074, 0x2074, 'A').	% No       SUPERSCRIPT FOUR
unicode_east_asian_width(0x207F, 0x207F, 'A').	% Lm       SUPERSCRIPT LATIN SMALL LETTER N
unicode_east_asian_width(0x2081, 0x2084, 'A').	% No   [4] SUBSCRIPT ONE..SUBSCRIPT FOUR
unicode_east_asian_width(0x20AC, 0x20AC, 'A').	% Sc       EURO SIGN
unicode_east_asian_width(0x2103, 0x2103, 'A').	% So       DEGREE CELSIUS
unicode_east_asian_width(0x2105, 0x2105, 'A').	% So       CARE OF
unicode_east_asian_width(0x2109, 0x2109, 'A').	% So       DEGREE FAHRENHEIT
unicode_east_asian_width(0x2113, 0x2113, 'A').	% L&       SCRIPT SMALL L
unicode_east_asian_width(0x2116, 0x2116, 'A').	% So       NUMERO SIGN
unicode_east_asian_width(0x2121, 0x2122, 'A').	% So   [2] TELEPHONE SIGN..TRADE MARK SIGN
unicode_east_asian_width(0x2126, 0x2126, 'A').	% L&       OHM SIGN
unicode_east_asian_width(0x212B, 0x212B, 'A').	% L&       ANGSTROM SIGN
unicode_east_asian_width(0x2153, 0x2154, 'A').	% No   [2] VULGAR FRACTION ONE THIRD..VULGAR FRACTION TWO THIRDS
unicode_east_asian_width(0x215B, 0x215E, 'A').	% No   [4] VULGAR FRACTION ONE EIGHTH..VULGAR FRACTION SEVEN EIGHTHS
unicode_east_asian_width(0x2160, 0x216B, 'A').	% Nl  [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
unicode_east_asian_width(0x2170, 0x2179, 'A').	% Nl  [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
unicode_east_asian_width(0x2189, 0x2189, 'A').	% No       VULGAR FRACTION ZERO THIRDS
unicode_east_asian_width(0x2190, 0x2194, 'A').	% Sm   [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
unicode_east_asian_width(0x2195, 0x2199, 'A').	% So   [5] UP DOWN ARROW..SOUTH WEST ARROW
unicode_east_asian_width(0x21B8, 0x21B9, 'A').	% So   [2] NORTH WEST ARROW TO LONG BAR..LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR
unicode_east_asian_width(0x21D2, 0x21D2, 'A').	% Sm       RIGHTWARDS DOUBLE ARROW
unicode_east_asian_width(0x21D4, 0x21D4, 'A').	% Sm       LEFT RIGHT DOUBLE ARROW
unicode_east_asian_width(0x21E7, 0x21E7, 'A').	% So       UPWARDS WHITE ARROW
unicode_east_asian_width(0x2200, 0x2200, 'A').	% Sm       FOR ALL
unicode_east_asian_width(0x2202, 0x2203, 'A').	% Sm   [2] PARTIAL DIFFERENTIAL..THERE EXISTS
unicode_east_asian_width(0x2207, 0x2208, 'A').	% Sm   [2] NABLA..ELEMENT OF
unicode_east_asian_width(0x220B, 0x220B, 'A').	% Sm       CONTAINS AS MEMBER
unicode_east_asian_width(0x220F, 0x220F, 'A').	% Sm       N-ARY PRODUCT
unicode_east_asian_width(0x2211, 0x2211, 'A').	% Sm       N-ARY SUMMATION
unicode_east_asian_width(0x2215, 0x2215, 'A').	% Sm       DIVISION SLASH
unicode_east_asian_width(0x221A, 0x221A, 'A').	% Sm       SQUARE ROOT
unicode_east_asian_width(0x221D, 0x2220, 'A').	% Sm   [4] PROPORTIONAL TO..ANGLE
unicode_east_asian_width(0x2223, 0x2223, 'A').	% Sm       DIVIDES
unicode_east_asian_width(0x2225, 0x2225, 'A').	% Sm       PARALLEL TO
unicode_east_asian_width(0x2227, 0x222C, 'A').	% Sm   [6] LOGICAL AND..DOUBLE INTEGRAL
unicode_east_asian_width(0x222E, 0x222E, 'A').	% Sm       CONTOUR INTEGRAL
unicode_east_asian_width(0x2234, 0x2237, 'A').	% Sm   [4] THEREFORE..PROPORTION
unicode_east_asian_width(0x223C, 0x223D, 'A').	% Sm   [2] TILDE OPERATOR..REVERSED TILDE
unicode_east_asian_width(0x2248, 0x2248, 'A').	% Sm       ALMOST EQUAL TO
unicode_east_asian_width(0x224C, 0x224C, 'A').	% Sm       ALL EQUAL TO
unicode_east_asian_width(0x2252, 0x2252, 'A').	% Sm       APPROXIMATELY EQUAL TO OR THE IMAGE OF
unicode_east_asian_width(0x2260, 0x2261, 'A').	% Sm   [2] NOT EQUAL TO..IDENTICAL TO
unicode_east_asian_width(0x2264, 0x2267, 'A').	% Sm   [4] LESS-THAN OR EQUAL TO..GREATER-THAN OVER EQUAL TO
unicode_east_asian_width(0x226A, 0x226B, 'A').	% Sm   [2] MUCH LESS-THAN..MUCH GREATER-THAN
unicode_east_asian_width(0x226E, 0x226F, 'A').	% Sm   [2] NOT LESS-THAN..NOT GREATER-THAN
unicode_east_asian_width(0x2282, 0x2283, 'A').	% Sm   [2] SUBSET OF..SUPERSET OF
unicode_east_asian_width(0x2286, 0x2287, 'A').	% Sm   [2] SUBSET OF OR EQUAL TO..SUPERSET OF OR EQUAL TO
unicode_east_asian_width(0x2295, 0x2295, 'A').	% Sm       CIRCLED PLUS
unicode_east_asian_width(0x2299, 0x2299, 'A').	% Sm       CIRCLED DOT OPERATOR
unicode_east_asian_width(0x22A5, 0x22A5, 'A').	% Sm       UP TACK
unicode_east_asian_width(0x22BF, 0x22BF, 'A').	% Sm       RIGHT TRIANGLE
unicode_east_asian_width(0x2312, 0x2312, 'A').	% So       ARC
unicode_east_asian_width(0x2460, 0x249B, 'A').	% No  [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
unicode_east_asian_width(0x249C, 0x24E9, 'A').	% So  [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
unicode_east_asian_width(0x24EB, 0x24FF, 'A').	% No  [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
unicode_east_asian_width(0x2500, 0x254B, 'A').	% So  [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
unicode_east_asian_width(0x2550, 0x2573, 'A').	% So  [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
unicode_east_asian_width(0x2580, 0x258F, 'A').	% So  [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
unicode_east_asian_width(0x2592, 0x2595, 'A').	% So   [4] MEDIUM SHADE..RIGHT ONE EIGHTH BLOCK
unicode_east_asian_width(0x25A0, 0x25A1, 'A').	% So   [2] BLACK SQUARE..WHITE SQUARE
unicode_east_asian_width(0x25A3, 0x25A9, 'A').	% So   [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
unicode_east_asian_width(0x25B2, 0x25B3, 'A').	% So   [2] BLACK UP-POINTING TRIANGLE..WHITE UP-POINTING TRIANGLE
unicode_east_asian_width(0x25B6, 0x25B6, 'A').	% So       BLACK RIGHT-POINTING TRIANGLE
unicode_east_asian_width(0x25B7, 0x25B7, 'A').	% Sm       WHITE RIGHT-POINTING TRIANGLE
unicode_east_asian_width(0x25BC, 0x25BD, 'A').	% So   [2] BLACK DOWN-POINTING TRIANGLE..WHITE DOWN-POINTING TRIANGLE
unicode_east_asian_width(0x25C0, 0x25C0, 'A').	% So       BLACK LEFT-POINTING TRIANGLE
unicode_east_asian_width(0x25C1, 0x25C1, 'A').	% Sm       WHITE LEFT-POINTING TRIANGLE
unicode_east_asian_width(0x25C6, 0x25C8, 'A').	% So   [3] BLACK DIAMOND..WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
unicode_east_asian_width(0x25CB, 0x25CB, 'A').	% So       WHITE CIRCLE
unicode_east_asian_width(0x25CE, 0x25D1, 'A').	% So   [4] BULLSEYE..CIRCLE WITH RIGHT HALF BLACK
unicode_east_asian_width(0x25E2, 0x25E5, 'A').	% So   [4] BLACK LOWER RIGHT TRIANGLE..BLACK UPPER RIGHT TRIANGLE
unicode_east_asian_width(0x25EF, 0x25EF, 'A').	% So       LARGE CIRCLE
unicode_east_asian_width(0x2605, 0x2606, 'A').	% So   [2] BLACK STAR..WHITE STAR
unicode_east_asian_width(0x2609, 0x2609, 'A').	% So       SUN
unicode_east_asian_width(0x260E, 0x260F, 'A').	% So   [2] BLACK TELEPHONE..WHITE TELEPHONE
unicode_east_asian_width(0x2614, 0x2615, 'A').	% So   [2] UMBRELLA WITH RAIN DROPS..HOT BEVERAGE
unicode_east_asian_width(0x261C, 0x261C, 'A').	% So       WHITE LEFT POINTING INDEX
unicode_east_asian_width(0x261E, 0x261E, 'A').	% So       WHITE RIGHT POINTING INDEX
unicode_east_asian_width(0x2640, 0x2640, 'A').	% So       FEMALE SIGN
unicode_east_asian_width(0x2642, 0x2642, 'A').	% So       MALE SIGN
unicode_east_asian_width(0x2660, 0x2661, 'A').	% So   [2] BLACK SPADE SUIT..WHITE HEART SUIT
unicode_east_asian_width(0x2663, 0x2665, 'A').	% So   [3] BLACK CLUB SUIT..BLACK HEART SUIT
unicode_east_asian_width(0x2667, 0x266A, 'A').	% So   [4] WHITE CLUB SUIT..EIGHTH NOTE
unicode_east_asian_width(0x266C, 0x266D, 'A').	% So   [2] BEAMED SIXTEENTH NOTES..MUSIC FLAT SIGN
unicode_east_asian_width(0x266F, 0x266F, 'A').	% Sm       MUSIC SHARP SIGN
unicode_east_asian_width(0x269E, 0x269F, 'A').	% So   [2] THREE LINES CONVERGING RIGHT..THREE LINES CONVERGING LEFT
unicode_east_asian_width(0x26BE, 0x26BF, 'A').	% So   [2] BASEBALL..SQUARED KEY
unicode_east_asian_width(0x26C4, 0x26CD, 'A').	% So  [10] SNOWMAN WITHOUT SNOW..DISABLED CAR
unicode_east_asian_width(0x26CF, 0x26E1, 'A').	% So  [19] PICK..RESTRICTED LEFT ENTRY-2
unicode_east_asian_width(0x26E3, 0x26E3, 'A').	% So       HEAVY CIRCLE WITH STROKE AND TWO DOTS ABOVE
unicode_east_asian_width(0x26E8, 0x26FF, 'A').	% So  [24] BLACK CROSS ON SHIELD..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
unicode_east_asian_width(0x273D, 0x273D, 'A').	% So       HEAVY TEARDROP-SPOKED ASTERISK
unicode_east_asian_width(0x2757, 0x2757, 'A').	% So       HEAVY EXCLAMATION MARK SYMBOL
unicode_east_asian_width(0x2776, 0x277F, 'A').	% No  [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
unicode_east_asian_width(0x2B55, 0x2B59, 'A').	% So   [5] HEAVY LARGE CIRCLE..HEAVY CIRCLED SALTIRE
unicode_east_asian_width(0x3248, 0x324F, 'A').	% No   [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
unicode_east_asian_width(0xE000, 0xF8FF, 'A').	% Co [6400] <private-use-E000>..<private-use-F8FF>
unicode_east_asian_width(0xFE00, 0xFE0F, 'A').	% Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
unicode_east_asian_width(0xFFFD, 0xFFFD, 'A').	% So       REPLACEMENT CHARACTER
unicode_east_asian_width(0x1F100, 0x1F10A, 'A').	% No  [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
unicode_east_asian_width(0x1F110, 0x1F12D, 'A').	% So  [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
unicode_east_asian_width(0x1F130, 0x1F169, 'A').	% So  [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
unicode_east_asian_width(0x1F170, 0x1F19A, 'A').	% So  [43] NEGATIVE SQUARED LATIN CAPITAL LETTER A..SQUARED VS
unicode_east_asian_width(0xE0100, 0xE01EF, 'A').	% Mn [240] VARIATION SELECTOR-17..VARIATION SELECTOR-256
unicode_east_asian_width(0xF0000, 0xFFFFD, 'A').	% Co [65534] <private-use-F0000>..<private-use-FFFFD>
unicode_east_asian_width(0x100000, 0x10FFFD, 'A').	% Co [65534] <private-use-100000>..<private-use-10FFFD>

% Total code points: 138746

% ================================================

% East_Asian_Width=Halfwidth

unicode_east_asian_width(0x20A9, 0x20A9, 'H').	% Sc       WON SIGN
unicode_east_asian_width(0xFF61, 0xFF61, 'H').	% Po       HALFWIDTH IDEOGRAPHIC FULL STOP
unicode_east_asian_width(0xFF62, 0xFF62, 'H').	% Ps       HALFWIDTH LEFT CORNER BRACKET
unicode_east_asian_width(0xFF63, 0xFF63, 'H').	% Pe       HALFWIDTH RIGHT CORNER BRACKET
unicode_east_asian_width(0xFF64, 0xFF65, 'H').	% Po   [2] HALFWIDTH IDEOGRAPHIC COMMA..HALFWIDTH KATAKANA MIDDLE DOT
unicode_east_asian_width(0xFF66, 0xFF6F, 'H').	% Lo  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
unicode_east_asian_width(0xFF70, 0xFF70, 'H').	% Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
unicode_east_asian_width(0xFF71, 0xFF9D, 'H').	% Lo  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
unicode_east_asian_width(0xFF9E, 0xFF9F, 'H').	% Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
unicode_east_asian_width(0xFFA0, 0xFFBE, 'H').	% Lo  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
unicode_east_asian_width(0xFFC2, 0xFFC7, 'H').	% Lo   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
unicode_east_asian_width(0xFFCA, 0xFFCF, 'H').	% Lo   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
unicode_east_asian_width(0xFFD2, 0xFFD7, 'H').	% Lo   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
unicode_east_asian_width(0xFFDA, 0xFFDC, 'H').	% Lo   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
unicode_east_asian_width(0xFFE8, 0xFFE8, 'H').	% So       HALFWIDTH FORMS LIGHT VERTICAL
unicode_east_asian_width(0xFFE9, 0xFFEC, 'H').	% Sm   [4] HALFWIDTH LEFTWARDS ARROW..HALFWIDTH DOWNWARDS ARROW
unicode_east_asian_width(0xFFED, 0xFFEE, 'H').	% So   [2] HALFWIDTH BLACK SQUARE..HALFWIDTH WHITE CIRCLE

% Total code points: 123

% ================================================

% East_Asian_Width=Wide

unicode_east_asian_width(0x1100, 0x115F, 'W').	% Lo  [96] HANGUL CHOSEONG KIYEOK..HANGUL CHOSEONG FILLER
unicode_east_asian_width(0x11A3, 0x11A7, 'W').	% Lo   [5] HANGUL JUNGSEONG A-EU..HANGUL JUNGSEONG O-YAE
unicode_east_asian_width(0x11FA, 0x11FF, 'W').	% Lo   [6] HANGUL JONGSEONG KIYEOK-NIEUN..HANGUL JONGSEONG SSANGNIEUN
unicode_east_asian_width(0x2329, 0x2329, 'W').	% Ps       LEFT-POINTING ANGLE BRACKET
unicode_east_asian_width(0x232A, 0x232A, 'W').	% Pe       RIGHT-POINTING ANGLE BRACKET
unicode_east_asian_width(0x2E80, 0x2E99, 'W').	% So  [26] CJK RADICAL REPEAT..CJK RADICAL RAP
unicode_east_asian_width(0x2E9B, 0x2EF3, 'W').	% So  [89] CJK RADICAL CHOKE..CJK RADICAL C-SIMPLIFIED TURTLE
unicode_east_asian_width(0x2F00, 0x2FD5, 'W').	% So [214] KANGXI RADICAL ONE..KANGXI RADICAL FLUTE
unicode_east_asian_width(0x2FF0, 0x2FFB, 'W').	% So  [12] IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT..IDEOGRAPHIC DESCRIPTION CHARACTER OVERLAID
unicode_east_asian_width(0x3001, 0x3003, 'W').	% Po   [3] IDEOGRAPHIC COMMA..DITTO MARK
unicode_east_asian_width(0x3004, 0x3004, 'W').	% So       JAPANESE INDUSTRIAL STANDARD SYMBOL
unicode_east_asian_width(0x3005, 0x3005, 'W').	% Lm       IDEOGRAPHIC ITERATION MARK
unicode_east_asian_width(0x3006, 0x3006, 'W').	% Lo       IDEOGRAPHIC CLOSING MARK
unicode_east_asian_width(0x3007, 0x3007, 'W').	% Nl       IDEOGRAPHIC NUMBER ZERO
unicode_east_asian_width(0x3008, 0x3008, 'W').	% Ps       LEFT ANGLE BRACKET
unicode_east_asian_width(0x3009, 0x3009, 'W').	% Pe       RIGHT ANGLE BRACKET
unicode_east_asian_width(0x300A, 0x300A, 'W').	% Ps       LEFT DOUBLE ANGLE BRACKET
unicode_east_asian_width(0x300B, 0x300B, 'W').	% Pe       RIGHT DOUBLE ANGLE BRACKET
unicode_east_asian_width(0x300C, 0x300C, 'W').	% Ps       LEFT CORNER BRACKET
unicode_east_asian_width(0x300D, 0x300D, 'W').	% Pe       RIGHT CORNER BRACKET
unicode_east_asian_width(0x300E, 0x300E, 'W').	% Ps       LEFT WHITE CORNER BRACKET
unicode_east_asian_width(0x300F, 0x300F, 'W').	% Pe       RIGHT WHITE CORNER BRACKET
unicode_east_asian_width(0x3010, 0x3010, 'W').	% Ps       LEFT BLACK LENTICULAR BRACKET
unicode_east_asian_width(0x3011, 0x3011, 'W').	% Pe       RIGHT BLACK LENTICULAR BRACKET
unicode_east_asian_width(0x3012, 0x3013, 'W').	% So   [2] POSTAL MARK..GETA MARK
unicode_east_asian_width(0x3014, 0x3014, 'W').	% Ps       LEFT TORTOISE SHELL BRACKET
unicode_east_asian_width(0x3015, 0x3015, 'W').	% Pe       RIGHT TORTOISE SHELL BRACKET
unicode_east_asian_width(0x3016, 0x3016, 'W').	% Ps       LEFT WHITE LENTICULAR BRACKET
unicode_east_asian_width(0x3017, 0x3017, 'W').	% Pe       RIGHT WHITE LENTICULAR BRACKET
unicode_east_asian_width(0x3018, 0x3018, 'W').	% Ps       LEFT WHITE TORTOISE SHELL BRACKET
unicode_east_asian_width(0x3019, 0x3019, 'W').	% Pe       RIGHT WHITE TORTOISE SHELL BRACKET
unicode_east_asian_width(0x301A, 0x301A, 'W').	% Ps       LEFT WHITE SQUARE BRACKET
unicode_east_asian_width(0x301B, 0x301B, 'W').	% Pe       RIGHT WHITE SQUARE BRACKET
unicode_east_asian_width(0x301C, 0x301C, 'W').	% Pd       WAVE DASH
unicode_east_asian_width(0x301D, 0x301D, 'W').	% Ps       REVERSED DOUBLE PRIME QUOTATION MARK
unicode_east_asian_width(0x301E, 0x301F, 'W').	% Pe   [2] DOUBLE PRIME QUOTATION MARK..LOW DOUBLE PRIME QUOTATION MARK
unicode_east_asian_width(0x3020, 0x3020, 'W').	% So       POSTAL MARK FACE
unicode_east_asian_width(0x3021, 0x3029, 'W').	% Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
unicode_east_asian_width(0x302A, 0x302D, 'W').	% Mn   [4] IDEOGRAPHIC LEVEL TONE MARK..IDEOGRAPHIC ENTERING TONE MARK
unicode_east_asian_width(0x302E, 0x302F, 'W').	% Mc   [2] HANGUL SINGLE DOT TONE MARK..HANGUL DOUBLE DOT TONE MARK
unicode_east_asian_width(0x3030, 0x3030, 'W').	% Pd       WAVY DASH
unicode_east_asian_width(0x3031, 0x3035, 'W').	% Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
unicode_east_asian_width(0x3036, 0x3037, 'W').	% So   [2] CIRCLED POSTAL MARK..IDEOGRAPHIC TELEGRAPH LINE FEED SEPARATOR SYMBOL
unicode_east_asian_width(0x3038, 0x303A, 'W').	% Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
unicode_east_asian_width(0x303B, 0x303B, 'W').	% Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
unicode_east_asian_width(0x303C, 0x303C, 'W').	% Lo       MASU MARK
unicode_east_asian_width(0x303D, 0x303D, 'W').	% Po       PART ALTERNATION MARK
unicode_east_asian_width(0x303E, 0x303E, 'W').	% So       IDEOGRAPHIC VARIATION INDICATOR
unicode_east_asian_width(0x3041, 0x3096, 'W').	% Lo  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
unicode_east_asian_width(0x3099, 0x309A, 'W').	% Mn   [2] COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK..COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_east_asian_width(0x309B, 0x309C, 'W').	% Sk   [2] KATAKANA-HIRAGANA VOICED SOUND MARK..KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
unicode_east_asian_width(0x309D, 0x309E, 'W').	% Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
unicode_east_asian_width(0x309F, 0x309F, 'W').	% Lo       HIRAGANA DIGRAPH YORI
unicode_east_asian_width(0x30A0, 0x30A0, 'W').	% Pd       KATAKANA-HIRAGANA DOUBLE HYPHEN
unicode_east_asian_width(0x30A1, 0x30FA, 'W').	% Lo  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
unicode_east_asian_width(0x30FB, 0x30FB, 'W').	% Po       KATAKANA MIDDLE DOT
unicode_east_asian_width(0x30FC, 0x30FE, 'W').	% Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
unicode_east_asian_width(0x30FF, 0x30FF, 'W').	% Lo       KATAKANA DIGRAPH KOTO
unicode_east_asian_width(0x3105, 0x312D, 'W').	% Lo  [41] BOPOMOFO LETTER B..BOPOMOFO LETTER IH
unicode_east_asian_width(0x3131, 0x318E, 'W').	% Lo  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
unicode_east_asian_width(0x3190, 0x3191, 'W').	% So   [2] IDEOGRAPHIC ANNOTATION LINKING MARK..IDEOGRAPHIC ANNOTATION REVERSE MARK
unicode_east_asian_width(0x3192, 0x3195, 'W').	% No   [4] IDEOGRAPHIC ANNOTATION ONE MARK..IDEOGRAPHIC ANNOTATION FOUR MARK
unicode_east_asian_width(0x3196, 0x319F, 'W').	% So  [10] IDEOGRAPHIC ANNOTATION TOP MARK..IDEOGRAPHIC ANNOTATION MAN MARK
unicode_east_asian_width(0x31A0, 0x31BA, 'W').	% Lo  [27] BOPOMOFO LETTER BU..BOPOMOFO LETTER ZY
unicode_east_asian_width(0x31C0, 0x31E3, 'W').	% So  [36] CJK STROKE T..CJK STROKE Q
unicode_east_asian_width(0x31F0, 0x31FF, 'W').	% Lo  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
unicode_east_asian_width(0x3200, 0x321E, 'W').	% So  [31] PARENTHESIZED HANGUL KIYEOK..PARENTHESIZED KOREAN CHARACTER O HU
unicode_east_asian_width(0x3220, 0x3229, 'W').	% No  [10] PARENTHESIZED IDEOGRAPH ONE..PARENTHESIZED IDEOGRAPH TEN
unicode_east_asian_width(0x322A, 0x3247, 'W').	% So  [30] PARENTHESIZED IDEOGRAPH MOON..CIRCLED IDEOGRAPH KOTO
unicode_east_asian_width(0x3250, 0x3250, 'W').	% So       PARTNERSHIP SIGN
unicode_east_asian_width(0x3251, 0x325F, 'W').	% No  [15] CIRCLED NUMBER TWENTY ONE..CIRCLED NUMBER THIRTY FIVE
unicode_east_asian_width(0x3260, 0x327F, 'W').	% So  [32] CIRCLED HANGUL KIYEOK..KOREAN STANDARD SYMBOL
unicode_east_asian_width(0x3280, 0x3289, 'W').	% No  [10] CIRCLED IDEOGRAPH ONE..CIRCLED IDEOGRAPH TEN
unicode_east_asian_width(0x328A, 0x32B0, 'W').	% So  [39] CIRCLED IDEOGRAPH MOON..CIRCLED IDEOGRAPH NIGHT
unicode_east_asian_width(0x32B1, 0x32BF, 'W').	% No  [15] CIRCLED NUMBER THIRTY SIX..CIRCLED NUMBER FIFTY
unicode_east_asian_width(0x32C0, 0x32FE, 'W').	% So  [63] IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY..CIRCLED KATAKANA WO
unicode_east_asian_width(0x3300, 0x33FF, 'W').	% So [256] SQUARE APAATO..SQUARE GAL
unicode_east_asian_width(0x3400, 0x4DB5, 'W').	% Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
unicode_east_asian_width(0x4DB6, 0x4DBF, 'W').	% Cn  [10] <reserved-4DB6>..<reserved-4DBF>
unicode_east_asian_width(0x4E00, 0x9FCC, 'W').	% Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
unicode_east_asian_width(0x9FCD, 0x9FFF, 'W').	% Cn  [51] <reserved-9FCD>..<reserved-9FFF>
unicode_east_asian_width(0xA000, 0xA014, 'W').	% Lo  [21] YI SYLLABLE IT..YI SYLLABLE E
unicode_east_asian_width(0xA015, 0xA015, 'W').	% Lm       YI SYLLABLE WU
unicode_east_asian_width(0xA016, 0xA48C, 'W').	% Lo [1143] YI SYLLABLE BIT..YI SYLLABLE YYR
unicode_east_asian_width(0xA490, 0xA4C6, 'W').	% So  [55] YI RADICAL QOT..YI RADICAL KE
unicode_east_asian_width(0xA960, 0xA97C, 'W').	% Lo  [29] HANGUL CHOSEONG TIKEUT-MIEUM..HANGUL CHOSEONG SSANGYEORINHIEUH
unicode_east_asian_width(0xAC00, 0xD7A3, 'W').	% Lo [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
unicode_east_asian_width(0xD7B0, 0xD7C6, 'W').	% Lo  [23] HANGUL JUNGSEONG O-YEO..HANGUL JUNGSEONG ARAEA-E
unicode_east_asian_width(0xD7CB, 0xD7FB, 'W').	% Lo  [49] HANGUL JONGSEONG NIEUN-RIEUL..HANGUL JONGSEONG PHIEUPH-THIEUTH
unicode_east_asian_width(0xF900, 0xFA6D, 'W').	% Lo [366] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA6D
unicode_east_asian_width(0xFA6E, 0xFA6F, 'W').	% Cn   [2] <reserved-FA6E>..<reserved-FA6F>
unicode_east_asian_width(0xFA70, 0xFAD9, 'W').	% Lo [106] CJK COMPATIBILITY IDEOGRAPH-FA70..CJK COMPATIBILITY IDEOGRAPH-FAD9
unicode_east_asian_width(0xFADA, 0xFAFF, 'W').	% Cn  [38] <reserved-FADA>..<reserved-FAFF>
unicode_east_asian_width(0xFE10, 0xFE16, 'W').	% Po   [7] PRESENTATION FORM FOR VERTICAL COMMA..PRESENTATION FORM FOR VERTICAL QUESTION MARK
unicode_east_asian_width(0xFE17, 0xFE17, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE LENTICULAR BRACKET
unicode_east_asian_width(0xFE18, 0xFE18, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET
unicode_east_asian_width(0xFE19, 0xFE19, 'W').	% Po       PRESENTATION FORM FOR VERTICAL HORIZONTAL ELLIPSIS
unicode_east_asian_width(0xFE30, 0xFE30, 'W').	% Po       PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
unicode_east_asian_width(0xFE31, 0xFE32, 'W').	% Pd   [2] PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
unicode_east_asian_width(0xFE33, 0xFE34, 'W').	% Pc   [2] PRESENTATION FORM FOR VERTICAL LOW LINE..PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
unicode_east_asian_width(0xFE35, 0xFE35, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
unicode_east_asian_width(0xFE36, 0xFE36, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
unicode_east_asian_width(0xFE37, 0xFE37, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
unicode_east_asian_width(0xFE38, 0xFE38, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
unicode_east_asian_width(0xFE39, 0xFE39, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
unicode_east_asian_width(0xFE3A, 0xFE3A, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
unicode_east_asian_width(0xFE3B, 0xFE3B, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
unicode_east_asian_width(0xFE3C, 0xFE3C, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
unicode_east_asian_width(0xFE3D, 0xFE3D, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
unicode_east_asian_width(0xFE3E, 0xFE3E, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
unicode_east_asian_width(0xFE3F, 0xFE3F, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
unicode_east_asian_width(0xFE40, 0xFE40, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
unicode_east_asian_width(0xFE41, 0xFE41, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
unicode_east_asian_width(0xFE42, 0xFE42, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
unicode_east_asian_width(0xFE43, 0xFE43, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
unicode_east_asian_width(0xFE44, 0xFE44, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
unicode_east_asian_width(0xFE45, 0xFE46, 'W').	% Po   [2] SESAME DOT..WHITE SESAME DOT
unicode_east_asian_width(0xFE47, 0xFE47, 'W').	% Ps       PRESENTATION FORM FOR VERTICAL LEFT SQUARE BRACKET
unicode_east_asian_width(0xFE48, 0xFE48, 'W').	% Pe       PRESENTATION FORM FOR VERTICAL RIGHT SQUARE BRACKET
unicode_east_asian_width(0xFE49, 0xFE4C, 'W').	% Po   [4] DASHED OVERLINE..DOUBLE WAVY OVERLINE
unicode_east_asian_width(0xFE4D, 0xFE4F, 'W').	% Pc   [3] DASHED LOW LINE..WAVY LOW LINE
unicode_east_asian_width(0xFE50, 0xFE52, 'W').	% Po   [3] SMALL COMMA..SMALL FULL STOP
unicode_east_asian_width(0xFE54, 0xFE57, 'W').	% Po   [4] SMALL SEMICOLON..SMALL EXCLAMATION MARK
unicode_east_asian_width(0xFE58, 0xFE58, 'W').	% Pd       SMALL EM DASH
unicode_east_asian_width(0xFE59, 0xFE59, 'W').	% Ps       SMALL LEFT PARENTHESIS
unicode_east_asian_width(0xFE5A, 0xFE5A, 'W').	% Pe       SMALL RIGHT PARENTHESIS
unicode_east_asian_width(0xFE5B, 0xFE5B, 'W').	% Ps       SMALL LEFT CURLY BRACKET
unicode_east_asian_width(0xFE5C, 0xFE5C, 'W').	% Pe       SMALL RIGHT CURLY BRACKET
unicode_east_asian_width(0xFE5D, 0xFE5D, 'W').	% Ps       SMALL LEFT TORTOISE SHELL BRACKET
unicode_east_asian_width(0xFE5E, 0xFE5E, 'W').	% Pe       SMALL RIGHT TORTOISE SHELL BRACKET
unicode_east_asian_width(0xFE5F, 0xFE61, 'W').	% Po   [3] SMALL NUMBER SIGN..SMALL ASTERISK
unicode_east_asian_width(0xFE62, 0xFE62, 'W').	% Sm       SMALL PLUS SIGN
unicode_east_asian_width(0xFE63, 0xFE63, 'W').	% Pd       SMALL HYPHEN-MINUS
unicode_east_asian_width(0xFE64, 0xFE66, 'W').	% Sm   [3] SMALL LESS-THAN SIGN..SMALL EQUALS SIGN
unicode_east_asian_width(0xFE68, 0xFE68, 'W').	% Po       SMALL REVERSE SOLIDUS
unicode_east_asian_width(0xFE69, 0xFE69, 'W').	% Sc       SMALL DOLLAR SIGN
unicode_east_asian_width(0xFE6A, 0xFE6B, 'W').	% Po   [2] SMALL PERCENT SIGN..SMALL COMMERCIAL AT
unicode_east_asian_width(0x1B000, 0x1B001, 'W').	% Lo   [2] KATAKANA LETTER ARCHAIC E..HIRAGANA LETTER ARCHAIC YE
unicode_east_asian_width(0x1F200, 0x1F202, 'W').	% So   [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
unicode_east_asian_width(0x1F210, 0x1F23A, 'W').	% So  [43] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-55B6
unicode_east_asian_width(0x1F240, 0x1F248, 'W').	% So   [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
unicode_east_asian_width(0x1F250, 0x1F251, 'W').	% So   [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
unicode_east_asian_width(0x20000, 0x2A6D6, 'W').	% Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
unicode_east_asian_width(0x2A6D7, 0x2A6FF, 'W').	% Cn  [41] <reserved-2A6D7>..<reserved-2A6FF>
unicode_east_asian_width(0x2A700, 0x2B734, 'W').	% Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
unicode_east_asian_width(0x2B735, 0x2B73F, 'W').	% Cn  [11] <reserved-2B735>..<reserved-2B73F>
unicode_east_asian_width(0x2B740, 0x2B81D, 'W').	% Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
unicode_east_asian_width(0x2B81E, 0x2F7FF, 'W').	% Cn [16354] <reserved-2B81E>..<reserved-2F7FF>
unicode_east_asian_width(0x2F800, 0x2FA1D, 'W').	% Lo [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
unicode_east_asian_width(0x2FA1E, 0x2FFFD, 'W').	% Cn [1504] <reserved-2FA1E>..<reserved-2FFFD>
unicode_east_asian_width(0x30000, 0x3FFFD, 'W').	% Cn [65534] <reserved-30000>..<reserved-3FFFD>

% Total code points: 173217

% ================================================

% East_Asian_Width=Fullwidth

unicode_east_asian_width(0x3000, 0x3000, 'F').	% Zs       IDEOGRAPHIC SPACE
unicode_east_asian_width(0xFF01, 0xFF03, 'F').	% Po   [3] FULLWIDTH EXCLAMATION MARK..FULLWIDTH NUMBER SIGN
unicode_east_asian_width(0xFF04, 0xFF04, 'F').	% Sc       FULLWIDTH DOLLAR SIGN
unicode_east_asian_width(0xFF05, 0xFF07, 'F').	% Po   [3] FULLWIDTH PERCENT SIGN..FULLWIDTH APOSTROPHE
unicode_east_asian_width(0xFF08, 0xFF08, 'F').	% Ps       FULLWIDTH LEFT PARENTHESIS
unicode_east_asian_width(0xFF09, 0xFF09, 'F').	% Pe       FULLWIDTH RIGHT PARENTHESIS
unicode_east_asian_width(0xFF0A, 0xFF0A, 'F').	% Po       FULLWIDTH ASTERISK
unicode_east_asian_width(0xFF0B, 0xFF0B, 'F').	% Sm       FULLWIDTH PLUS SIGN
unicode_east_asian_width(0xFF0C, 0xFF0C, 'F').	% Po       FULLWIDTH COMMA
unicode_east_asian_width(0xFF0D, 0xFF0D, 'F').	% Pd       FULLWIDTH HYPHEN-MINUS
unicode_east_asian_width(0xFF0E, 0xFF0F, 'F').	% Po   [2] FULLWIDTH FULL STOP..FULLWIDTH SOLIDUS
unicode_east_asian_width(0xFF10, 0xFF19, 'F').	% Nd  [10] FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
unicode_east_asian_width(0xFF1A, 0xFF1B, 'F').	% Po   [2] FULLWIDTH COLON..FULLWIDTH SEMICOLON
unicode_east_asian_width(0xFF1C, 0xFF1E, 'F').	% Sm   [3] FULLWIDTH LESS-THAN SIGN..FULLWIDTH GREATER-THAN SIGN
unicode_east_asian_width(0xFF1F, 0xFF20, 'F').	% Po   [2] FULLWIDTH QUESTION MARK..FULLWIDTH COMMERCIAL AT
unicode_east_asian_width(0xFF21, 0xFF3A, 'F').	% L&  [26] FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER Z
unicode_east_asian_width(0xFF3B, 0xFF3B, 'F').	% Ps       FULLWIDTH LEFT SQUARE BRACKET
unicode_east_asian_width(0xFF3C, 0xFF3C, 'F').	% Po       FULLWIDTH REVERSE SOLIDUS
unicode_east_asian_width(0xFF3D, 0xFF3D, 'F').	% Pe       FULLWIDTH RIGHT SQUARE BRACKET
unicode_east_asian_width(0xFF3E, 0xFF3E, 'F').	% Sk       FULLWIDTH CIRCUMFLEX ACCENT
unicode_east_asian_width(0xFF3F, 0xFF3F, 'F').	% Pc       FULLWIDTH LOW LINE
unicode_east_asian_width(0xFF40, 0xFF40, 'F').	% Sk       FULLWIDTH GRAVE ACCENT
unicode_east_asian_width(0xFF41, 0xFF5A, 'F').	% L&  [26] FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER Z
unicode_east_asian_width(0xFF5B, 0xFF5B, 'F').	% Ps       FULLWIDTH LEFT CURLY BRACKET
unicode_east_asian_width(0xFF5C, 0xFF5C, 'F').	% Sm       FULLWIDTH VERTICAL LINE
unicode_east_asian_width(0xFF5D, 0xFF5D, 'F').	% Pe       FULLWIDTH RIGHT CURLY BRACKET
unicode_east_asian_width(0xFF5E, 0xFF5E, 'F').	% Sm       FULLWIDTH TILDE
unicode_east_asian_width(0xFF5F, 0xFF5F, 'F').	% Ps       FULLWIDTH LEFT WHITE PARENTHESIS
unicode_east_asian_width(0xFF60, 0xFF60, 'F').	% Pe       FULLWIDTH RIGHT WHITE PARENTHESIS
unicode_east_asian_width(0xFFE0, 0xFFE1, 'F').	% Sc   [2] FULLWIDTH CENT SIGN..FULLWIDTH POUND SIGN
unicode_east_asian_width(0xFFE2, 0xFFE2, 'F').	% Sm       FULLWIDTH NOT SIGN
unicode_east_asian_width(0xFFE3, 0xFFE3, 'F').	% Sk       FULLWIDTH MACRON
unicode_east_asian_width(0xFFE4, 0xFFE4, 'F').	% So       FULLWIDTH BROKEN BAR
unicode_east_asian_width(0xFFE5, 0xFFE6, 'F').	% Sc   [2] FULLWIDTH YEN SIGN..FULLWIDTH WON SIGN

% Total code points: 104

% ================================================

% East_Asian_Width=Narrow

unicode_east_asian_width(0x0020, 0x0020, 'Na').	% Zs       SPACE
unicode_east_asian_width(0x0021, 0x0023, 'Na').	% Po   [3] EXCLAMATION MARK..NUMBER SIGN
unicode_east_asian_width(0x0024, 0x0024, 'Na').	% Sc       DOLLAR SIGN
unicode_east_asian_width(0x0025, 0x0027, 'Na').	% Po   [3] PERCENT SIGN..APOSTROPHE
unicode_east_asian_width(0x0028, 0x0028, 'Na').	% Ps       LEFT PARENTHESIS
unicode_east_asian_width(0x0029, 0x0029, 'Na').	% Pe       RIGHT PARENTHESIS
unicode_east_asian_width(0x002A, 0x002A, 'Na').	% Po       ASTERISK
unicode_east_asian_width(0x002B, 0x002B, 'Na').	% Sm       PLUS SIGN
unicode_east_asian_width(0x002C, 0x002C, 'Na').	% Po       COMMA
unicode_east_asian_width(0x002D, 0x002D, 'Na').	% Pd       HYPHEN-MINUS
unicode_east_asian_width(0x002E, 0x002F, 'Na').	% Po   [2] FULL STOP..SOLIDUS
unicode_east_asian_width(0x0030, 0x0039, 'Na').	% Nd  [10] DIGIT ZERO..DIGIT NINE
unicode_east_asian_width(0x003A, 0x003B, 'Na').	% Po   [2] COLON..SEMICOLON
unicode_east_asian_width(0x003C, 0x003E, 'Na').	% Sm   [3] LESS-THAN SIGN..GREATER-THAN SIGN
unicode_east_asian_width(0x003F, 0x0040, 'Na').	% Po   [2] QUESTION MARK..COMMERCIAL AT
unicode_east_asian_width(0x0041, 0x005A, 'Na').	% L&  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z
unicode_east_asian_width(0x005B, 0x005B, 'Na').	% Ps       LEFT SQUARE BRACKET
unicode_east_asian_width(0x005C, 0x005C, 'Na').	% Po       REVERSE SOLIDUS
unicode_east_asian_width(0x005D, 0x005D, 'Na').	% Pe       RIGHT SQUARE BRACKET
unicode_east_asian_width(0x005E, 0x005E, 'Na').	% Sk       CIRCUMFLEX ACCENT
unicode_east_asian_width(0x005F, 0x005F, 'Na').	% Pc       LOW LINE
unicode_east_asian_width(0x0060, 0x0060, 'Na').	% Sk       GRAVE ACCENT
unicode_east_asian_width(0x0061, 0x007A, 'Na').	% L&  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z
unicode_east_asian_width(0x007B, 0x007B, 'Na').	% Ps       LEFT CURLY BRACKET
unicode_east_asian_width(0x007C, 0x007C, 'Na').	% Sm       VERTICAL LINE
unicode_east_asian_width(0x007D, 0x007D, 'Na').	% Pe       RIGHT CURLY BRACKET
unicode_east_asian_width(0x007E, 0x007E, 'Na').	% Sm       TILDE
unicode_east_asian_width(0x00A2, 0x00A3, 'Na').	% Sc   [2] CENT SIGN..POUND SIGN
unicode_east_asian_width(0x00A5, 0x00A5, 'Na').	% Sc       YEN SIGN
unicode_east_asian_width(0x00A6, 0x00A6, 'Na').	% So       BROKEN BAR
unicode_east_asian_width(0x00AC, 0x00AC, 'Na').	% Sm       NOT SIGN
unicode_east_asian_width(0x00AF, 0x00AF, 'Na').	% Sk       MACRON
unicode_east_asian_width(0x27E6, 0x27E6, 'Na').	% Ps       MATHEMATICAL LEFT WHITE SQUARE BRACKET
unicode_east_asian_width(0x27E7, 0x27E7, 'Na').	% Pe       MATHEMATICAL RIGHT WHITE SQUARE BRACKET
unicode_east_asian_width(0x27E8, 0x27E8, 'Na').	% Ps       MATHEMATICAL LEFT ANGLE BRACKET
unicode_east_asian_width(0x27E9, 0x27E9, 'Na').	% Pe       MATHEMATICAL RIGHT ANGLE BRACKET
unicode_east_asian_width(0x27EA, 0x27EA, 'Na').	% Ps       MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
unicode_east_asian_width(0x27EB, 0x27EB, 'Na').	% Pe       MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
unicode_east_asian_width(0x27EC, 0x27EC, 'Na').	% Ps       MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
unicode_east_asian_width(0x27ED, 0x27ED, 'Na').	% Pe       MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
unicode_east_asian_width(0x2985, 0x2985, 'Na').	% Ps       LEFT WHITE PARENTHESIS
unicode_east_asian_width(0x2986, 0x2986, 'Na').	% Pe       RIGHT WHITE PARENTHESIS

% Total code points: 111

% EOF
