%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 28, 2012
%
%  Original Unicode file header comments follow

/*
# IndicSyllabicCategory-6.1.0.txt
# Date: 2011-08-31, 23:54:00 GMT [KW]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see UAX #44.
#
# This file defines the following provisional property:
#
#    Indic_Syllabic_Category     enumerated property
#
# NB: Provisional properties and data files have no associated stability 
# guarantees. They are provided in part to determine the possible usefulness
# of a property or other data and to encourage analysis and further investigation
# which may result in their improvement. Provisional properties and
# data files may change arbitrarily, or may even be removed in a future version of the
# Unicode Character Database, if they prove not to be useful.
#
# Scope: This provisional property is aimed at two general problem
# areas involving the analysis and processing of Indic scripts:
#
#   1. Specification of syllabic structure.
#   2. Specification of segmentation rules.
#
# Both of these problem areas may benefit from having defined subtypes
# of Indic script characters which are relevant to how Indic
# syllables (or aksaras) are constructed. Note that rules for
# syllabic structure in Indic scripts may differ significantly
# from how phonological syllables are defined. 
#
# Format:
#    Field 0  Code Point or Code Point Range
#    Field 1  Indic_Syllabic_Category
#
# A comment field shows General_Category property values and character names.
#
# The scripts assessed as Indic in the
# structural sense used for the Indic_Syllabic_Category are:
#
# Devanagari, Bengali, Gurmukhi, Gujarati, Oriya, Tamil, Telugu,
# Kannada, Malayalam, Sinhala, Thai, Lao, Tibetan, Myanmar,
# Tagalog, Hanunoo, Buhid, Tagbanwa, Khmer, Limbu, Tai Le, New Tai Lue,
# Buginese, Tai Tham, Balinese, Sundanese, Batak, Lepcha,
# Syloti Nagri, Phags-Pa, Saurashtra, Kayah Li, Rejang, Javanese, Cham, Tai Viet,
# Meetei Mayek, Kharoshthi, Brahmi, Kaithi, Chakma, Sharada, Takri
#
# All characters for all other scripts not in that list
# take the default value for this property, unless they
# are individually listed in this data file.
#

# ================================================

# Property: Indic_Syllabic_Category
#
#  All code points not explicitly listed for Indic_Syllabic_Category
#  have the value Other.
#
# @missing: 0000..10FFFF; Other
*/

unicode_indic_syllabic_category(CodePoint, Category) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_indic_syllabic_category(CodePointStart, CodePointEnd, Category),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_indic_syllabic_category(CodePoint, _, CodePointCategory) ->
		Category = CodePointCategory
	;	% look for a code point range that includes the given code point
		unicode_indic_syllabic_category(CodePointStart, CodePointEnd, CodePointCategory),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Category = CodePointCategory
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Category = 'Other'
	).

% ================================================

% Indic_Syllabic_Category=Bindu

% Bindu/Anusvara (nasalization or -n)
% Excludes various Vedic nasalization signs.

% [Not derivable]

unicode_indic_syllabic_category(0x0900, 0x0902, 'Bindu'). % Mn   [3] DEVANAGARI SIGN INVERTED CANDRABINDU..DEVANAGARI SIGN ANUSVARA
unicode_indic_syllabic_category(0x0981, 0x0981, 'Bindu'). % Mn       BENGALI SIGN CANDRABINDU
unicode_indic_syllabic_category(0x0982, 0x0982, 'Bindu'). % Mc       BENGALI SIGN ANUSVARA
unicode_indic_syllabic_category(0x0A01, 0x0A02, 'Bindu'). % Mn   [2] GURMUKHI SIGN ADAK BINDI..GURMUKHI SIGN BINDI
unicode_indic_syllabic_category(0x0A70, 0x0A70, 'Bindu'). % Mn       GURMUKHI TIPPI
unicode_indic_syllabic_category(0x0A81, 0x0A82, 'Bindu'). % Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
unicode_indic_syllabic_category(0x0B01, 0x0B01, 'Bindu'). % Mn       ORIYA SIGN CANDRABINDU
unicode_indic_syllabic_category(0x0B02, 0x0B02, 'Bindu'). % Mc       ORIYA SIGN ANUSVARA
unicode_indic_syllabic_category(0x0B82, 0x0B82, 'Bindu'). % Mn       TAMIL SIGN ANUSVARA
unicode_indic_syllabic_category(0x0C01, 0x0C02, 'Bindu'). % Mc   [2] TELUGU SIGN CANDRABINDU..TELUGU SIGN ANUSVARA
unicode_indic_syllabic_category(0x0C82, 0x0C82, 'Bindu'). % Mc       KANNADA SIGN ANUSVARA
unicode_indic_syllabic_category(0x0D02, 0x0D02, 'Bindu'). % Mc       MALAYALAM SIGN ANUSVARA
unicode_indic_syllabic_category(0x0D82, 0x0D82, 'Bindu'). % Mc       SINHALA SIGN ANUSVARAYA
unicode_indic_syllabic_category(0x0E4D, 0x0E4D, 'Bindu'). % Mn       THAI CHARACTER NIKHAHIT
unicode_indic_syllabic_category(0x0ECD, 0x0ECD, 'Bindu'). % Mn       LAO NIGGAHITA
unicode_indic_syllabic_category(0x0F7E, 0x0F7E, 'Bindu'). % Mn       TIBETAN SIGN RJES SU NGA RO
unicode_indic_syllabic_category(0x0F82, 0x0F83, 'Bindu'). % Mn   [2] TIBETAN SIGN NYI ZLA NAA DA..TIBETAN SIGN SNA LDAN
unicode_indic_syllabic_category(0x1036, 0x1036, 'Bindu'). % Mn       MYANMAR SIGN ANUSVARA
unicode_indic_syllabic_category(0x17C6, 0x17C6, 'Bindu'). % Mn       KHMER SIGN NIKAHIT
unicode_indic_syllabic_category(0x1932, 0x1932, 'Bindu'). % Mn       LIMBU SMALL LETTER ANUSVARA
unicode_indic_syllabic_category(0x1B00, 0x1B02, 'Bindu'). % Mn   [3] BALINESE SIGN ULU RICEM..BALINESE SIGN CECEK
unicode_indic_syllabic_category(0xA80B, 0xA80B, 'Bindu'). % Mn       SYLOTI NAGRI SIGN ANUSVARA
unicode_indic_syllabic_category(0xA873, 0xA873, 'Bindu'). % Lo       PHAGS-PA LETTER CANDRABINDU
unicode_indic_syllabic_category(0xA980, 0xA981, 'Bindu'). % Mn   [2] JAVANESE SIGN PANYANGGA..JAVANESE SIGN CECAK
unicode_indic_syllabic_category(0x1B80, 0x1B80, 'Bindu'). % Mn       SUNDANESE SIGN PANYECEK
unicode_indic_syllabic_category(0x1C34, 0x1C35, 'Bindu'). % Mc   [2] LEPCHA CONSONANT SIGN NYIN-DO..LEPCHA CONSONANT SIGN KANG
unicode_indic_syllabic_category(0xA880, 0xA880, 'Bindu'). % Mc       SAURASHTRA SIGN ANUSVARA
unicode_indic_syllabic_category(0x10A0E, 0x10A0E, 'Bindu'). % Mn       KHAROSHTHI SIGN ANUSVARA
unicode_indic_syllabic_category(0x11000, 0x11000, 'Bindu'). % Mc       BRAHMI SIGN CANDRABINDU
unicode_indic_syllabic_category(0x11001, 0x11001, 'Bindu'). % Mn       BRAHMI SIGN ANUSVARA
unicode_indic_syllabic_category(0x11080, 0x11081, 'Bindu'). % Mn   [2] KAITHI SIGN CANDRABINDU..KAITHI SIGN ANUSVARA      
unicode_indic_syllabic_category(0x11100, 0x11101, 'Bindu'). % Mn       CHAKMA SIGN CANDRABINDU..CHAKMA SIGN ANUSVARA
unicode_indic_syllabic_category(0x11180, 0x11181, 'Bindu'). % Mn       SHARADA SIGN CANDRABINDU..SHARADA SIGN ANUSVARA
unicode_indic_syllabic_category(0x116AB, 0x116AB, 'Bindu'). % Mn       TAKRI SIGN ANUSVARA

% ================================================

% Indic_Syllabic_Category=Visarga

% Visarga (-h)
% Includes specialized case for Sanskrit: ardhavisarga
% Excludes letters for jihvamuliya and upadhmaniya, which are
%   related, but structured somewhat differently. 

% [Not derivable]

unicode_indic_syllabic_category(0x0903, 0x0903 , 'Visarga'). % Mc       DEVANAGARI SIGN VISARGA
unicode_indic_syllabic_category(0x0983, 0x0983 , 'Visarga'). % Mc       BENGALI SIGN VISARGA
unicode_indic_syllabic_category(0x0A03, 0x0A03 , 'Visarga'). % Mc       GURMUKHI SIGN VISARGA
unicode_indic_syllabic_category(0x0A83, 0x0A83 , 'Visarga'). % Mc       GUJARATI SIGN VISARGA
unicode_indic_syllabic_category(0x0B03, 0x0B03 , 'Visarga'). % Mc       ORIYA SIGN VISARGA
unicode_indic_syllabic_category(0x0C03, 0x0C03 , 'Visarga'). % Mc       TELUGU SIGN VISARGA
unicode_indic_syllabic_category(0x0C83, 0x0C83 , 'Visarga'). % Mc       KANNADA SIGN VISARGA
unicode_indic_syllabic_category(0x0D03, 0x0D03 , 'Visarga'). % Mc       MALAYALAM SIGN VISARGA
unicode_indic_syllabic_category(0x0D83, 0x0D83 , 'Visarga'). % Mc       SINHALA SIGN VISARGAYA
unicode_indic_syllabic_category(0x0F7F, 0x0F7F , 'Visarga'). % Mc       TIBETAN SIGN RNAM BCAD
unicode_indic_syllabic_category(0x1038, 0x1038 , 'Visarga'). % Mc       MYANMAR SIGN VISARGA
unicode_indic_syllabic_category(0x17C7, 0x17C7 , 'Visarga'). % Mc       KHMER SIGN REAHMUK
unicode_indic_syllabic_category(0x1B04, 0x1B04 , 'Visarga'). % Mc       BALINESE SIGN BISAH
unicode_indic_syllabic_category(0x1B82, 0x1B82 , 'Visarga'). % Mc       SUNDANESE SIGN PANGWISAD
unicode_indic_syllabic_category(0x1CF2, 0x1CF2 , 'Visarga'). % Mc       VEDIC SIGN ARDHAVISARGA
unicode_indic_syllabic_category(0x1CF3, 0x1CF3 , 'Visarga'). % Mc       VEDIC SIGN ROTATED ARDHAVISARGA
unicode_indic_syllabic_category(0xA881, 0xA881 , 'Visarga'). % Mc       SAURASHTRA SIGN VISARGA
unicode_indic_syllabic_category(0xA983, 0xA983 , 'Visarga'). % Mc       JAVANESE SIGN WIGNYAN
unicode_indic_syllabic_category(0xAAF5, 0xAAF5 , 'Visarga'). % Mc       MEETEI MAYEK VOWEL SIGN VISARGA
unicode_indic_syllabic_category(0x10A0F, 0x10A0F, 'Visarga'). % Mn       KHAROSHTHI SIGN VISARGA
unicode_indic_syllabic_category(0x11002, 0x11002, 'Visarga'). % Mc       BRAHMI SIGN VISARGA
unicode_indic_syllabic_category(0x11082, 0x11082, 'Visarga'). % Mc       KAITHI SIGN VISARGA
unicode_indic_syllabic_category(0x11102, 0x11102, 'Visarga'). % Mn       CHAKMA SIGN VISARGA
unicode_indic_syllabic_category(0x11182, 0x11182, 'Visarga'). % Mn       SHARADA SIGN VISARGA
unicode_indic_syllabic_category(0x116AC, 0x116AC, 'Visarga'). % Mc       TAKRI SIGN VISARGA

% ================================================

% Indic_Syllabic_Category=Avagraha

% Avagraha (elision of initial a- in sandhi)

% [Not derivable]

unicode_indic_syllabic_category(0x093D, 0x093D  , 'Avagraha'). % Lo       DEVANAGARI SIGN AVAGRAHA
unicode_indic_syllabic_category(0x09BD, 0x09BD  , 'Avagraha'). % Lo       BENGALI SIGN AVAGRAHA
unicode_indic_syllabic_category(0x0ABD, 0x0ABD  , 'Avagraha'). % Lo       GUJARATI SIGN AVAGRAHA
unicode_indic_syllabic_category(0x0B3D, 0x0B3D  , 'Avagraha'). % Lo       ORIYA SIGN AVAGRAHA
unicode_indic_syllabic_category(0x0C3D, 0x0C3D  , 'Avagraha'). % Lo       TELUGU SIGN AVAGRAHA
unicode_indic_syllabic_category(0x0CBD, 0x0CBD  , 'Avagraha'). % Lo       KANNADA SIGN AVAGRAHA
unicode_indic_syllabic_category(0x0D3D, 0x0D3D  , 'Avagraha'). % Lo       MALAYALAM SIGN AVAGRAHA
unicode_indic_syllabic_category(0x0F85, 0x0F85  , 'Avagraha'). % Po       TIBETAN MARK PALUTA
unicode_indic_syllabic_category(0x17DC, 0x17DC  , 'Avagraha'). % Lo       KHMER SIGN AVAKRAHASANYA
unicode_indic_syllabic_category(0x1BBA, 0x1BBA  , 'Avagraha'). % Lo       SUNDANESE AVAGRAHA
unicode_indic_syllabic_category(0x111C1, 0x111C1, 'Avagraha'). % Lo       SHARADA SIGN AVAGRAHA

% ================================================

% Indic_Syllabic_Category=Nukta

% Nukta (diacritic for borrowed consonants)

% [Derivation: (ccc=7) - 1037]

unicode_indic_syllabic_category(0x093C, 0x093C  , 'Nukta'). % Mn       DEVANAGARI SIGN NUKTA
unicode_indic_syllabic_category(0x09BC, 0x09BC  , 'Nukta'). % Mn       BENGALI SIGN NUKTA
unicode_indic_syllabic_category(0x0A3C, 0x0A3C  , 'Nukta'). % Mn       GURMUKHI SIGN NUKTA
unicode_indic_syllabic_category(0x0ABC, 0x0ABC  , 'Nukta'). % Mn       GUJARATI SIGN NUKTA
unicode_indic_syllabic_category(0x0B3C, 0x0B3C  , 'Nukta'). % Mn       ORIYA SIGN NUKTA
unicode_indic_syllabic_category(0x0CBC, 0x0CBC  , 'Nukta'). % Mn       KANNADA SIGN NUKTA
unicode_indic_syllabic_category(0x1B34, 0x1B34  , 'Nukta'). % Mn       BALINESE SIGN REREKAN
unicode_indic_syllabic_category(0x1BE6, 0x1BE6  , 'Nukta'). % Mn       BATAK SIGN TOMPI
unicode_indic_syllabic_category(0x1C37, 0x1C37  , 'Nukta'). % Mn       LEPCHA SIGN NUKTA
unicode_indic_syllabic_category(0xA9B3, 0xA9B3  , 'Nukta'). % Mn       JAVANESE SIGN CECAK TELU
unicode_indic_syllabic_category(0x110BA, 0x110BA, 'Nukta'). % Mn       KAITHI SIGN NUKTA
unicode_indic_syllabic_category(0x116B7, 0x116B7, 'Nukta'). % Mn       TAKRI SIGN NUKTA

% ================================================

% Indic_Syllabic_Category=Virama

% Virama (killing of inherent vowel in consonant sequence,
%         or consonant stacker, depending on model)
% Also includes pure killers.

% [Derivation: (ccc=9) + 0E4E + 17D1]

unicode_indic_syllabic_category(0x094D, 0x094D, 'Virama'). % Mn       DEVANAGARI SIGN VIRAMA
unicode_indic_syllabic_category(0x09CD, 0x09CD, 'Virama'). % Mn       BENGALI SIGN VIRAMA
unicode_indic_syllabic_category(0x0A4D, 0x0A4D, 'Virama'). % Mn       GURMUKHI SIGN VIRAMA
unicode_indic_syllabic_category(0x0ACD, 0x0ACD, 'Virama'). % Mn       GUJARATI SIGN VIRAMA
unicode_indic_syllabic_category(0x0B4D, 0x0B4D, 'Virama'). % Mn       ORIYA SIGN VIRAMA
unicode_indic_syllabic_category(0x0BCD, 0x0BCD, 'Virama'). % Mn       TAMIL SIGN VIRAMA
unicode_indic_syllabic_category(0x0C4D, 0x0C4D, 'Virama'). % Mn       TELUGU SIGN VIRAMA
unicode_indic_syllabic_category(0x0CCD, 0x0CCD, 'Virama'). % Mn       KANNADA SIGN VIRAMA
unicode_indic_syllabic_category(0x0D4D, 0x0D4D, 'Virama'). % Mn       MALAYALAM SIGN VIRAMA
unicode_indic_syllabic_category(0x0DCA, 0x0DCA, 'Virama'). % Mn       SINHALA SIGN AL-LAKUNA
unicode_indic_syllabic_category(0x0E3A, 0x0E3A, 'Virama'). % Mn       THAI CHARACTER PHINTHU
unicode_indic_syllabic_category(0x0E4E, 0x0E4E, 'Virama'). % Mn       THAI CHARACTER YAMAKKAN
unicode_indic_syllabic_category(0x0F84, 0x0F84, 'Virama'). % Mn       TIBETAN MARK HALANTA
unicode_indic_syllabic_category(0x1039, 0x103A, 'Virama'). % Mn   [2] MYANMAR SIGN VIRAMA..MYANMAR SIGN ASAT
unicode_indic_syllabic_category(0x1714, 0x1714, 'Virama'). % Mn       TAGALOG SIGN VIRAMA
unicode_indic_syllabic_category(0x1734, 0x1734, 'Virama'). % Mn       HANUNOO SIGN PAMUDPOD
unicode_indic_syllabic_category(0x17D1, 0x17D2, 'Virama'). % Mn   [2] KHMER SIGN VIRIAM..KHMER SIGN COENG
unicode_indic_syllabic_category(0x1A60, 0x1A60, 'Virama'). % Mn       TAI THAM SIGN SAKOT
unicode_indic_syllabic_category(0x1B44, 0x1B44, 'Virama'). % Mc       BALINESE ADEG ADEG
unicode_indic_syllabic_category(0x1BAA, 0x1BAA, 'Virama'). % Mc       SUNDANESE SIGN PAMAAEH
unicode_indic_syllabic_category(0x1BAB, 0x1BAB, 'Virama'). % Mc       SUNDANESE SIGN VIRAMA
unicode_indic_syllabic_category(0x1BF2, 0x1BF3, 'Virama'). % Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_indic_syllabic_category(0xA806, 0xA806, 'Virama'). % Mn       SYLOTI NAGRI SIGN HASANTA
unicode_indic_syllabic_category(0xA8C4, 0xA8C4, 'Virama'). % Mn       SAURASHTRA SIGN VIRAMA
unicode_indic_syllabic_category(0xA953, 0xA953, 'Virama'). % Mc       REJANG VIRAMA
unicode_indic_syllabic_category(0xA9C0, 0xA9C0, 'Virama'). % Mc       JAVANESE PANGKON
unicode_indic_syllabic_category(0xAAF6, 0xAAF6, 'Virama'). % Mn       MEETEI MAYEK VIRAMA
unicode_indic_syllabic_category(0xABED, 0xABED, 'Virama'). % Mn       MEETEI MAYEK APUN IYEK
unicode_indic_syllabic_category(0x10A3F, 0x10A3F, 'Virama'). % Mn       KHAROSHTHI VIRAMA
unicode_indic_syllabic_category(0x11046, 0x11046, 'Virama'). % Mn       BRAHMI VIRAMA
unicode_indic_syllabic_category(0x110B9, 0x110B9, 'Virama'). % Mn       KAITHI SIGN VIRAMA
unicode_indic_syllabic_category(0x11133, 0x11134, 'Virama'). % Mn       CHAKMA VIRAMA..CHAKMA MAAYYAA
unicode_indic_syllabic_category(0x111C0, 0x111C0, 'Virama'). % Mc       SHARADA SIGN VIRAMA
unicode_indic_syllabic_category(0x116B6, 0x116B6, 'Virama'). % Mn       TAKRI SIGN VIRAMA

% ================================================

% Indic_Syllabic_Category=Vowel_Independent

% Independent Vowels (contrasted with matras)

% [Not derivable]

unicode_indic_syllabic_category(0x0904, 0x0914, 'Vowel_Independent'). % Lo  [17] DEVANAGARI LETTER SHORT A..DEVANAGARI LETTER AU
unicode_indic_syllabic_category(0x0960, 0x0961, 'Vowel_Independent'). % Lo   [2] DEVANAGARI LETTER VOCALIC RR..DEVANAGARI LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0972, 0x0977, 'Vowel_Independent'). % Lo   [6] DEVANAGARI LETTER CANDRA A..DEVANAGARI LETTER UUE
unicode_indic_syllabic_category(0x0985, 0x098C, 'Vowel_Independent'). % Lo   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
unicode_indic_syllabic_category(0x098F, 0x0990, 'Vowel_Independent'). % Lo   [2] BENGALI LETTER E..BENGALI LETTER AI
unicode_indic_syllabic_category(0x0993, 0x0994, 'Vowel_Independent'). % Lo   [2] BENGALI LETTER O..BENGALI LETTER AU
unicode_indic_syllabic_category(0x09E0, 0x09E1, 'Vowel_Independent'). % Lo   [2] BENGALI LETTER VOCALIC RR..BENGALI LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0A05, 0x0A0A, 'Vowel_Independent'). % Lo   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
unicode_indic_syllabic_category(0x0A0F, 0x0A10, 'Vowel_Independent'). % Lo   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
unicode_indic_syllabic_category(0x0A13, 0x0A14, 'Vowel_Independent'). % Lo   [2] GURMUKHI LETTER OO..GURMUKHI LETTER AU
unicode_indic_syllabic_category(0x0A85, 0x0A8D, 'Vowel_Independent'). % Lo   [9] GUJARATI LETTER A..GUJARATI VOWEL CANDRA E
unicode_indic_syllabic_category(0x0A8F, 0x0A91, 'Vowel_Independent'). % Lo   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
unicode_indic_syllabic_category(0x0A93, 0x0A94, 'Vowel_Independent'). % Lo   [2] GUJARATI LETTER O..GUJARATI LETTER AU
unicode_indic_syllabic_category(0x0AE0, 0x0AE1, 'Vowel_Independent'). % Lo   [2] GUJARATI LETTER VOCALIC RR..GUJARATI LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0B05, 0x0B0C, 'Vowel_Independent'). % Lo   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
unicode_indic_syllabic_category(0x0B0F, 0x0B10, 'Vowel_Independent'). % Lo   [2] ORIYA LETTER E..ORIYA LETTER AI
unicode_indic_syllabic_category(0x0B13, 0x0B14, 'Vowel_Independent'). % Lo   [2] ORIYA LETTER O..ORIYA LETTER AU
unicode_indic_syllabic_category(0x0B60, 0x0B61, 'Vowel_Independent'). % Lo   [2] ORIYA LETTER VOCALIC RR..ORIYA LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0B85, 0x0B8A, 'Vowel_Independent'). % Lo   [6] TAMIL LETTER A..TAMIL LETTER UU
unicode_indic_syllabic_category(0x0B8E, 0x0B90, 'Vowel_Independent'). % Lo   [3] TAMIL LETTER E..TAMIL LETTER AI
unicode_indic_syllabic_category(0x0B92, 0x0B94, 'Vowel_Independent'). % Lo   [3] TAMIL LETTER O..TAMIL LETTER AU
unicode_indic_syllabic_category(0x0C05, 0x0C0C, 'Vowel_Independent'). % Lo   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
unicode_indic_syllabic_category(0x0C0E, 0x0C10, 'Vowel_Independent'). % Lo   [3] TELUGU LETTER E..TELUGU LETTER AI
unicode_indic_syllabic_category(0x0C12, 0x0C14, 'Vowel_Independent'). % Lo   [3] TELUGU LETTER O..TELUGU LETTER AU
unicode_indic_syllabic_category(0x0C60, 0x0C61, 'Vowel_Independent'). % Lo   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0C85, 0x0C8C, 'Vowel_Independent'). % Lo   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
unicode_indic_syllabic_category(0x0C8E, 0x0C90, 'Vowel_Independent'). % Lo   [3] KANNADA LETTER E..KANNADA LETTER AI
unicode_indic_syllabic_category(0x0C92, 0x0C94, 'Vowel_Independent'). % Lo   [3] KANNADA LETTER O..KANNADA LETTER AU
unicode_indic_syllabic_category(0x0CE0, 0x0CE1, 'Vowel_Independent'). % Lo   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0D05, 0x0D0C, 'Vowel_Independent'). % Lo   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
unicode_indic_syllabic_category(0x0D0E, 0x0D10, 'Vowel_Independent'). % Lo   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
unicode_indic_syllabic_category(0x0D12, 0x0D14, 'Vowel_Independent'). % Lo   [3] MALAYALAM LETTER O..MALAYALAM LETTER AU
unicode_indic_syllabic_category(0x0D60, 0x0D61, 'Vowel_Independent'). % Lo   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
unicode_indic_syllabic_category(0x0D85, 0x0D96, 'Vowel_Independent'). % Lo  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
unicode_indic_syllabic_category(0x1021, 0x102A, 'Vowel_Independent'). % Lo  [10] MYANMAR LETTER A..MYANMAR LETTER AU
unicode_indic_syllabic_category(0x1052, 0x1055, 'Vowel_Independent'). % Lo   [4] MYANMAR LETTER VOCALIC R..MYANMAR LETTER VOCALIC LL
unicode_indic_syllabic_category(0x1700, 0x1702, 'Vowel_Independent'). % Lo   [3] TAGALOG LETTER A..TAGALOG LETTER U
unicode_indic_syllabic_category(0x1720, 0x1722, 'Vowel_Independent'). % Lo   [3] HANUNOO LETTER A..HANUNOO LETTER U
unicode_indic_syllabic_category(0x1740, 0x1742, 'Vowel_Independent'). % Lo   [3] BUHID LETTER A..BUHID LETTER U
unicode_indic_syllabic_category(0x1760, 0x1762, 'Vowel_Independent'). % Lo   [3] TAGBANWA LETTER A..TAGBANWA LETTER U
unicode_indic_syllabic_category(0x17A3, 0x17B3, 'Vowel_Independent'). % Lo  [17] KHMER INDEPENDENT VOWEL QAQ..KHMER INDEPENDENT VOWEL QAU
unicode_indic_syllabic_category(0x1A4D, 0x1A52, 'Vowel_Independent'). % Lo   [6] TAI THAM LETTER I..TAI THAM LETTER OO
unicode_indic_syllabic_category(0x1B05, 0x1B12, 'Vowel_Independent'). % Lo  [14] BALINESE LETTER AKARA..BALINESE LETTER OKARA TEDUNG
unicode_indic_syllabic_category(0x1B83, 0x1B89, 'Vowel_Independent'). % Lo   [7] SUNDANESE LETTER A..SUNDANESE LETTER EU
unicode_indic_syllabic_category(0x1BE4, 0x1BE5, 'Vowel_Independent'). % Lo   [2] BATAK LETTER I..BATAK LETTER U
unicode_indic_syllabic_category(0xA800, 0xA801, 'Vowel_Independent'). % Lo   [2] SYLOTI NAGRI LETTER A..SYLOTI NAGRI LETTER I
unicode_indic_syllabic_category(0xA803, 0xA805, 'Vowel_Independent'). % Lo   [3] SYLOTI NAGRI LETTER U..SYLOTI NAGRI LETTER O
unicode_indic_syllabic_category(0xA882, 0xA891, 'Vowel_Independent'). % Lo  [16] SAURASHTRA LETTER A..SAURASHTRA LETTER AU
unicode_indic_syllabic_category(0xA984, 0xA988, 'Vowel_Independent'). % Lo   [5] JAVANESE LETTER A..JAVANESE LETTER U
unicode_indic_syllabic_category(0xA98C, 0xA98E, 'Vowel_Independent'). % Lo   [3] JAVANESE LETTER E..JAVANESE LETTER O
unicode_indic_syllabic_category(0xAA00, 0xAA05, 'Vowel_Independent'). % Lo   [6] CHAM LETTER A..CHAM LETTER O
unicode_indic_syllabic_category(0xAAE0, 0xAAE1, 'Vowel_Independent'). % Lo   [2] MEETEI MAYEK LETTER E..MEETEI MAYEK LETTER O
unicode_indic_syllabic_category(0xABCE, 0xABCF, 'Vowel_Independent'). % Lo   [2] MEETEI MAYEK LETTER UN..MEETEI MAYEK LETTER I
unicode_indic_syllabic_category(0xABD1, 0xABD1, 'Vowel_Independent'). % Lo       MEETEI MAYEK LETTER ATIYA
unicode_indic_syllabic_category(0x11005, 0x11012, 'Vowel_Independent'). % Lo  [14] BRAHMI LETTER A..BRAHMI LETTER AU
unicode_indic_syllabic_category(0x11083, 0x1108C, 'Vowel_Independent'). % Lo  [10] KAITHI LETTER A..KAITHI LETTER AU
unicode_indic_syllabic_category(0x11103, 0x11106, 'Vowel_Independent'). % Lo   [4] CHAKMA LETTER AA..CHAKMA LETTER E
unicode_indic_syllabic_category(0x11183, 0x11190, 'Vowel_Independent'). % Lo  [14] SHARADA LETTER A..SHARADA LETTER AU
unicode_indic_syllabic_category(0x11680, 0x11689, 'Vowel_Independent'). % Lo  [10] TAKRI LETTER A..TAKRI LETTER AU

% ================================================

% Indic_Syllabic_Category=Vowel_Dependent

% Dependent Vowels (contrasted with independent vowels and/or with complex placement)
% Matras (in Indic scripts)

% [Not derivable]

unicode_indic_syllabic_category(0x093A, 0x093A, 'Vowel_Dependent'). % Mn       DEVANAGARI VOWEL SIGN OE
unicode_indic_syllabic_category(0x093B, 0x093B, 'Vowel_Dependent'). % Mc       DEVANAGARI VOWEL SIGN OOE
unicode_indic_syllabic_category(0x093E, 0x0940, 'Vowel_Dependent'). % Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
unicode_indic_syllabic_category(0x0941, 0x0948, 'Vowel_Dependent'). % Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
unicode_indic_syllabic_category(0x0949, 0x094C, 'Vowel_Dependent'). % Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_indic_syllabic_category(0x094E, 0x094F, 'Vowel_Dependent'). % Mc   [2] DEVANAGARI VOWEL SIGN PRISHTHAMATRA E..DEVANAGARI VOWEL SIGN AW
unicode_indic_syllabic_category(0x0955, 0x0957, 'Vowel_Dependent'). % Mn       DEVANAGARI VOWEL SIGN CANDRA LONG E..DEVANAGARI VOWEL SIGN UUE
unicode_indic_syllabic_category(0x0962, 0x0963, 'Vowel_Dependent'). % Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x09BE, 0x09C0, 'Vowel_Dependent'). % Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
unicode_indic_syllabic_category(0x09C1, 0x09C4, 'Vowel_Dependent'). % Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_indic_syllabic_category(0x09C7, 0x09C8, 'Vowel_Dependent'). % Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_indic_syllabic_category(0x09CB, 0x09CC, 'Vowel_Dependent'). % Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_indic_syllabic_category(0x09D7, 0x09D7, 'Vowel_Dependent'). % Mc       BENGALI AU LENGTH MARK
unicode_indic_syllabic_category(0x09E2, 0x09E3, 'Vowel_Dependent'). % Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x0A3E, 0x0A40, 'Vowel_Dependent'). % Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
unicode_indic_syllabic_category(0x0A41, 0x0A42, 'Vowel_Dependent'). % Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_indic_syllabic_category(0x0A47, 0x0A48, 'Vowel_Dependent'). % Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_indic_syllabic_category(0x0A4B, 0x0A4C, 'Vowel_Dependent'). % Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
unicode_indic_syllabic_category(0x0ABE, 0x0AC0, 'Vowel_Dependent'). % Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
unicode_indic_syllabic_category(0x0AC1, 0x0AC5, 'Vowel_Dependent'). % Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
unicode_indic_syllabic_category(0x0AC7, 0x0AC8, 'Vowel_Dependent'). % Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_indic_syllabic_category(0x0AC9, 0x0AC9, 'Vowel_Dependent'). % Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_indic_syllabic_category(0x0ACB, 0x0ACC, 'Vowel_Dependent'). % Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_indic_syllabic_category(0x0AE2, 0x0AE3, 'Vowel_Dependent'). % Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x0B3E, 0x0B3E, 'Vowel_Dependent'). % Mc       ORIYA VOWEL SIGN AA
unicode_indic_syllabic_category(0x0B3F, 0x0B3F, 'Vowel_Dependent'). % Mn       ORIYA VOWEL SIGN I
unicode_indic_syllabic_category(0x0B40, 0x0B40, 'Vowel_Dependent'). % Mc       ORIYA VOWEL SIGN II
unicode_indic_syllabic_category(0x0B41, 0x0B44, 'Vowel_Dependent'). % Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_indic_syllabic_category(0x0B47, 0x0B48, 'Vowel_Dependent'). % Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
unicode_indic_syllabic_category(0x0B4B, 0x0B4C, 'Vowel_Dependent'). % Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
unicode_indic_syllabic_category(0x0B56, 0x0B56, 'Vowel_Dependent'). % Mn       ORIYA AI LENGTH MARK
unicode_indic_syllabic_category(0x0B57, 0x0B57, 'Vowel_Dependent'). % Mc       ORIYA AU LENGTH MARK
unicode_indic_syllabic_category(0x0B62, 0x0B63, 'Vowel_Dependent'). % Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x0BBE, 0x0BBF, 'Vowel_Dependent'). % Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_indic_syllabic_category(0x0BC0, 0x0BC0, 'Vowel_Dependent'). % Mn       TAMIL VOWEL SIGN II
unicode_indic_syllabic_category(0x0BC1, 0x0BC2, 'Vowel_Dependent'). % Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
unicode_indic_syllabic_category(0x0BC6, 0x0BC8, 'Vowel_Dependent'). % Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_indic_syllabic_category(0x0BCA, 0x0BCC, 'Vowel_Dependent'). % Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_indic_syllabic_category(0x0BD7, 0x0BD7, 'Vowel_Dependent'). % Mc       TAMIL AU LENGTH MARK
unicode_indic_syllabic_category(0x0C3E, 0x0C40, 'Vowel_Dependent'). % Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_indic_syllabic_category(0x0C41, 0x0C44, 'Vowel_Dependent'). % Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_indic_syllabic_category(0x0C46, 0x0C48, 'Vowel_Dependent'). % Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
unicode_indic_syllabic_category(0x0C4A, 0x0C4C, 'Vowel_Dependent'). % Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
unicode_indic_syllabic_category(0x0C55, 0x0C56, 'Vowel_Dependent'). % Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
unicode_indic_syllabic_category(0x0C62, 0x0C63, 'Vowel_Dependent'). % Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x0CBE, 0x0CBE, 'Vowel_Dependent'). % Mc       KANNADA VOWEL SIGN AA
unicode_indic_syllabic_category(0x0CBF, 0x0CBF, 'Vowel_Dependent'). % Mn       KANNADA VOWEL SIGN I
unicode_indic_syllabic_category(0x0CC0, 0x0CC4, 'Vowel_Dependent'). % Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
unicode_indic_syllabic_category(0x0CC6, 0x0CC6, 'Vowel_Dependent'). % Mn       KANNADA VOWEL SIGN E
unicode_indic_syllabic_category(0x0CC7, 0x0CC8, 'Vowel_Dependent'). % Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_indic_syllabic_category(0x0CCA, 0x0CCB, 'Vowel_Dependent'). % Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_indic_syllabic_category(0x0CCC, 0x0CCC, 'Vowel_Dependent'). % Mn       KANNADA VOWEL SIGN AU
unicode_indic_syllabic_category(0x0CD5, 0x0CD6, 'Vowel_Dependent'). % Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_indic_syllabic_category(0x0CE2, 0x0CE3, 'Vowel_Dependent'). % Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x0D3E, 0x0D40, 'Vowel_Dependent'). % Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_indic_syllabic_category(0x0D41, 0x0D44, 'Vowel_Dependent'). % Mn   [4] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_indic_syllabic_category(0x0D46, 0x0D48, 'Vowel_Dependent'). % Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_indic_syllabic_category(0x0D4A, 0x0D4C, 'Vowel_Dependent'). % Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_indic_syllabic_category(0x0D57, 0x0D57, 'Vowel_Dependent'). % Mc       MALAYALAM AU LENGTH MARK
unicode_indic_syllabic_category(0x0D62, 0x0D63, 'Vowel_Dependent'). % Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x0DCF, 0x0DD1, 'Vowel_Dependent'). % Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_indic_syllabic_category(0x0DD2, 0x0DD4, 'Vowel_Dependent'). % Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_indic_syllabic_category(0x0DD6, 0x0DD6, 'Vowel_Dependent'). % Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_indic_syllabic_category(0x0DD8, 0x0DDF, 'Vowel_Dependent'). % Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
unicode_indic_syllabic_category(0x0DF2, 0x0DF3, 'Vowel_Dependent'). % Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_indic_syllabic_category(0x0E30, 0x0E30, 'Vowel_Dependent'). % Lo       THAI CHARACTER SARA A
unicode_indic_syllabic_category(0x0E31, 0x0E31, 'Vowel_Dependent'). % Mn       THAI CHARACTER MAI HAN-AKAT
unicode_indic_syllabic_category(0x0E32, 0x0E33, 'Vowel_Dependent'). % Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_indic_syllabic_category(0x0E34, 0x0E39, 'Vowel_Dependent'). % Mn   [6] THAI CHARACTER SARA I..THAI CHARACTER SARA UU
unicode_indic_syllabic_category(0x0E40, 0x0E45, 'Vowel_Dependent'). % Lo   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
unicode_indic_syllabic_category(0x0E47, 0x0E47, 'Vowel_Dependent'). % Mn       THAI CHARACTER MAITAIKHU
unicode_indic_syllabic_category(0x0EB0, 0x0EB0, 'Vowel_Dependent'). % Lo       LAO VOWEL SIGN A
unicode_indic_syllabic_category(0x0EB1, 0x0EB1, 'Vowel_Dependent'). % Mn       LAO VOWEL SIGN MAI KAN
unicode_indic_syllabic_category(0x0EB2, 0x0EB3, 'Vowel_Dependent'). % Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_indic_syllabic_category(0x0EB4, 0x0EB9, 'Vowel_Dependent'). % Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
unicode_indic_syllabic_category(0x0EBB, 0x0EBB, 'Vowel_Dependent'). % Mn       LAO VOWEL SIGN MAI KON
unicode_indic_syllabic_category(0x0EC0, 0x0EC4, 'Vowel_Dependent'). % Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_indic_syllabic_category(0x0F71, 0x0F7D, 'Vowel_Dependent'). % Mn  [13] TIBETAN VOWEL SIGN AA..TIBETAN VOWEL SIGN OO
unicode_indic_syllabic_category(0x0F80, 0x0F81, 'Vowel_Dependent'). % Mn   [2] TIBETAN VOWEL SIGN REVERSED I..TIBETAN VOWEL SIGN REVERSED II
unicode_indic_syllabic_category(0x102B, 0x102C, 'Vowel_Dependent'). % Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_indic_syllabic_category(0x102D, 0x1030, 'Vowel_Dependent'). % Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
unicode_indic_syllabic_category(0x1031, 0x1031, 'Vowel_Dependent'). % Mc       MYANMAR VOWEL SIGN E
unicode_indic_syllabic_category(0x1032, 0x1035, 'Vowel_Dependent'). % Mn   [4] MYANMAR VOWEL SIGN AI..MYANMAR VOWEL SIGN E ABOVE
unicode_indic_syllabic_category(0x1056, 0x1057, 'Vowel_Dependent'). % Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_indic_syllabic_category(0x1058, 0x1059, 'Vowel_Dependent'). % Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_indic_syllabic_category(0x1062, 0x1062, 'Vowel_Dependent'). % Mc       MYANMAR VOWEL SIGN SGAW KAREN EU
unicode_indic_syllabic_category(0x1067, 0x1068, 'Vowel_Dependent'). % Mc   [2] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR VOWEL SIGN WESTERN PWO KAREN UE
unicode_indic_syllabic_category(0x1071, 0x1074, 'Vowel_Dependent'). % Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_indic_syllabic_category(0x1083, 0x1084, 'Vowel_Dependent'). % Mc   [2] MYANMAR VOWEL SIGN SHAN AA..MYANMAR VOWEL SIGN SHAN E
unicode_indic_syllabic_category(0x1085, 0x1086, 'Vowel_Dependent'). % Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_indic_syllabic_category(0x109C, 0x109C, 'Vowel_Dependent'). % Mc       MYANMAR VOWEL SIGN AITON A
unicode_indic_syllabic_category(0x109D, 0x109D, 'Vowel_Dependent'). % Mn       MYANMAR VOWEL SIGN AITON AI
unicode_indic_syllabic_category(0x1712, 0x1713, 'Vowel_Dependent'). % Mn   [2] TAGALOG VOWEL SIGN I..TAGALOG VOWEL SIGN U
unicode_indic_syllabic_category(0x1732, 0x1733, 'Vowel_Dependent'). % Mn   [2] HANUNOO VOWEL SIGN I..HANUNOO VOWEL SIGN U
unicode_indic_syllabic_category(0x1752, 0x1753, 'Vowel_Dependent'). % Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
unicode_indic_syllabic_category(0x1772, 0x1773, 'Vowel_Dependent'). % Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
unicode_indic_syllabic_category(0x17B6, 0x17B6, 'Vowel_Dependent'). % Mc       KHMER VOWEL SIGN AA
unicode_indic_syllabic_category(0x17B7, 0x17BD, 'Vowel_Dependent'). % Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
unicode_indic_syllabic_category(0x17BE, 0x17C5, 'Vowel_Dependent'). % Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
unicode_indic_syllabic_category(0x17C8, 0x17C8, 'Vowel_Dependent'). % Mc       KHMER SIGN YUUKALEAPINTU
unicode_indic_syllabic_category(0x1920, 0x1922, 'Vowel_Dependent'). % Mn   [3] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN U
unicode_indic_syllabic_category(0x1923, 0x1926, 'Vowel_Dependent'). % Mc   [4] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AU
unicode_indic_syllabic_category(0x1927, 0x1928, 'Vowel_Dependent'). % Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_indic_syllabic_category(0x19B0, 0x19C0, 'Vowel_Dependent'). % Mc  [17] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN IY
unicode_indic_syllabic_category(0x1A17, 0x1A18, 'Vowel_Dependent'). % Mn   [2] BUGINESE VOWEL SIGN I..BUGINESE VOWEL SIGN U
unicode_indic_syllabic_category(0x1A19, 0x1A1B, 'Vowel_Dependent'). % Mc   [3] BUGINESE VOWEL SIGN E..BUGINESE VOWEL SIGN AE
unicode_indic_syllabic_category(0x1A61, 0x1A61, 'Vowel_Dependent'). % Mc       TAI THAM VOWEL SIGN A
unicode_indic_syllabic_category(0x1A62, 0x1A62, 'Vowel_Dependent'). % Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_indic_syllabic_category(0x1A63, 0x1A64, 'Vowel_Dependent'). % Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_indic_syllabic_category(0x1A65, 0x1A6C, 'Vowel_Dependent'). % Mn   [8] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN OA BELOW
unicode_indic_syllabic_category(0x1A6D, 0x1A72, 'Vowel_Dependent'). % Mc   [6] TAI THAM VOWEL SIGN OY..TAI THAM VOWEL SIGN THAM AI
unicode_indic_syllabic_category(0x1A73, 0x1A74, 'Vowel_Dependent'). % Mn   [2] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN MAI KANG
unicode_indic_syllabic_category(0x1B35, 0x1B35, 'Vowel_Dependent'). % Mc       BALINESE VOWEL SIGN TEDUNG
unicode_indic_syllabic_category(0x1B36, 0x1B3A, 'Vowel_Dependent'). % Mn   [5] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN RA REPA
unicode_indic_syllabic_category(0x1B3B, 0x1B3B, 'Vowel_Dependent'). % Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_indic_syllabic_category(0x1B3C, 0x1B3C, 'Vowel_Dependent'). % Mn       BALINESE VOWEL SIGN LA LENGA
unicode_indic_syllabic_category(0x1B3D, 0x1B41, 'Vowel_Dependent'). % Mc   [5] BALINESE VOWEL SIGN LA LENGA TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG
unicode_indic_syllabic_category(0x1B42, 0x1B42, 'Vowel_Dependent'). % Mn       BALINESE VOWEL SIGN PEPET
unicode_indic_syllabic_category(0x1B43, 0x1B43, 'Vowel_Dependent'). % Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_indic_syllabic_category(0x1BA4, 0x1BA5, 'Vowel_Dependent'). % Mn   [2] SUNDANESE VOWEL SIGN PANGHULU..SUNDANESE VOWEL SIGN PANYUKU
unicode_indic_syllabic_category(0x1BA6, 0x1BA7, 'Vowel_Dependent'). % Mc   [2] SUNDANESE VOWEL SIGN PANAELAENG..SUNDANESE VOWEL SIGN PANOLONG
unicode_indic_syllabic_category(0x1BA8, 0x1BA9, 'Vowel_Dependent'). % Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_indic_syllabic_category(0x1BE7, 0x1BE7, 'Vowel_Dependent'). % Mc       BATAK VOWEL SIGN E
unicode_indic_syllabic_category(0x1BE8, 0x1BE9, 'Vowel_Dependent'). % Mn   [2] BATAK VOWEL SIGN PAKPAK E..BATAK VOWEL SIGN EE
unicode_indic_syllabic_category(0x1BEA, 0x1BEC, 'Vowel_Dependent'). % Mc   [3] BATAK VOWEL SIGN I..BATAK VOWEL SIGN O
unicode_indic_syllabic_category(0x1BED, 0x1BED, 'Vowel_Dependent'). % Mn       BATAK VOWEL SIGN KARO O
unicode_indic_syllabic_category(0x1BEE, 0x1BEE, 'Vowel_Dependent'). % Mc       BATAK VOWEL SIGN U
unicode_indic_syllabic_category(0x1BEF, 0x1BEF, 'Vowel_Dependent'). % Mn       BATAK VOWEL SIGN U FOR SIMALUNGUN SA
unicode_indic_syllabic_category(0x1C26, 0x1C2B, 'Vowel_Dependent'). % Mc   [6] LEPCHA VOWEL SIGN AA..LEPCHA VOWEL SIGN UU
unicode_indic_syllabic_category(0x1C2C, 0x1C2C, 'Vowel_Dependent'). % Mn       LEPCHA VOWEL SIGN E
unicode_indic_syllabic_category(0xA823, 0xA824, 'Vowel_Dependent'). % Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_indic_syllabic_category(0xA825, 0xA826, 'Vowel_Dependent'). % Mn   [2] SYLOTI NAGRI VOWEL SIGN U..SYLOTI NAGRI VOWEL SIGN E
unicode_indic_syllabic_category(0xA827, 0xA827, 'Vowel_Dependent'). % Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_indic_syllabic_category(0xA8B5, 0xA8C3, 'Vowel_Dependent'). % Mc  [15] SAURASHTRA VOWEL SIGN AA..SAURASHTRA VOWEL SIGN AU
unicode_indic_syllabic_category(0xA947, 0xA94E, 'Vowel_Dependent'). % Mn   [8] REJANG VOWEL SIGN I..REJANG VOWEL SIGN EA
unicode_indic_syllabic_category(0xA9B4, 0xA9B5, 'Vowel_Dependent'). % Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_indic_syllabic_category(0xA9B6, 0xA9B9, 'Vowel_Dependent'). % Mn   [4] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_indic_syllabic_category(0xA9BA, 0xA9BB, 'Vowel_Dependent'). % Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_indic_syllabic_category(0xA9BC, 0xA9BC, 'Vowel_Dependent'). % Mn       JAVANESE VOWEL SIGN PEPET
unicode_indic_syllabic_category(0xAA29, 0xAA2E, 'Vowel_Dependent'). % Mn   [6] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN OE
unicode_indic_syllabic_category(0xAA2F, 0xAA30, 'Vowel_Dependent'). % Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_indic_syllabic_category(0xAA31, 0xAA32, 'Vowel_Dependent'). % Mn   [2] CHAM VOWEL SIGN AU..CHAM VOWEL SIGN UE
unicode_indic_syllabic_category(0xAAB0, 0xAAB0, 'Vowel_Dependent'). % Mn       TAI VIET MAI KANG
unicode_indic_syllabic_category(0xAAB1, 0xAAB1, 'Vowel_Dependent'). % Lo       TAI VIET VOWEL AA
unicode_indic_syllabic_category(0xAAB2, 0xAAB4, 'Vowel_Dependent'). % Mn   [3] TAI VIET VOWEL I..TAI VIET VOWEL U
unicode_indic_syllabic_category(0xAAB5, 0xAAB6, 'Vowel_Dependent'). % Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_indic_syllabic_category(0xAAB7, 0xAAB8, 'Vowel_Dependent'). % Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_indic_syllabic_category(0xAAB9, 0xAABD, 'Vowel_Dependent'). % Lo   [5] TAI VIET VOWEL UEA..TAI VIET VOWEL AN
unicode_indic_syllabic_category(0xAABE, 0xAABE, 'Vowel_Dependent'). % Mn       TAI VIET VOWEL AM
unicode_indic_syllabic_category(0xAAEB, 0xAAEF, 'Vowel_Dependent'). % Mc   [5] MEETEI MAYEK VOWEL SIGN II..MEETEI MAYEK VOWEL SIGN AAU
unicode_indic_syllabic_category(0xABE3, 0xABE4, 'Vowel_Dependent'). % Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_indic_syllabic_category(0xABE5, 0xABE5, 'Vowel_Dependent'). % Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_indic_syllabic_category(0xABE6, 0xABE7, 'Vowel_Dependent'). % Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_indic_syllabic_category(0xABE8, 0xABE8, 'Vowel_Dependent'). % Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_indic_syllabic_category(0xABE9, 0xABEA, 'Vowel_Dependent'). % Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_indic_syllabic_category(0x10A01, 0x10A03, 'Vowel_Dependent'). % Mn   [3] KHAROSHTHI VOWEL SIGN I..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_indic_syllabic_category(0x10A05, 0x10A06, 'Vowel_Dependent'). % Mn   [2] KHAROSHTHI VOWEL SIGN E..KHAROSHTHI VOWEL SIGN O
unicode_indic_syllabic_category(0x10A0C, 0x10A0C, 'Vowel_Dependent'). % Mn       KHAROSHTHI VOWEL LENGTH MARK
unicode_indic_syllabic_category(0x11038, 0x11045, 'Vowel_Dependent'). % Mn  [14] BRAHMI VOWEL SIGN AA..BRAHMI VOWEL SIGN AU
unicode_indic_syllabic_category(0x110B0, 0x110B2, 'Vowel_Dependent'). % Mc   [3] KAITHI VOWEL SIGN AA..KAITHI VOWEL SIGN II
unicode_indic_syllabic_category(0x110B3, 0x110B6, 'Vowel_Dependent'). % Mn   [4] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN AI
unicode_indic_syllabic_category(0x110B7, 0x110B8, 'Vowel_Dependent'). % Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_indic_syllabic_category(0x11127, 0x11132, 'Vowel_Dependent'). % Mn  [12] CHAKMA VOWEL SIGN A..CHAKMA AU MARK
unicode_indic_syllabic_category(0x111B3, 0x111BF, 'Vowel_Dependent'). % Mn  [13] SHARADA VOWEL SIGN AA..SHARADA VOWEL SIGN AU
unicode_indic_syllabic_category(0x116AD, 0x116B5, 'Vowel_Dependent'). % Mn   [9] TAKRI VOWEL SIGN AA..TAKRI VOWEL SIGN AU

% ================================================

% Indic_Syllabic_Category=Vowel

% (Other) Vowels (reanalyzed as ordinary alphabetic letters or marks)

% [Not derivable]

unicode_indic_syllabic_category(0x1963, 0x196D, 'Vowel'). % Lo  [11] TAI LE LETTER A..TAI LE LETTER AI
unicode_indic_syllabic_category(0xA85E, 0xA861, 'Vowel'). % Lo   [4] PHAGS-PA LETTER I..PHAGS-PA LETTER O
unicode_indic_syllabic_category(0xA866, 0xA866, 'Vowel'). % Lo   [1] PHAGS-PA LETTER EE
unicode_indic_syllabic_category(0xA922, 0xA925, 'Vowel'). % Lo   [4] KAYAH LI LETTER A..KAYAH LI LETTER OO
unicode_indic_syllabic_category(0xA926, 0xA92A, 'Vowel'). % Mn   [5] KAYAH LI VOWEL UE..KAYAH LI VOWEL O

% ================================================

% Indic_Syllabic_Category=Consonant_Placeholder

% Consonant Placeholder
% This includes generic placeholders used for
% Indic script layout (NBSP and dotted circle), as well as a few script-
% specific vowel-holder characters which are not technically
% consonants, but serve instead as bases for placement of vowel marks.

% [Not derivable]

unicode_indic_syllabic_category(0x00A0, 0x00A0, 'Consonant_Placeholder'). % Zs       NO-BREAK SPACE
unicode_indic_syllabic_category(0x0A72, 0x0A73, 'Consonant_Placeholder'). % Lo   [2] GURMUKHI IRI..GURMUKHI URA
unicode_indic_syllabic_category(0x1900, 0x1900, 'Consonant_Placeholder'). % Lo       LIMBU VOWEL-CARRIER LETTER
unicode_indic_syllabic_category(0x25CC, 0x25CC, 'Consonant_Placeholder'). % So       DOTTED CIRCLE

% ================================================

% Indic_Syllabic_Category=Consonant

% Consonant (ordinary abugida consonants, with inherent vowels)

% [Not derivable]

unicode_indic_syllabic_category(0x0915, 0x0939, 'Consonant'). % Lo  [35] DEVANAGARI LETTER KA..DEVANAGARI LETTER HA
unicode_indic_syllabic_category(0x0958, 0x095F, 'Consonant'). % Lo   [8] DEVANAGARI LETTER QA..DEVANAGARI LETTER YYA
unicode_indic_syllabic_category(0x0979, 0x097F, 'Consonant'). % Lo   [7] DEVANAGARI LETTER ZHA..DEVANAGARI LETTER BBA
unicode_indic_syllabic_category(0x0995, 0x09A8, 'Consonant'). % Lo  [20] BENGALI LETTER KA..BENGALI LETTER NA
unicode_indic_syllabic_category(0x09AA, 0x09B0, 'Consonant'). % Lo   [7] BENGALI LETTER PA..BENGALI LETTER RA
unicode_indic_syllabic_category(0x09B2, 0x09B2, 'Consonant'). % Lo       BENGALI LETTER LA
unicode_indic_syllabic_category(0x09B6, 0x09B9, 'Consonant'). % Lo   [4] BENGALI LETTER SHA..BENGALI LETTER HA
unicode_indic_syllabic_category(0x09DC, 0x09DD, 'Consonant'). % Lo   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
unicode_indic_syllabic_category(0x09DF, 0x09DF, 'Consonant'). % Lo       BENGALI LETTER YYA
unicode_indic_syllabic_category(0x09F0, 0x09F1, 'Consonant'). % Lo   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
unicode_indic_syllabic_category(0x0A15, 0x0A28, 'Consonant'). % Lo  [20] GURMUKHI LETTER KA..GURMUKHI LETTER NA
unicode_indic_syllabic_category(0x0A2A, 0x0A30, 'Consonant'). % Lo   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
unicode_indic_syllabic_category(0x0A32, 0x0A33, 'Consonant'). % Lo   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
unicode_indic_syllabic_category(0x0A35, 0x0A36, 'Consonant'). % Lo   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
unicode_indic_syllabic_category(0x0A38, 0x0A39, 'Consonant'). % Lo   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
unicode_indic_syllabic_category(0x0A59, 0x0A5C, 'Consonant'). % Lo   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
unicode_indic_syllabic_category(0x0A5E, 0x0A5E, 'Consonant'). % Lo       GURMUKHI LETTER FA
unicode_indic_syllabic_category(0x0A95, 0x0AA8, 'Consonant'). % Lo  [20] GUJARATI LETTER KA..GUJARATI LETTER NA
unicode_indic_syllabic_category(0x0AAA, 0x0AB0, 'Consonant'). % Lo   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
unicode_indic_syllabic_category(0x0AB2, 0x0AB3, 'Consonant'). % Lo   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
unicode_indic_syllabic_category(0x0AB5, 0x0AB9, 'Consonant'). % Lo   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
unicode_indic_syllabic_category(0x0B15, 0x0B28, 'Consonant'). % Lo  [20] ORIYA LETTER KA..ORIYA LETTER NA
unicode_indic_syllabic_category(0x0B2A, 0x0B30, 'Consonant'). % Lo   [7] ORIYA LETTER PA..ORIYA LETTER RA
unicode_indic_syllabic_category(0x0B32, 0x0B33, 'Consonant'). % Lo   [2] ORIYA LETTER LA..ORIYA LETTER LLA
unicode_indic_syllabic_category(0x0B35, 0x0B39, 'Consonant'). % Lo   [5] ORIYA LETTER VA..ORIYA LETTER HA
unicode_indic_syllabic_category(0x0B5C, 0x0B5D, 'Consonant'). % Lo   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
unicode_indic_syllabic_category(0x0B5F, 0x0B5F, 'Consonant'). % Lo       ORIYA LETTER YYA
unicode_indic_syllabic_category(0x0B71, 0x0B71, 'Consonant'). % Lo       ORIYA LETTER WA
unicode_indic_syllabic_category(0x0B95, 0x0B95, 'Consonant'). % Lo       TAMIL LETTER KA
unicode_indic_syllabic_category(0x0B99, 0x0B9A, 'Consonant'). % Lo   [2] TAMIL LETTER NGA..TAMIL LETTER CA
unicode_indic_syllabic_category(0x0B9C, 0x0B9C, 'Consonant'). % Lo       TAMIL LETTER JA
unicode_indic_syllabic_category(0x0B9E, 0x0B9F, 'Consonant'). % Lo   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
unicode_indic_syllabic_category(0x0BA3, 0x0BA4, 'Consonant'). % Lo   [2] TAMIL LETTER NNA..TAMIL LETTER TA
unicode_indic_syllabic_category(0x0BA8, 0x0BAA, 'Consonant'). % Lo   [3] TAMIL LETTER NA..TAMIL LETTER PA
unicode_indic_syllabic_category(0x0BAE, 0x0BB9, 'Consonant'). % Lo  [12] TAMIL LETTER MA..TAMIL LETTER HA
unicode_indic_syllabic_category(0x0C15, 0x0C28, 'Consonant'). % Lo  [20] TELUGU LETTER KA..TELUGU LETTER NA
unicode_indic_syllabic_category(0x0C2A, 0x0C33, 'Consonant'). % Lo  [10] TELUGU LETTER PA..TELUGU LETTER LLA
unicode_indic_syllabic_category(0x0C35, 0x0C39, 'Consonant'). % Lo   [5] TELUGU LETTER VA..TELUGU LETTER HA
unicode_indic_syllabic_category(0x0C58, 0x0C59, 'Consonant'). % Lo   [2] TELUGU LETTER TSA..TELUGU LETTER DZA
unicode_indic_syllabic_category(0x0C95, 0x0CA8, 'Consonant'). % Lo  [20] KANNADA LETTER KA..KANNADA LETTER NA
unicode_indic_syllabic_category(0x0CAA, 0x0CB3, 'Consonant'). % Lo  [10] KANNADA LETTER PA..KANNADA LETTER LLA
unicode_indic_syllabic_category(0x0CB5, 0x0CB9, 'Consonant'). % Lo   [5] KANNADA LETTER VA..KANNADA LETTER HA
unicode_indic_syllabic_category(0x0CDE, 0x0CDE, 'Consonant'). % Lo       KANNADA LETTER FA
unicode_indic_syllabic_category(0x0D15, 0x0D3A, 'Consonant'). % Lo  [38] MALAYALAM LETTER KA..MALAYALAM LETTER TTTA
unicode_indic_syllabic_category(0x0D9A, 0x0DB1, 'Consonant'). % Lo  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
unicode_indic_syllabic_category(0x0DB3, 0x0DBB, 'Consonant'). % Lo   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
unicode_indic_syllabic_category(0x0DBD, 0x0DBD, 'Consonant'). % Lo       SINHALA LETTER DANTAJA LAYANNA
unicode_indic_syllabic_category(0x0DC0, 0x0DC6, 'Consonant'). % Lo   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
unicode_indic_syllabic_category(0x0E01, 0x0E2E, 'Consonant'). % Lo  [46] THAI CHARACTER KO KAI..THAI CHARACTER NOKHUK
unicode_indic_syllabic_category(0x0E81, 0x0E82, 'Consonant'). % Lo   [2] LAO LETTER KO..LAO LETTER KHO SUNG
unicode_indic_syllabic_category(0x0E84, 0x0E84, 'Consonant'). % Lo       LAO LETTER KHO TAM
unicode_indic_syllabic_category(0x0E87, 0x0E88, 'Consonant'). % Lo   [2] LAO LETTER NGO..LAO LETTER CO
unicode_indic_syllabic_category(0x0E8A, 0x0E8A, 'Consonant'). % Lo       LAO LETTER SO TAM
unicode_indic_syllabic_category(0x0E8D, 0x0E8D, 'Consonant'). % Lo       LAO LETTER NYO
unicode_indic_syllabic_category(0x0E94, 0x0E97, 'Consonant'). % Lo   [4] LAO LETTER DO..LAO LETTER THO TAM
unicode_indic_syllabic_category(0x0E99, 0x0E9F, 'Consonant'). % Lo   [7] LAO LETTER NO..LAO LETTER FO SUNG
unicode_indic_syllabic_category(0x0EA1, 0x0EA3, 'Consonant'). % Lo   [3] LAO LETTER MO..LAO LETTER LO LING
unicode_indic_syllabic_category(0x0EA5, 0x0EA5, 'Consonant'). % Lo       LAO LETTER LO LOOT
unicode_indic_syllabic_category(0x0EA7, 0x0EA7, 'Consonant'). % Lo       LAO LETTER WO
unicode_indic_syllabic_category(0x0EAA, 0x0EAB, 'Consonant'). % Lo   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
unicode_indic_syllabic_category(0x0EAD, 0x0EAE, 'Consonant'). % Lo   [2] LAO LETTER O..LAO LETTER HO TAM
unicode_indic_syllabic_category(0x0EDC, 0x0EDD, 'Consonant'). % Lo   [2] LAO HO NO..LAO HO MO
unicode_indic_syllabic_category(0x0F40, 0x0F47, 'Consonant'). % Lo   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
unicode_indic_syllabic_category(0x0F49, 0x0F6C, 'Consonant'). % Lo  [36] TIBETAN LETTER NYA..TIBETAN LETTER RRA
unicode_indic_syllabic_category(0x1000, 0x1020, 'Consonant'). % Lo  [33] MYANMAR LETTER KA..MYANMAR LETTER LLA
unicode_indic_syllabic_category(0x103F, 0x103F, 'Consonant'). % Lo       MYANMAR LETTER GREAT SA
unicode_indic_syllabic_category(0x1050, 0x1051, 'Consonant'). % Lo   [2] MYANMAR LETTER SHA..MYANMAR LETTER SSA
unicode_indic_syllabic_category(0x105A, 0x105D, 'Consonant'). % Lo   [4] MYANMAR LETTER MON NGA..MYANMAR LETTER MON BBE
unicode_indic_syllabic_category(0x1061, 0x1061, 'Consonant'). % Lo       MYANMAR LETTER SGAW KAREN SHA
unicode_indic_syllabic_category(0x1065, 0x1066, 'Consonant'). % Lo   [2] MYANMAR LETTER WESTERN PWO KAREN THA..MYANMAR LETTER WESTERN PWO KAREN PWA
unicode_indic_syllabic_category(0x106E, 0x1070, 'Consonant'). % Lo   [3] MYANMAR LETTER EASTERN PWO KAREN NNA..MYANMAR LETTER EASTERN PWO KAREN GHWA
unicode_indic_syllabic_category(0x1075, 0x1081, 'Consonant'). % Lo  [13] MYANMAR LETTER SHAN KA..MYANMAR LETTER SHAN HA
unicode_indic_syllabic_category(0x108E, 0x108E, 'Consonant'). % Lo       MYANMAR LETTER RUMAI PALAUNG FA
unicode_indic_syllabic_category(0x1703, 0x170C, 'Consonant'). % Lo  [10] TAGALOG LETTER KA..TAGALOG LETTER YA
unicode_indic_syllabic_category(0x170E, 0x1711, 'Consonant'). % Lo   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
unicode_indic_syllabic_category(0x1723, 0x1731, 'Consonant'). % Lo  [15] HANUNOO LETTER KA..HANUNOO LETTER HA
unicode_indic_syllabic_category(0x1743, 0x1751, 'Consonant'). % Lo  [15] BUHID LETTER KA..BUHID LETTER HA
unicode_indic_syllabic_category(0x1763, 0x176C, 'Consonant'). % Lo  [10] TAGBANWA LETTER KA..TAGBANWA LETTER YA
unicode_indic_syllabic_category(0x176E, 0x1770, 'Consonant'). % Lo   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
unicode_indic_syllabic_category(0x1780, 0x17A2, 'Consonant'). % Lo  [35] KHMER LETTER KA..KHMER LETTER QA
unicode_indic_syllabic_category(0x1901, 0x191C, 'Consonant'). % Lo  [28] LIMBU LETTER KA..LIMBU LETTER HA
unicode_indic_syllabic_category(0x1950, 0x1962, 'Consonant'). % Lo  [19] TAI LE LETTER KA..TAI LE LETTER NA
unicode_indic_syllabic_category(0x1980, 0x19AB, 'Consonant'). % Lo  [44] NEW TAI LUE LETTER HIGH QA..NEW TAI LUE LETTER LOW SUA
unicode_indic_syllabic_category(0x1A00, 0x1A16, 'Consonant'). % Lo  [23] BUGINESE LETTER KA..BUGINESE LETTER HA
unicode_indic_syllabic_category(0x1A20, 0x1A4C, 'Consonant'). % Lo  [45] TAI THAM LETTER HIGH KA..TAI THAM LETTER LOW HA
unicode_indic_syllabic_category(0x1A53, 0x1A54, 'Consonant'). % Lo   [2] TAI THAM LETTER LAE..TAI THAM LETTER GREAT SA
unicode_indic_syllabic_category(0x1B13, 0x1B33, 'Consonant'). % Lo  [33] BALINESE LETTER KA..BALINESE LETTER HA
unicode_indic_syllabic_category(0x1B45, 0x1B4B, 'Consonant'). % Lo   [7] BALINESE LETTER KAF SASAK..BALINESE LETTER ASYURA SASAK
unicode_indic_syllabic_category(0x1B8A, 0x1BA0, 'Consonant'). % Lo  [23] SUNDANESE LETTER KA..SUNDANESE LETTER HA
unicode_indic_syllabic_category(0x1BAE, 0x1BAF, 'Consonant'). % Lo   [2] SUNDANESE LETTER KHA..SUNDANESE LETTER SYA
unicode_indic_syllabic_category(0x1BBB, 0x1BBD, 'Consonant'). % Lo   [3] SUNDANESE LETTER REU..SUNDANESE LETTER BHA
unicode_indic_syllabic_category(0x1BC0, 0x1BE3, 'Consonant'). % Lo  [36] BATAK LETTER A..BATAK LETTER MBA
unicode_indic_syllabic_category(0x1C00, 0x1C23, 'Consonant'). % Lo  [36] LEPCHA LETTER KA..LEPCHA LETTER A
unicode_indic_syllabic_category(0x1C4D, 0x1C4F, 'Consonant'). % Lo   [3] LEPCHA LETTER TTA..LEPCHA LETTER DDA
unicode_indic_syllabic_category(0xA807, 0xA80A, 'Consonant'). % Lo   [4] SYLOTI NAGRI LETTER KO..SYLOTI NAGRI LETTER GHO
unicode_indic_syllabic_category(0xA80C, 0xA822, 'Consonant'). % Lo  [23] SYLOTI NAGRI LETTER CO..SYLOTI NAGRI LETTER HO
unicode_indic_syllabic_category(0xA840, 0xA85D, 'Consonant'). % Lo  [30] PHAGS-PA LETTER KA..PHAGS-PA LETTER A
unicode_indic_syllabic_category(0xA862, 0xA865, 'Consonant'). % Lo   [4] PHAGS-PA LETTER QA..PHAGS-PA LETTER GGA
unicode_indic_syllabic_category(0xA869, 0xA870, 'Consonant'). % Lo   [8] PHAGS-PA LETTER TTA..PHAGS-PA LETTER ASPIRATED FA
unicode_indic_syllabic_category(0xA872, 0xA872, 'Consonant'). % Lo       PHAGS-PA SUPERFIXED LETTER RA
unicode_indic_syllabic_category(0xA892, 0xA8B3, 'Consonant'). % Lo  [34] SAURASHTRA LETTER KA..SAURASHTRA LETTER LLA
unicode_indic_syllabic_category(0xA90A, 0xA921, 'Consonant'). % Lo  [24] KAYAH LI LETTER KA..KAYAH LI LETTER CA
unicode_indic_syllabic_category(0xA930, 0xA946, 'Consonant'). % Lo  [23] REJANG LETTER KA..REJANG LETTER A
unicode_indic_syllabic_category(0xA989, 0xA98B, 'Consonant'). % Lo   [3] JAVANESE LETTER PA CEREK..JAVANESE LETTER NGA LELET RASWADI
unicode_indic_syllabic_category(0xA98F, 0xA9B2, 'Consonant'). % Lo  [34] JAVANESE LETTER KA..JAVANESE LETTER HA
unicode_indic_syllabic_category(0xAA06, 0xAA28, 'Consonant'). % Lo  [35] CHAM LETTER KA..CHAM LETTER HA
unicode_indic_syllabic_category(0xAA60, 0xAA6F, 'Consonant'). % Lo  [16] MYANMAR LETTER KHAMTI GA..MYANMAR LETTER KHAMTI FA
unicode_indic_syllabic_category(0xAA71, 0xAA73, 'Consonant'). % Lo   [3] MYANMAR LETTER KHAMTI XA..MYAMNAR LETTER KHAMTI RA
unicode_indic_syllabic_category(0xAA7A, 0xAA7A, 'Consonant'). % Lo       MYANMAR LETTER AITON RA
unicode_indic_syllabic_category(0xAA80, 0xAAAF, 'Consonant'). % Lo  [48] TAI VIET LETTER LOW KO..TAI VIET LETTER HIGH O
unicode_indic_syllabic_category(0xAAE2, 0xAAEA, 'Consonant'). % Lo   [9] MEETEI MAYEK LETTER CHA..MEETEI MAYEK LETTER SSA
unicode_indic_syllabic_category(0xABC0, 0xABCD, 'Consonant'). % Lo  [14] MEETEI MAYEK LETTER KOK..MEETEI MAYEK LETTER HUK
unicode_indic_syllabic_category(0xABD0, 0xABD0, 'Consonant'). % Lo       MEETEI MAYEK LETTER PHAM
unicode_indic_syllabic_category(0xABD2, 0xABDA, 'Consonant'). % Lo   [9] MEETEI MAYEK LETTER GOK..MEETEI MAYEK LETTER BHAM
unicode_indic_syllabic_category(0x10A00, 0x10A00, 'Consonant'). % Lo       KHAROSHTHI LETTER A
unicode_indic_syllabic_category(0x10A10, 0x10A13, 'Consonant'). % Lo   [4] KHAROSHTHI LETTER KA..KHAROSHTHI LETTER GHA
unicode_indic_syllabic_category(0x10A15, 0x10A17, 'Consonant'). % Lo   [3] KHAROSHTHI LETTER CA..KHAROSHTHI LETTER JA
unicode_indic_syllabic_category(0x10A19, 0x10A33, 'Consonant'). % Lo  [27] KHAROSHTHI LETTER NYA..KHAROSHTHI LETTER TTTHA
unicode_indic_syllabic_category(0x11013, 0x11037, 'Consonant'). % Lo  [37] BRAHMI LETTER KA..BRAHMI LETTER OLD TAMIL NNNA
unicode_indic_syllabic_category(0x1108D, 0x110AF, 'Consonant'). % Lo  [35] KAITHI LETTER KA..KAITHI LETTER HA
unicode_indic_syllabic_category(0x11107, 0x11126, 'Consonant'). % Lo  [32] CHAKMA LETTER KAA..CHAKMA LETTER HAA
unicode_indic_syllabic_category(0x11191, 0x111B2, 'Consonant'). % Lo  [34] SHARADA LETTER KA..SHARADA LETTER HA
unicode_indic_syllabic_category(0x1168A, 0x116AA, 'Consonant'). % Lo  [34] TAKRI LETTER KA..TAKRI LETTER RRA

% ================================================

% Indic_Syllabic_Category=Consonant_Dead

% Dead Consonant (special consonant with killed vowel)

% [Not derivable]

unicode_indic_syllabic_category(0x09CE, 0x09CE, 'Consonant_Dead'). % Lo       BENGALI LETTER KHANDA TA
unicode_indic_syllabic_category(0x0D7A, 0x0D7F, 'Consonant_Dead'). % Lo   [6] MALAYALAM LETTER CHILLU NN..MALAYALAM LETTER CHILLU K

% ================================================

% Indic_Syllabic_Category=Consonant_Repha

% Repha Form of RA (reanalyzed in some scripts)

% [Not derivable]

unicode_indic_syllabic_category(0x0D4E, 0x0D4E, 'Consonant_Repha'). % Lo       MALAYALAM LETTER DOT REPH
unicode_indic_syllabic_category(0x17CC, 0x17CC, 'Consonant_Repha'). % Mn       KHMER SIGN ROBAT
unicode_indic_syllabic_category(0x1B03, 0x1B03, 'Consonant_Repha'). % Mn       BALINESE SIGN SURANG
unicode_indic_syllabic_category(0x1B81, 0x1B81, 'Consonant_Repha'). % Mn       SUNDANESE SIGN PANGLAYAR
unicode_indic_syllabic_category(0xA982, 0xA982, 'Consonant_Repha'). % Mn       JAVANESE SIGN LAYAR

% ================================================

% Indic_Syllabic_Category=Consonant_Subjoined

% Subjoined Consonant (C2 form subtending a base consonant in Tibetan, etc.)

% [Not derivable]

unicode_indic_syllabic_category(0x0F8D, 0x0F97, 'Consonant_Subjoined'). % Mn  [11] TIBETAN SUBJOINED SIGN LCE TSA CAN..TIBETAN SUBJOINED LETTER JA
unicode_indic_syllabic_category(0x0F99, 0x0FBC, 'Consonant_Subjoined'). % Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
unicode_indic_syllabic_category(0x1929, 0x192B, 'Consonant_Subjoined'). % Mc   [3] LIMBU SUBJOINED LETTER YA..LIMBU SUBJOINED LETTER WA
unicode_indic_syllabic_category(0x1BA1, 0x1BA1, 'Consonant_Subjoined'). % Mc       SUNDANESE CONSONANT SIGN PAMINGKAL
unicode_indic_syllabic_category(0x1BA2, 0x1BA3, 'Consonant_Subjoined'). % Mn   [2] SUNDANESE CONSONANT SIGN PANYAKRA..SUNDANESE CONSONANT SIGN PANYIKU
unicode_indic_syllabic_category(0x1BAC, 0x1BAD, 'Consonant_Subjoined'). % Mc   [2] SUNDANESE CONSONANT SIGN PASANGAN MA..SUNDANESE CONSONANT SIGN PASANGAN WA
unicode_indic_syllabic_category(0x1C24, 0x1C25, 'Consonant_Subjoined'). % Mc   [2] LEPCHA SUBJOINED LETTER YA..LEPCHA SUBJOINED LETTER RA
unicode_indic_syllabic_category(0xA867, 0xA868, 'Consonant_Subjoined'). % Lo   [2] PHAGS-PA SUBJOINED LETTER WA..PHAGS-PA SUBJOINED LETTER YA
unicode_indic_syllabic_category(0xA871, 0xA871, 'Consonant_Subjoined'). % Lo       PHAGS-PA SUBJOINED LETTER RA
unicode_indic_syllabic_category(0xA9BD, 0xA9BD, 'Consonant_Subjoined'). % Mc       JAVANESE CONSONANT SIGN KERET

% ================================================

% Indic_Syllabic_Category=Consonant_Medial

% Medial Consonant (medial liquid, occurring in clusters)

% [Not derivable]

unicode_indic_syllabic_category(0x0A75, 0x0A75, 'Consonant_Medial'). % Mn       GURMUKHI SIGN YAKASH
unicode_indic_syllabic_category(0x0EBC, 0x0EBC, 'Consonant_Medial'). % Mn       LAO SEMIVOWEL SIGN LO
unicode_indic_syllabic_category(0x0EBD, 0x0EBD, 'Consonant_Medial'). % Lo       LAO SEMIVOWEL SIGN NYO
unicode_indic_syllabic_category(0x103B, 0x103C, 'Consonant_Medial'). % Mc   [2] MYANMAR CONSONANT SIGN MEDIAL YA..MYANMAR CONSONANT SIGN MEDIAL RA
unicode_indic_syllabic_category(0x103D, 0x103E, 'Consonant_Medial'). % Mn   [2] MYANMAR CONSONANT SIGN MEDIAL WA..MYANMAR CONSONANT SIGN MEDIAL HA
unicode_indic_syllabic_category(0x105E, 0x1060, 'Consonant_Medial'). % Mn   [3] MYANMAR CONSONANT SIGN MON MEDIAL NA..MYANMAR CONSONANT SIGN MON MEDIAL LA
unicode_indic_syllabic_category(0x1082, 0x1082, 'Consonant_Medial'). % Mn       MYANMAR CONSONANT SIGN SHAN MEDIAL WA
unicode_indic_syllabic_category(0x1A55, 0x1A55, 'Consonant_Medial'). % Mc       TAI THAM CONSONANT SIGN MEDIAL RA
unicode_indic_syllabic_category(0x1A56, 0x1A56, 'Consonant_Medial'). % Mn       TAI THAM CONSONANT SIGN MEDIAL LA
unicode_indic_syllabic_category(0xA9BE, 0xA9BF, 'Consonant_Medial'). % Mc       JAVANESE CONSONANT SIGN PENGKAL..JAVANESE CONSONANT SIGN CAKRA
unicode_indic_syllabic_category(0xAA33, 0xAA34, 'Consonant_Medial'). % Mc   [2] CHAM CONSONANT SIGN YA..CHAM CONSONANT SIGN RA
unicode_indic_syllabic_category(0xAA35, 0xAA36, 'Consonant_Medial'). % Mn   [2] CHAM CONSONANT SIGN LA..CHAM CONSONANT SIGN WA

% ================================================

% Indic_Syllabic_Category=Consonant_Final

% Final Consonant (special final forms which do not take vowels)

% [Not derivable]

unicode_indic_syllabic_category(0x1930, 0x1931, 'Consonant_Final'). % Mc   [2] LIMBU SMALL LETTER KA..LIMBU SMALL LETTER NGA
unicode_indic_syllabic_category(0x1933, 0x1938, 'Consonant_Final'). % Mc   [6] LIMBU SMALL LETTER TA..LIMBU SMALL LETTER LA
unicode_indic_syllabic_category(0x19C1, 0x19C7, 'Consonant_Final'). % Lo   [7] NEW TAI LUE LETTER FINAL V..NEW TAI LUE LETTER FINAL B
unicode_indic_syllabic_category(0x1A57, 0x1A57, 'Consonant_Final'). % Mc       TAI THAM CONSONANT SIGN LA TANG LAI
unicode_indic_syllabic_category(0x1A58, 0x1A5E, 'Consonant_Final'). % Mn   [7] TAI THAM SIGN MAI KANG LAI..TAI THAM CONSONANT SIGN SA
unicode_indic_syllabic_category(0x1BBE, 0x1BBF, 'Consonant_Final'). % Lo   [2] SUNDANESE LETTER FINAL K..SUNDANESE LETTER FINAL M
unicode_indic_syllabic_category(0x1BF0, 0x1BF1, 'Consonant_Final'). % Mn   [2] BATAK CONSONANT SIGN NG..BATAK CONSONANT SIGN H
unicode_indic_syllabic_category(0x1C2D, 0x1C33, 'Consonant_Final'). % Mn   [7] LEPCHA CONSONANT SIGN K..LEPCHA CONSONANT SIGN T
unicode_indic_syllabic_category(0xA8B4, 0xA8B4, 'Consonant_Final'). % Mc       SAURASHTRA CONSONANT SIGN HAARU
unicode_indic_syllabic_category(0xA94F, 0xA951, 'Consonant_Final'). % Mn   [3] REJANG CONSONANT SIGN NG..REJANG CONSONANT SIGN R
unicode_indic_syllabic_category(0xA952, 0xA952, 'Consonant_Final'). % Mc       REJANG CONSONANT SIGN H
unicode_indic_syllabic_category(0xAA40, 0xAA42, 'Consonant_Final'). % Lo   [3] CHAM LETTER FINAL K..CHAM LETTER FINAL NG
unicode_indic_syllabic_category(0xAA43, 0xAA43, 'Consonant_Final'). % Mn       CHAM CONSONANT SIGN FINAL NG
unicode_indic_syllabic_category(0xAA44, 0xAA4B, 'Consonant_Final'). % Lo   [8] CHAM LETTER FINAL CH..CHAM LETTER FINAL SS
unicode_indic_syllabic_category(0xAA4C, 0xAA4C, 'Consonant_Final'). % Mn       CHAM CONSONANT SIGN FINAL M
unicode_indic_syllabic_category(0xAA4D, 0xAA4D, 'Consonant_Final'). % Mc       CHAM CONSONANT SIGN FINAL H
unicode_indic_syllabic_category(0xABDB, 0xABE2, 'Consonant_Final'). % Lo   [8] MEETEI MAYEK LETTER KOK LONSUM..MEETEI MAYEK LETTER I LONSUM

% ================================================

% Indic_Syllabic_Category=Consonant_Head_Letter

% Head Letter (Tibetan)

% [Not derivable]

unicode_indic_syllabic_category(0x0F88, 0x0F8C, 'Consonant_Head_Letter'). % Lo   [5] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN INVERTED MCHU CAN

% ================================================

% Indic_Syllabic_Category=Modifying_Letter

% Reanalyzed letters not participating in the abugida structure, but
% serving to modify the sound of an adjacent vowel or consonant.
% Note that this is not the same as General_Category=Modifier_Letter.

% [Not derivable]

unicode_indic_syllabic_category(0x0B83, 0x0B83, 'Modifying_Letter'). % Lo       TAMIL SIGN VISARGA (aytham)

% ================================================

% Indic_Syllabic_Category=Tone_Letter

% Tone Letter (spacing lexical tone mark with status as a letter)

% [Not derivable]

unicode_indic_syllabic_category(0x1970, 0x1974, 'Tone_Letter'). % Lo   [5] TAI LE LETTER TONE-2..TAI LE LETTER TONE-6
unicode_indic_syllabic_category(0xAAC0, 0xAAC0, 'Tone_Letter'). % Lo       TAI VIET TONE MAI NUENG
unicode_indic_syllabic_category(0xAAC2, 0xAAC2, 'Tone_Letter'). % Lo       TAI VIET TONE MAI SONG

% ================================================

% Indic_Syllabic_Category=Tone_Mark

% Tone Mark (nonspacing or spacing lexical tone mark)
% Excludes Vedic tone marks, which are more akin to cantillation marks.

% [Not derivable]

unicode_indic_syllabic_category(0x0E48, 0x0E4B, 'Tone_Mark'). % Mn   [4] THAI CHARACTER MAI EK..THAI CHARACTER MAI CHATTAWA
unicode_indic_syllabic_category(0x0EC8, 0x0ECB, 'Tone_Mark'). % Mn   [4] LAO TONE MAI EK..LAO TONE MAI CATAWA
unicode_indic_syllabic_category(0x1037, 0x1037, 'Tone_Mark'). % Mn       MYANMAR SIGN DOT BELOW
unicode_indic_syllabic_category(0x1063, 0x1064, 'Tone_Mark'). % Mc   [2] MYANMAR TONE MARK SGAW KAREN HATHI..MYANMAR TONE MARK SGAW KAREN KE PHO
unicode_indic_syllabic_category(0x1069, 0x106D, 'Tone_Mark'). % Mc   [5] MYANMAR SIGN WESTERN PWO KAREN TONE-1..MYANMAR SIGN WESTERN PWO KAREN TONE-5
unicode_indic_syllabic_category(0x1087, 0x108C, 'Tone_Mark'). % Mc   [6] MYANMAR SIGN SHAN TONE-2..MYANMAR SIGN SHAN COUNCIL TONE-3
unicode_indic_syllabic_category(0x108D, 0x108D, 'Tone_Mark'). % Mn       MYANMAR SIGN SHAN COUNCIL EMPHATIC TONE
unicode_indic_syllabic_category(0x108F, 0x108F, 'Tone_Mark'). % Mc       MYANMAR SIGN RUMAI PALAUNG TONE-5
unicode_indic_syllabic_category(0x109A, 0x109B, 'Tone_Mark'). % Mc   [2] MYANMAR SIGN KHAMTI TONE-1..MYANMAR SIGN KHAMTI TONE-3
unicode_indic_syllabic_category(0x19C8, 0x19C9, 'Tone_Mark'). % Mc   [2] NEW TAI LUE TONE MARK-1..NEW TAI LUE TONE MARK-2
unicode_indic_syllabic_category(0x1A75, 0x1A79, 'Tone_Mark'). % Mn   [5] TAI THAM SIGN TONE-1..TAI THAM SIGN KHUEN TONE-5
unicode_indic_syllabic_category(0xA92B, 0xA92D, 'Tone_Mark'). % Mn   [3] KAYAH LI TONE PLOPHU..KAYAH LI TONE CALYA PLOPHU
unicode_indic_syllabic_category(0xAA7B, 0xAA7B, 'Tone_Mark'). % Mc       MYANMAR SIGN PAO KAREN TONE
unicode_indic_syllabic_category(0xAABF, 0xAABF, 'Tone_Mark'). % Mn       TAI VIET TONE MAI EK
unicode_indic_syllabic_category(0xAAC1, 0xAAC1, 'Tone_Mark'). % Mn       TAI VIET TONE MAI THO
unicode_indic_syllabic_category(0xABEC, 0xABEC, 'Tone_Mark'). % Mc       MEETEI MAYEK LUM IYEK

% ================================================

% Indic_Syllabic_Category=Register_Shifter

% Register Shifter (shifts register for consonants, akin to a tone mark)

% [Not derivable]

unicode_indic_syllabic_category(0x17C9, 0x17CA, 'Register_Shifter'). % Mn   [2] KHMER SIGN MUUSIKATOAN..KHMER SIGN TRIISAP

% EOF
