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
# IndicMatraCategory-6.1.0.txt
# Date: 2011-08-31, 23:50:00 GMT [KW]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see UAX #44.
#
# This file defines the following provisional property:
#
#    Indic_Matra_Category     enumerated property
#
# NB: Provisional properties and data files have no associated stability 
# guarantees. They are provided in part to determine the possible usefulness
# of a property or other data and to encourage analysis and further investigation
# which may result in their improvement. Provisional properties and
# data files may change arbitrarily, or may even be removed in a future version of the
# Unicode Character Database, if they prove not to be useful.
#
# Scope: This provisional property is aimed at the problem of
# the specification of syllabic structure for Indic scripts.
# Because dependent vowels (and visible viramas, where they occur)
# are placed in notional slots around the consonant (or consonant
# cluster) core of an Indic syllable, there may be cooccurrence
# constraints or other interactions. Also, it may be desirable,
# in cases where more than one dependent vowel may occur in
# sequence, as for example, in a top slot and a bottom slot, to
# specify preferred orders for spelling. As such, this property
# is designed primarily to supplement the Indic_Syllabic_Category
# property.
#
# Note that this provisional property is *not* intended as
# a prescriptive property regarding display or font design,
# for a number of reasons. Good font design requires information
# that is outside the context of a character encoding standard,
# and is best handled in other venues. For Indic dependent
# vowels, in particular:
#
#   1. Matra placement may vary somewhat based on typeface design.
#   2. Matra placement, even within a single script, may vary
#      somewhat according to historic period or local conventions.
#   3. Matra placement may be changed by explicit orthographic reform
#      decisions.
#   4. Matras may ligate in various ways with a consonant (or even
#      other elements of a syllable) instead of occurring in a
#      discrete location.
#   4. Matra display may be contextually determined. This is
#      notable, for example, in the Tamil script, where the shape
#      and placement of -u and -uu vowels depends strongly on
#      which consonant they adjoin. 
#
# Format:
#    Field 0  Code Point or Code Point Range
#    Field 1  Indic_Matra_Category
#
# A comment field shows General_Category property values and character names.
#
# The scripts assessed as containing dependent vowels or viramas in the
# structural sense used for the Indic_Matra_Category are:
#
# Devanagari, Bengali, Gurmukhi, Gujarati, Oriya, Tamil, Telugu,
# Kannada, Malayalam, Sinhala, Thai, Lao, Tibetan, Myanmar,
# Tagalog, Hanunoo, Buhid, Tagbanwa, Khmer, Limbu, New Tai Lue,
# Buginese, Tai Tham, Balinese, Sundanese, Batak, Lepcha,
# Syloti Nagri, Saurashtra, Rejang, Javanese, Cham, Tai Viet,
# Meetei Mayek, Kharoshthi, Brahmi, Kaithi, Chakma, Sharada, Takri
#
# All characters for all other scripts not in that list
# take the default value for this property.
#
# See IndicSyllabicCategory.txt for a slightly more extended
# list of Indic scripts, including those which do not have
# dependent vowel characters. Currently, those additional
# Indic scripts without dependent vowel characters are
# Tai Le, Phags-pa, and Kayah Li.
#

# ================================================

# Property: Indic_Matra_Category
#
#  All code points not explicitly listed for Indic_Matra_Category
#  have the value NA (Not_Applicable).
#
# @missing: 0000..10FFFF; NA

# Only Indic dependent vowels (Indic_Syllabic_Category=Vowel_Dependent)
# and viramas (Indic_Syllabic_Category=Virama)
# have a non-default value for this property.

# ------------------------------------------------
*/

unicode_indic_matra_category(CodePoint, Category) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_indic_matra_category(CodePointStart, CodePointEnd, Category),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_indic_matra_category(CodePoint, _, CodePointCategory) ->
		Category = CodePointCategory
	;	% look for a code point range that includes the given code point
		unicode_indic_matra_category(CodePointStart, CodePointEnd, CodePointCategory),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		Category = CodePointCategory
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Category = 'Not_Applicable'
	).

% Indic_Matra_Category=Right

unicode_indic_matra_category(0x093B, 0x093B, 'Right'). % Mc       DEVANAGARI VOWEL SIGN OOE
unicode_indic_matra_category(0x093E, 0x093E, 'Right'). % Mc       DEVANAGARI VOWEL SIGN AA
unicode_indic_matra_category(0x0940, 0x0940, 'Right'). % Mc       DEVANAGARI VOWEL SIGN II
unicode_indic_matra_category(0x0949, 0x094C, 'Right'). % Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
unicode_indic_matra_category(0x094F, 0x094F, 'Right'). % Mc       DEVANAGARI VOWEL SIGN AW
unicode_indic_matra_category(0x09BE, 0x09BE, 'Right'). % Mc       BENGALI VOWEL SIGN AA
unicode_indic_matra_category(0x09C0, 0x09C0, 'Right'). % Mc       BENGALI VOWEL SIGN II
unicode_indic_matra_category(0x09D7, 0x09D7, 'Right'). % Mc       BENGALI AU LENGTH MARK
unicode_indic_matra_category(0x0A3E, 0x0A3E, 'Right'). % Mc       GURMUKHI VOWEL SIGN AA
unicode_indic_matra_category(0x0A40, 0x0A40, 'Right'). % Mc       GURMUKHI VOWEL SIGN II
unicode_indic_matra_category(0x0ABE, 0x0ABE, 'Right'). % Mc       GUJARATI VOWEL SIGN AA
unicode_indic_matra_category(0x0AC0, 0x0AC0, 'Right'). % Mc       GUJARATI VOWEL SIGN II
unicode_indic_matra_category(0x0ACB, 0x0ACC, 'Right'). % Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
unicode_indic_matra_category(0x0B3E, 0x0B3E, 'Right'). % Mc       ORIYA VOWEL SIGN AA
unicode_indic_matra_category(0x0B40, 0x0B40, 'Right'). % Mc       ORIYA VOWEL SIGN II
unicode_indic_matra_category(0x0BBE, 0x0BBF, 'Right'). % Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
unicode_indic_matra_category(0x0BD7, 0x0BD7, 'Right'). % Mc       TAMIL AU LENGTH MARK
unicode_indic_matra_category(0x0C41, 0x0C44, 'Right'). % Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x0CBE, 0x0CBE, 'Right'). % Mc       KANNADA VOWEL SIGN AA
unicode_indic_matra_category(0x0CC1, 0x0CC4, 'Right'). % Mc   [4] KANNADA VOWEL SIGN U..KANNADA VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x0CD5, 0x0CD6, 'Right'). % Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
unicode_indic_matra_category(0x0D3E, 0x0D40, 'Right'). % Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
unicode_indic_matra_category(0x0D41, 0x0D42, 'Right'). % Mn   [2] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN UU
% Note: U and UU form complex ligatures with consonants in older Malayalam orthography
unicode_indic_matra_category(0x0D57, 0x0D57, 'Right'). % Mc       MALAYALAM AU LENGTH MARK
unicode_indic_matra_category(0x0DCF, 0x0DD1, 'Right'). % Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
unicode_indic_matra_category(0x0DD8, 0x0DD8, 'Right'). % Mc       SINHALA VOWEL SIGN GAETTA-PILLA
unicode_indic_matra_category(0x0DDF, 0x0DDF, 'Right'). % Mc       SINHALA VOWEL SIGN GAYANUKITTA
unicode_indic_matra_category(0x0DF2, 0x0DF3, 'Right'). % Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
unicode_indic_matra_category(0x0E30, 0x0E30, 'Right'). % Lo       THAI CHARACTER SARA A
unicode_indic_matra_category(0x0E32, 0x0E33, 'Right'). % Lo   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
unicode_indic_matra_category(0x0E45, 0x0E45, 'Right'). % Lo       THAI CHARACTER LAKKHANGYAO
unicode_indic_matra_category(0x0EB0, 0x0EB0, 'Right'). % Lo       LAO VOWEL SIGN A
unicode_indic_matra_category(0x0EB2, 0x0EB3, 'Right'). % Lo   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
unicode_indic_matra_category(0x102B, 0x102C, 'Right'). % Mc   [2] MYANMAR VOWEL SIGN TALL AA..MYANMAR VOWEL SIGN AA
unicode_indic_matra_category(0x1056, 0x1057, 'Right'). % Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x1062, 0x1062, 'Right'). % Mc       MYANMAR VOWEL SIGN SGAW KAREN EU
unicode_indic_matra_category(0x1067, 0x1068, 'Right'). % Mc   [2] MYANMAR VOWEL SIGN WESTERN PWO KAREN EU..MYANMAR VOWEL SIGN WESTERN PWO KAREN UE
unicode_indic_matra_category(0x1083, 0x1083, 'Right'). % Mc       MYANMAR VOWEL SIGN SHAN AA
unicode_indic_matra_category(0x109C, 0x109C, 'Right'). % Mc       MYANMAR VOWEL SIGN AITON A
unicode_indic_matra_category(0x17B6, 0x17B6, 'Right'). % Mc       KHMER VOWEL SIGN AA
unicode_indic_matra_category(0x17C8, 0x17C8, 'Right'). % Mc       KHMER SIGN YUUKALEAPINTU
unicode_indic_matra_category(0x1923, 0x1924, 'Right'). % Mc   [2] LIMBU VOWEL SIGN EE..LIMBU VOWEL SIGN AI
unicode_indic_matra_category(0x19B0, 0x19B4, 'Right'). % Mc   [5] NEW TAI LUE VOWEL SIGN VOWEL SHORTENER..NEW TAI LUE VOWEL SIGN UU
unicode_indic_matra_category(0x19B8, 0x19B9, 'Right'). % Mc   [2] NEW TAI LUE VOWEL SIGN OA..NEW TAI LUE VOWEL SIGN UE
unicode_indic_matra_category(0x19BB, 0x19C0, 'Right'). % Mc   [6] NEW TAI LUE VOWEL SIGN AAY..NEW TAI LUE VOWEL SIGN IY
unicode_indic_matra_category(0x1A1A, 0x1A1A, 'Right'). % Mc       BUGINESE VOWEL SIGN O
unicode_indic_matra_category(0x1A61, 0x1A61, 'Right'). % Mc       TAI THAM VOWEL SIGN A
unicode_indic_matra_category(0x1A63, 0x1A64, 'Right'). % Mc   [2] TAI THAM VOWEL SIGN AA..TAI THAM VOWEL SIGN TALL AA
unicode_indic_matra_category(0x1A6D, 0x1A6D, 'Right'). % Mc       TAI THAM VOWEL SIGN OY
unicode_indic_matra_category(0x1B35, 0x1B35, 'Right'). % Mc       BALINESE VOWEL SIGN TEDUNG
unicode_indic_matra_category(0x1B44, 0x1B44, 'Right'). % Mc       BALINESE ADEG ADEG
unicode_indic_matra_category(0x1BA7, 0x1BA7, 'Right'). % Mc       SUNDANESE VOWEL SIGN PANOLONG
unicode_indic_matra_category(0x1BAA, 0x1BAA, 'Right'). % Mc       SUNDANESE SIGN PAMAAEH
unicode_indic_matra_category(0x1BF2, 0x1BF3, 'Right'). % Mc   [2] BATAK PANGOLAT..BATAK PANONGONAN
unicode_indic_matra_category(0x1C26, 0x1C26, 'Right'). % Mc       LEPCHA VOWEL SIGN AA
unicode_indic_matra_category(0x1C2A, 0x1C2B, 'Right'). % Mc   [2] LEPCHA VOWEL SIGN U..LEPCHA VOWEL SIGN UU
unicode_indic_matra_category(0xA823, 0xA824, 'Right'). % Mc   [2] SYLOTI NAGRI VOWEL SIGN A..SYLOTI NAGRI VOWEL SIGN I
unicode_indic_matra_category(0xA827, 0xA827, 'Right'). % Mc       SYLOTI NAGRI VOWEL SIGN OO
unicode_indic_matra_category(0xA8B5, 0xA8C3, 'Right'). % Mc  [15] SAURASHTRA VOWEL SIGN AA..SAURASHTRA VOWEL SIGN AU
unicode_indic_matra_category(0xA953, 0xA953, 'Right'). % Mc       REJANG VIRAMA
unicode_indic_matra_category(0xA9B4, 0xA9B5, 'Right'). % Mc   [2] JAVANESE VOWEL SIGN TARUNG..JAVANESE VOWEL SIGN TOLONG
unicode_indic_matra_category(0xAAB1, 0xAAB1, 'Right'). % Lo       TAI VIET VOWEL AA
unicode_indic_matra_category(0xAABA, 0xAABA, 'Right'). % Lo       TAI VIET VOWEL UA
unicode_indic_matra_category(0xAABD, 0xAABD, 'Right'). % Lo       TAI VIET VOWEL AN
unicode_indic_matra_category(0xAAEF, 0xAAEF, 'Right'). % Mc       MEETEI MAYEK VOWEL SIGN AAU
unicode_indic_matra_category(0xABE3, 0xABE4, 'Right'). % Mc   [2] MEETEI MAYEK VOWEL SIGN ONAP..MEETEI MAYEK VOWEL SIGN INAP
unicode_indic_matra_category(0xABE6, 0xABE7, 'Right'). % Mc   [2] MEETEI MAYEK VOWEL SIGN YENAP..MEETEI MAYEK VOWEL SIGN SOUNAP
unicode_indic_matra_category(0xABE9, 0xABEA, 'Right'). % Mc   [2] MEETEI MAYEK VOWEL SIGN CHEINAP..MEETEI MAYEK VOWEL SIGN NUNG
unicode_indic_matra_category(0x110B0, 0x110B0, 'Right'). % Mc       KAITHI VOWEL SIGN AA
unicode_indic_matra_category(0x110B2, 0x110B2, 'Right'). % Mc       KAITHI VOWEL SIGN II
unicode_indic_matra_category(0x110B7, 0x110B8, 'Right'). % Mc   [2] KAITHI VOWEL SIGN O..KAITHI VOWEL SIGN AU
unicode_indic_matra_category(0x111B3, 0x111B3, 'Right'). % Mc       SHARADA VOWEL SIGN AA
unicode_indic_matra_category(0x111B5, 0x111B5, 'Right'). % Mc       SHARADA VOWEL SIGN II
unicode_indic_matra_category(0x111C0, 0x111C0, 'Right'). % Mc       SHARADA SIGN VIRAMA
unicode_indic_matra_category(0x116AF, 0x116AF, 'Right'). % Mc       TAKRI VOWEL SIGN II

% Indic_Matra_Category=Left

unicode_indic_matra_category(0x093F, 0x093F, 'Left'). % Mc       DEVANAGARI VOWEL SIGN I
unicode_indic_matra_category(0x094E, 0x094E, 'Left'). % Mc       DEVANAGARI VOWEL SIGN PRISHTHAMATRA E
unicode_indic_matra_category(0x09BF, 0x09BF, 'Left'). % Mc       BENGALI VOWEL SIGN I
unicode_indic_matra_category(0x09C7, 0x09C8, 'Left'). % Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
unicode_indic_matra_category(0x0A3F, 0x0A3F, 'Left'). % Mc       GURMUKHI VOWEL SIGN I
unicode_indic_matra_category(0x0ABF, 0x0ABF, 'Left'). % Mc       GUJARATI VOWEL SIGN I
unicode_indic_matra_category(0x0B47, 0x0B47, 'Left'). % Mc       ORIYA VOWEL SIGN E
unicode_indic_matra_category(0x0BC6, 0x0BC8, 'Left'). % Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
unicode_indic_matra_category(0x0D46, 0x0D48, 'Left'). % Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
unicode_indic_matra_category(0x0DD9, 0x0DD9, 'Left'). % Mc       SINHALA VOWEL SIGN KOMBUVA
unicode_indic_matra_category(0x0DDB, 0x0DDB, 'Left'). % Mc       SINHALA VOWEL SIGN KOMBU DEKA
unicode_indic_matra_category(0x1031, 0x1031, 'Left'). % Mc       MYANMAR VOWEL SIGN E
unicode_indic_matra_category(0x1084, 0x1084, 'Left'). % Mc       MYANMAR VOWEL SIGN SHAN E
unicode_indic_matra_category(0x17C1, 0x17C3, 'Left'). % Mc   [3] KHMER VOWEL SIGN E..KHMER VOWEL SIGN AI
unicode_indic_matra_category(0x19B5, 0x19B7, 'Left'). % Mc   [3] NEW TAI LUE VOWEL SIGN E..NEW TAI LUE VOWEL SIGN O
unicode_indic_matra_category(0x19BA, 0x19BA, 'Left'). % Mc       NEW TAI LUE VOWEL SIGN AY
unicode_indic_matra_category(0x1A19, 0x1A19, 'Left'). % Mc       BUGINESE VOWEL SIGN E
unicode_indic_matra_category(0x1A1B, 0x1A1B, 'Left'). % Mc       BUGINESE VOWEL SIGN AE
unicode_indic_matra_category(0x1A6E, 0x1A72, 'Left'). % Mc   [5] TAI THAM VOWEL SIGN E..TAI THAM VOWEL SIGN THAM AI
unicode_indic_matra_category(0x1B3E, 0x1B3F, 'Left'). % Mc   [2] BALINESE VOWEL SIGN TALING..BALINESE VOWEL SIGN TALING REPA
unicode_indic_matra_category(0x1BA6, 0x1BA6, 'Left'). % Mc       SUNDANESE VOWEL SIGN PANAELAENG
unicode_indic_matra_category(0x1C27, 0x1C28, 'Left'). % Mc   [2] LEPCHA VOWEL SIGN I..LEPCHA VOWEL SIGN O
unicode_indic_matra_category(0xA9BA, 0xA9BB, 'Left'). % Mc   [2] JAVANESE VOWEL SIGN TALING..JAVANESE VOWEL SIGN DIRGA MURE
unicode_indic_matra_category(0xAA2F, 0xAA30, 'Left'). % Mc   [2] CHAM VOWEL SIGN O..CHAM VOWEL SIGN AI
unicode_indic_matra_category(0xAAEB, 0xAAEB, 'Left'). % Mc       MEETEI MAYEK VOWEL SIGN II
unicode_indic_matra_category(0xAAEE, 0xAAEE, 'Left'). % Mc       MEETEI MAYEK VOWEL SIGN AU
unicode_indic_matra_category(0x110B1, 0x110B1, 'Left'). % Mc       KAITHI VOWEL SIGN I
unicode_indic_matra_category(0x1112C, 0x1112C, 'Left'). % Mc       CHAKMA VOWEL SIGN E
unicode_indic_matra_category(0x111B4, 0x111B4, 'Left'). % Mc       SHARADA VOWEL SIGN I
unicode_indic_matra_category(0x116AE, 0x116AE, 'Left'). % Mc       TAKRI VOWEL SIGN I

% Indic_Matra_Category=Visual_Order_Left

% These are dependent vowels that occur to the left of the consonant
% letter in a syllable, but which occur in scripts using the visual order
% model, instead of the logical order model. Because of the different
% model, these left-side vowels occur first in the backing store (before
% the consonant letter) and are not reordered during text rendering.
%
% [Derivation: Logical_Order_Exception=True]

unicode_indic_matra_category(0x0E40, 0x0E44, 'Visual_Order_Left'). % Lo   [5] THAI CHARACTER SARA E..THAI CHARACTER SARA AI MAIMALAI
unicode_indic_matra_category(0x0EC0, 0x0EC4, 'Visual_Order_Left'). % Lo   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
unicode_indic_matra_category(0xAAB5, 0xAAB6, 'Visual_Order_Left'). % Lo   [2] TAI VIET VOWEL E..TAI VIET VOWEL O
unicode_indic_matra_category(0xAAB9, 0xAAB9, 'Visual_Order_Left'). % Lo       TAI VIET VOWEL UEA
unicode_indic_matra_category(0xAABB, 0xAABC, 'Visual_Order_Left'). % Lo   [2] TAI VIET VOWEL AUE..TAI VIET VOWEL AY

% Indic_Matra_Category=Left_And_Right

unicode_indic_matra_category(0x09CB, 0x09CC, 'Left_And_Right'). % Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
unicode_indic_matra_category(0x0B4B, 0x0B4B, 'Left_And_Right'). % Mc       ORIYA VOWEL SIGN O
unicode_indic_matra_category(0x0BCA, 0x0BCC, 'Left_And_Right'). % Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
unicode_indic_matra_category(0x0D4A, 0x0D4C, 'Left_And_Right'). % Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
unicode_indic_matra_category(0x0DDC, 0x0DDE, 'Left_And_Right'). % Mc   [3] SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA..SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
unicode_indic_matra_category(0x17C0, 0x17C0, 'Left_And_Right'). % Mc       KHMER VOWEL SIGN IE
unicode_indic_matra_category(0x17C4, 0x17C5, 'Left_And_Right'). % Mc   [2] KHMER VOWEL SIGN OO..KHMER VOWEL SIGN AU
unicode_indic_matra_category(0x1B40, 0x1B41, 'Left_And_Right'). % Mc   [2] BALINESE VOWEL SIGN TALING TEDUNG..BALINESE VOWEL SIGN TALING REPA TEDUNG

% Indic_Matra_Category=Top

unicode_indic_matra_category(0x093A, 0x093A, 'Top'). % Mn       DEVANAGARI VOWEL SIGN OE
unicode_indic_matra_category(0x0945, 0x0948, 'Top'). % Mn   [4] DEVANAGARI VOWEL SIGN CANDRA E..DEVANAGARI VOWEL SIGN AI
unicode_indic_matra_category(0x0955, 0x0955, 'Top'). % Mn       DEVANAGARI VOWEL SIGN CANDRA LONG E
unicode_indic_matra_category(0x0A47, 0x0A48, 'Top'). % Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
unicode_indic_matra_category(0x0A4B, 0x0A4C, 'Top'). % Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
unicode_indic_matra_category(0x0AC5, 0x0AC5, 'Top'). % Mn       GUJARATI VOWEL SIGN CANDRA E
unicode_indic_matra_category(0x0AC7, 0x0AC8, 'Top'). % Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
unicode_indic_matra_category(0x0B3F, 0x0B3F, 'Top'). % Mn       ORIYA VOWEL SIGN I
unicode_indic_matra_category(0x0B56, 0x0B56, 'Top'). % Mn       ORIYA AI LENGTH MARK
unicode_indic_matra_category(0x0BC0, 0x0BC0, 'Top'). % Mn       TAMIL VOWEL SIGN II
unicode_indic_matra_category(0x0BCD, 0x0BCD, 'Top'). % Mn       TAMIL SIGN VIRAMA
unicode_indic_matra_category(0x0C3E, 0x0C40, 'Top'). % Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
unicode_indic_matra_category(0x0C46, 0x0C47, 'Top'). % Mn   [2] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN EE
unicode_indic_matra_category(0x0C4A, 0x0C4C, 'Top'). % Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
unicode_indic_matra_category(0x0C4D, 0x0C4D, 'Top'). % Mn       TELUGU SIGN VIRAMA
unicode_indic_matra_category(0x0C55, 0x0C55, 'Top'). % Mn       TELUGU LENGTH MARK
unicode_indic_matra_category(0x0CBF, 0x0CBF, 'Top'). % Mn       KANNADA VOWEL SIGN I
unicode_indic_matra_category(0x0CC6, 0x0CC6, 'Top'). % Mn       KANNADA VOWEL SIGN E
unicode_indic_matra_category(0x0CCC, 0x0CCC, 'Top'). % Mn       KANNADA VOWEL SIGN AU
unicode_indic_matra_category(0x0CCD, 0x0CCD, 'Top'). % Mn       KANNADA SIGN VIRAMA
unicode_indic_matra_category(0x0D4D, 0x0D4D, 'Top'). % Mn       MALAYALAM SIGN VIRAMA
unicode_indic_matra_category(0x0DCA, 0x0DCA, 'Top'). % Mn       SINHALA SIGN AL-LAKUNA
unicode_indic_matra_category(0x0DD2, 0x0DD3, 'Top'). % Mn   [2] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN DIGA IS-PILLA
unicode_indic_matra_category(0x0E31, 0x0E31, 'Top'). % Mn       THAI CHARACTER MAI HAN-AKAT
unicode_indic_matra_category(0x0E34, 0x0E37, 'Top'). % Mn   [4] THAI CHARACTER SARA I..THAI CHARACTER SARA UEE
unicode_indic_matra_category(0x0E47, 0x0E47, 'Top'). % Mn       THAI CHARACTER MAITAIKHU
unicode_indic_matra_category(0x0E4E, 0x0E4E, 'Top'). % Mn       THAI CHARACTER YAMAKKAN
unicode_indic_matra_category(0x0EB1, 0x0EB1, 'Top'). % Mn       LAO VOWEL SIGN MAI KAN
unicode_indic_matra_category(0x0EB4, 0x0EB7, 'Top'). % Mn   [4] LAO VOWEL SIGN I..LAO VOWEL SIGN YY
unicode_indic_matra_category(0x0EBB, 0x0EBB, 'Top'). % Mn       LAO VOWEL SIGN MAI KON
unicode_indic_matra_category(0x0F72, 0x0F72, 'Top'). % Mn       TIBETAN VOWEL SIGN I
unicode_indic_matra_category(0x0F7A, 0x0F7D, 'Top'). % Mn   [4] TIBETAN VOWEL SIGN E..TIBETAN VOWEL SIGN OO
unicode_indic_matra_category(0x0F80, 0x0F80, 'Top'). % Mn       TIBETAN VOWEL SIGN REVERSED I
unicode_indic_matra_category(0x102D, 0x102E, 'Top'). % Mn   [2] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN II
unicode_indic_matra_category(0x1032, 0x1035, 'Top'). % Mn   [4] MYANMAR VOWEL SIGN AI..MYANMAR VOWEL SIGN E ABOVE
unicode_indic_matra_category(0x103A, 0x103A, 'Top'). % Mn       MYANMAR SIGN ASAT
unicode_indic_matra_category(0x1071, 0x1074, 'Top'). % Mn   [4] MYANMAR VOWEL SIGN GEBA KAREN I..MYANMAR VOWEL SIGN KAYAH EE
unicode_indic_matra_category(0x1085, 0x1086, 'Top'). % Mn   [2] MYANMAR VOWEL SIGN SHAN E ABOVE..MYANMAR VOWEL SIGN SHAN FINAL Y
unicode_indic_matra_category(0x109D, 0x109D, 'Top'). % Mn       MYANMAR VOWEL SIGN AITON AI
unicode_indic_matra_category(0x1712, 0x1712, 'Top'). % Mn       TAGALOG VOWEL SIGN I
unicode_indic_matra_category(0x1732, 0x1732, 'Top'). % Mn       HANUNOO VOWEL SIGN I
unicode_indic_matra_category(0x1752, 0x1752, 'Top'). % Mn       BUHID VOWEL SIGN I
unicode_indic_matra_category(0x1772, 0x1772, 'Top'). % Mn       TAGBANWA VOWEL SIGN I
unicode_indic_matra_category(0x17B7, 0x17BA, 'Top'). % Mn   [4] KHMER VOWEL SIGN I..KHMER VOWEL SIGN YY
unicode_indic_matra_category(0x17D1, 0x17D1, 'Top'). % Mn       KHMER SIGN VIRIAM
unicode_indic_matra_category(0x1920, 0x1921, 'Top'). % Mn   [2] LIMBU VOWEL SIGN A..LIMBU VOWEL SIGN I
unicode_indic_matra_category(0x1927, 0x1928, 'Top'). % Mn   [2] LIMBU VOWEL SIGN E..LIMBU VOWEL SIGN O
unicode_indic_matra_category(0x1A17, 0x1A17, 'Top'). % Mn       BUGINESE VOWEL SIGN I
unicode_indic_matra_category(0x1A62, 0x1A62, 'Top'). % Mn       TAI THAM VOWEL SIGN MAI SAT
unicode_indic_matra_category(0x1A65, 0x1A68, 'Top'). % Mn   [4] TAI THAM VOWEL SIGN I..TAI THAM VOWEL SIGN UUE
unicode_indic_matra_category(0x1A6B, 0x1A6B, 'Top'). % Mn       TAI THAM VOWEL SIGN O
unicode_indic_matra_category(0x1A73, 0x1A74, 'Top'). % Mn   [2] TAI THAM VOWEL SIGN OA ABOVE..TAI THAM SIGN MAI KANG
unicode_indic_matra_category(0x1B36, 0x1B37, 'Top'). % Mn   [2] BALINESE VOWEL SIGN ULU..BALINESE VOWEL SIGN ULU SARI
unicode_indic_matra_category(0x1B42, 0x1B42, 'Top'). % Mn       BALINESE VOWEL SIGN PEPET
unicode_indic_matra_category(0x1BA4, 0x1BA4, 'Top'). % Mn       SUNDANESE VOWEL SIGN PANGHULU
unicode_indic_matra_category(0x1BA8, 0x1BA9, 'Top'). % Mn   [2] SUNDANESE VOWEL SIGN PAMEPET..SUNDANESE VOWEL SIGN PANEULEUNG
unicode_indic_matra_category(0xA806, 0xA806, 'Top'). % Mn       SYLOTI NAGRI SIGN HASANTA
unicode_indic_matra_category(0xA826, 0xA826, 'Top'). % Mn       SYLOTI NAGRI VOWEL SIGN E
unicode_indic_matra_category(0xA94A, 0xA94A, 'Top'). % Mn       REJANG VOWEL SIGN AI
unicode_indic_matra_category(0xA9B6, 0xA9B7, 'Top'). % Mn   [2] JAVANESE VOWEL SIGN WULU..JAVANESE VOWEL SIGN WULU MELIK
unicode_indic_matra_category(0xA9BC, 0xA9BC, 'Top'). % Mn       JAVANESE VOWEL SIGN PEPET
unicode_indic_matra_category(0xAA29, 0xAA2C, 'Top'). % Mn   [4] CHAM VOWEL SIGN AA..CHAM VOWEL SIGN EI
unicode_indic_matra_category(0xAA2E, 0xAA2E, 'Top'). % Mn       CHAM VOWEL SIGN OE
unicode_indic_matra_category(0xAA31, 0xAA31, 'Top'). % Mn       CHAM VOWEL SIGN AU
unicode_indic_matra_category(0xAAB0, 0xAAB0, 'Top'). % Mn       TAI VIET MAI KANG
unicode_indic_matra_category(0xAAB2, 0xAAB3, 'Top'). % Mn   [2] TAI VIET VOWEL I..TAI VIET VOWEL UE
unicode_indic_matra_category(0xAAB7, 0xAAB8, 'Top'). % Mn   [2] TAI VIET MAI KHIT..TAI VIET VOWEL IA
unicode_indic_matra_category(0xAABE, 0xAABE, 'Top'). % Mn       TAI VIET VOWEL AM
unicode_indic_matra_category(0xAAED, 0xAAED, 'Top'). % Mn       MEETEI MAYEK VOWEL SIGN AAI
unicode_indic_matra_category(0xABE5, 0xABE5, 'Top'). % Mn       MEETEI MAYEK VOWEL SIGN ANAP
unicode_indic_matra_category(0x10A05, 0x10A05, 'Top'). % Mn       KHAROSHTHI VOWEL SIGN E
unicode_indic_matra_category(0x11038, 0x1103B, 'Top'). % Mn   [4] BRAHMI VOWEL SIGN AA..BRAHMI VOWEL SIGN II
unicode_indic_matra_category(0x11042, 0x11045, 'Top'). % Mn   [4] BRAHMI VOWEL SIGN E..BRAHMI VOWEL SIGN AU
unicode_indic_matra_category(0x11046, 0x11046, 'Top'). % Mn       BRAHMI VIRAMA
unicode_indic_matra_category(0x110B5, 0x110B6, 'Top'). % Mn   [2] KAITHI VOWEL SIGN E..KAITHI VOWEL SIGN AI
unicode_indic_matra_category(0x11127, 0x11129, 'Top'). % Mn   [3] CHAKMA VOWEL SIGN A..CHAKMA VOWEL SIGN II
unicode_indic_matra_category(0x1112D, 0x1112D, 'Top'). % Mn       CHAKMA VOWEL SIGN AI
unicode_indic_matra_category(0x11130, 0x11130, 'Top'). % Mn       CHAKMA VOWEL SIGN OI
unicode_indic_matra_category(0x11134, 0x11134, 'Top'). % Mn       CHAKMA MAAYYAA
unicode_indic_matra_category(0x111BC, 0x111BE, 'Top'). % Mn   [3] SHARADA VOWEL SIGN E..SHARADA VOWEL SIGN O
unicode_indic_matra_category(0x116AD, 0x116AD, 'Top'). % Mn       TAKRI VOWEL SIGN AA
unicode_indic_matra_category(0x116B2, 0x116B5, 'Top'). % Mn   [4] TAKRI VOWEL SIGN E..TAKRI VOWEL SIGN AU
unicode_indic_matra_category(0x116B6, 0x116B6, 'Top'). % Mn       TAKRI SIGN VIRAMA

% Indic_Matra_Category=Bottom

unicode_indic_matra_category(0x0941, 0x0944, 'Bottom'). % Mn   [4] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x094D, 0x094D, 'Bottom'). % Mn       DEVANAGARI SIGN VIRAMA
unicode_indic_matra_category(0x0956, 0x0957, 'Bottom'). % Mn   [2] DEVANAGARI VOWEL SIGN UE..DEVANAGARI VOWEL SIGN UUE
unicode_indic_matra_category(0x0962, 0x0963, 'Bottom'). % Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x09C1, 0x09C4, 'Bottom'). % Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x09CD, 0x09CD, 'Bottom'). % Mn       BENGALI SIGN VIRAMA
unicode_indic_matra_category(0x09E2, 0x09E3, 'Bottom'). % Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0A41, 0x0A42, 'Bottom'). % Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
unicode_indic_matra_category(0x0A4D, 0x0A4D, 'Bottom'). % Mn       GURMUKHI SIGN VIRAMA
unicode_indic_matra_category(0x0AC1, 0x0AC4, 'Bottom'). % Mn   [4] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x0ACD, 0x0ACD, 'Bottom'). % Mn       GUJARATI SIGN VIRAMA
unicode_indic_matra_category(0x0AE2, 0x0AE3, 'Bottom'). % Mn   [2] GUJARATI VOWEL SIGN VOCALIC L..GUJARATI VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0B41, 0x0B44, 'Bottom'). % Mn   [4] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x0B4D, 0x0B4D, 'Bottom'). % Mn       ORIYA SIGN VIRAMA
unicode_indic_matra_category(0x0B62, 0x0B63, 'Bottom'). % Mn   [2] ORIYA VOWEL SIGN VOCALIC L..ORIYA VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0BC1, 0x0BC2, 'Bottom'). % Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
% Note: U and UU have contextually variable placement in Tamil.
unicode_indic_matra_category(0x0C56, 0x0C56, 'Bottom'). % Mn       TELUGU AI LENGTH MARK
unicode_indic_matra_category(0x0C62, 0x0C63, 'Bottom'). % Mn   [2] TELUGU VOWEL SIGN VOCALIC L..TELUGU VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0CE2, 0x0CE3, 'Bottom'). % Mn   [2] KANNADA VOWEL SIGN VOCALIC L..KANNADA VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0D43, 0x0D44, 'Bottom'). % Mn   [2] MALAYALAM VOWEL SIGN VOCALIC R..MALAYALAM VOWEL SIGN VOCALIC RR
unicode_indic_matra_category(0x0D62, 0x0D63, 'Bottom'). % Mn   [2] MALAYALAM VOWEL SIGN VOCALIC L..MALAYALAM VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0DD4, 0x0DD4, 'Bottom'). % Mn       SINHALA VOWEL SIGN KETTI PAA-PILLA
unicode_indic_matra_category(0x0DD6, 0x0DD6, 'Bottom'). % Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
unicode_indic_matra_category(0x0E38, 0x0E39, 'Bottom'). % Mn   [2] THAI CHARACTER SARA U..THAI CHARACTER SARA UU
unicode_indic_matra_category(0x0E3A, 0x0E3A, 'Bottom'). % Mn       THAI CHARACTER PHINTHU
unicode_indic_matra_category(0x0EB8, 0x0EB9, 'Bottom'). % Mn   [2] LAO VOWEL SIGN U..LAO VOWEL SIGN UU
unicode_indic_matra_category(0x0F71, 0x0F71, 'Bottom'). % Mn       TIBETAN VOWEL SIGN AA
unicode_indic_matra_category(0x0F74, 0x0F75, 'Bottom'). % Mn   [2] TIBETAN VOWEL SIGN U..TIBETAN VOWEL SIGN UU
unicode_indic_matra_category(0x0F84, 0x0F84, 'Bottom'). % Mn       TIBETAN MARK HALANTA
unicode_indic_matra_category(0x102F, 0x1030, 'Bottom'). % Mn   [2] MYANMAR VOWEL SIGN U..MYANMAR VOWEL SIGN UU
unicode_indic_matra_category(0x1058, 0x1059, 'Bottom'). % Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x1713, 0x1713, 'Bottom'). % Mn       TAGALOG VOWEL SIGN U
unicode_indic_matra_category(0x1714, 0x1714, 'Bottom'). % Mn       TAGALOG SIGN VIRAMA
unicode_indic_matra_category(0x1733, 0x1733, 'Bottom'). % Mn       HANUNOO VOWEL SIGN U
unicode_indic_matra_category(0x1734, 0x1734, 'Bottom'). % Mn       HANUNOO SIGN PAMUDPOD
unicode_indic_matra_category(0x1753, 0x1753, 'Bottom'). % Mn       BUHID VOWEL SIGN U
unicode_indic_matra_category(0x1773, 0x1773, 'Bottom'). % Mn       TAGBANWA VOWEL SIGN U
unicode_indic_matra_category(0x17BB, 0x17BD, 'Bottom'). % Mn   [3] KHMER VOWEL SIGN U..KHMER VOWEL SIGN UA
unicode_indic_matra_category(0x1922, 0x1922, 'Bottom'). % Mn       LIMBU VOWEL SIGN U
unicode_indic_matra_category(0x1A18, 0x1A18, 'Bottom'). % Mn       BUGINESE VOWEL SIGN U
unicode_indic_matra_category(0x1A69, 0x1A6A, 'Bottom'). % Mn   [2] TAI THAM VOWEL SIGN U..TAI THAM VOWEL SIGN UU
unicode_indic_matra_category(0x1A6C, 0x1A6C, 'Bottom'). % Mn       TAI THAM VOWEL SIGN OA BELOW
unicode_indic_matra_category(0x1B38, 0x1B3A, 'Bottom'). % Mn   [3] BALINESE VOWEL SIGN SUKU..BALINESE VOWEL SIGN RA REPA
unicode_indic_matra_category(0x1BA5, 0x1BA5, 'Bottom'). % Mn       SUNDANESE VOWEL SIGN PANYUKU
unicode_indic_matra_category(0x1C2C, 0x1C2C, 'Bottom'). % Mn       LEPCHA VOWEL SIGN E
unicode_indic_matra_category(0xA825, 0xA825, 'Bottom'). % Mn       SYLOTI NAGRI VOWEL SIGN U
unicode_indic_matra_category(0xA8C4, 0xA8C4, 'Bottom'). % Mn       SAURASHTRA SIGN VIRAMA
unicode_indic_matra_category(0xA947, 0xA949, 'Bottom'). % Mn   [3] REJANG VOWEL SIGN I..REJANG VOWEL SIGN E
unicode_indic_matra_category(0xA94B, 0xA94E, 'Bottom'). % Mn   [4] REJANG VOWEL SIGN O..REJANG VOWEL SIGN EA
unicode_indic_matra_category(0xA9B8, 0xA9B9, 'Bottom'). % Mn   [2] JAVANESE VOWEL SIGN SUKU..JAVANESE VOWEL SIGN SUKU MENDUT
unicode_indic_matra_category(0xAA2D, 0xAA2D, 'Bottom'). % Mn       CHAM VOWEL SIGN U
unicode_indic_matra_category(0xAA32, 0xAA32, 'Bottom'). % Mn       CHAM VOWEL SIGN UE
unicode_indic_matra_category(0xAAB4, 0xAAB4, 'Bottom'). % Mn       TAI VIET VOWEL U
unicode_indic_matra_category(0xAAEC, 0xAAEC, 'Bottom'). % Mn       MEETEI MAYEK VOWEL SIGN UU
unicode_indic_matra_category(0xABE8, 0xABE8, 'Bottom'). % Mn       MEETEI MAYEK VOWEL SIGN UNAP
unicode_indic_matra_category(0xABED, 0xABED, 'Bottom'). % Mn       MEETEI MAYEK APUN IYEK
unicode_indic_matra_category(0x10A02, 0x10A03, 'Bottom'). % Mn   [2] KHAROSHTHI VOWEL SIGN U..KHAROSHTHI VOWEL SIGN VOCALIC R
unicode_indic_matra_category(0x10A0C, 0x10A0C, 'Bottom'). % Mn       KHAROSHTHI VOWEL LENGTH MARK
unicode_indic_matra_category(0x1103C, 0x11041, 'Bottom'). % Mn   [6] BRAHMI VOWEL SIGN U..BRAHMI VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x110B3, 0x110B4, 'Bottom'). % Mn   [2] KAITHI VOWEL SIGN U..KAITHI VOWEL SIGN UU
unicode_indic_matra_category(0x110B9, 0x110B9, 'Bottom'). % Mn       KAITHI SIGN VIRAMA
unicode_indic_matra_category(0x1112A, 0x1112B, 'Bottom'). % Mn   [2] CHAKMA VOWEL SIGN U..CHAKMA VOWEL SIGN UU
unicode_indic_matra_category(0x11131, 0x11132, 'Bottom'). % Mn   [2] CHAKMA O MARK..CHAKMA AU MARK
unicode_indic_matra_category(0x111B6, 0x111BB, 'Bottom'). % Mn   [6] SHARADA VOWEL SIGN U..SHARADA VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x116B0, 0x116B1, 'Bottom'). % Mn   [2] TAKRI VOWEL SIGN U..TAKRI VOWEL SIGN UU

% Indic_Matra_Category=Top_And_Bottom

unicode_indic_matra_category(0x0C48, 0x0C48, 'Top_And_Bottom'). % Mn       TELUGU VOWEL SIGN AI
unicode_indic_matra_category(0x0F73, 0x0F73, 'Top_And_Bottom'). % Mn       TIBETAN VOWEL SIGN II
unicode_indic_matra_category(0x0F76, 0x0F79, 'Top_And_Bottom'). % Mn   [4] TIBETAN VOWEL SIGN VOCALIC R..TIBETAN VOWEL SIGN VOCALIC LL
unicode_indic_matra_category(0x0F81, 0x0F81, 'Top_And_Bottom'). % Mn       TIBETAN VOWEL SIGN REVERSED II
unicode_indic_matra_category(0x1B3C, 0x1B3C, 'Top_And_Bottom'). % Mn       BALINESE VOWEL SIGN LA LENGA
unicode_indic_matra_category(0x1112E, 0x1112F, 'Top_And_Bottom'). % Mn   [2] CHAKMA VOWEL SIGN O..CHAKMA VOWEL SIGN AU

% Indic_Matra_Category=Top_And_Right

unicode_indic_matra_category(0x0AC9, 0x0AC9, 'Top_And_Right'). % Mc       GUJARATI VOWEL SIGN CANDRA O
unicode_indic_matra_category(0x0B57, 0x0B57, 'Top_And_Right'). % Mc       ORIYA AU LENGTH MARK
unicode_indic_matra_category(0x0CC0, 0x0CC0, 'Top_And_Right'). % Mc       KANNADA VOWEL SIGN II
unicode_indic_matra_category(0x0CC7, 0x0CC8, 'Top_And_Right'). % Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
unicode_indic_matra_category(0x0CCA, 0x0CCB, 'Top_And_Right'). % Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
unicode_indic_matra_category(0x1925, 0x1926, 'Top_And_Right'). % Mc   [2] LIMBU VOWEL SIGN OO..LIMBU VOWEL SIGN AU
unicode_indic_matra_category(0x1B43, 0x1B43, 'Top_And_Right'). % Mc       BALINESE VOWEL SIGN PEPET TEDUNG
unicode_indic_matra_category(0x111BF, 0x111BF, 'Top_And_Right'). % Mc       SHARADA VOWEL SIGN AU

% Indic_Matra_Category=Top_And_Left

unicode_indic_matra_category(0x0B48, 0x0B48 , 'Top_And_Left'). % Mc       ORIYA VOWEL SIGN AI
unicode_indic_matra_category(0x0DDA, 0x0DDA , 'Top_And_Left'). % Mc       SINHALA VOWEL SIGN DIGA KOMBUVA
unicode_indic_matra_category(0x17BE, 0x17BE , 'Top_And_Left'). % Mc       KHMER VOWEL SIGN OE
unicode_indic_matra_category(0x1C29, 0x1C29 , 'Top_And_Left'). % Mc       LEPCHA VOWEL SIGN OO

% Indic_Matra_Category=Top_And_Left_And_Right

unicode_indic_matra_category(0x0B4C, 0x0B4C, 'Top_And_Left_And_Right'). % Mc       ORIYA VOWEL SIGN AU
unicode_indic_matra_category(0x17BF, 0x17BF, 'Top_And_Left_And_Right'). % Mc       KHMER VOWEL SIGN YA

% Indic_Matra_Category=Bottom_And_Right

unicode_indic_matra_category(0x1B3B, 0x1B3B , 'Bottom_And_Right'). % Mc       BALINESE VOWEL SIGN RA REPA TEDUNG
unicode_indic_matra_category(0xA9C0, 0xA9C0 , 'Bottom_And_Right'). % Mc       JAVANESE PANGKON

% Indic_Matra_Category=Top_And_Bottom_And_Right

unicode_indic_matra_category(0x1B3D, 0x1B3D, 'Top_And_Bottom_And_Right'). % Mc       BALINESE VOWEL SIGN LA LENGA TEDUNG

% Indic_Matra_Category=Overstruck

unicode_indic_matra_category(0x10A01, 0x10A01, 'Overstruck'). % Mn       KHAROSHTHI VOWEL SIGN I
unicode_indic_matra_category(0x10A06, 0x10A06, 'Overstruck'). % Mn       KHAROSHTHI VOWEL SIGN O

% Indic_Matra_Category=Invisible

% The Invisible category refers to viramas for certain script
% which are used only to control consonant stacking or cluster
% formation, and which have no visible display on their own.

unicode_indic_matra_category(0x1039, 0x1039, 'Invisible'). % Mn       MYANMAR SIGN VIRAMA
unicode_indic_matra_category(0x17D2, 0x17D2, 'Invisible'). % Mn       KHMER SIGN COENG
unicode_indic_matra_category(0x1A60, 0x1A60, 'Invisible'). % Mn       TAI THAM SIGN SAKOT
unicode_indic_matra_category(0xAAF6, 0xAAF6, 'Invisible'). % Mn       MEETEI MAYEK VIRAMA
unicode_indic_matra_category(0x10A3F, 0x10A3F, 'Invisible'). % Mn       KHAROSHTHI VIRAMA
unicode_indic_matra_category(0x11133, 0x11133, 'Invisible'). % Mn       CHAKMA VIRAMA

% EOF
