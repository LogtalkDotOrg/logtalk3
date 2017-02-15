%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: April 26, 2013
%
%  Original Unicode file header comments follow

/*
http://www.unicode.org/Public/UNIDATA/Blocks.txt

# Blocks-6.1.0.txt
# Date: 2011-06-14, 18:26:00 GMT [KW, LI]
#
# Unicode Character Database
# Copyright (c) 1991-2011 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
#
# Note:   The casing of block names is not normative.
#         For example, "Basic Latin" and "BASIC LATIN" are equivalent.
#
# Format:
# Start Code..End Code; Block Name

# ================================================

# Note:   When comparing block names, casing, whitespace, hyphens,
#         and underbars are ignored.
#         For example, "Latin Extended-A" and "latin extended a" are equivalent.
#         For more information on the comparison of property values, 
#            see UAX #44: http://www.unicode.org/reports/tr44/
#
#  All code points not explicitly listed for Block
#  have the value No_Block.

# Property:	Block
#
# @missing: 0000..10FFFF; No_Block
*/

unicode_block(CodePoint, Block) :-
	(	var(CodePoint) ->
		% generate code point pairs
		unicode_block(CodePointStart, CodePointEnd, Block),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_block(CodePoint, _, CodePointBlock) ->
		Block = CodePointBlock
	;	% if the block name is known, go straight to it
		nonvar(Block) ->
		unicode_block(CodePointStart, CodePointEnd, Block),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% look for a code point range that includes the given code point
		unicode_block(CodePointStart, CodePointEnd, Block),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Block = 'No_Block'
	).

unicode_block(0x0000, 0x007F, 'Basic Latin').
unicode_block(0x0080, 0x00FF, 'Latin-1 Supplement').
unicode_block(0x0100, 0x017F, 'Latin Extended-A').
unicode_block(0x0180, 0x024F, 'Latin Extended-B').
unicode_block(0x0250, 0x02AF, 'IPA Extensions').
unicode_block(0x02B0, 0x02FF, 'Spacing Modifier Letters').
unicode_block(0x0300, 0x036F, 'Combining Diacritical Marks').
unicode_block(0x0370, 0x03FF, 'Greek and Coptic').
unicode_block(0x0400, 0x04FF, 'Cyrillic').
unicode_block(0x0500, 0x052F, 'Cyrillic Supplement').
unicode_block(0x0530, 0x058F, 'Armenian').
unicode_block(0x0590, 0x05FF, 'Hebrew').
unicode_block(0x0600, 0x06FF, 'Arabic').
unicode_block(0x0700, 0x074F, 'Syriac').
unicode_block(0x0750, 0x077F, 'Arabic Supplement').
unicode_block(0x0780, 0x07BF, 'Thaana').
unicode_block(0x07C0, 0x07FF, 'NKo').
unicode_block(0x0800, 0x083F, 'Samaritan').
unicode_block(0x0840, 0x085F, 'Mandaic').
unicode_block(0x08A0, 0x08FF, 'Arabic Extended-A').
unicode_block(0x0900, 0x097F, 'Devanagari').
unicode_block(0x0980, 0x09FF, 'Bengali').
unicode_block(0x0A00, 0x0A7F, 'Gurmukhi').
unicode_block(0x0A80, 0x0AFF, 'Gujarati').
unicode_block(0x0B00, 0x0B7F, 'Oriya').
unicode_block(0x0B80, 0x0BFF, 'Tamil').
unicode_block(0x0C00, 0x0C7F, 'Telugu').
unicode_block(0x0C80, 0x0CFF, 'Kannada').
unicode_block(0x0D00, 0x0D7F, 'Malayalam').
unicode_block(0x0D80, 0x0DFF, 'Sinhala').
unicode_block(0x0E00, 0x0E7F, 'Thai').
unicode_block(0x0E80, 0x0EFF, 'Lao').
unicode_block(0x0F00, 0x0FFF, 'Tibetan').
unicode_block(0x1000, 0x109F, 'Myanmar').
unicode_block(0x10A0, 0x10FF, 'Georgian').
unicode_block(0x1100, 0x11FF, 'Hangul Jamo').
unicode_block(0x1200, 0x137F, 'Ethiopic').
unicode_block(0x1380, 0x139F, 'Ethiopic Supplement').
unicode_block(0x13A0, 0x13FF, 'Cherokee').
unicode_block(0x1400, 0x167F, 'Unified Canadian Aboriginal Syllabics').
unicode_block(0x1680, 0x169F, 'Ogham').
unicode_block(0x16A0, 0x16FF, 'Runic').
unicode_block(0x1700, 0x171F, 'Tagalog').
unicode_block(0x1720, 0x173F, 'Hanunoo').
unicode_block(0x1740, 0x175F, 'Buhid').
unicode_block(0x1760, 0x177F, 'Tagbanwa').
unicode_block(0x1780, 0x17FF, 'Khmer').
unicode_block(0x1800, 0x18AF, 'Mongolian').
unicode_block(0x18B0, 0x18FF, 'Unified Canadian Aboriginal Syllabics Extended').
unicode_block(0x1900, 0x194F, 'Limbu').
unicode_block(0x1950, 0x197F, 'Tai Le').
unicode_block(0x1980, 0x19DF, 'New Tai Lue').
unicode_block(0x19E0, 0x19FF, 'Khmer Symbols').
unicode_block(0x1A00, 0x1A1F, 'Buginese').
unicode_block(0x1A20, 0x1AAF, 'Tai Tham').
unicode_block(0x1B00, 0x1B7F, 'Balinese').
unicode_block(0x1B80, 0x1BBF, 'Sundanese').
unicode_block(0x1BC0, 0x1BFF, 'Batak').
unicode_block(0x1C00, 0x1C4F, 'Lepcha').
unicode_block(0x1C50, 0x1C7F, 'Ol Chiki').
unicode_block(0x1CC0, 0x1CCF, 'Sundanese Supplement').
unicode_block(0x1CD0, 0x1CFF, 'Vedic Extensions').
unicode_block(0x1D00, 0x1D7F, 'Phonetic Extensions').
unicode_block(0x1D80, 0x1DBF, 'Phonetic Extensions Supplement').
unicode_block(0x1DC0, 0x1DFF, 'Combining Diacritical Marks Supplement').
unicode_block(0x1E00, 0x1EFF, 'Latin Extended Additional').
unicode_block(0x1F00, 0x1FFF, 'Greek Extended').
unicode_block(0x2000, 0x206F, 'General Punctuation').
unicode_block(0x2070, 0x209F, 'Superscripts and Subscripts').
unicode_block(0x20A0, 0x20CF, 'Currency Symbols').
unicode_block(0x20D0, 0x20FF, 'Combining Diacritical Marks for Symbols').
unicode_block(0x2100, 0x214F, 'Letterlike Symbols').
unicode_block(0x2150, 0x218F, 'Number Forms').
unicode_block(0x2190, 0x21FF, 'Arrows').
unicode_block(0x2200, 0x22FF, 'Mathematical Operators').
unicode_block(0x2300, 0x23FF, 'Miscellaneous Technical').
unicode_block(0x2400, 0x243F, 'Control Pictures').
unicode_block(0x2440, 0x245F, 'Optical Character Recognition').
unicode_block(0x2460, 0x24FF, 'Enclosed Alphanumerics').
unicode_block(0x2500, 0x257F, 'Box Drawing').
unicode_block(0x2580, 0x259F, 'Block Elements').
unicode_block(0x25A0, 0x25FF, 'Geometric Shapes').
unicode_block(0x2600, 0x26FF, 'Miscellaneous Symbols').
unicode_block(0x2700, 0x27BF, 'Dingbats').
unicode_block(0x27C0, 0x27EF, 'Miscellaneous Mathematical Symbols-A').
unicode_block(0x27F0, 0x27FF, 'Supplemental Arrows-A').
unicode_block(0x2800, 0x28FF, 'Braille Patterns').
unicode_block(0x2900, 0x297F, 'Supplemental Arrows-B').
unicode_block(0x2980, 0x29FF, 'Miscellaneous Mathematical Symbols-B').
unicode_block(0x2A00, 0x2AFF, 'Supplemental Mathematical Operators').
unicode_block(0x2B00, 0x2BFF, 'Miscellaneous Symbols and Arrows').
unicode_block(0x2C00, 0x2C5F, 'Glagolitic').
unicode_block(0x2C60, 0x2C7F, 'Latin Extended-C').
unicode_block(0x2C80, 0x2CFF, 'Coptic').
unicode_block(0x2D00, 0x2D2F, 'Georgian Supplement').
unicode_block(0x2D30, 0x2D7F, 'Tifinagh').
unicode_block(0x2D80, 0x2DDF, 'Ethiopic Extended').
unicode_block(0x2DE0, 0x2DFF, 'Cyrillic Extended-A').
unicode_block(0x2E00, 0x2E7F, 'Supplemental Punctuation').
unicode_block(0x2E80, 0x2EFF, 'CJK Radicals Supplement').
unicode_block(0x2F00, 0x2FDF, 'Kangxi Radicals').
unicode_block(0x2FF0, 0x2FFF, 'Ideographic Description Characters').
unicode_block(0x3000, 0x303F, 'CJK Symbols and Punctuation').
unicode_block(0x3040, 0x309F, 'Hiragana').
unicode_block(0x30A0, 0x30FF, 'Katakana').
unicode_block(0x3100, 0x312F, 'Bopomofo').
unicode_block(0x3130, 0x318F, 'Hangul Compatibility Jamo').
unicode_block(0x3190, 0x319F, 'Kanbun').
unicode_block(0x31A0, 0x31BF, 'Bopomofo Extended').
unicode_block(0x31C0, 0x31EF, 'CJK Strokes').
unicode_block(0x31F0, 0x31FF, 'Katakana Phonetic Extensions').
unicode_block(0x3200, 0x32FF, 'Enclosed CJK Letters and Months').
unicode_block(0x3300, 0x33FF, 'CJK Compatibility').
unicode_block(0x3400, 0x4DBF, 'CJK Unified Ideographs Extension A').
unicode_block(0x4DC0, 0x4DFF, 'Yijing Hexagram Symbols').
unicode_block(0x4E00, 0x9FFF, 'CJK Unified Ideographs').
unicode_block(0xA000, 0xA48F, 'Yi Syllables').
unicode_block(0xA490, 0xA4CF, 'Yi Radicals').
unicode_block(0xA4D0, 0xA4FF, 'Lisu').
unicode_block(0xA500, 0xA63F, 'Vai').
unicode_block(0xA640, 0xA69F, 'Cyrillic Extended-B').
unicode_block(0xA6A0, 0xA6FF, 'Bamum').
unicode_block(0xA700, 0xA71F, 'Modifier Tone Letters').
unicode_block(0xA720, 0xA7FF, 'Latin Extended-D').
unicode_block(0xA800, 0xA82F, 'Syloti Nagri').
unicode_block(0xA830, 0xA83F, 'Common Indic Number Forms').
unicode_block(0xA840, 0xA87F, 'Phags-pa').
unicode_block(0xA880, 0xA8DF, 'Saurashtra').
unicode_block(0xA8E0, 0xA8FF, 'Devanagari Extended').
unicode_block(0xA900, 0xA92F, 'Kayah Li').
unicode_block(0xA930, 0xA95F, 'Rejang').
unicode_block(0xA960, 0xA97F, 'Hangul Jamo Extended-A').
unicode_block(0xA980, 0xA9DF, 'Javanese').
unicode_block(0xAA00, 0xAA5F, 'Cham').
unicode_block(0xAA60, 0xAA7F, 'Myanmar Extended-A').
unicode_block(0xAA80, 0xAADF, 'Tai Viet').
unicode_block(0xAAE0, 0xAAFF, 'Meetei Mayek Extensions').
unicode_block(0xAB00, 0xAB2F, 'Ethiopic Extended-A').
unicode_block(0xABC0, 0xABFF, 'Meetei Mayek').
unicode_block(0xAC00, 0xD7AF, 'Hangul Syllables').
unicode_block(0xD7B0, 0xD7FF, 'Hangul Jamo Extended-B').
unicode_block(0xD800, 0xDB7F, 'High Surrogates').
unicode_block(0xDB80, 0xDBFF, 'High Private Use Surrogates').
unicode_block(0xDC00, 0xDFFF, 'Low Surrogates').
unicode_block(0xE000, 0xF8FF, 'Private Use Area').
unicode_block(0xF900, 0xFAFF, 'CJK Compatibility Ideographs').
unicode_block(0xFB00, 0xFB4F, 'Alphabetic Presentation Forms').
unicode_block(0xFB50, 0xFDFF, 'Arabic Presentation Forms-A').
unicode_block(0xFE00, 0xFE0F, 'Variation Selectors').
unicode_block(0xFE10, 0xFE1F, 'Vertical Forms').
unicode_block(0xFE20, 0xFE2F, 'Combining Half Marks').
unicode_block(0xFE30, 0xFE4F, 'CJK Compatibility Forms').
unicode_block(0xFE50, 0xFE6F, 'Small Form Variants').
unicode_block(0xFE70, 0xFEFF, 'Arabic Presentation Forms-B').
unicode_block(0xFF00, 0xFFEF, 'Halfwidth and Fullwidth Forms').
unicode_block(0xFFF0, 0xFFFF, 'Specials').
unicode_block(0x10000, 0x1007F, 'Linear B Syllabary').
unicode_block(0x10080, 0x100FF, 'Linear B Ideograms').
unicode_block(0x10100, 0x1013F, 'Aegean Numbers').
unicode_block(0x10140, 0x1018F, 'Ancient Greek Numbers').
unicode_block(0x10190, 0x101CF, 'Ancient Symbols').
unicode_block(0x101D0, 0x101FF, 'Phaistos Disc').
unicode_block(0x10280, 0x1029F, 'Lycian').
unicode_block(0x102A0, 0x102DF, 'Carian').
unicode_block(0x10300, 0x1032F, 'Old Italic').
unicode_block(0x10330, 0x1034F, 'Gothic').
unicode_block(0x10380, 0x1039F, 'Ugaritic').
unicode_block(0x103A0, 0x103DF, 'Old Persian').
unicode_block(0x10400, 0x1044F, 'Deseret').
unicode_block(0x10450, 0x1047F, 'Shavian').
unicode_block(0x10480, 0x104AF, 'Osmanya').
unicode_block(0x10800, 0x1083F, 'Cypriot Syllabary').
unicode_block(0x10840, 0x1085F, 'Imperial Aramaic').
unicode_block(0x10900, 0x1091F, 'Phoenician').
unicode_block(0x10920, 0x1093F, 'Lydian').
unicode_block(0x10980, 0x1099F, 'Meroitic Hieroglyphs').
unicode_block(0x109A0, 0x109FF, 'Meroitic Cursive').
unicode_block(0x10A00, 0x10A5F, 'Kharoshthi').
unicode_block(0x10A60, 0x10A7F, 'Old South Arabian').
unicode_block(0x10B00, 0x10B3F, 'Avestan').
unicode_block(0x10B40, 0x10B5F, 'Inscriptional Parthian').
unicode_block(0x10B60, 0x10B7F, 'Inscriptional Pahlavi').
unicode_block(0x10C00, 0x10C4F, 'Old Turkic').
unicode_block(0x10E60, 0x10E7F, 'Rumi Numeral Symbols').
unicode_block(0x11000, 0x1107F, 'Brahmi').
unicode_block(0x11080, 0x110CF, 'Kaithi').
unicode_block(0x110D0, 0x110FF, 'Sora Sompeng').
unicode_block(0x11100, 0x1114F, 'Chakma').
unicode_block(0x11180, 0x111DF, 'Sharada').
unicode_block(0x11680, 0x116CF, 'Takri').
unicode_block(0x12000, 0x123FF, 'Cuneiform').
unicode_block(0x12400, 0x1247F, 'Cuneiform Numbers and Punctuation').
unicode_block(0x13000, 0x1342F, 'Egyptian Hieroglyphs').
unicode_block(0x16800, 0x16A3F, 'Bamum Supplement').
unicode_block(0x16F00, 0x16F9F, 'Miao').
unicode_block(0x1B000, 0x1B0FF, 'Kana Supplement').
unicode_block(0x1D000, 0x1D0FF, 'Byzantine Musical Symbols').
unicode_block(0x1D100, 0x1D1FF, 'Musical Symbols').
unicode_block(0x1D200, 0x1D24F, 'Ancient Greek Musical Notation').
unicode_block(0x1D300, 0x1D35F, 'Tai Xuan Jing Symbols').
unicode_block(0x1D360, 0x1D37F, 'Counting Rod Numerals').
unicode_block(0x1D400, 0x1D7FF, 'Mathematical Alphanumeric Symbols').
unicode_block(0x1EE00, 0x1EEFF, 'Arabic Mathematical Alphabetic Symbols').
unicode_block(0x1F000, 0x1F02F, 'Mahjong Tiles').
unicode_block(0x1F030, 0x1F09F, 'Domino Tiles').
unicode_block(0x1F0A0, 0x1F0FF, 'Playing Cards').
unicode_block(0x1F100, 0x1F1FF, 'Enclosed Alphanumeric Supplement').
unicode_block(0x1F200, 0x1F2FF, 'Enclosed Ideographic Supplement').
unicode_block(0x1F300, 0x1F5FF, 'Miscellaneous Symbols And Pictographs').
unicode_block(0x1F600, 0x1F64F, 'Emoticons').
unicode_block(0x1F680, 0x1F6FF, 'Transport And Map Symbols').
unicode_block(0x1F700, 0x1F77F, 'Alchemical Symbols').
unicode_block(0x20000, 0x2A6DF, 'CJK Unified Ideographs Extension B').
unicode_block(0x2A700, 0x2B73F, 'CJK Unified Ideographs Extension C').
unicode_block(0x2B740, 0x2B81F, 'CJK Unified Ideographs Extension D').
unicode_block(0x2F800, 0x2FA1F, 'CJK Compatibility Ideographs Supplement').
unicode_block(0xE0000, 0xE007F, 'Tags').
unicode_block(0xE0100, 0xE01EF, 'Variation Selectors Supplement').
unicode_block(0xF0000, 0xFFFFF, 'Supplementary Private Use Area-A').
unicode_block(0x100000, 0x10FFFF, 'Supplementary Private Use Area-B').
