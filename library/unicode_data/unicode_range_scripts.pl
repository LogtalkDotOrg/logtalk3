%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: August 23, 2013
%
%  File derived from the "unicode_scripts.pl" file by merging the consecutive
%  intervals for better performance of the unicode_script/2 predicate

unicode_script(CodePoint, Script) :-
	(	var(CodePoint) ->
		% generate CodePoint-script pairs
		unicode_range_script(CodePointStart, CodePointEnd, Script),
		between(CodePointStart, CodePointEnd, CodePoint)
	;	% try first-argument indexing first
		unicode_range_script(CodePoint, _, CodePointScript) ->
		Script = CodePointScript
	;	% if the script name is known, go straight to it
		nonvar(Script) ->
		(	unicode_range_script(CodePointStart, CodePointEnd, Script),
			between(CodePointStart, CodePointEnd, CodePoint) ->
			true
		;	fail
		)
	;	% look for a code point range that includes the given code point
		unicode_range_script(CodePointStart, CodePointEnd, Script),
		between(CodePointStart, CodePointEnd, CodePoint) ->
		true
	;	% missing code point; see original comment above
		between(0x0000, 0x10FFFF, CodePoint),
		Script = 'Zzzz'
	).

unicode_range_script(0x0000, 0x0040, 'Common').
unicode_range_script(0x005B, 0x0060, 'Common').
unicode_range_script(0x007B, 0x00A9, 'Common').
unicode_range_script(0x00AB, 0x00B9, 'Common').
unicode_range_script(0x00BB, 0x00BF, 'Common').
unicode_range_script(0x00D7, 0x00D7, 'Common').
unicode_range_script(0x00F7, 0x00F7, 'Common').
unicode_range_script(0x02B9, 0x02DF, 'Common').
unicode_range_script(0x02E5, 0x02E9, 'Common').
unicode_range_script(0x02EC, 0x02FF, 'Common').
unicode_range_script(0x0374, 0x0374, 'Common').
unicode_range_script(0x037E, 0x037E, 'Common').
unicode_range_script(0x0385, 0x0385, 'Common').
unicode_range_script(0x0387, 0x0387, 'Common').
unicode_range_script(0x0589, 0x0589, 'Common').
unicode_range_script(0x060C, 0x060C, 'Common').
unicode_range_script(0x061B, 0x061B, 'Common').
unicode_range_script(0x061F, 0x061F, 'Common').
unicode_range_script(0x0640, 0x0640, 'Common').
unicode_range_script(0x0660, 0x0669, 'Common').
unicode_range_script(0x06DD, 0x06DD, 'Common').
unicode_range_script(0x0964, 0x0965, 'Common').
unicode_range_script(0x0E3F, 0x0E3F, 'Common').
unicode_range_script(0x0FD5, 0x0FD8, 'Common').
unicode_range_script(0x10FB, 0x10FB, 'Common').
unicode_range_script(0x16EB, 0x16ED, 'Common').
unicode_range_script(0x1735, 0x1736, 'Common').
unicode_range_script(0x1802, 0x1803, 'Common').
unicode_range_script(0x1805, 0x1805, 'Common').
unicode_range_script(0x1CD3, 0x1CD3, 'Common').
unicode_range_script(0x1CE1, 0x1CE1, 'Common').
unicode_range_script(0x1CE9, 0x1CEC, 'Common').
unicode_range_script(0x1CEE, 0x1CF3, 'Common').
unicode_range_script(0x1CF5, 0x1CF6, 'Common').
unicode_range_script(0x2000, 0x200B, 'Common').
unicode_range_script(0x200E, 0x2064, 'Common').
unicode_range_script(0x206A, 0x2070, 'Common').
unicode_range_script(0x2074, 0x207E, 'Common').
unicode_range_script(0x2080, 0x208E, 'Common').
unicode_range_script(0x20A0, 0x20BA, 'Common').
unicode_range_script(0x2100, 0x2125, 'Common').
unicode_range_script(0x2127, 0x2129, 'Common').
unicode_range_script(0x212C, 0x2131, 'Common').
unicode_range_script(0x2133, 0x214D, 'Common').
unicode_range_script(0x214F, 0x215F, 'Common').
unicode_range_script(0x2189, 0x2189, 'Common').
unicode_range_script(0x2190, 0x23F3, 'Common').
unicode_range_script(0x2400, 0x2426, 'Common').
unicode_range_script(0x2440, 0x244A, 'Common').
unicode_range_script(0x2460, 0x26FF, 'Common').
unicode_range_script(0x2701, 0x2769, 'Common').
unicode_range_script(0x276A, 0x27FF, 'Common').
unicode_range_script(0x2900, 0x2B4C, 'Common').
unicode_range_script(0x2B50, 0x2B59, 'Common').
unicode_range_script(0x2E00, 0x2E3B, 'Common').
unicode_range_script(0x2FF0, 0x2FFB, 'Common').
unicode_range_script(0x3000, 0x3004, 'Common').
unicode_range_script(0x3006, 0x3006, 'Common').
unicode_range_script(0x3008, 0x3020, 'Common').
unicode_range_script(0x3030, 0x3037, 'Common').
unicode_range_script(0x303C, 0x303F, 'Common').
unicode_range_script(0x309B, 0x309C, 'Common').
unicode_range_script(0x30A0, 0x30A0, 'Common').
unicode_range_script(0x30FB, 0x30FC, 'Common').
unicode_range_script(0x3190, 0x319F, 'Common').
unicode_range_script(0x31C0, 0x31E3, 'Common').
unicode_range_script(0x3220, 0x325F, 'Common').
unicode_range_script(0x327F, 0x32CF, 'Common').
unicode_range_script(0x3358, 0x33FF, 'Common').
unicode_range_script(0x4DC0, 0x4DFF, 'Common').
unicode_range_script(0xA700, 0xA721, 'Common').
unicode_range_script(0xA788, 0xA78A, 'Common').
unicode_range_script(0xA830, 0xA839, 'Common').
unicode_range_script(0xFD3E, 0xFD3F, 'Common').
unicode_range_script(0xFDFD, 0xFDFD, 'Common').
unicode_range_script(0xFE10, 0xFE19, 'Common').
unicode_range_script(0xFE30, 0xFE52, 'Common').
unicode_range_script(0xFE54, 0xFE66, 'Common').
unicode_range_script(0xFE68, 0xFE6B, 'Common').
unicode_range_script(0xFEFF, 0xFEFF, 'Common').
unicode_range_script(0xFF01, 0xFF20, 'Common').
unicode_range_script(0xFF3B, 0xFF40, 'Common').
unicode_range_script(0xFF5B, 0xFF65, 'Common').
unicode_range_script(0xFF70, 0xFF70, 'Common').
unicode_range_script(0xFF9E, 0xFF9F, 'Common').
unicode_range_script(0xFFE0, 0xFFE6, 'Common').
unicode_range_script(0xFFE8, 0xFFEE, 'Common').
unicode_range_script(0xFFF9, 0xFFFD, 'Common').
unicode_range_script(0x10100, 0x10102, 'Common').
unicode_range_script(0x10107, 0x10133, 'Common').
unicode_range_script(0x10137, 0x1013F, 'Common').
unicode_range_script(0x10190, 0x1019B, 'Common').
unicode_range_script(0x101D0, 0x101FC, 'Common').
unicode_range_script(0x1D000, 0x1D0F5, 'Common').
unicode_range_script(0x1D100, 0x1D126, 'Common').
unicode_range_script(0x1D129, 0x1D166, 'Common').
unicode_range_script(0x1D16A, 0x1D17A, 'Common').
unicode_range_script(0x1D183, 0x1D184, 'Common').
unicode_range_script(0x1D18C, 0x1D1A9, 'Common').
unicode_range_script(0x1D1AE, 0x1D1DD, 'Common').
unicode_range_script(0x1D300, 0x1D356, 'Common').
unicode_range_script(0x1D360, 0x1D371, 'Common').
unicode_range_script(0x1D400, 0x1D454, 'Common').
unicode_range_script(0x1D456, 0x1D49C, 'Common').
unicode_range_script(0x1D49E, 0x1D49F, 'Common').
unicode_range_script(0x1D4A2, 0x1D4A2, 'Common').
unicode_range_script(0x1D4A5, 0x1D4A6, 'Common').
unicode_range_script(0x1D4A9, 0x1D4AC, 'Common').
unicode_range_script(0x1D4AE, 0x1D4B9, 'Common').
unicode_range_script(0x1D4BB, 0x1D4BB, 'Common').
unicode_range_script(0x1D4BD, 0x1D4C3, 'Common').
unicode_range_script(0x1D4C5, 0x1D505, 'Common').
unicode_range_script(0x1D507, 0x1D50A, 'Common').
unicode_range_script(0x1D50D, 0x1D514, 'Common').
unicode_range_script(0x1D516, 0x1D51C, 'Common').
unicode_range_script(0x1D51E, 0x1D539, 'Common').
unicode_range_script(0x1D53B, 0x1D53E, 'Common').
unicode_range_script(0x1D540, 0x1D544, 'Common').
unicode_range_script(0x1D546, 0x1D546, 'Common').
unicode_range_script(0x1D54A, 0x1D550, 'Common').
unicode_range_script(0x1D552, 0x1D6A5, 'Common').
unicode_range_script(0x1D6A8, 0x1D7CB, 'Common').
unicode_range_script(0x1D7CE, 0x1D7FF, 'Common').
unicode_range_script(0x1F000, 0x1F02B, 'Common').
unicode_range_script(0x1F030, 0x1F093, 'Common').
unicode_range_script(0x1F0A0, 0x1F0AE, 'Common').
unicode_range_script(0x1F0B1, 0x1F0BE, 'Common').
unicode_range_script(0x1F0C1, 0x1F0CF, 'Common').
unicode_range_script(0x1F0D1, 0x1F0DF, 'Common').
unicode_range_script(0x1F100, 0x1F10A, 'Common').
unicode_range_script(0x1F110, 0x1F12E, 'Common').
unicode_range_script(0x1F130, 0x1F16B, 'Common').
unicode_range_script(0x1F170, 0x1F19A, 'Common').
unicode_range_script(0x1F1E6, 0x1F1FF, 'Common').
unicode_range_script(0x1F201, 0x1F202, 'Common').
unicode_range_script(0x1F210, 0x1F23A, 'Common').
unicode_range_script(0x1F240, 0x1F248, 'Common').
unicode_range_script(0x1F250, 0x1F251, 'Common').
unicode_range_script(0x1F300, 0x1F320, 'Common').
unicode_range_script(0x1F330, 0x1F335, 'Common').
unicode_range_script(0x1F337, 0x1F37C, 'Common').
unicode_range_script(0x1F380, 0x1F393, 'Common').
unicode_range_script(0x1F3A0, 0x1F3C4, 'Common').
unicode_range_script(0x1F3C6, 0x1F3CA, 'Common').
unicode_range_script(0x1F3E0, 0x1F3F0, 'Common').
unicode_range_script(0x1F400, 0x1F43E, 'Common').
unicode_range_script(0x1F440, 0x1F440, 'Common').
unicode_range_script(0x1F442, 0x1F4F7, 'Common').
unicode_range_script(0x1F4F9, 0x1F4FC, 'Common').
unicode_range_script(0x1F500, 0x1F53D, 'Common').
unicode_range_script(0x1F540, 0x1F543, 'Common').
unicode_range_script(0x1F550, 0x1F567, 'Common').
unicode_range_script(0x1F5FB, 0x1F640, 'Common').
unicode_range_script(0x1F645, 0x1F64F, 'Common').
unicode_range_script(0x1F680, 0x1F6C5, 'Common').
unicode_range_script(0x1F700, 0x1F773, 'Common').
unicode_range_script(0xE0001, 0xE0001, 'Common').
unicode_range_script(0xE0020, 0xE007F, 'Common').

% Total code points: 6412

% ================================================

unicode_range_script(0x0041, 0x005A, 'Latin').
unicode_range_script(0x0061, 0x007A, 'Latin').
unicode_range_script(0x00AA, 0x00AA, 'Latin').
unicode_range_script(0x00BA, 0x00BA, 'Latin').
unicode_range_script(0x00C0, 0x00D6, 'Latin').
unicode_range_script(0x00D8, 0x00F6, 'Latin').
unicode_range_script(0x00F8, 0x02B8, 'Latin').
unicode_range_script(0x02E0, 0x02E4, 'Latin').
unicode_range_script(0x1D00, 0x1D25, 'Latin').
unicode_range_script(0x1D2C, 0x1D5C, 'Latin').
unicode_range_script(0x1D62, 0x1D65, 'Latin').
unicode_range_script(0x1D6B, 0x1D77, 'Latin').
unicode_range_script(0x1D79, 0x1DBE, 'Latin').
unicode_range_script(0x1E00, 0x1EFF, 'Latin').
unicode_range_script(0x2071, 0x2071, 'Latin').
unicode_range_script(0x207F, 0x207F, 'Latin').
unicode_range_script(0x2090, 0x209C, 'Latin').
unicode_range_script(0x212A, 0x212B, 'Latin').
unicode_range_script(0x2132, 0x2132, 'Latin').

unicode_range_script(0x214E, 0x214E, 'Latin').
unicode_range_script(0x2160, 0x2188, 'Latin').
unicode_range_script(0x2C60, 0x2C7F, 'Latin').
unicode_range_script(0xA722, 0xA787, 'Latin').
unicode_range_script(0xA78B, 0xA78E, 'Latin').
unicode_range_script(0xA790, 0xA793, 'Latin').
unicode_range_script(0xA7A0, 0xA7AA, 'Latin').
unicode_range_script(0xA7F8, 0xA7FF, 'Latin').
unicode_range_script(0xFB00, 0xFB06, 'Latin').
unicode_range_script(0xFF21, 0xFF3A, 'Latin').
unicode_range_script(0xFF41, 0xFF5A, 'Latin').

% Total code points: 1272

% ================================================

unicode_range_script(0x0370, 0x0373, 'Greek').
unicode_range_script(0x0375, 0x0377, 'Greek').
unicode_range_script(0x037A, 0x037D, 'Greek').
unicode_range_script(0x0384, 0x0384, 'Greek').
unicode_range_script(0x0386, 0x0386, 'Greek').
unicode_range_script(0x0388, 0x038A, 'Greek').
unicode_range_script(0x038C, 0x038C, 'Greek').
unicode_range_script(0x038E, 0x03A1, 'Greek').
unicode_range_script(0x03A3, 0x03E1, 'Greek').
unicode_range_script(0x03F0, 0x03FF, 'Greek').
unicode_range_script(0x1D26, 0x1D2A, 'Greek').
unicode_range_script(0x1D5D, 0x1D61, 'Greek').
unicode_range_script(0x1D66, 0x1D6A, 'Greek').
unicode_range_script(0x1DBF, 0x1DBF, 'Greek').
unicode_range_script(0x1F00, 0x1F15, 'Greek').
unicode_range_script(0x1F18, 0x1F1D, 'Greek').
unicode_range_script(0x1F20, 0x1F45, 'Greek').
unicode_range_script(0x1F48, 0x1F4D, 'Greek').
unicode_range_script(0x1F50, 0x1F57, 'Greek').
unicode_range_script(0x1F59, 0x1F59, 'Greek').
unicode_range_script(0x1F5B, 0x1F5B, 'Greek').
unicode_range_script(0x1F5D, 0x1F5D, 'Greek').
unicode_range_script(0x1F5F, 0x1F7D, 'Greek').
unicode_range_script(0x1F80, 0x1FB4, 'Greek').
unicode_range_script(0x1FB6, 0x1FC4, 'Greek').
unicode_range_script(0x1FC6, 0x1FD3, 'Greek').
unicode_range_script(0x1FD6, 0x1FDB, 'Greek').
unicode_range_script(0x1FDD, 0x1FEF, 'Greek').
unicode_range_script(0x1FF2, 0x1FF4, 'Greek').
unicode_range_script(0x1FF6, 0x1FFE, 'Greek').
unicode_range_script(0x2126, 0x2126, 'Greek').
unicode_range_script(0x10140, 0x1018A, 'Greek').
unicode_range_script(0x1D200, 0x1D245, 'Greek').

% Total code points: 511

% ================================================

unicode_range_script(0x0400, 0x0484, 'Cyrillic').
unicode_range_script(0x0487, 0x0527, 'Cyrillic').
unicode_range_script(0x1D2B, 0x1D2B, 'Cyrillic').
unicode_range_script(0x1D78, 0x1D78, 'Cyrillic').
unicode_range_script(0x2DE0, 0x2DFF, 'Cyrillic').
unicode_range_script(0xA640, 0xA697, 'Cyrillic').
unicode_range_script(0xA69F, 0xA69F, 'Cyrillic').

% Total code points: 417

% ================================================

unicode_range_script(0x0531, 0x0556, 'Armenian').
unicode_range_script(0x0559, 0x055F, 'Armenian').
unicode_range_script(0x0561, 0x0587, 'Armenian').
unicode_range_script(0x058A, 0x058A, 'Armenian').
unicode_range_script(0x058F, 0x058F, 'Armenian').
unicode_range_script(0xFB13, 0xFB17, 'Armenian').

% Total code points: 91

% ================================================

unicode_range_script(0x0591, 0x05C7, 'Hebrew').
unicode_range_script(0x05D0, 0x05EA, 'Hebrew').
unicode_range_script(0x05F0, 0x05F4, 'Hebrew').
unicode_range_script(0xFB1D, 0xFB36, 'Hebrew').
unicode_range_script(0xFB38, 0xFB3C, 'Hebrew').
unicode_range_script(0xFB3E, 0xFB3E, 'Hebrew').
unicode_range_script(0xFB40, 0xFB41, 'Hebrew').
unicode_range_script(0xFB43, 0xFB44, 'Hebrew').
unicode_range_script(0xFB46, 0xFB4F, 'Hebrew').

% Total code points: 133

% ================================================

unicode_range_script(0x0600, 0x0604, 'Arabic').
unicode_range_script(0x0606, 0x060B, 'Arabic').
unicode_range_script(0x060D, 0x061A, 'Arabic').
unicode_range_script(0x061E, 0x061E, 'Arabic').
unicode_range_script(0x0620, 0x063F, 'Arabic').
unicode_range_script(0x0641, 0x064A, 'Arabic').
unicode_range_script(0x0656, 0x065E, 'Arabic').
unicode_range_script(0x066A, 0x066F, 'Arabic').
unicode_range_script(0x0671, 0x06DC, 'Arabic').
unicode_range_script(0x06DE, 0x06FF, 'Arabic').
unicode_range_script(0x0750, 0x077F, 'Arabic').
unicode_range_script(0x08A0, 0x08A0, 'Arabic').
unicode_range_script(0x08A2, 0x08AC, 'Arabic').
unicode_range_script(0x08E4, 0x08FE, 'Arabic').
unicode_range_script(0xFB50, 0xFBC1, 'Arabic').
unicode_range_script(0xFBD3, 0xFD3D, 'Arabic').
unicode_range_script(0xFD50, 0xFD8F, 'Arabic').
unicode_range_script(0xFD92, 0xFDC7, 'Arabic').
unicode_range_script(0xFDF0, 0xFDFC, 'Arabic').
unicode_range_script(0xFE70, 0xFE74, 'Arabic').
unicode_range_script(0xFE76, 0xFEFC, 'Arabic').
unicode_range_script(0x10E60, 0x10E7E, 'Arabic').
unicode_range_script(0x1EE00, 0x1EE03, 'Arabic').
unicode_range_script(0x1EE05, 0x1EE1F, 'Arabic').
unicode_range_script(0x1EE21, 0x1EE22, 'Arabic').
unicode_range_script(0x1EE24, 0x1EE24, 'Arabic').
unicode_range_script(0x1EE27, 0x1EE27, 'Arabic').
unicode_range_script(0x1EE29, 0x1EE32, 'Arabic').
unicode_range_script(0x1EE34, 0x1EE37, 'Arabic').
unicode_range_script(0x1EE39, 0x1EE39, 'Arabic').
unicode_range_script(0x1EE3B, 0x1EE3B, 'Arabic').
unicode_range_script(0x1EE42, 0x1EE42, 'Arabic').
unicode_range_script(0x1EE47, 0x1EE47, 'Arabic').
unicode_range_script(0x1EE49, 0x1EE49, 'Arabic').
unicode_range_script(0x1EE4B, 0x1EE4B, 'Arabic').
unicode_range_script(0x1EE4D, 0x1EE4F, 'Arabic').
unicode_range_script(0x1EE51, 0x1EE52, 'Arabic').
unicode_range_script(0x1EE54, 0x1EE54, 'Arabic').
unicode_range_script(0x1EE57, 0x1EE57, 'Arabic').
unicode_range_script(0x1EE59, 0x1EE59, 'Arabic').
unicode_range_script(0x1EE5B, 0x1EE5B, 'Arabic').
unicode_range_script(0x1EE5D, 0x1EE5D, 'Arabic').
unicode_range_script(0x1EE5F, 0x1EE5F, 'Arabic').
unicode_range_script(0x1EE61, 0x1EE62, 'Arabic').
unicode_range_script(0x1EE64, 0x1EE64, 'Arabic').
unicode_range_script(0x1EE67, 0x1EE6A, 'Arabic').
unicode_range_script(0x1EE6C, 0x1EE72, 'Arabic').
unicode_range_script(0x1EE74, 0x1EE77, 'Arabic').
unicode_range_script(0x1EE79, 0x1EE7C, 'Arabic').
unicode_range_script(0x1EE7E, 0x1EE7E, 'Arabic').
unicode_range_script(0x1EE80, 0x1EE89, 'Arabic').
unicode_range_script(0x1EE8B, 0x1EE9B, 'Arabic').
unicode_range_script(0x1EEA1, 0x1EEA3, 'Arabic').
unicode_range_script(0x1EEA5, 0x1EEA9, 'Arabic').
unicode_range_script(0x1EEAB, 0x1EEBB, 'Arabic').
unicode_range_script(0x1EEF0, 0x1EEF1, 'Arabic').

% Total code points: 1234

% ================================================

unicode_range_script(0x0700, 0x070D, 'Syriac').
unicode_range_script(0x070F, 0x074A, 'Syriac').
unicode_range_script(0x074D, 0x074F, 'Syriac').

% Total code points: 77

% ================================================

unicode_range_script(0x0780, 0x07B1, 'Thaana').

% Total code points: 50

% ================================================

unicode_range_script(0x0900, 0x0950, 'Devanagari').
unicode_range_script(0x0953, 0x0963, 'Devanagari').
unicode_range_script(0x0966, 0x0977, 'Devanagari').
unicode_range_script(0x0979, 0x097F, 'Devanagari').
unicode_range_script(0xA8E0, 0xA8FB, 'Devanagari').

% Total code points: 151

% ================================================

unicode_range_script(0x0981, 0x0983, 'Bengali').
unicode_range_script(0x0985, 0x098C, 'Bengali').
unicode_range_script(0x098F, 0x0990, 'Bengali').
unicode_range_script(0x0993, 0x09A8, 'Bengali').
unicode_range_script(0x09AA, 0x09B0, 'Bengali').
unicode_range_script(0x09B2, 0x09B2, 'Bengali').
unicode_range_script(0x09B6, 0x09B9, 'Bengali').
unicode_range_script(0x09BC, 0x09C4, 'Bengali').
unicode_range_script(0x09C7, 0x09C8, 'Bengali').
unicode_range_script(0x09CB, 0x09CE, 'Bengali').
unicode_range_script(0x09D7, 0x09D7, 'Bengali').
unicode_range_script(0x09DC, 0x09DD, 'Bengali').
unicode_range_script(0x09DF, 0x09E3, 'Bengali').
unicode_range_script(0x09E6, 0x09FB, 'Bengali').

% Total code points: 92

% ================================================

unicode_range_script(0x0A01, 0x0A03, 'Gurmukhi').
unicode_range_script(0x0A05, 0x0A0A, 'Gurmukhi').
unicode_range_script(0x0A0F, 0x0A10, 'Gurmukhi').
unicode_range_script(0x0A13, 0x0A28, 'Gurmukhi').
unicode_range_script(0x0A2A, 0x0A30, 'Gurmukhi').
unicode_range_script(0x0A32, 0x0A33, 'Gurmukhi').
unicode_range_script(0x0A35, 0x0A36, 'Gurmukhi').
unicode_range_script(0x0A38, 0x0A39, 'Gurmukhi').
unicode_range_script(0x0A3C, 0x0A3C, 'Gurmukhi').
unicode_range_script(0x0A3E, 0x0A42, 'Gurmukhi').
unicode_range_script(0x0A47, 0x0A48, 'Gurmukhi').
unicode_range_script(0x0A4B, 0x0A4D, 'Gurmukhi').
unicode_range_script(0x0A51, 0x0A51, 'Gurmukhi').
unicode_range_script(0x0A59, 0x0A5C, 'Gurmukhi').
unicode_range_script(0x0A5E, 0x0A5E, 'Gurmukhi').
unicode_range_script(0x0A66, 0x0A75, 'Gurmukhi').

% Total code points: 79

% ================================================

unicode_range_script(0x0A81, 0x0A83, 'Gujarati').
unicode_range_script(0x0A85, 0x0A8D, 'Gujarati').
unicode_range_script(0x0A8F, 0x0A91, 'Gujarati').
unicode_range_script(0x0A93, 0x0AA8, 'Gujarati').
unicode_range_script(0x0AAA, 0x0AB0, 'Gujarati').
unicode_range_script(0x0AB2, 0x0AB3, 'Gujarati').
unicode_range_script(0x0AB5, 0x0AB9, 'Gujarati').
unicode_range_script(0x0ABC, 0x0AC5, 'Gujarati').
unicode_range_script(0x0AC7, 0x0AC9, 'Gujarati').
unicode_range_script(0x0ACB, 0x0ACD, 'Gujarati').
unicode_range_script(0x0AD0, 0x0AD0, 'Gujarati').
unicode_range_script(0x0AE0, 0x0AE3, 'Gujarati').
unicode_range_script(0x0AE6, 0x0AF1, 'Gujarati').

% Total code points: 84

% ================================================

unicode_range_script(0x0B01, 0x0B03, 'Oriya').
unicode_range_script(0x0B05, 0x0B0C, 'Oriya').
unicode_range_script(0x0B0F, 0x0B10, 'Oriya').
unicode_range_script(0x0B13, 0x0B28, 'Oriya').
unicode_range_script(0x0B2A, 0x0B30, 'Oriya').
unicode_range_script(0x0B32, 0x0B33, 'Oriya').
unicode_range_script(0x0B35, 0x0B39, 'Oriya').
unicode_range_script(0x0B3C, 0x0B44, 'Oriya').
unicode_range_script(0x0B47, 0x0B48, 'Oriya').
unicode_range_script(0x0B4B, 0x0B4D, 'Oriya').
unicode_range_script(0x0B56, 0x0B57, 'Oriya').
unicode_range_script(0x0B5C, 0x0B5D, 'Oriya').
unicode_range_script(0x0B5F, 0x0B63, 'Oriya').
unicode_range_script(0x0B66, 0x0B77, 'Oriya').

% Total code points: 90

% ================================================

unicode_range_script(0x0B82, 0x0B83, 'Tamil').
unicode_range_script(0x0B85, 0x0B8A, 'Tamil').
unicode_range_script(0x0B8E, 0x0B90, 'Tamil').
unicode_range_script(0x0B92, 0x0B95, 'Tamil').
unicode_range_script(0x0B99, 0x0B9A, 'Tamil').
unicode_range_script(0x0B9C, 0x0B9C, 'Tamil').
unicode_range_script(0x0B9E, 0x0B9F, 'Tamil').
unicode_range_script(0x0BA3, 0x0BA4, 'Tamil').
unicode_range_script(0x0BA8, 0x0BAA, 'Tamil').
unicode_range_script(0x0BAE, 0x0BB9, 'Tamil').
unicode_range_script(0x0BBE, 0x0BC2, 'Tamil').
unicode_range_script(0x0BC6, 0x0BC8, 'Tamil').
unicode_range_script(0x0BCA, 0x0BCD, 'Tamil').
unicode_range_script(0x0BD0, 0x0BD0, 'Tamil').
unicode_range_script(0x0BD7, 0x0BD7, 'Tamil').
unicode_range_script(0x0BE6, 0x0BFA, 'Tamil').

% Total code points: 72

% ================================================

unicode_range_script(0x0C01, 0x0C03, 'Telugu').
unicode_range_script(0x0C05, 0x0C0C, 'Telugu').
unicode_range_script(0x0C0E, 0x0C10, 'Telugu').
unicode_range_script(0x0C12, 0x0C28, 'Telugu').
unicode_range_script(0x0C2A, 0x0C33, 'Telugu').
unicode_range_script(0x0C35, 0x0C39, 'Telugu').
unicode_range_script(0x0C3D, 0x0C44, 'Telugu').
unicode_range_script(0x0C46, 0x0C48, 'Telugu').
unicode_range_script(0x0C4A, 0x0C4D, 'Telugu').
unicode_range_script(0x0C55, 0x0C56, 'Telugu').
unicode_range_script(0x0C58, 0x0C59, 'Telugu').
unicode_range_script(0x0C60, 0x0C63, 'Telugu').
unicode_range_script(0x0C66, 0x0C6F, 'Telugu').
unicode_range_script(0x0C78, 0x0C7F, 'Telugu').

% Total code points: 93

% ================================================

unicode_range_script(0x0C82, 0x0C83, 'Kannada').
unicode_range_script(0x0C85, 0x0C8C, 'Kannada').
unicode_range_script(0x0C8E, 0x0C90, 'Kannada').
unicode_range_script(0x0C92, 0x0CA8, 'Kannada').
unicode_range_script(0x0CAA, 0x0CB3, 'Kannada').
unicode_range_script(0x0CB5, 0x0CB9, 'Kannada').
unicode_range_script(0x0CBC, 0x0CC4, 'Kannada').
unicode_range_script(0x0CC6, 0x0CC8, 'Kannada').
unicode_range_script(0x0CCA, 0x0CCD, 'Kannada').
unicode_range_script(0x0CD5, 0x0CD6, 'Kannada').
unicode_range_script(0x0CDE, 0x0CDE, 'Kannada').
unicode_range_script(0x0CE0, 0x0CE3, 'Kannada').
unicode_range_script(0x0CE6, 0x0CEF, 'Kannada').
unicode_range_script(0x0CF1, 0x0CF2, 'Kannada').

% Total code points: 86

% ================================================

unicode_range_script(0x0D02, 0x0D03, 'Malayalam').
unicode_range_script(0x0D05, 0x0D0C, 'Malayalam').
unicode_range_script(0x0D0E, 0x0D10, 'Malayalam').
unicode_range_script(0x0D12, 0x0D3A, 'Malayalam').
unicode_range_script(0x0D3D, 0x0D44, 'Malayalam').
unicode_range_script(0x0D46, 0x0D48, 'Malayalam').
unicode_range_script(0x0D4A, 0x0D4E, 'Malayalam').
unicode_range_script(0x0D57, 0x0D57, 'Malayalam').
unicode_range_script(0x0D60, 0x0D63, 'Malayalam').
unicode_range_script(0x0D66, 0x0D75, 'Malayalam').
unicode_range_script(0x0D79, 0x0D7F, 'Malayalam').

% Total code points: 98

% ================================================

unicode_range_script(0x0D82, 0x0D83, 'Sinhala').
unicode_range_script(0x0D85, 0x0D96, 'Sinhala').
unicode_range_script(0x0D9A, 0x0DB1, 'Sinhala').
unicode_range_script(0x0DB3, 0x0DBB, 'Sinhala').
unicode_range_script(0x0DBD, 0x0DBD, 'Sinhala').
unicode_range_script(0x0DC0, 0x0DC6, 'Sinhala').
unicode_range_script(0x0DCA, 0x0DCA, 'Sinhala').
unicode_range_script(0x0DCF, 0x0DD4, 'Sinhala').
unicode_range_script(0x0DD6, 0x0DD6, 'Sinhala').
unicode_range_script(0x0DD8, 0x0DDF, 'Sinhala').
unicode_range_script(0x0DF2, 0x0DF4, 'Sinhala').

% Total code points: 80

% ================================================

unicode_range_script(0x0E01, 0x0E3A, 'Thai').
unicode_range_script(0x0E40, 0x0E5B, 'Thai').

% Total code points: 86

% ================================================

unicode_range_script(0x0E81, 0x0E82, 'Lao').
unicode_range_script(0x0E84, 0x0E84, 'Lao').
unicode_range_script(0x0E87, 0x0E88, 'Lao').
unicode_range_script(0x0E8A, 0x0E8A, 'Lao').
unicode_range_script(0x0E8D, 0x0E8D, 'Lao').
unicode_range_script(0x0E94, 0x0E97, 'Lao').
unicode_range_script(0x0E99, 0x0E9F, 'Lao').
unicode_range_script(0x0EA1, 0x0EA3, 'Lao').
unicode_range_script(0x0EA5, 0x0EA5, 'Lao').
unicode_range_script(0x0EA7, 0x0EA7, 'Lao').
unicode_range_script(0x0EAA, 0x0EAB, 'Lao').
unicode_range_script(0x0EAD, 0x0EB9, 'Lao').
unicode_range_script(0x0EBB, 0x0EBD, 'Lao').
unicode_range_script(0x0EC0, 0x0EC4, 'Lao').
unicode_range_script(0x0EC6, 0x0EC6, 'Lao').
unicode_range_script(0x0EC8, 0x0ECD, 'Lao').
unicode_range_script(0x0ED0, 0x0ED9, 'Lao').
unicode_range_script(0x0EDC, 0x0EDF, 'Lao').

% Total code points: 67

% ================================================

unicode_range_script(0x0F00, 0x0F47, 'Tibetan').
unicode_range_script(0x0F49, 0x0F6C, 'Tibetan').
unicode_range_script(0x0F71, 0x0F97, 'Tibetan').
unicode_range_script(0x0F99, 0x0FBC, 'Tibetan').
unicode_range_script(0x0FBE, 0x0FCC, 'Tibetan').
unicode_range_script(0x0FCE, 0x0FD4, 'Tibetan').
unicode_range_script(0x0FD9, 0x0FDA, 'Tibetan').

% Total code points: 207

% ================================================

unicode_range_script(0x1000, 0x109F, 'Myanmar').
unicode_range_script(0xAA60, 0xAA7B, 'Myanmar').

% Total code points: 188

% ================================================

unicode_range_script(0x10A0, 0x10C5, 'Georgian').
unicode_range_script(0x10C7, 0x10C7, 'Georgian').
unicode_range_script(0x10CD, 0x10CD, 'Georgian').
unicode_range_script(0x10D0, 0x10FA, 'Georgian').
unicode_range_script(0x10FC, 0x10FF, 'Georgian').
unicode_range_script(0x2D00, 0x2D25, 'Georgian').
unicode_range_script(0x2D27, 0x2D27, 'Georgian').
unicode_range_script(0x2D2D, 0x2D2D, 'Georgian').

% Total code points: 127

% ================================================

unicode_range_script(0x1100, 0x11FF, 'Hangul').
unicode_range_script(0x302E, 0x302F, 'Hangul').
unicode_range_script(0x3131, 0x318E, 'Hangul').
unicode_range_script(0x3200, 0x321E, 'Hangul').
unicode_range_script(0x3260, 0x327E, 'Hangul').
unicode_range_script(0xA960, 0xA97C, 'Hangul').
unicode_range_script(0xAC00, 0xD7A3, 'Hangul').
unicode_range_script(0xD7B0, 0xD7C6, 'Hangul').
unicode_range_script(0xD7CB, 0xD7FB, 'Hangul').
unicode_range_script(0xFFA0, 0xFFBE, 'Hangul').
unicode_range_script(0xFFC2, 0xFFC7, 'Hangul').
unicode_range_script(0xFFCA, 0xFFCF, 'Hangul').
unicode_range_script(0xFFD2, 0xFFD7, 'Hangul').
unicode_range_script(0xFFDA, 0xFFDC, 'Hangul').

% Total code points: 11739

% ================================================

unicode_range_script(0x1200, 0x1248, 'Ethiopic').
unicode_range_script(0x124A, 0x124D, 'Ethiopic').
unicode_range_script(0x1250, 0x1256, 'Ethiopic').
unicode_range_script(0x1258, 0x1258, 'Ethiopic').
unicode_range_script(0x125A, 0x125D, 'Ethiopic').
unicode_range_script(0x1260, 0x1288, 'Ethiopic').
unicode_range_script(0x128A, 0x128D, 'Ethiopic').
unicode_range_script(0x1290, 0x12B0, 'Ethiopic').
unicode_range_script(0x12B2, 0x12B5, 'Ethiopic').
unicode_range_script(0x12B8, 0x12BE, 'Ethiopic').
unicode_range_script(0x12C0, 0x12C0, 'Ethiopic').
unicode_range_script(0x12C2, 0x12C5, 'Ethiopic').
unicode_range_script(0x12C8, 0x12D6, 'Ethiopic').
unicode_range_script(0x12D8, 0x1310, 'Ethiopic').
unicode_range_script(0x1312, 0x1315, 'Ethiopic').
unicode_range_script(0x1318, 0x135A, 'Ethiopic').
unicode_range_script(0x135D, 0x137C, 'Ethiopic').
unicode_range_script(0x1380, 0x1399, 'Ethiopic').
unicode_range_script(0x2D80, 0x2D96, 'Ethiopic').
unicode_range_script(0x2DA0, 0x2DA6, 'Ethiopic').
unicode_range_script(0x2DA8, 0x2DAE, 'Ethiopic').
unicode_range_script(0x2DB0, 0x2DB6, 'Ethiopic').
unicode_range_script(0x2DB8, 0x2DBE, 'Ethiopic').
unicode_range_script(0x2DC0, 0x2DC6, 'Ethiopic').
unicode_range_script(0x2DC8, 0x2DCE, 'Ethiopic').
unicode_range_script(0x2DD0, 0x2DD6, 'Ethiopic').
unicode_range_script(0x2DD8, 0x2DDE, 'Ethiopic').
unicode_range_script(0xAB01, 0xAB06, 'Ethiopic').
unicode_range_script(0xAB09, 0xAB0E, 'Ethiopic').
unicode_range_script(0xAB11, 0xAB16, 'Ethiopic').
unicode_range_script(0xAB20, 0xAB26, 'Ethiopic').
unicode_range_script(0xAB28, 0xAB2E, 'Ethiopic').

% Total code points: 495

% ================================================

unicode_range_script(0x13A0, 0x13F4, 'Cherokee').

% Total code points: 85

% ================================================

unicode_range_script(0x1400, 0x167F, 'Canadian_Aboriginal').
unicode_range_script(0x18B0, 0x18F5, 'Canadian_Aboriginal').

% Total code points: 710

% ================================================

unicode_range_script(0x1680, 0x169C, 'Ogham').

% Total code points: 29

% ================================================

unicode_range_script(0x16A0, 0x16EA, 'Runic').
unicode_range_script(0x16EE, 0x16F0, 'Runic').

% Total code points: 78

% ================================================

unicode_range_script(0x1780, 0x17DD, 'Khmer').
unicode_range_script(0x17E0, 0x17E9, 'Khmer').
unicode_range_script(0x17F0, 0x17F9, 'Khmer').
unicode_range_script(0x19E0, 0x19FF, 'Khmer').

% Total code points: 146

% ================================================

unicode_range_script(0x1800, 0x1801, 'Mongolian').
unicode_range_script(0x1804, 0x1804, 'Mongolian').
unicode_range_script(0x1806, 0x180E, 'Mongolian').
unicode_range_script(0x1810, 0x1819, 'Mongolian').
unicode_range_script(0x1820, 0x1877, 'Mongolian').
unicode_range_script(0x1880, 0x18AA, 'Mongolian').

% Total code points: 153

% ================================================

unicode_range_script(0x3041, 0x3096, 'Hiragana').
unicode_range_script(0x309D, 0x309F, 'Hiragana').
unicode_range_script(0x1B001, 0x1B001, 'Hiragana').
unicode_range_script(0x1F200, 0x1F200, 'Hiragana').

% Total code points: 91

% ================================================

unicode_range_script(0x30A1, 0x30FA, 'Katakana').
unicode_range_script(0x30FD, 0x30FF, 'Katakana').
unicode_range_script(0x31F0, 0x31FF, 'Katakana').
unicode_range_script(0x32D0, 0x32FE, 'Katakana').
unicode_range_script(0x3300, 0x3357, 'Katakana').
unicode_range_script(0xFF66, 0xFF6F, 'Katakana').
unicode_range_script(0xFF71, 0xFF9D, 'Katakana').
unicode_range_script(0x1B000, 0x1B000, 'Katakana').

% Total code points: 300

% ================================================

unicode_range_script(0x02EA, 0x02EB, 'Bopomofo').
unicode_range_script(0x3105, 0x312D, 'Bopomofo').
unicode_range_script(0x31A0, 0x31BA, 'Bopomofo').

% Total code points: 70

% ================================================

unicode_range_script(0x2E80, 0x2E99, 'Han').
unicode_range_script(0x2E9B, 0x2EF3, 'Han').
unicode_range_script(0x2F00, 0x2FD5, 'Han').
unicode_range_script(0x3005, 0x3005, 'Han').
unicode_range_script(0x3007, 0x3007, 'Han').
unicode_range_script(0x3021, 0x3029, 'Han').
unicode_range_script(0x3038, 0x303B, 'Han').
unicode_range_script(0x3400, 0x4DB5, 'Han').
unicode_range_script(0x4E00, 0x9FCC, 'Han').
unicode_range_script(0xF900, 0xFA6D, 'Han').
unicode_range_script(0xFA70, 0xFAD9, 'Han').
unicode_range_script(0x20000, 0x2A6D6, 'Han').
unicode_range_script(0x2A700, 0x2B734, 'Han').
unicode_range_script(0x2B740, 0x2B81D, 'Han').
unicode_range_script(0x2F800, 0x2FA1D, 'Han').

% Total code points: 75963

% ================================================

unicode_range_script(0xA000, 0xA48C, 'Yi').
unicode_range_script(0xA490, 0xA4C6, 'Yi').

% Total code points: 1220

% ================================================

unicode_range_script(0x10300, 0x1031E, 'Old_Italic').
unicode_range_script(0x10320, 0x10323, 'Old_Italic').

% Total code points: 35

% ================================================

unicode_range_script(0x10330, 0x1034A, 'Gothic').

% Total code points: 27

% ================================================

unicode_range_script(0x10400, 0x1044F, 'Deseret').

% Total code points: 80

% ================================================

unicode_range_script(0x0300, 0x036F, 'Inherited').
unicode_range_script(0x0485, 0x0486, 'Inherited').
unicode_range_script(0x064B, 0x0655, 'Inherited').
unicode_range_script(0x065F, 0x065F, 'Inherited').
unicode_range_script(0x0670, 0x0670, 'Inherited').
unicode_range_script(0x0951, 0x0952, 'Inherited').
unicode_range_script(0x1CD0, 0x1CD2, 'Inherited').
unicode_range_script(0x1CD4, 0x1CE0, 'Inherited').
unicode_range_script(0x1CE2, 0x1CE8, 'Inherited').
unicode_range_script(0x1CED, 0x1CED, 'Inherited').
unicode_range_script(0x1CF4, 0x1CF4, 'Inherited').
unicode_range_script(0x1DC0, 0x1DE6, 'Inherited').
unicode_range_script(0x1DFC, 0x1DFF, 'Inherited').
unicode_range_script(0x200C, 0x200D, 'Inherited').
unicode_range_script(0x20D0, 0x20F0, 'Inherited').
unicode_range_script(0x302A, 0x302D, 'Inherited').
unicode_range_script(0x3099, 0x309A, 'Inherited').
unicode_range_script(0xFE00, 0xFE0F, 'Inherited').
unicode_range_script(0xFE20, 0xFE26, 'Inherited').
unicode_range_script(0x101FD, 0x101FD, 'Inherited').
unicode_range_script(0x1D167, 0x1D169, 'Inherited').
unicode_range_script(0x1D17B, 0x1D182, 'Inherited').
unicode_range_script(0x1D185, 0x1D18B, 'Inherited').
unicode_range_script(0x1D1AA, 0x1D1AD, 'Inherited').
unicode_range_script(0xE0100, 0xE01EF, 'Inherited').

% Total code points: 524

% ================================================

unicode_range_script(0x1700, 0x170C, 'Tagalog').
unicode_range_script(0x170E, 0x1714, 'Tagalog').

% Total code points: 20

% ================================================

unicode_range_script(0x1720, 0x1734, 'Hanunoo').

% Total code points: 21

% ================================================

unicode_range_script(0x1740, 0x1753, 'Buhid').

% Total code points: 20

% ================================================

unicode_range_script(0x1760, 0x176C, 'Tagbanwa').
unicode_range_script(0x176E, 0x1770, 'Tagbanwa').
unicode_range_script(0x1772, 0x1773, 'Tagbanwa').

% Total code points: 18

% ================================================

unicode_range_script(0x1900, 0x191C, 'Limbu').
unicode_range_script(0x1930, 0x193B, 'Limbu').
unicode_range_script(0x1940, 0x1940, 'Limbu').
unicode_range_script(0x1944, 0x194F, 'Limbu').
unicode_range_script(0x1920, 0x192B, 'Limbu').
unicode_range_script(0x1930, 0x193B, 'Limbu').
unicode_range_script(0x1940, 0x1940, 'Limbu').
unicode_range_script(0x1944, 0x194F, 'Limbu').

% Total code points: 66

% ================================================

unicode_range_script(0x1950, 0x196D, 'Tai_Le').
unicode_range_script(0x1970, 0x1974, 'Tai_Le').

% Total code points: 35

% ================================================

unicode_range_script(0x10000, 0x1000B, 'Linear_B').
unicode_range_script(0x1000D, 0x10026, 'Linear_B').
unicode_range_script(0x10028, 0x1003A, 'Linear_B').
unicode_range_script(0x1003C, 0x1003D, 'Linear_B').
unicode_range_script(0x1003F, 0x1004D, 'Linear_B').
unicode_range_script(0x10050, 0x1005D, 'Linear_B').
unicode_range_script(0x10080, 0x100FA, 'Linear_B').

% Total code points: 211

% ================================================

unicode_range_script(0x10380, 0x1039D, 'Ugaritic').
unicode_range_script(0x1039F, 0x1039F, 'Ugaritic').

% Total code points: 31

% ================================================

unicode_range_script(0x10450, 0x1047F, 'Shavian').

% Total code points: 48

% ================================================

unicode_range_script(0x10480, 0x1049D, 'Osmanya').
unicode_range_script(0x104A0, 0x104A9, 'Osmanya').

% Total code points: 40

% ================================================

unicode_range_script(0x10800, 0x10805, 'Cypriot').
unicode_range_script(0x10808, 0x10808, 'Cypriot').
unicode_range_script(0x1080A, 0x10835, 'Cypriot').
unicode_range_script(0x10837, 0x10838, 'Cypriot').
unicode_range_script(0x1083C, 0x1083C, 'Cypriot').
unicode_range_script(0x1083F, 0x1083F, 'Cypriot').

% Total code points: 55

% ================================================

unicode_range_script(0x2800, 0x28FF, 'Braille').

% Total code points: 256

% ================================================

unicode_range_script(0x1A00, 0x1A1B, 'Buginese').
unicode_range_script(0x1A1E, 0x1A1F, 'Buginese').

% Total code points: 30

% ================================================

unicode_range_script(0x03E2, 0x03EF, 'Coptic').
unicode_range_script(0x2C80, 0x2CF3, 'Coptic').
unicode_range_script(0x2CF9, 0x2CFF, 'Coptic').

% Total code points: 137

% ================================================

unicode_range_script(0x1980, 0x19AB, 'New_Tai_Lue').
unicode_range_script(0x19B0, 0x19C9, 'New_Tai_Lue').
unicode_range_script(0x19D0, 0x19DA, 'New_Tai_Lue').
unicode_range_script(0x19DE, 0x19DF, 'New_Tai_Lue').


% Total code points: 83

% ================================================

unicode_range_script(0x2C00, 0x2C2E, 'Glagolitic').
unicode_range_script(0x2C30, 0x2C5E, 'Glagolitic').

% Total code points: 94

% ================================================

unicode_range_script(0x2D30, 0x2D67, 'Tifinagh').
unicode_range_script(0x2D6F, 0x2D70, 'Tifinagh').
unicode_range_script(0x2D7F, 0x2D7F, 'Tifinagh').

% Total code points: 59

% ================================================

unicode_range_script(0xA800, 0xA82B, 'Syloti_Nagri').

% Total code points: 44

% ================================================

unicode_range_script(0x103A0, 0x103C3, 'Old_Persian').
unicode_range_script(0x103C8, 0x103D5, 'Old_Persian').

% Total code points: 50

% ================================================

unicode_range_script(0x10A00, 0x10A03, 'Kharoshthi').
unicode_range_script(0x10A05, 0x10A06, 'Kharoshthi').
unicode_range_script(0x10A0C, 0x10A13, 'Kharoshthi').
unicode_range_script(0x10A15, 0x10A17, 'Kharoshthi').
unicode_range_script(0x10A19, 0x10A33, 'Kharoshthi').
unicode_range_script(0x10A38, 0x10A3A, 'Kharoshthi').
unicode_range_script(0x10A3F, 0x10A47, 'Kharoshthi').
unicode_range_script(0x10A50, 0x10A58, 'Kharoshthi').

% Total code points: 65

% ================================================

unicode_range_script(0x1B00, 0x1B4B, 'Balinese').
unicode_range_script(0x1B50, 0x1B7C, 'Balinese').

% Total code points: 121

% ================================================

unicode_range_script(0x12000, 0x1236E, 'Cuneiform').
unicode_range_script(0x12400, 0x12462, 'Cuneiform').
unicode_range_script(0x12470, 0x12473, 'Cuneiform').

% Total code points: 982

% ================================================

unicode_range_script(0x10900, 0x1091B, 'Phoenician').
unicode_range_script(0x1091F, 0x1091F, 'Phoenician').

% Total code points: 29

% ================================================

unicode_range_script(0xA840, 0xA877, 'Phags_Pa').

% Total code points: 56

% ================================================

unicode_range_script(0x07C0, 0x07FA, 'Nko').

% Total code points: 59

% ================================================

unicode_range_script(0x1B80, 0x1BBF, 'Sundanese').
unicode_range_script(0x1CC0, 0x1CC7, 'Sundanese').

% Total code points: 72

% ================================================

unicode_range_script(0x1C00, 0x1C37, 'Lepcha').
unicode_range_script(0x1C3B, 0x1C49, 'Lepcha').
unicode_range_script(0x1C4D, 0x1C4F, 'Lepcha').

% Total code points: 74

% ================================================

unicode_range_script(0x1C50, 0x1C7F, 'Ol_Chiki').

% Total code points: 48

% ================================================

unicode_range_script(0xA500, 0xA62B, 'Vai').

% Total code points: 300

% ================================================

unicode_range_script(0xA880, 0xA8C4, 'Saurashtra').
unicode_range_script(0xA8CE, 0xA8D9, 'Saurashtra').

% Total code points: 81

% ================================================

unicode_range_script(0xA900, 0xA92F, 'Kayah_Li').

% Total code points: 48

% ================================================

unicode_range_script(0xA930, 0xA953, 'Rejang').
unicode_range_script(0xA95F, 0xA95F, 'Rejang').

% Total code points: 37

% ================================================

unicode_range_script(0x10280, 0x1029C, 'Lycian').

% Total code points: 29

% ================================================

unicode_range_script(0x102A0, 0x102D0, 'Carian').

% Total code points: 49

% ================================================

unicode_range_script(0x10920, 0x10939, 'Lydian').
unicode_range_script(0x1093F, 0x1093F, 'Lydian').

% Total code points: 27

% ================================================

unicode_range_script(0xAA00, 0xAA36, 'Cham').
unicode_range_script(0xAA40, 0xAA4D, 'Cham').
unicode_range_script(0xAA50, 0xAA59, 'Cham').
unicode_range_script(0xAA5C, 0xAA5F, 'Cham').

% Total code points: 83

% ================================================

unicode_range_script(0x1A20, 0x1A5E, 'Tai_Tham').
unicode_range_script(0x1A60, 0x1A7C, 'Tai_Tham').
unicode_range_script(0x1A7F, 0x1A89, 'Tai_Tham').
unicode_range_script(0x1A90, 0x1A99, 'Tai_Tham').
unicode_range_script(0x1AA0, 0x1AAD, 'Tai_Tham').

% Total code points: 127

% ================================================

unicode_range_script(0xAA80, 0xAAC2, 'Tai_Viet').
unicode_range_script(0xAADB, 0xAADF, 'Tai_Viet').

% Total code points: 72

% ================================================

unicode_range_script(0x10B00, 0x10B35, 'Avestan').
unicode_range_script(0x10B39, 0x10B3F, 'Avestan').

% Total code points: 61

% ================================================

unicode_range_script(0x13000, 0x1342E, 'Egyptian_Hieroglyphs').

% Total code points: 1071

% ================================================

unicode_range_script(0x0800, 0x082D, 'Samaritan').
unicode_range_script(0x0830, 0x083E, 'Samaritan').

% Total code points: 61

% ================================================

unicode_range_script(0xA4D0, 0xA4FF, 'Lisu').

% Total code points: 48

% ================================================

unicode_range_script(0xA6A0, 0xA6F7, 'Bamum').
unicode_range_script(0x16800, 0x16A38, 'Bamum').

% Total code points: 657

% ================================================

unicode_range_script(0xA980, 0xA9CD, 'Javanese').
unicode_range_script(0xA9CF, 0xA9D9, 'Javanese').
unicode_range_script(0xA9DE, 0xA9DF, 'Javanese').

% Total code points: 91

% ================================================

unicode_range_script(0xAAE0, 0xAAF6, 'Meetei_Mayek').
unicode_range_script(0xABC0, 0xABED, 'Meetei_Mayek').
unicode_range_script(0xABF0, 0xABF9, 'Meetei_Mayek').

% Total code points: 79

% ================================================

unicode_range_script(0x10840, 0x10855, 'Imperial_Aramaic').
unicode_range_script(0x10857, 0x1085F, 'Imperial_Aramaic').

% Total code points: 31

% ================================================

unicode_range_script(0x10A60, 0x10A7F, 'Old_South_Arabian').

% Total code points: 32

% ================================================

unicode_range_script(0x10B40, 0x10B55, 'Inscriptional_Parthian').
unicode_range_script(0x10B58, 0x10B5F, 'Inscriptional_Parthian').

% Total code points: 30

% ================================================

unicode_range_script(0x10B60, 0x10B72, 'Inscriptional_Pahlavi').
unicode_range_script(0x10B78, 0x10B7F, 'Inscriptional_Pahlavi').

% Total code points: 27

% ================================================

unicode_range_script(0x10C00, 0x10C48, 'Old_Turkic').

% Total code points: 73

% ================================================

unicode_range_script(0x11080, 0x110C1, 'Kaithi').

% Total code points: 66

% ================================================

unicode_range_script(0x1BC0, 0x1BF3, 'Batak').
unicode_range_script(0x1BFC, 0x1BFF, 'Batak').

% Total code points: 56

% ================================================

unicode_range_script(0x11000, 0x1104D, 'Brahmi').
unicode_range_script(0x11052, 0x1106F, 'Brahmi').

% Total code points: 108

% ================================================

unicode_range_script(0x0840, 0x085B, 'Mandaic').
unicode_range_script(0x085E, 0x085E, 'Mandaic').

% Total code points: 29

% ================================================

unicode_range_script(0x11100, 0x11134, 'Chakma').
unicode_range_script(0x11136, 0x11143, 'Chakma').

% Total code points: 67

% ================================================

unicode_range_script(0x109A0, 0x109B7, 'Meroitic_Cursive').
unicode_range_script(0x109BE, 0x109BF, 'Meroitic_Cursive').

% Total code points: 26

% ================================================

unicode_range_script(0x10980, 0x1099F, 'Meroitic_Hieroglyphs').

% Total code points: 32

% ================================================

unicode_range_script(0x16F00, 0x16F44, 'Miao').
unicode_range_script(0x16F50, 0x16F7E, 'Miao').
unicode_range_script(0x16F8F, 0x16F9F, 'Miao').

% Total code points: 133

% ================================================

unicode_range_script(0x11180, 0x111C8, 'Sharada').
unicode_range_script(0x111D0, 0x111D9, 'Sharada').

% Total code points: 83

% ================================================

unicode_range_script(0x110D0, 0x110E8, 'Sora_Sompeng').
unicode_range_script(0x110F0, 0x110F9, 'Sora_Sompeng').

% Total code points: 35

% ================================================

unicode_range_script(0x11680, 0x116B7, 'Takri').
unicode_range_script(0x116C0, 0x116C9, 'Takri').

% Total code points: 66

% EOF
