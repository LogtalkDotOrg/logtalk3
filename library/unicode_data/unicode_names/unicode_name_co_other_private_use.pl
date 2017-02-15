%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of VivoMind Prolog Unicode Resources  
%  
%  VivoMind Prolog Unicode Resources is free software distributed using the
%  Creative Commons CC0 1.0 Universal (CC0 1.0) - Public Domain Dedication
%  license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Last modified: March 13, 2012

unicode_name(0xE000, '<Private Use, First>').
unicode_name(0xF8FF, '<Private Use, Last>').
unicode_name(0xF0000, '<Plane 15 Private Use, First>').
unicode_name(0xFFFFD, '<Plane 15 Private Use, Last>').
unicode_name(0x100000, '<Plane 16 Private Use, First>').
unicode_name(0x10FFFD, '<Plane 16 Private Use, Last>').


unicode_name(CodePoint, '<Private Use>') :-
	between(0xE001, 0xF8FE, CodePoint).

