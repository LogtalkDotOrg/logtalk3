%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Integration file for Quintus Prolog
%  Last updated on October 11, 2013
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


quintus_reverse_slashes([], []).
quintus_reverse_slashes([Char| Chars], [ConvertedChar| ConvertedChars]) :-
	(	Char =:= 0'\ ->
		ConvertedChar = 0'/
	;	ConvertedChar = Char
	),
	quintus_reverse_slashes(Chars, ConvertedChars).

:- 	unix(args([LOGTALKHOME| _])),	% hack for workaround the lack of support for environment variables in file names
	atom_chars(LOGTALKHOME, LH0),
	quintus_reverse_slashes(LH0, LH),
	atom_chars('/adapters/quintus.pl', LC), append(LH, LC, L1), atom_chars(AdapterFile, L1), compile(AdapterFile),
	atom_chars('/paths/paths.pl', LP), append(LH, LP, L3), atom_chars(PathsFile, L3), compile(PathsFile),
	atom_chars('/core/core.pl', LL), append(LH, LL, L2), atom_chars(CompilerFile, L2), compile(CompilerFile).
