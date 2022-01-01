%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- category(gui).

	:- multifile(logtalk::question_hook/6).
	:- dynamic(logtalk::question_hook/6).
	:- meta_predicate(question_hook(*, *, *, *, 1, *)).

	logtalk::question_hook(ultimate_question, _, hitchhikers, Tokens, Check, Answer) :-
		tokens_to_text(Tokens, Question),
		java('javax.swing.JFrame')::new(['The Hitchhiker''s Guide to the Galaxy'], Frame),
		% repeat the question until we get the correct answer
		repeat,
			java('javax.swing.JOptionPane', Answer0)::showInputDialog(Frame, Question),
			atom_codes(Answer0, Codes),
			catch(number_codes(Answer, Codes), _, fail),
		call(Check, Answer),
		!,
		java(Frame)::dispose.

	% just a hack for this example
	tokens_to_text([Question-[], _], Question).

:- end_category.
