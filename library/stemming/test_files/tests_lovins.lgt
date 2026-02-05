%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


% Lovins stemmer test cases
% These test cases are derived from the original Lovins algorithm paper
% and common reference implementations


	cover(lovins_stemmer(_)).

	% -----------------------------------------------------------------
	% Empty and short words
	% -----------------------------------------------------------------

	test(lovins_stem_2_empty, true(Stem == "")) :-
		lovins_stemmer::stem("", Stem).

	test(lovins_stem_2_single_char, true(Stem == "a")) :-
		lovins_stemmer::stem("a", Stem).

	test(lovins_stem_2_two_chars, true(Stem == "an")) :-
		lovins_stemmer::stem("an", Stem).

	% -----------------------------------------------------------------
	% Common word endings
	% -----------------------------------------------------------------

	test(lovins_stem_2_ing, true(Stem == "run")) :-
		lovins_stemmer::stem("running", Stem).

	test(lovins_stem_2_tion, true(Stem == "connect")) :-
		lovins_stemmer::stem("connection", Stem).

	test(lovins_stem_2_ation, true(Stem == "gener")) :-
		lovins_stemmer::stem("generation", Stem).

	test(lovins_stem_2_ness, true(Stem == "good")) :-
		lovins_stemmer::stem("goodness", Stem).

	test(lovins_stem_2_ment, true(Stem == "adjust")) :-
		lovins_stemmer::stem("adjustment", Stem).

	test(lovins_stem_2_able, true(Stem == "adjust")) :-
		lovins_stemmer::stem("adjustable", Stem).

	test(lovins_stem_2_ible, true(Stem == "defens")) :-
		lovins_stemmer::stem("defensible", Stem).

	test(lovins_stem_2_al, true(Stem == "reviv")) :-
		lovins_stemmer::stem("revival", Stem).

	test(lovins_stem_2_ful, true(Stem == "hope")) :-
		lovins_stemmer::stem("hopeful", Stem).

	test(lovins_stem_2_less, true(Stem == "hope")) :-
		lovins_stemmer::stem("hopeless", Stem).

	test(lovins_stem_2_ous, true(Stem == "gener")) :-
		lovins_stemmer::stem("generous", Stem).

	test(lovins_stem_2_ive, true(Stem == "effect")) :-
		lovins_stemmer::stem("effective", Stem).

	test(lovins_stem_2_ly, true(Stem == "quick")) :-
		lovins_stemmer::stem("quickly", Stem).

	test(lovins_stem_2_ed, true(Stem == "walk")) :-
		lovins_stemmer::stem("walked", Stem).

	test(lovins_stem_2_er, true(Stem == "walk")) :-
		lovins_stemmer::stem("walker", Stem).

	test(lovins_stem_2_est, true(Stem == "larg")) :-
		lovins_stemmer::stem("largest", Stem).

	% -----------------------------------------------------------------
	% Plurals
	% -----------------------------------------------------------------

	test(lovins_stem_2_s_cats, true(Stem == "cat")) :-
		lovins_stemmer::stem("cats", Stem).

	test(lovins_stem_2_es, true(Stem == "box")) :-
		lovins_stemmer::stem("boxes", Stem).

	test(lovins_stem_2_ies, true(Stem == "stor")) :-
		lovins_stemmer::stem("stories", Stem).

	% -----------------------------------------------------------------
	% Verb forms
	% -----------------------------------------------------------------

	test(lovins_stem_2_walks, true(Stem == "walk")) :-
		lovins_stemmer::stem("walks", Stem).

	test(lovins_stem_2_walking, true(Stem == "walk")) :-
		lovins_stemmer::stem("walking", Stem).

	test(lovins_stem_2_generates, true(Stem == "gener")) :-
		lovins_stemmer::stem("generates", Stem).

	test(lovins_stem_2_generated, true(Stem == "gener")) :-
		lovins_stemmer::stem("generated", Stem).

	test(lovins_stem_2_generating, true(Stem == "gener")) :-
		lovins_stemmer::stem("generating", Stem).

	% -----------------------------------------------------------------
	% Complex suffixes
	% -----------------------------------------------------------------

	test(lovins_stem_2_ization, true(Stem == "organ")) :-
		lovins_stemmer::stem("organization", Stem).

	test(lovins_stem_2_ational, true(Stem == "nat")) :-
		lovins_stemmer::stem("national", Stem).

	test(lovins_stem_2_ionally, true(Stem == "nat")) :-
		lovins_stemmer::stem("nationally", Stem).

	test(lovins_stem_2_fully, true(Stem == "hope")) :-
		lovins_stemmer::stem("hopefully", Stem).

	test(lovins_stem_2_fulness, true(Stem == "hope")) :-
		lovins_stemmer::stem("hopefulness", Stem).

	test(lovins_stem_2_ousness, true(Stem == "gener")) :-
		lovins_stemmer::stem("generousness", Stem).

	test(lovins_stem_2_iveness, true(Stem == "effect")) :-
		lovins_stemmer::stem("effectiveness", Stem).

	test(lovins_stem_2_ibility, true(Stem == "flec")) :-
		lovins_stemmer::stem("flexibility", Stem).

	test(lovins_stem_2_ability, true(Stem == "avail")) :-
		lovins_stemmer::stem("availability", Stem).

	% -----------------------------------------------------------------
	% Words with various suffixes from the Lovins paper
	% -----------------------------------------------------------------

	test(lovins_stem_2_absorb, true(Stem == "absorb")) :-
		lovins_stemmer::stem("absorb", Stem).

	test(lovins_stem_2_absorption, true(Stem == "absorb")) :-
		lovins_stemmer::stem("absorption", Stem).

	test(lovins_stem_2_active, true(Stem == "act")) :-
		lovins_stemmer::stem("active", Stem).

	test(lovins_stem_2_activate, true(Stem == "activ")) :-
		lovins_stemmer::stem("activate", Stem).

	test(lovins_stem_2_activation, true(Stem == "activ")) :-
		lovins_stemmer::stem("activation", Stem).

	test(lovins_stem_2_activity, true(Stem == "act")) :-
		lovins_stemmer::stem("activity", Stem).

	test(lovins_stem_2_addition, true(Stem == "addit")) :-
		lovins_stemmer::stem("addition", Stem).

	test(lovins_stem_2_additional, true(Stem == "addit")) :-
		lovins_stemmer::stem("additional", Stem).

	test(lovins_stem_2_apply, true(Stem == "ap")) :-
		lovins_stemmer::stem("apply", Stem).

	test(lovins_stem_2_application, true(Stem == "applic")) :-
		lovins_stemmer::stem("application", Stem).

	test(lovins_stem_2_applied, true(Stem == "appl")) :-
		lovins_stemmer::stem("applied", Stem).

	test(lovins_stem_2_beautiful, true(Stem == "beaut")) :-
		lovins_stemmer::stem("beautiful", Stem).

	test(lovins_stem_2_beauty, true(Stem == "beaut")) :-
		lovins_stemmer::stem("beauty", Stem).

	test(lovins_stem_2_communicate, true(Stem == "communic")) :-
		lovins_stemmer::stem("communicate", Stem).

	test(lovins_stem_2_communication, true(Stem == "communic")) :-
		lovins_stemmer::stem("communication", Stem).

	test(lovins_stem_2_compute, true(Stem == "comput")) :-
		lovins_stemmer::stem("compute", Stem).

	test(lovins_stem_2_computer, true(Stem == "comput")) :-
		lovins_stemmer::stem("computer", Stem).

	test(lovins_stem_2_computing, true(Stem == "comput")) :-
		lovins_stemmer::stem("computing", Stem).

	test(lovins_stem_2_creation, true(Stem == "cre")) :-
		lovins_stemmer::stem("creation", Stem).

	test(lovins_stem_2_creative, true(Stem == "cre")) :-
		lovins_stemmer::stem("creative", Stem).

	test(lovins_stem_2_different, true(Stem == "differ")) :-
		lovins_stemmer::stem("different", Stem).

	test(lovins_stem_2_difference, true(Stem == "differ")) :-
		lovins_stemmer::stem("difference", Stem).

	test(lovins_stem_2_education, true(Stem == "educ")) :-
		lovins_stemmer::stem("education", Stem).

	test(lovins_stem_2_educational, true(Stem == "educ")) :-
		lovins_stemmer::stem("educational", Stem).

	test(lovins_stem_2_electric, true(Stem == "electr")) :-
		lovins_stemmer::stem("electric", Stem).

	test(lovins_stem_2_electrical, true(Stem == "electr")) :-
		lovins_stemmer::stem("electrical", Stem).

	test(lovins_stem_2_electricity, true(Stem == "electr")) :-
		lovins_stemmer::stem("electricity", Stem).

	test(lovins_stem_2_function, true(Stem == "funct")) :-
		lovins_stemmer::stem("function", Stem).

	test(lovins_stem_2_functional, true(Stem == "funct")) :-
		lovins_stemmer::stem("functional", Stem).

	test(lovins_stem_2_information, true(Stem == "inform")) :-
		lovins_stemmer::stem("information", Stem).

	test(lovins_stem_2_informative, true(Stem == "inform")) :-
		lovins_stemmer::stem("informative", Stem).

	test(lovins_stem_2_natural, true(Stem == "natur")) :-
		lovins_stemmer::stem("natural", Stem).

	test(lovins_stem_2_naturally, true(Stem == "natur")) :-
		lovins_stemmer::stem("naturally", Stem).

	test(lovins_stem_2_operation, true(Stem == "oper")) :-
		lovins_stemmer::stem("operation", Stem).

	test(lovins_stem_2_operational, true(Stem == "oper")) :-
		lovins_stemmer::stem("operational", Stem).

	test(lovins_stem_2_personal, true(Stem == "person")) :-
		lovins_stemmer::stem("personal", Stem).

	test(lovins_stem_2_personally, true(Stem == "person")) :-
		lovins_stemmer::stem("personally", Stem).

	test(lovins_stem_2_possible, true(Stem == "pos")) :-
		lovins_stemmer::stem("possible", Stem).

	test(lovins_stem_2_possibility, true(Stem == "pos")) :-
		lovins_stemmer::stem("possibility", Stem).

	test(lovins_stem_2_production, true(Stem == "produc")) :-
		lovins_stemmer::stem("production", Stem).

	test(lovins_stem_2_productive, true(Stem == "produc")) :-
		lovins_stemmer::stem("productive", Stem).

	test(lovins_stem_2_reason, true(Stem == "reason")) :-
		lovins_stemmer::stem("reason", Stem).

	test(lovins_stem_2_reasonable, true(Stem == "reason")) :-
		lovins_stemmer::stem("reasonable", Stem).

	test(lovins_stem_2_science, true(Stem == "sci")) :-
		lovins_stemmer::stem("science", Stem).

	test(lovins_stem_2_scientific, true(Stem == "scientif")) :-
		lovins_stemmer::stem("scientific", Stem).

	test(lovins_stem_2_social, true(Stem == "soc")) :-
		lovins_stemmer::stem("social", Stem).

	test(lovins_stem_2_society, true(Stem == "socies")) :-
		lovins_stemmer::stem("society", Stem).

	test(lovins_stem_2_special, true(Stem == "spec")) :-
		lovins_stemmer::stem("special", Stem).

	test(lovins_stem_2_specially, true(Stem == "spec")) :-
		lovins_stemmer::stem("specially", Stem).

	test(lovins_stem_2_technical, true(Stem == "techn")) :-
		lovins_stemmer::stem("technical", Stem).

	test(lovins_stem_2_technology, true(Stem == "techn")) :-
		lovins_stemmer::stem("technology", Stem).

	test(lovins_stem_2_understand, true(Stem == "understand")) :-
		lovins_stemmer::stem("understand", Stem).

	test(lovins_stem_2_understanding, true(Stem == "understand")) :-
		lovins_stemmer::stem("understanding", Stem).

	% -----------------------------------------------------------------
	% stems/2 tests
	% -----------------------------------------------------------------

	test(lovins_stems_2_empty_list, true(Stems == [])) :-
		lovins_stemmer::stems([], Stems).

	test(lovins_stems_2_single, true(Stems == ["run"])) :-
		lovins_stemmer::stems(["running"], Stems).

	test(lovins_stems_2_multiple, true(Stems == ["run", "walk", "eas"])) :-
		lovins_stemmer::stems(["running", "walks", "easily"], Stems).

	test(lovins_stems_2_various, true(Stems == ["connect", "gener", "beaut"])) :-
		lovins_stemmer::stems(["connection", "generate", "beautiful"], Stems).
