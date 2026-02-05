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


% Porter stemmer test cases
% These test cases are derived from the original Porter algorithm paper
% and common reference implementations (NLTK, Snowball, etc.)


	cover(porter_stemmer(_)).

	% -----------------------------------------------------------------
	% Empty and short words
	% -----------------------------------------------------------------

	test(porter_stem_2_empty, true(Stem == "")) :-
		porter_stemmer::stem("", Stem).

	test(porter_stem_2_single_char, true(Stem == "a")) :-
		porter_stemmer::stem("a", Stem).

	test(porter_stem_2_two_chars, true(Stem == "an")) :-
		porter_stemmer::stem("an", Stem).

	% -----------------------------------------------------------------
	% Step 1a: Plurals (SSES, IES, SS, S)
	% -----------------------------------------------------------------

	test(porter_stem_2_sses, true(Stem == "caress")) :-
		porter_stemmer::stem("caresses", Stem).

	test(porter_stem_2_ies, true(Stem == "poni")) :-
		porter_stemmer::stem("ponies", Stem).

	test(porter_stem_2_ties, true(Stem == "ti")) :-
		porter_stemmer::stem("ties", Stem).

	test(porter_stem_2_ss, true(Stem == "caress")) :-
		porter_stemmer::stem("caress", Stem).

	test(porter_stem_2_s_cats, true(Stem == "cat")) :-
		porter_stemmer::stem("cats", Stem).

	% -----------------------------------------------------------------
	% Step 1b: Past participles and progressive forms (EED, ED, ING)
	% -----------------------------------------------------------------

	test(porter_stem_2_eed_agreed, true(Stem == "agre")) :-
		porter_stemmer::stem("agreed", Stem).

	test(porter_stem_2_eed_feed, true(Stem == "feed")) :-
		porter_stemmer::stem("feed", Stem).

	test(porter_stem_2_ed_plastered, true(Stem == "plaster")) :-
		porter_stemmer::stem("plastered", Stem).

	test(porter_stem_2_ed_bled, true(Stem == "bled")) :-
		porter_stemmer::stem("bled", Stem).

	test(porter_stem_2_ing_motoring, true(Stem == "motor")) :-
		porter_stemmer::stem("motoring", Stem).

	test(porter_stem_2_ing_sing, true(Stem == "sing")) :-
		porter_stemmer::stem("sing", Stem).

	test(porter_stem_2_ing_conflating, true(Stem == "conflat")) :-
		porter_stemmer::stem("conflating", Stem).

	test(porter_stem_2_ing_troubling, true(Stem == "troubl")) :-
		porter_stemmer::stem("troubling", Stem).

	test(porter_stem_2_ing_sized, true(Stem == "size")) :-
		porter_stemmer::stem("sized", Stem).

	test(porter_stem_2_ing_hopping, true(Stem == "hop")) :-
		porter_stemmer::stem("hopping", Stem).

	test(porter_stem_2_ing_tanned, true(Stem == "tan")) :-
		porter_stemmer::stem("tanned", Stem).

	test(porter_stem_2_ing_failing, true(Stem == "fail")) :-
		porter_stemmer::stem("failing", Stem).

	test(porter_stem_2_ing_filing, true(Stem == "file")) :-
		porter_stemmer::stem("filing", Stem).

	% -----------------------------------------------------------------
	% Step 1c: Y -> I
	% -----------------------------------------------------------------

	test(porter_stem_2_y_happy, true(Stem == "happi")) :-
		porter_stemmer::stem("happy", Stem).

	test(porter_stem_2_y_sky, true(Stem == "sky")) :-
		porter_stemmer::stem("sky", Stem).

	% -----------------------------------------------------------------
	% Step 2: Suffix removal
	% -----------------------------------------------------------------

	test(porter_stem_2_ational, true(Stem == "relat")) :-
		porter_stemmer::stem("relational", Stem).

	test(porter_stem_2_tional, true(Stem == "condit")) :-
		porter_stemmer::stem("conditional", Stem).

	test(porter_stem_2_enci, true(Stem == "valenc")) :-
		porter_stemmer::stem("valenci", Stem).

	test(porter_stem_2_anci, true(Stem == "hesit")) :-
		porter_stemmer::stem("hesitanci", Stem).

	test(porter_stem_2_izer, true(Stem == "digit")) :-
		porter_stemmer::stem("digitizer", Stem).

	test(porter_stem_2_abli, true(Stem == "conform")) :-
		porter_stemmer::stem("conformabli", Stem).

	test(porter_stem_2_alli, true(Stem == "radic")) :-
		porter_stemmer::stem("radicalli", Stem).

	test(porter_stem_2_entli, true(Stem == "differ")) :-
		porter_stemmer::stem("differentli", Stem).

	test(porter_stem_2_eli, true(Stem == "vile")) :-
		porter_stemmer::stem("vileli", Stem).

	test(porter_stem_2_ousli, true(Stem == "analog")) :-
		porter_stemmer::stem("analogousli", Stem).

	test(porter_stem_2_ization, true(Stem == "vietnam")) :-
		porter_stemmer::stem("vietnamization", Stem).

	test(porter_stem_2_ation, true(Stem == "predic")) :-
		porter_stemmer::stem("predication", Stem).

	test(porter_stem_2_ator, true(Stem == "oper")) :-
		porter_stemmer::stem("operator", Stem).

	test(porter_stem_2_alism, true(Stem == "feudal")) :-
		porter_stemmer::stem("feudalism", Stem).

	test(porter_stem_2_iveness, true(Stem == "decis")) :-
		porter_stemmer::stem("decisiveness", Stem).

	test(porter_stem_2_fulness, true(Stem == "hope")) :-
		porter_stemmer::stem("hopefulness", Stem).

	test(porter_stem_2_ousness, true(Stem == "callous")) :-
		porter_stemmer::stem("callousness", Stem).

	test(porter_stem_2_aliti, true(Stem == "formal")) :-
		porter_stemmer::stem("formaliti", Stem).

	test(porter_stem_2_iviti, true(Stem == "sensit")) :-
		porter_stemmer::stem("sensitiviti", Stem).

	test(porter_stem_2_biliti, true(Stem == "sensibl")) :-
		porter_stemmer::stem("sensibiliti", Stem).

	% -----------------------------------------------------------------
	% Step 3: Suffix removal
	% -----------------------------------------------------------------

	test(porter_stem_2_icate, true(Stem == "triplic")) :-
		porter_stemmer::stem("triplicate", Stem).

	test(porter_stem_2_ative, true(Stem == "form")) :-
		porter_stemmer::stem("formative", Stem).

	test(porter_stem_2_alize, true(Stem == "formal")) :-
		porter_stemmer::stem("formalize", Stem).

	test(porter_stem_2_iciti, true(Stem == "electr")) :-
		porter_stemmer::stem("electriciti", Stem).

	test(porter_stem_2_ical, true(Stem == "electr")) :-
		porter_stemmer::stem("electrical", Stem).

	test(porter_stem_2_ful, true(Stem == "hope")) :-
		porter_stemmer::stem("hopeful", Stem).

	test(porter_stem_2_ness, true(Stem == "good")) :-
		porter_stemmer::stem("goodness", Stem).

	% -----------------------------------------------------------------
	% Step 4: Suffix removal with m > 1
	% -----------------------------------------------------------------

	test(porter_stem_2_al, true(Stem == "reviv")) :-
		porter_stemmer::stem("revival", Stem).

	test(porter_stem_2_ance, true(Stem == "allow")) :-
		porter_stemmer::stem("allowance", Stem).

	test(porter_stem_2_ence, true(Stem == "infer")) :-
		porter_stemmer::stem("inference", Stem).

	test(porter_stem_2_er, true(Stem == "airlin")) :-
		porter_stemmer::stem("airliner", Stem).

	test(porter_stem_2_ic, true(Stem == "gyroscop")) :-
		porter_stemmer::stem("gyroscopic", Stem).

	test(porter_stem_2_able, true(Stem == "adjust")) :-
		porter_stemmer::stem("adjustable", Stem).

	test(porter_stem_2_ible, true(Stem == "defens")) :-
		porter_stemmer::stem("defensible", Stem).

	test(porter_stem_2_ant, true(Stem == "irrit")) :-
		porter_stemmer::stem("irritant", Stem).

	test(porter_stem_2_ement, true(Stem == "replac")) :-
		porter_stemmer::stem("replacement", Stem).

	test(porter_stem_2_ment, true(Stem == "adjust")) :-
		porter_stemmer::stem("adjustment", Stem).

	test(porter_stem_2_ent, true(Stem == "depend")) :-
		porter_stemmer::stem("dependent", Stem).

	test(porter_stem_2_ion, true(Stem == "adopt")) :-
		porter_stemmer::stem("adoption", Stem).

	test(porter_stem_2_ism, true(Stem == "commun")) :-
		porter_stemmer::stem("communism", Stem).

	test(porter_stem_2_ate, true(Stem == "activ")) :-
		porter_stemmer::stem("activate", Stem).

	test(porter_stem_2_iti, true(Stem == "angular")) :-
		porter_stemmer::stem("angulariti", Stem).

	test(porter_stem_2_ous, true(Stem == "homolog")) :-
		porter_stemmer::stem("homologous", Stem).

	test(porter_stem_2_ive, true(Stem == "effect")) :-
		porter_stemmer::stem("effective", Stem).

	test(porter_stem_2_ize, true(Stem == "bowdler")) :-
		porter_stemmer::stem("bowdlerize", Stem).

	% -----------------------------------------------------------------
	% Step 5: Cleanup
	% -----------------------------------------------------------------

	test(porter_stem_2_final_e, true(Stem == "probat")) :-
		porter_stemmer::stem("probate", Stem).

	test(porter_stem_2_double_l, true(Stem == "control")) :-
		porter_stemmer::stem("controll", Stem).

	test(porter_stem_2_roll, true(Stem == "roll")) :-
		porter_stemmer::stem("roll", Stem).

	% -----------------------------------------------------------------
	% Common words - reference test cases from NLTK/Snowball
	% -----------------------------------------------------------------

	test(porter_stem_2_connect, true(Stem == "connect")) :-
		porter_stemmer::stem("connection", Stem).

	test(porter_stem_2_argue, true(Stem == "argu")) :-
		porter_stemmer::stem("argue", Stem).

	test(porter_stem_2_arguing, true(Stem == "argu")) :-
		porter_stemmer::stem("arguing", Stem).

	test(porter_stem_2_argued, true(Stem == "argu")) :-
		porter_stemmer::stem("argued", Stem).

	test(porter_stem_2_argues, true(Stem == "argu")) :-
		porter_stemmer::stem("argues", Stem).

	test(porter_stem_2_argument, true(Stem == "argument")) :-
		porter_stemmer::stem("argument", Stem).

	test(porter_stem_2_run, true(Stem == "run")) :-
		porter_stemmer::stem("run", Stem).

	test(porter_stem_2_running, true(Stem == "run")) :-
		porter_stemmer::stem("running", Stem).

	test(porter_stem_2_runs, true(Stem == "run")) :-
		porter_stemmer::stem("runs", Stem).

	test(porter_stem_2_walk, true(Stem == "walk")) :-
		porter_stemmer::stem("walk", Stem).

	test(porter_stem_2_walking, true(Stem == "walk")) :-
		porter_stemmer::stem("walking", Stem).

	test(porter_stem_2_walks, true(Stem == "walk")) :-
		porter_stemmer::stem("walks", Stem).

	test(porter_stem_2_walked, true(Stem == "walk")) :-
		porter_stemmer::stem("walked", Stem).

	test(porter_stem_2_easily, true(Stem == "easili")) :-
		porter_stemmer::stem("easily", Stem).

	test(porter_stem_2_easy, true(Stem == "easi")) :-
		porter_stemmer::stem("easy", Stem).

	test(porter_stem_2_easier, true(Stem == "easier")) :-
		porter_stemmer::stem("easier", Stem).

	test(porter_stem_2_beautiful, true(Stem == "beauti")) :-
		porter_stemmer::stem("beautiful", Stem).

	test(porter_stem_2_beauty, true(Stem == "beauti")) :-
		porter_stemmer::stem("beauty", Stem).

	test(porter_stem_2_generation, true(Stem == "gener")) :-
		porter_stemmer::stem("generation", Stem).

	test(porter_stem_2_generate, true(Stem == "gener")) :-
		porter_stemmer::stem("generate", Stem).

	test(porter_stem_2_generates, true(Stem == "gener")) :-
		porter_stemmer::stem("generates", Stem).

	test(porter_stem_2_generated, true(Stem == "gener")) :-
		porter_stemmer::stem("generated", Stem).

	test(porter_stem_2_generating, true(Stem == "gener")) :-
		porter_stemmer::stem("generating", Stem).

	test(porter_stem_2_general, true(Stem == "gener")) :-
		porter_stemmer::stem("general", Stem).

	test(porter_stem_2_generally, true(Stem == "gener")) :-
		porter_stemmer::stem("generally", Stem).

	test(porter_stem_2_generous, true(Stem == "gener")) :-
		porter_stemmer::stem("generous", Stem).

	test(porter_stem_2_generosity, true(Stem == "generos")) :-
		porter_stemmer::stem("generosity", Stem).

	% -----------------------------------------------------------------
	% stems/2 tests
	% -----------------------------------------------------------------

	test(porter_stems_2_empty_list, true(Stems == [])) :-
		porter_stemmer::stems([], Stems).

	test(porter_stems_2_single, true(Stems == ["run"])) :-
		porter_stemmer::stems(["running"], Stems).

	test(porter_stems_2_multiple, true(Stems == ["run", "walk", "easili"])) :-
		porter_stemmer::stems(["running", "walks", "easily"], Stems).

	test(porter_stems_2_various, true(Stems == ["connect", "gener", "beauti"])) :-
		porter_stemmer::stems(["connection", "generate", "beautiful"], Stems).
