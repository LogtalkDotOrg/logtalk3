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


:- object(english_lemmatizer,
	implements(lemmatizer_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'English lemmatization provider using curated exceptions, base forms, and conservative inflection rules.',
		see_also is [lemmatizer(_, _)]
	]).

	:- uses(list, [
		append/3, member/2, remove_duplicates/2
	]).

	lemma(Word, Lemma) :-
		findall(Candidate, (part_of_speech(PartOfSpeech), lemma(Word, PartOfSpeech, Candidate)), Candidates),
		remove_duplicates(Candidates, Lemmas),
		member(Lemma, Lemmas).

	lemma(Word, PartOfSpeech, Lemma) :-
		findall(Candidate, lemma_candidate(Word, PartOfSpeech, Candidate), Candidates),
		remove_duplicates(Candidates, Lemmas),
		member(Lemma, Lemmas).

	part_of_speech(noun).
	part_of_speech(verb).
	part_of_speech(adjective).
	part_of_speech(adverb).

	lemma_candidate(Word, PartOfSpeech, Lemma) :-
		exception(Word, PartOfSpeech, Lemma).
	lemma_candidate(Word, PartOfSpeech, Lemma) :-
		dictionary_lemma(Word, PartOfSpeech, Lemma).
	lemma_candidate(Word, PartOfSpeech, Lemma) :-
		rule_lemma(Word, PartOfSpeech, Lemma).

	dictionary_lemma(Word, PartOfSpeech, Word) :-
		base_form(PartOfSpeech, Word).

	rule_lemma(Word, noun, Lemma) :-
		atom_concat(Stem, ies, Word),
		atom_concat(Stem, y, Lemma),
		base_form(noun, Lemma).
	rule_lemma(Word, noun, Lemma) :-
		atom_concat(Lemma, es, Word),
		noun_es_base(Lemma),
		base_form(noun, Lemma).
	rule_lemma(Word, noun, Lemma) :-
		atom_concat(Lemma, s, Word),
		base_form(noun, Lemma).

	rule_lemma(Word, verb, Lemma) :-
		atom_concat(Stem, ies, Word),
		atom_concat(Stem, y, Lemma),
		base_form(verb, Lemma).
	rule_lemma(Word, verb, Lemma) :-
		atom_concat(Lemma, es, Word),
		verb_es_base(Lemma),
		base_form(verb, Lemma).
	rule_lemma(Word, verb, Lemma) :-
		atom_concat(Lemma, s, Word),
		base_form(verb, Lemma).
	rule_lemma(Word, verb, Lemma) :-
		atom_concat(Stem, ied, Word),
		atom_concat(Stem, y, Lemma),
		base_form(verb, Lemma).
	rule_lemma(Word, verb, Lemma) :-
		atom_concat(Stem, ed, Word),
		verb_stem_candidate(Stem, Lemma),
		base_form(verb, Lemma).
	rule_lemma(Word, verb, Lemma) :-
		atom_concat(Stem, ing, Word),
		verb_stem_candidate(Stem, Lemma),
		base_form(verb, Lemma).

	rule_lemma(Word, adjective, Lemma) :-
		atom_concat(Stem, ier, Word),
		atom_concat(Stem, y, Lemma),
		base_form(adjective, Lemma).
	rule_lemma(Word, adjective, Lemma) :-
		atom_concat(Stem, iest, Word),
		atom_concat(Stem, y, Lemma),
		base_form(adjective, Lemma).
	rule_lemma(Word, adjective, Lemma) :-
		atom_concat(Stem, er, Word),
		adjective_stem_candidate(Stem, Lemma),
		base_form(adjective, Lemma).
	rule_lemma(Word, adjective, Lemma) :-
		atom_concat(Stem, est, Word),
		adjective_stem_candidate(Stem, Lemma),
		base_form(adjective, Lemma).

	verb_stem_candidate(Stem, Stem).
	verb_stem_candidate(Stem, Lemma) :-
		atom_concat(Stem, e, Lemma).
	verb_stem_candidate(Stem, Lemma) :-
		remove_doubled_consonant(Stem, Lemma).

	adjective_stem_candidate(Stem, Stem).
	adjective_stem_candidate(Stem, Lemma) :-
		atom_concat(Stem, e, Lemma).
	adjective_stem_candidate(Stem, Lemma) :-
		remove_doubled_consonant(Stem, Lemma).

	remove_doubled_consonant(Atom, Undoubled) :-
		atom_chars(Atom, Chars),
		append(Prefix, [Character, Character], Chars),
		consonant(Character),
		append(Prefix, [Character], UndoubledChars),
		atom_chars(Undoubled, UndoubledChars).

	noun_es_base(Base) :-
		member(Suffix, [s, x, z, ch, sh, o]),
		atom_concat(_, Suffix, Base).

	verb_es_base(Base) :-
		member(Suffix, [s, x, z, ch, sh, o]),
		atom_concat(_, Suffix, Base).

	consonant(b).
	consonant(c).
	consonant(d).
	consonant(f).
	consonant(g).
	consonant(h).
	consonant(j).
	consonant(k).
	consonant(l).
	consonant(m).
	consonant(n).
	consonant(p).
	consonant(q).
	consonant(r).
	consonant(s).
	consonant(t).
	consonant(v).
	consonant(w).
	consonant(x).
	consonant(y).
	consonant(z).

	exception(men, noun, man).
	exception(women, noun, woman).
	exception(children, noun, child).
	exception(mice, noun, mouse).
	exception(geese, noun, goose).
	exception(people, noun, person).
	exception(teeth, noun, tooth).
	exception(feet, noun, foot).
	exception(oxen, noun, ox).
	exception(leaves, noun, leaf).
	exception(lives, noun, life).
	exception(wives, noun, wife).
	exception(knives, noun, knife).
	exception(wolves, noun, wolf).
	exception(loaves, noun, loaf).
	exception(shelves, noun, shelf).
	exception(calves, noun, calf).
	exception(halves, noun, half).
	exception(scarves, noun, scarf).
	exception(thieves, noun, thief).
	exception(analyses, noun, analysis).
	exception(bases, noun, basis).
	exception(crises, noun, crisis).
	exception(phenomena, noun, phenomenon).
	exception(criteria, noun, criterion).
	exception(data, noun, datum).
	exception(indices, noun, index).
	exception(matrices, noun, matrix).
	exception(axes, noun, axis).

	exception(am, verb, be).
	exception(is, verb, be).
	exception(are, verb, be).
	exception(was, verb, be).
	exception(were, verb, be).
	exception(been, verb, be).
	exception(being, verb, be).
	exception(has, verb, have).
	exception(had, verb, have).
	exception(having, verb, have).
	exception(does, verb, do).
	exception(did, verb, do).
	exception(done, verb, do).
	exception(went, verb, go).
	exception(gone, verb, go).
	exception(saw, verb, see).
	exception(seen, verb, see).
	exception(ran, verb, run).
	exception(took, verb, take).
	exception(taken, verb, take).
	exception(came, verb, come).
	exception(got, verb, get).
	exception(given, verb, give).
	exception(gave, verb, give).
	exception(knew, verb, know).
	exception(known, verb, know).
	exception(thought, verb, think).
	exception(said, verb, say).
	exception(told, verb, tell).
	exception(found, verb, find).
	exception(became, verb, become).
	exception(shown, verb, show).
	exception(left, verb, leave).
	exception(felt, verb, feel).
	exception(brought, verb, bring).
	exception(began, verb, begin).
	exception(begun, verb, begin).
	exception(kept, verb, keep).
	exception(held, verb, hold).
	exception(wrote, verb, write).
	exception(written, verb, write).
	exception(stood, verb, stand).
	exception(heard, verb, hear).
	exception(meant, verb, mean).
	exception(met, verb, meet).
	exception(paid, verb, pay).
	exception(sat, verb, sit).
	exception(spoke, verb, speak).
	exception(spoken, verb, speak).
	exception(lay, verb, lie).
	exception(lain, verb, lie).
	exception(led, verb, lead).
	exception(grew, verb, grow).
	exception(grown, verb, grow).
	exception(lost, verb, lose).
	exception(fell, verb, fall).
	exception(fallen, verb, fall).
	exception(sent, verb, send).
	exception(built, verb, build).
	exception(understood, verb, understand).
	exception(drew, verb, draw).
	exception(drawn, verb, draw).
	exception(broke, verb, break).
	exception(broken, verb, break).
	exception(spent, verb, spend).
	exception(rose, verb, rise).
	exception(risen, verb, rise).
	exception(drove, verb, drive).
	exception(driven, verb, drive).
	exception(bought, verb, buy).
	exception(wore, verb, wear).
	exception(worn, verb, wear).
	exception(chose, verb, choose).
	exception(chosen, verb, choose).
	exception(ate, verb, eat).
	exception(eaten, verb, eat).
	exception(drank, verb, drink).
	exception(drunk, verb, drink).
	exception(slept, verb, sleep).
	exception(swimming, verb, swim).
	exception(swam, verb, swim).
	exception(swum, verb, swim).

	exception(better, adjective, good).
	exception(best, adjective, good).
	exception(worse, adjective, bad).
	exception(worst, adjective, bad).
	exception(farther, adjective, far).
	exception(farthest, adjective, far).
	exception(further, adjective, far).
	exception(furthest, adjective, far).
	exception(less, adjective, little).
	exception(least, adjective, little).
	exception(more, adjective, many).
	exception(most, adjective, many).

	exception(better, adverb, well).
	exception(best, adverb, well).
	exception(worse, adverb, badly).
	exception(worst, adverb, badly).
	exception(farther, adverb, far).
	exception(farthest, adverb, far).
	exception(further, adverb, far).
	exception(furthest, adverb, far).
	exception(less, adverb, little).
	exception(least, adverb, little).
	exception(more, adverb, much).
	exception(most, adverb, much).

	base_form(noun, man).
	base_form(noun, woman).
	base_form(noun, child).
	base_form(noun, mouse).
	base_form(noun, goose).
	base_form(noun, person).
	base_form(noun, tooth).
	base_form(noun, foot).
	base_form(noun, ox).
	base_form(noun, analysis).
	base_form(noun, basis).
	base_form(noun, crisis).
	base_form(noun, phenomenon).
	base_form(noun, criterion).
	base_form(noun, datum).
	base_form(noun, index).
	base_form(noun, matrix).
	base_form(noun, axis).
	base_form(noun, leaf).
	base_form(noun, life).
	base_form(noun, wife).
	base_form(noun, knife).
	base_form(noun, wolf).
	base_form(noun, loaf).
	base_form(noun, shelf).
	base_form(noun, calf).
	base_form(noun, half).
	base_form(noun, scarf).
	base_form(noun, thief).
	base_form(noun, city).
	base_form(noun, baby).
	base_form(noun, story).
	base_form(noun, party).
	base_form(noun, country).
	base_form(noun, box).
	base_form(noun, bus).
	base_form(noun, class).
	base_form(noun, church).
	base_form(noun, watch).
	base_form(noun, dish).
	base_form(noun, hero).
	base_form(noun, potato).
	base_form(noun, tomato).
	base_form(noun, house).
	base_form(noun, horse).
	base_form(noun, case).
	base_form(noun, rose).
	base_form(noun, dog).
	base_form(noun, cat).
	base_form(noun, book).
	base_form(noun, car).
	base_form(noun, tree).
	base_form(noun, day).
	base_form(noun, boy).
	base_form(noun, key).
	base_form(noun, toy).
	base_form(noun, saw).
	base_form(noun, series).
	base_form(noun, species).
	base_form(noun, fish).
	base_form(noun, sheep).
	base_form(noun, deer).

	base_form(verb, be).
	base_form(verb, have).
	base_form(verb, do).
	base_form(verb, go).
	base_form(verb, see).
	base_form(verb, run).
	base_form(verb, make).
	base_form(verb, take).
	base_form(verb, come).
	base_form(verb, get).
	base_form(verb, give).
	base_form(verb, know).
	base_form(verb, think).
	base_form(verb, say).
	base_form(verb, tell).
	base_form(verb, find).
	base_form(verb, become).
	base_form(verb, show).
	base_form(verb, leave).
	base_form(verb, feel).
	base_form(verb, put).
	base_form(verb, bring).
	base_form(verb, begin).
	base_form(verb, keep).
	base_form(verb, hold).
	base_form(verb, write).
	base_form(verb, stand).
	base_form(verb, hear).
	base_form(verb, let).
	base_form(verb, mean).
	base_form(verb, set).
	base_form(verb, meet).
	base_form(verb, pay).
	base_form(verb, sit).
	base_form(verb, speak).
	base_form(verb, lie).
	base_form(verb, lead).
	base_form(verb, read).
	base_form(verb, grow).
	base_form(verb, lose).
	base_form(verb, fall).
	base_form(verb, send).
	base_form(verb, build).
	base_form(verb, understand).
	base_form(verb, draw).
	base_form(verb, break).
	base_form(verb, spend).
	base_form(verb, cut).
	base_form(verb, rise).
	base_form(verb, drive).
	base_form(verb, buy).
	base_form(verb, wear).
	base_form(verb, choose).
	base_form(verb, eat).
	base_form(verb, drink).
	base_form(verb, sleep).
	base_form(verb, stop).
	base_form(verb, plan).
	base_form(verb, swim).
	base_form(verb, travel).
	base_form(verb, study).
	base_form(verb, try).
	base_form(verb, carry).
	base_form(verb, cry).
	base_form(verb, fly).
	base_form(verb, play).
	base_form(verb, enjoy).
	base_form(verb, stay).
	base_form(verb, obey).
	base_form(verb, watch).
	base_form(verb, wash).
	base_form(verb, fix).
	base_form(verb, buzz).
	base_form(verb, pass).
	base_form(verb, miss).
	base_form(verb, push).
	base_form(verb, catch).
	base_form(verb, teach).
	base_form(verb, reach).
	base_form(verb, box).
	base_form(verb, echo).
	base_form(verb, open).
	base_form(verb, close).
	base_form(verb, love).
	base_form(verb, move).
	base_form(verb, use).
	base_form(verb, like).
	base_form(verb, dance).
	base_form(verb, change).
	base_form(verb, create).
	base_form(verb, hope).
	base_form(verb, agree).
	base_form(verb, die).
	base_form(verb, tie).
	base_form(verb, arrive).
	base_form(verb, save).
	base_form(verb, bake).

	base_form(adjective, good).
	base_form(adjective, bad).
	base_form(adjective, far).
	base_form(adjective, little).
	base_form(adjective, many).
	base_form(adjective, much).
	base_form(adjective, old).
	base_form(adjective, late).
	base_form(adjective, happy).
	base_form(adjective, easy).
	base_form(adjective, busy).
	base_form(adjective, pretty).
	base_form(adjective, heavy).
	base_form(adjective, early).
	base_form(adjective, big).
	base_form(adjective, hot).
	base_form(adjective, thin).
	base_form(adjective, sad).
	base_form(adjective, flat).
	base_form(adjective, nice).
	base_form(adjective, large).
	base_form(adjective, wise).
	base_form(adjective, brave).
	base_form(adjective, safe).
	base_form(adjective, close).
	base_form(adjective, fast).
	base_form(adjective, slow).
	base_form(adjective, tall).
	base_form(adjective, small).
	base_form(adjective, short).
	base_form(adjective, long).
	base_form(adjective, young).
	base_form(adjective, high).
	base_form(adjective, low).
	base_form(adjective, hard).
	base_form(adjective, soft).
	base_form(adjective, bright).
	base_form(adjective, dark).
	base_form(adjective, warm).
	base_form(adjective, cold).

	base_form(adverb, well).
	base_form(adverb, badly).
	base_form(adverb, far).
	base_form(adverb, little).
	base_form(adverb, much).
	base_form(adverb, fast).
	base_form(adverb, hard).
	base_form(adverb, early).
	base_form(adverb, late).

:- end_object.