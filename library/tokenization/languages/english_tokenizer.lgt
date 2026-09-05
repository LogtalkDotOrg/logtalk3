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


:- object(english_tokenizer,
	implements(tokenizer_language_protocol),
	imports(tokenizer_rules)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'English tokenization and sentence splitting provider using portable rule-based heuristics and a curated abbreviation inventory.',
		see_also is [tokenizer(_, _), tokenizer_rules]
	]).

	% honorifics, titles, and ranks
	abbreviation('adm.').
	abbreviation('amb.').
	abbreviation('capt.').
	abbreviation('cmdr.').
	abbreviation('col.').
	abbreviation('cpl.').
	abbreviation('det.').
	abbreviation('dr.').
	abbreviation('fr.').
	abbreviation('gen.').
	abbreviation('gov.').
	abbreviation('hon.').
	abbreviation('insp.').
	abbreviation('lt.').
	abbreviation('maj.').
	abbreviation('mr.').
	abbreviation('mrs.').
	abbreviation('ms.').
	abbreviation('mx.').
	abbreviation('pres.').
	abbreviation('prof.').
	abbreviation('rep.').
	abbreviation('rev.').
	abbreviation('sen.').
	abbreviation('sgt.').
	abbreviation('st.').
	abbreviation('supt.').
	abbreviation('treas.').

	% personal suffixes and academic or professional credentials
	abbreviation('b.a.').
	abbreviation('b.s.').
	abbreviation('d.d.s.').
	abbreviation('d.v.m.').
	abbreviation('esq.').
	abbreviation('j.d.').
	abbreviation('jr.').
	abbreviation('m.a.').
	abbreviation('m.d.').
	abbreviation('m.s.').
	abbreviation('ph.d.').
	abbreviation('sr.').

	% calendar and time abbreviations
	abbreviation('a.m.').
	abbreviation('apr.').
	abbreviation('aug.').
	abbreviation('dec.').
	abbreviation('feb.').
	abbreviation('jan.').
	abbreviation('jul.').
	abbreviation('jun.').
	abbreviation('mar.').
	abbreviation('nov.').
	abbreviation('oct.').
	abbreviation('p.m.').
	abbreviation('sep.').
	abbreviation('sept.').

	% organizations and addresses
	abbreviation('assn.').
	abbreviation('ave.').
	abbreviation('bldg.').
	abbreviation('blvd.').
	abbreviation('co.').
	abbreviation('corp.').
	abbreviation('dept.').
	abbreviation('div.').
	abbreviation('est.').
	abbreviation('inc.').
	abbreviation('llc.').
	abbreviation('ltd.').
	abbreviation('univ.').

	% editorial and reference abbreviations
	abbreviation('approx.').
	abbreviation('ch.').
	abbreviation('ed.').
	abbreviation('eds.').
	abbreviation('eq.').
	abbreviation('fig.').
	abbreviation('figs.').
	abbreviation('misc.').
	abbreviation('no.').
	abbreviation('nos.').
	abbreviation('p.').
	abbreviation('pp.').
	abbreviation('sec.').
	abbreviation('secs.').
	abbreviation('vol.').
	abbreviation('vols.').

	% geographic and political initialisms
	abbreviation('d.c.').
	abbreviation('e.u.').
	abbreviation('u.k.').
	abbreviation('u.n.').
	abbreviation('u.s.').
	abbreviation('u.s.a.').

	% latin and general-purpose abbreviations
	abbreviation('ca.').
	abbreviation('cf.').
	abbreviation('e.g.').
	abbreviation('etc.').
	abbreviation('i.e.').
	abbreviation('viz.').
	abbreviation('vs.').

	% honorifics, titles, and ranks that do not terminate a sentence
	non_terminal_abbreviation('adm.').
	non_terminal_abbreviation('amb.').
	non_terminal_abbreviation('capt.').
	non_terminal_abbreviation('cmdr.').
	non_terminal_abbreviation('col.').
	non_terminal_abbreviation('cpl.').
	non_terminal_abbreviation('det.').
	non_terminal_abbreviation('dr.').
	non_terminal_abbreviation('fr.').
	non_terminal_abbreviation('gen.').
	non_terminal_abbreviation('gov.').
	non_terminal_abbreviation('hon.').
	non_terminal_abbreviation('insp.').
	non_terminal_abbreviation('lt.').
	non_terminal_abbreviation('maj.').
	non_terminal_abbreviation('mr.').
	non_terminal_abbreviation('mrs.').
	non_terminal_abbreviation('ms.').
	non_terminal_abbreviation('mx.').
	non_terminal_abbreviation('pres.').
	non_terminal_abbreviation('prof.').
	non_terminal_abbreviation('rep.').
	non_terminal_abbreviation('rev.').
	non_terminal_abbreviation('sen.').
	non_terminal_abbreviation('sgt.').
	non_terminal_abbreviation('st.').
	non_terminal_abbreviation('supt.').
	non_terminal_abbreviation('treas.').

	% personal suffixes that do not terminate a sentence
	non_terminal_abbreviation('jr.').
	non_terminal_abbreviation('sr.').

	internal_apostrophe(39).

	internal_hyphen(45).

:- end_object.
