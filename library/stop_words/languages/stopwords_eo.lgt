:- encoding('UTF-8').

/*
The MIT License (MIT)

Copyright (c) 2016 Gene Diaz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/


:- object(stopwords_eo,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the eo language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-eo',
			'Source commit' - '7cf2e8022c15818771d92c281e7c4756655fdc3b',
			'Generated entries' - '173.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('adiaŭ').
	stop_word('ajn').
	stop_word('al').
	stop_word('ankoraŭ').
	stop_word('antaŭ').
	stop_word('aŭ').
	stop_word('bonan').
	stop_word('bonvole').
	stop_word('bonvolu').
	stop_word('bv').
	stop_word('ci').
	stop_word('cia').
	stop_word('cian').
	stop_word('cin').
	stop_word('d-ro').
	stop_word('da').
	stop_word('de').
	stop_word('dek').
	stop_word('deka').
	stop_word('do').
	stop_word('doktor''').
	stop_word('doktoro').
	stop_word('du').
	stop_word('dua').
	stop_word('dum').
	stop_word('eble').
	stop_word('ekz').
	stop_word('ekzemple').
	stop_word('en').
	stop_word('estas').
	stop_word('estis').
	stop_word('estos').
	stop_word('estu').
	stop_word('estus').
	stop_word('eĉ').
	stop_word('f-no').
	stop_word('feliĉan').
	stop_word('for').
	stop_word('fraŭlino').
	stop_word('ha').
	stop_word('havas').
	stop_word('havis').
	stop_word('havos').
	stop_word('havu').
	stop_word('havus').
	stop_word('he').
	stop_word('ho').
	stop_word('hu').
	stop_word('ili').
	stop_word('ilia').
	stop_word('ilian').
	stop_word('ilin').
	stop_word('inter').
	stop_word('io').
	stop_word('ion').
	stop_word('iu').
	stop_word('iujn').
	stop_word('iun').
	stop_word('ja').
	stop_word('jam').
	stop_word('je').
	stop_word('jes').
	stop_word('k').
	stop_word('kaj').
	stop_word('ke').
	stop_word('kio').
	stop_word('kion').
	stop_word('kiu').
	stop_word('kiujn').
	stop_word('kiun').
	stop_word('kvankam').
	stop_word('kvar').
	stop_word('kvara').
	stop_word('kvazaŭ').
	stop_word('kvin').
	stop_word('kvina').
	stop_word('la').
	stop_word('li').
	stop_word('lia').
	stop_word('lian').
	stop_word('lin').
	stop_word('malantaŭ').
	stop_word('male').
	stop_word('malgraŭ').
	stop_word('mem').
	stop_word('mi').
	stop_word('mia').
	stop_word('mian').
	stop_word('min').
	stop_word('minus').
	stop_word('naŭ').
	stop_word('naŭa').
	stop_word('ne').
	stop_word('nek').
	stop_word('nenio').
	stop_word('nenion').
	stop_word('neniu').
	stop_word('neniun').
	stop_word('nepre').
	stop_word('ni').
	stop_word('nia').
	stop_word('nian').
	stop_word('nin').
	stop_word('nu').
	stop_word('nun').
	stop_word('nur').
	stop_word('ok').
	stop_word('oka').
	stop_word('oni').
	stop_word('onia').
	stop_word('onian').
	stop_word('onin').
	stop_word('plej').
	stop_word('pli').
	stop_word('plu').
	stop_word('plus').
	stop_word('por').
	stop_word('post').
	stop_word('preter').
	stop_word('s-no').
	stop_word('s-ro').
	stop_word('se').
	stop_word('sed').
	stop_word('sep').
	stop_word('sepa').
	stop_word('ses').
	stop_word('sesa').
	stop_word('si').
	stop_word('sia').
	stop_word('sian').
	stop_word('sin').
	stop_word('sinjor''').
	stop_word('sinjorino').
	stop_word('sinjoro').
	stop_word('sub').
	stop_word('super').
	stop_word('supren').
	stop_word('sur').
	stop_word('tamen').
	stop_word('tio').
	stop_word('tion').
	stop_word('tiu').
	stop_word('tiujn').
	stop_word('tiun').
	stop_word('tra').
	stop_word('tri').
	stop_word('tria').
	stop_word('tuj').
	stop_word('tute').
	stop_word('unu').
	stop_word('unua').
	stop_word('ve').
	stop_word('verŝajne').
	stop_word('vi').
	stop_word('via').
	stop_word('vian').
	stop_word('vin').
	stop_word('ĉi').
	stop_word('ĉio').
	stop_word('ĉion').
	stop_word('ĉiu').
	stop_word('ĉiujn').
	stop_word('ĉiun').
	stop_word('ĉu').
	stop_word('ĝi').
	stop_word('ĝia').
	stop_word('ĝian').
	stop_word('ĝin').
	stop_word('ĝis').
	stop_word('ĵus').
	stop_word('ŝi').
	stop_word('ŝia').
	stop_word('ŝin').

:- end_object.
