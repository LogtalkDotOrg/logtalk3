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


:- object(stopwords_no,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the no language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-no',
			'Source commit' - 'bdc5473085ba0d75a3efb7e6c152faa8f795d3c3',
			'Generated entries' - '221.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('alle').
	stop_word('andre').
	stop_word('arbeid').
	stop_word('at').
	stop_word('av').
	stop_word('bare').
	stop_word('begge').
	stop_word('ble').
	stop_word('blei').
	stop_word('bli').
	stop_word('blir').
	stop_word('blitt').
	stop_word('bort').
	stop_word('bra').
	stop_word('bruke').
	stop_word('både').
	stop_word('båe').
	stop_word('da').
	stop_word('de').
	stop_word('deg').
	stop_word('dei').
	stop_word('deim').
	stop_word('deira').
	stop_word('deires').
	stop_word('dem').
	stop_word('den').
	stop_word('denne').
	stop_word('der').
	stop_word('dere').
	stop_word('deres').
	stop_word('det').
	stop_word('dette').
	stop_word('di').
	stop_word('din').
	stop_word('disse').
	stop_word('ditt').
	stop_word('du').
	stop_word('dykk').
	stop_word('dykkar').
	stop_word('då').
	stop_word('eg').
	stop_word('ein').
	stop_word('eit').
	stop_word('eitt').
	stop_word('eller').
	stop_word('elles').
	stop_word('en').
	stop_word('ene').
	stop_word('eneste').
	stop_word('enhver').
	stop_word('enn').
	stop_word('er').
	stop_word('et').
	stop_word('ett').
	stop_word('etter').
	stop_word('folk').
	stop_word('for').
	stop_word('fordi').
	stop_word('forsûke').
	stop_word('fra').
	stop_word('få').
	stop_word('før').
	stop_word('fûr').
	stop_word('fûrst').
	stop_word('gjorde').
	stop_word('gjûre').
	stop_word('god').
	stop_word('gå').
	stop_word('ha').
	stop_word('hadde').
	stop_word('han').
	stop_word('hans').
	stop_word('har').
	stop_word('hennar').
	stop_word('henne').
	stop_word('hennes').
	stop_word('her').
	stop_word('hjå').
	stop_word('ho').
	stop_word('hoe').
	stop_word('honom').
	stop_word('hoss').
	stop_word('hossen').
	stop_word('hun').
	stop_word('hva').
	stop_word('hvem').
	stop_word('hver').
	stop_word('hvilke').
	stop_word('hvilken').
	stop_word('hvis').
	stop_word('hvor').
	stop_word('hvordan').
	stop_word('hvorfor').
	stop_word('i').
	stop_word('ikke').
	stop_word('ikkje').
	stop_word('ingen').
	stop_word('ingi').
	stop_word('inkje').
	stop_word('inn').
	stop_word('innen').
	stop_word('inni').
	stop_word('ja').
	stop_word('jeg').
	stop_word('kan').
	stop_word('kom').
	stop_word('korleis').
	stop_word('korso').
	stop_word('kun').
	stop_word('kunne').
	stop_word('kva').
	stop_word('kvar').
	stop_word('kvarhelst').
	stop_word('kven').
	stop_word('kvi').
	stop_word('kvifor').
	stop_word('lage').
	stop_word('lang').
	stop_word('lik').
	stop_word('like').
	stop_word('makt').
	stop_word('man').
	stop_word('mange').
	stop_word('me').
	stop_word('med').
	stop_word('medan').
	stop_word('meg').
	stop_word('meget').
	stop_word('mellom').
	stop_word('men').
	stop_word('mens').
	stop_word('mer').
	stop_word('mest').
	stop_word('mi').
	stop_word('min').
	stop_word('mine').
	stop_word('mitt').
	stop_word('mot').
	stop_word('mye').
	stop_word('mykje').
	stop_word('må').
	stop_word('måte').
	stop_word('navn').
	stop_word('ned').
	stop_word('nei').
	stop_word('no').
	stop_word('noe').
	stop_word('noen').
	stop_word('noka').
	stop_word('noko').
	stop_word('nokon').
	stop_word('nokor').
	stop_word('nokre').
	stop_word('ny').
	stop_word('nå').
	stop_word('når').
	stop_word('og').
	stop_word('også').
	stop_word('om').
	stop_word('opp').
	stop_word('oss').
	stop_word('over').
	stop_word('part').
	stop_word('punkt').
	stop_word('på').
	stop_word('rett').
	stop_word('riktig').
	stop_word('samme').
	stop_word('sant').
	stop_word('seg').
	stop_word('selv').
	stop_word('si').
	stop_word('sia').
	stop_word('sidan').
	stop_word('siden').
	stop_word('sin').
	stop_word('sine').
	stop_word('sist').
	stop_word('sitt').
	stop_word('sjøl').
	stop_word('skal').
	stop_word('skulle').
	stop_word('slik').
	stop_word('slutt').
	stop_word('so').
	stop_word('som').
	stop_word('somme').
	stop_word('somt').
	stop_word('start').
	stop_word('stille').
	stop_word('så').
	stop_word('sånn').
	stop_word('tid').
	stop_word('til').
	stop_word('tilbake').
	stop_word('tilstand').
	stop_word('um').
	stop_word('under').
	stop_word('upp').
	stop_word('ut').
	stop_word('uten').
	stop_word('var').
	stop_word('vart').
	stop_word('varte').
	stop_word('ved').
	stop_word('verdi').
	stop_word('vere').
	stop_word('verte').
	stop_word('vi').
	stop_word('vil').
	stop_word('ville').
	stop_word('vite').
	stop_word('vore').
	stop_word('vors').
	stop_word('vort').
	stop_word('vår').
	stop_word('være').
	stop_word('vært').
	stop_word('vöre').
	stop_word('vört').
	stop_word('å').

:- end_object.
