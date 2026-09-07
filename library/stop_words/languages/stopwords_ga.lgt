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


:- object(stopwords_ga,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the ga language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-ga',
			'Source commit' - '699a27ec708829ffe1d1c3b31ab5a700e83ddd09',
			'Generated entries' - '109.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('ach').
	stop_word('ag').
	stop_word('agus').
	stop_word('an').
	stop_word('aon').
	stop_word('ar').
	stop_word('arna').
	stop_word('as').
	stop_word('b''').
	stop_word('ba').
	stop_word('beirt').
	stop_word('bhúr').
	stop_word('caoga').
	stop_word('ceathair').
	stop_word('ceathrar').
	stop_word('chomh').
	stop_word('chtó').
	stop_word('chuig').
	stop_word('chun').
	stop_word('cois').
	stop_word('céad').
	stop_word('cúig').
	stop_word('cúigear').
	stop_word('d''').
	stop_word('daichead').
	stop_word('dar').
	stop_word('de').
	stop_word('deich').
	stop_word('deichniúr').
	stop_word('den').
	stop_word('dhá').
	stop_word('do').
	stop_word('don').
	stop_word('dtí').
	stop_word('dá').
	stop_word('dár').
	stop_word('dó').
	stop_word('faoi').
	stop_word('faoin').
	stop_word('faoina').
	stop_word('faoinár').
	stop_word('fara').
	stop_word('fiche').
	stop_word('gach').
	stop_word('gan').
	stop_word('go').
	stop_word('gur').
	stop_word('haon').
	stop_word('hocht').
	stop_word('i').
	stop_word('iad').
	stop_word('idir').
	stop_word('in').
	stop_word('ina').
	stop_word('ins').
	stop_word('inár').
	stop_word('is').
	stop_word('le').
	stop_word('leis').
	stop_word('lena').
	stop_word('lenár').
	stop_word('m''').
	stop_word('mar').
	stop_word('mo').
	stop_word('mé').
	stop_word('na').
	stop_word('nach').
	stop_word('naoi').
	stop_word('naonúr').
	stop_word('ná').
	stop_word('ní').
	stop_word('níor').
	stop_word('nó').
	stop_word('nócha').
	stop_word('ocht').
	stop_word('ochtar').
	stop_word('os').
	stop_word('roimh').
	stop_word('sa').
	stop_word('seacht').
	stop_word('seachtar').
	stop_word('seachtó').
	stop_word('seasca').
	stop_word('seisear').
	stop_word('siad').
	stop_word('sibh').
	stop_word('sinn').
	stop_word('sna').
	stop_word('sé').
	stop_word('sí').
	stop_word('tar').
	stop_word('thar').
	stop_word('thú').
	stop_word('triúr').
	stop_word('trí').
	stop_word('trína').
	stop_word('trínár').
	stop_word('tríocha').
	stop_word('tú').
	stop_word('um').
	stop_word('ár').
	stop_word('é').
	stop_word('éis').
	stop_word('í').
	stop_word('ó').
	stop_word('ón').
	stop_word('óna').
	stop_word('ónár').

:- end_object.
