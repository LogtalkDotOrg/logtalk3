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


:- object(stopwords_la,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the la language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-la',
			'Source commit' - '8bd6439e794e88744a2972ba73f094a19b073121',
			'Generated entries' - '49.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('ab').
	stop_word('ac').
	stop_word('ad').
	stop_word('at').
	stop_word('atque').
	stop_word('aut').
	stop_word('autem').
	stop_word('cum').
	stop_word('de').
	stop_word('dum').
	stop_word('e').
	stop_word('erant').
	stop_word('erat').
	stop_word('est').
	stop_word('et').
	stop_word('etiam').
	stop_word('ex').
	stop_word('haec').
	stop_word('hic').
	stop_word('hoc').
	stop_word('in').
	stop_word('ita').
	stop_word('me').
	stop_word('nec').
	stop_word('neque').
	stop_word('non').
	stop_word('per').
	stop_word('qua').
	stop_word('quae').
	stop_word('quam').
	stop_word('qui').
	stop_word('quibus').
	stop_word('quidem').
	stop_word('quo').
	stop_word('quod').
	stop_word('re').
	stop_word('rebus').
	stop_word('rem').
	stop_word('res').
	stop_word('sed').
	stop_word('si').
	stop_word('sic').
	stop_word('sunt').
	stop_word('tamen').
	stop_word('tandem').
	stop_word('te').
	stop_word('ut').
	stop_word('vel').

:- end_object.
