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


:- object(stopwords_so,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the so language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-so',
			'Source commit' - 'f67bd3bd239e7156c6c6aecb478a1c935d632a98',
			'Generated entries' - '30.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('aad').
	stop_word('albaabkii').
	stop_word('atabo').
	stop_word('ay').
	stop_word('ayaa').
	stop_word('ayee').
	stop_word('ayuu').
	stop_word('dhan').
	stop_word('hadana').
	stop_word('in').
	stop_word('inuu').
	stop_word('isku').
	stop_word('jiray').
	stop_word('jirtay').
	stop_word('ka').
	stop_word('kale').
	stop_word('kasoo').
	stop_word('ku').
	stop_word('kuu').
	stop_word('lakin').
	stop_word('markii').
	stop_word('oo').
	stop_word('si').
	stop_word('soo').
	stop_word('uga').
	stop_word('ugu').
	stop_word('uu').
	stop_word('waa').
	stop_word('waxa').
	stop_word('waxuu').

:- end_object.
