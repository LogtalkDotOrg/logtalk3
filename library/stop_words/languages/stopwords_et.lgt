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


:- object(stopwords_et,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the et language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-et',
			'Source commit' - '1ba777413664f4639d0c24511b90763cc88481a2',
			'Generated entries' - '35.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('aga').
	stop_word('ei').
	stop_word('et').
	stop_word('ja').
	stop_word('jah').
	stop_word('kas').
	stop_word('kui').
	stop_word('kõik').
	stop_word('ma').
	stop_word('me').
	stop_word('mida').
	stop_word('midagi').
	stop_word('mind').
	stop_word('minu').
	stop_word('mis').
	stop_word('mu').
	stop_word('mul').
	stop_word('mulle').
	stop_word('nad').
	stop_word('nii').
	stop_word('oled').
	stop_word('olen').
	stop_word('oli').
	stop_word('oma').
	stop_word('on').
	stop_word('pole').
	stop_word('sa').
	stop_word('seda').
	stop_word('see').
	stop_word('selle').
	stop_word('siin').
	stop_word('siis').
	stop_word('ta').
	stop_word('te').
	stop_word('ära').

:- end_object.
