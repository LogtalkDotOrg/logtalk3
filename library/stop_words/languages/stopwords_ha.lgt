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


:- object(stopwords_ha,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the ha language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-ha',
			'Source commit' - '8fbf04b5967ad5f88322da42826c7673efda2e2b',
			'Generated entries' - '39.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('amma').
	stop_word('ba').
	stop_word('ban').
	stop_word('ce').
	stop_word('cikin').
	stop_word('da').
	stop_word('don').
	stop_word('ga').
	stop_word('in').
	stop_word('ina').
	stop_word('ita').
	stop_word('ji').
	stop_word('ka').
	stop_word('ko').
	stop_word('kuma').
	stop_word('lokacin').
	stop_word('ma').
	stop_word('mai').
	stop_word('na').
	stop_word('ne').
	stop_word('ni').
	stop_word('sai').
	stop_word('shi').
	stop_word('su').
	stop_word('suka').
	stop_word('sun').
	stop_word('ta').
	stop_word('tafi').
	stop_word('take').
	stop_word('tana').
	stop_word('wani').
	stop_word('wannan').
	stop_word('wata').
	stop_word('ya').
	stop_word('yake').
	stop_word('yana').
	stop_word('yi').
	stop_word('za').

:- end_object.
