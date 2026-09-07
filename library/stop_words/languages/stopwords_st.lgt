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


:- object(stopwords_st,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the st language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-st',
			'Source commit' - '3934239b2a6ec28c9713373363e8d652a090456d',
			'Generated entries' - '31.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('a').
	stop_word('ba').
	stop_word('bane').
	stop_word('bona').
	stop_word('e').
	stop_word('ea').
	stop_word('eaba').
	stop_word('empa').
	stop_word('ena').
	stop_word('ha').
	stop_word('hae').
	stop_word('hape').
	stop_word('ho').
	stop_word('hore').
	stop_word('ka').
	stop_word('ke').
	stop_word('la').
	stop_word('le').
	stop_word('li').
	stop_word('me').
	stop_word('mo').
	stop_word('moo').
	stop_word('ne').
	stop_word('o').
	stop_word('oa').
	stop_word('re').
	stop_word('sa').
	stop_word('se').
	stop_word('tloha').
	stop_word('tsa').
	stop_word('tse').

:- end_object.
