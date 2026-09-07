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


:- object(stopwords_af,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the af language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-af',
			'Source commit' - '7bc0179a79af05dd1e57108168cd5e50c5f994c8',
			'Generated entries' - '51.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('''n').
	stop_word('aan').
	stop_word('af').
	stop_word('al').
	stop_word('as').
	stop_word('baie').
	stop_word('by').
	stop_word('daar').
	stop_word('dag').
	stop_word('dat').
	stop_word('die').
	stop_word('dit').
	stop_word('een').
	stop_word('ek').
	stop_word('en').
	stop_word('gaan').
	stop_word('gesê').
	stop_word('haar').
	stop_word('het').
	stop_word('hom').
	stop_word('hulle').
	stop_word('hy').
	stop_word('in').
	stop_word('is').
	stop_word('jou').
	stop_word('jy').
	stop_word('kan').
	stop_word('kom').
	stop_word('ma').
	stop_word('maar').
	stop_word('met').
	stop_word('my').
	stop_word('na').
	stop_word('nie').
	stop_word('om').
	stop_word('ons').
	stop_word('op').
	stop_word('saam').
	stop_word('sal').
	stop_word('se').
	stop_word('sien').
	stop_word('so').
	stop_word('sy').
	stop_word('te').
	stop_word('toe').
	stop_word('uit').
	stop_word('van').
	stop_word('vir').
	stop_word('was').
	stop_word('wat').
	stop_word('ŉ').

:- end_object.
