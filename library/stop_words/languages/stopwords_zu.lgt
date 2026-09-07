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


:- object(stopwords_zu,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the zu language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-zu',
			'Source commit' - '5f1e34361fe0d5fba6eb8659150e880b205bcfb0',
			'Generated entries' - '29.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('futhi').
	stop_word('kahle').
	stop_word('kakhulu').
	stop_word('kanye').
	stop_word('khona').
	stop_word('kodwa').
	stop_word('kungani').
	stop_word('kusho').
	stop_word('la').
	stop_word('lakhe').
	stop_word('lapho').
	stop_word('mina').
	stop_word('ngesikhathi').
	stop_word('nje').
	stop_word('phansi').
	stop_word('phezulu').
	stop_word('u').
	stop_word('ukuba').
	stop_word('ukuthi').
	stop_word('ukuze').
	stop_word('uma').
	stop_word('wahamba').
	stop_word('wakhe').
	stop_word('wami').
	stop_word('wase').
	stop_word('wathi').
	stop_word('yakhe').
	stop_word('zakhe').
	stop_word('zonke').

:- end_object.
