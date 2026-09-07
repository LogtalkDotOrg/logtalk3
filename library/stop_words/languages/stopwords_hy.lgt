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


:- object(stopwords_hy,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the hy language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-hy',
			'Source commit' - 'a381d40b0cfc970535caa1bcb0495430b19fd255',
			'Generated entries' - '45.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('այդ').
	stop_word('այլ').
	stop_word('այն').
	stop_word('այս').
	stop_word('դու').
	stop_word('դուք').
	stop_word('եմ').
	stop_word('են').
	stop_word('ենք').
	stop_word('ես').
	stop_word('եք').
	stop_word('է').
	stop_word('էի').
	stop_word('էին').
	stop_word('էինք').
	stop_word('էիր').
	stop_word('էիք').
	stop_word('էր').
	stop_word('ըստ').
	stop_word('թ').
	stop_word('ի').
	stop_word('ին').
	stop_word('իսկ').
	stop_word('իր').
	stop_word('կամ').
	stop_word('համար').
	stop_word('հետ').
	stop_word('հետո').
	stop_word('մենք').
	stop_word('մեջ').
	stop_word('մի').
	stop_word('ն').
	stop_word('նա').
	stop_word('նաև').
	stop_word('նրա').
	stop_word('նրանք').
	stop_word('որ').
	stop_word('որը').
	stop_word('որոնք').
	stop_word('որպես').
	stop_word('ու').
	stop_word('ում').
	stop_word('պիտի').
	stop_word('վրա').
	stop_word('և').

:- end_object.
