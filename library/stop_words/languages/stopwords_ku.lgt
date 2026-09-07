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


:- object(stopwords_ku,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the ku language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-ku',
			'Source commit' - '2a9d6483eef65231b3e63ef3f237c85d81f52ea0',
			'Generated entries' - '62.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('ئێمە').
	stop_word('ئێوە').
	stop_word('ئەم').
	stop_word('ئەو').
	stop_word('ئەوان').
	stop_word('ئەوەی').
	stop_word('بۆ').
	stop_word('بێ').
	stop_word('بێجگە').
	stop_word('بە').
	stop_word('بەبێ').
	stop_word('بەدەم').
	stop_word('بەردەم').
	stop_word('بەرلە').
	stop_word('بەرەوی').
	stop_word('بەرەوە').
	stop_word('بەلای').
	stop_word('بەپێی').
	stop_word('تۆ').
	stop_word('تێ').
	stop_word('جگە').
	stop_word('دوای').
	stop_word('دوو').
	stop_word('دە').
	stop_word('دەکات').
	stop_word('دەگەڵ').
	stop_word('سەر').
	stop_word('لێ').
	stop_word('لە').
	stop_word('لەبابەت').
	stop_word('لەباتی').
	stop_word('لەبارەی').
	stop_word('لەبرێتی').
	stop_word('لەبن').
	stop_word('لەبەر').
	stop_word('لەبەینی').
	stop_word('لەدەم').
	stop_word('لەرێ').
	stop_word('لەرێگا').
	stop_word('لەرەوی').
	stop_word('لەسەر').
	stop_word('لەلایەن').
	stop_word('لەناو').
	stop_word('لەنێو').
	stop_word('لەو').
	stop_word('لەپێناوی').
	stop_word('لەژێر').
	stop_word('لەگەڵ').
	stop_word('من').
	stop_word('ناو').
	stop_word('نێوان').
	stop_word('هەر').
	stop_word('هەروەها').
	stop_word('و').
	stop_word('وەک').
	stop_word('پاش').
	stop_word('پێ').
	stop_word('پێش').
	stop_word('چەند').
	stop_word('کرد').
	stop_word('کە').
	stop_word('ی').

:- end_object.
