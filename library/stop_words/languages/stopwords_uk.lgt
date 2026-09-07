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


:- object(stopwords_uk,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the uk language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-uk',
			'Source commit' - '83b24b92ff9f8af0bbad349522c783f9d5a28b88',
			'Generated entries' - '73.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('авжеж').
	stop_word('адже').
	stop_word('але').
	stop_word('б').
	stop_word('без').
	stop_word('був').
	stop_word('була').
	stop_word('були').
	stop_word('було').
	stop_word('бути').
	stop_word('більш').
	stop_word('вам').
	stop_word('вас').
	stop_word('весь').
	stop_word('вздовж').
	stop_word('ви').
	stop_word('вниз').
	stop_word('внизу').
	stop_word('вона').
	stop_word('вони').
	stop_word('воно').
	stop_word('все').
	stop_word('всередині').
	stop_word('всіх').
	stop_word('від').
	stop_word('він').
	stop_word('да').
	stop_word('давай').
	stop_word('давати').
	stop_word('де').
	stop_word('дещо').
	stop_word('для').
	stop_word('до').
	stop_word('з').
	stop_word('завжди').
	stop_word('замість').
	stop_word('й').
	stop_word('коли').
	stop_word('ледве').
	stop_word('майже').
	stop_word('ми').
	stop_word('навколо').
	stop_word('навіть').
	stop_word('нам').
	stop_word('от').
	stop_word('отже').
	stop_word('отож').
	stop_word('поза').
	stop_word('про').
	stop_word('під').
	stop_word('та').
	stop_word('так').
	stop_word('такий').
	stop_word('також').
	stop_word('те').
	stop_word('ти').
	stop_word('тобто').
	stop_word('тож').
	stop_word('тощо').
	stop_word('хоча').
	stop_word('це').
	stop_word('цей').
	stop_word('чи').
	stop_word('чого').
	stop_word('що').
	stop_word('як').
	stop_word('який').
	stop_word('якої').
	stop_word('є').
	stop_word('із').
	stop_word('інших').
	stop_word('їх').
	stop_word('її').

:- end_object.
