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


:- object(stopwords_ja,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the ja language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-ja',
			'Source commit' - '5a000f6a62f9e3a12f436f36d168e2fcd2fb1878',
			'Generated entries' - '134.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('あそこ').
	stop_word('あっ').
	stop_word('あの').
	stop_word('あのかた').
	stop_word('あの人').
	stop_word('あり').
	stop_word('あります').
	stop_word('ある').
	stop_word('あれ').
	stop_word('い').
	stop_word('いう').
	stop_word('います').
	stop_word('いる').
	stop_word('う').
	stop_word('うち').
	stop_word('え').
	stop_word('お').
	stop_word('および').
	stop_word('おり').
	stop_word('おります').
	stop_word('か').
	stop_word('かつて').
	stop_word('から').
	stop_word('が').
	stop_word('き').
	stop_word('ここ').
	stop_word('こちら').
	stop_word('こと').
	stop_word('この').
	stop_word('これ').
	stop_word('これら').
	stop_word('さ').
	stop_word('さらに').
	stop_word('し').
	stop_word('しかし').
	stop_word('する').
	stop_word('ず').
	stop_word('せ').
	stop_word('せる').
	stop_word('そこ').
	stop_word('そして').
	stop_word('その').
	stop_word('その他').
	stop_word('その後').
	stop_word('それ').
	stop_word('それぞれ').
	stop_word('それで').
	stop_word('た').
	stop_word('ただし').
	stop_word('たち').
	stop_word('ため').
	stop_word('たり').
	stop_word('だ').
	stop_word('だっ').
	stop_word('だれ').
	stop_word('つ').
	stop_word('て').
	stop_word('で').
	stop_word('でき').
	stop_word('できる').
	stop_word('です').
	stop_word('では').
	stop_word('でも').
	stop_word('と').
	stop_word('という').
	stop_word('といった').
	stop_word('とき').
	stop_word('ところ').
	stop_word('として').
	stop_word('とともに').
	stop_word('とも').
	stop_word('と共に').
	stop_word('どこ').
	stop_word('どの').
	stop_word('な').
	stop_word('ない').
	stop_word('なお').
	stop_word('なかっ').
	stop_word('ながら').
	stop_word('なく').
	stop_word('なっ').
	stop_word('など').
	stop_word('なに').
	stop_word('なら').
	stop_word('なり').
	stop_word('なる').
	stop_word('なん').
	stop_word('に').
	stop_word('において').
	stop_word('における').
	stop_word('について').
	stop_word('にて').
	stop_word('によって').
	stop_word('により').
	stop_word('による').
	stop_word('に対して').
	stop_word('に対する').
	stop_word('に関する').
	stop_word('の').
	stop_word('ので').
	stop_word('のみ').
	stop_word('は').
	stop_word('ば').
	stop_word('へ').
	stop_word('ほか').
	stop_word('ほとんど').
	stop_word('ほど').
	stop_word('ます').
	stop_word('また').
	stop_word('または').
	stop_word('まで').
	stop_word('も').
	stop_word('もの').
	stop_word('ものの').
	stop_word('や').
	stop_word('よう').
	stop_word('より').
	stop_word('ら').
	stop_word('られ').
	stop_word('られる').
	stop_word('れ').
	stop_word('れる').
	stop_word('を').
	stop_word('ん').
	stop_word('何').
	stop_word('及び').
	stop_word('彼').
	stop_word('彼女').
	stop_word('我々').
	stop_word('特に').
	stop_word('私').
	stop_word('私達').
	stop_word('貴方').
	stop_word('貴方方').

:- end_object.
