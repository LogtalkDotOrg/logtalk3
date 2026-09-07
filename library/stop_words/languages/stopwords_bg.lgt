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


:- object(stopwords_bg,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the bg language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-bg',
			'Source commit' - 'e9a4b791692ef3f6b89b3544ea9398a544793aee',
			'Generated entries' - '259.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('а').
	stop_word('автентичен').
	stop_word('аз').
	stop_word('ако').
	stop_word('ала').
	stop_word('бе').
	stop_word('без').
	stop_word('беше').
	stop_word('би').
	stop_word('бивш').
	stop_word('бивша').
	stop_word('бившо').
	stop_word('бил').
	stop_word('била').
	stop_word('били').
	stop_word('било').
	stop_word('благодаря').
	stop_word('близо').
	stop_word('бъдат').
	stop_word('бъде').
	stop_word('бяха').
	stop_word('в').
	stop_word('вас').
	stop_word('ваш').
	stop_word('ваша').
	stop_word('вероятно').
	stop_word('вече').
	stop_word('взема').
	stop_word('ви').
	stop_word('вие').
	stop_word('винаги').
	stop_word('внимава').
	stop_word('време').
	stop_word('все').
	stop_word('всеки').
	stop_word('всички').
	stop_word('всичко').
	stop_word('всяка').
	stop_word('във').
	stop_word('въпреки').
	stop_word('върху').
	stop_word('г').
	stop_word('ги').
	stop_word('главен').
	stop_word('главна').
	stop_word('главно').
	stop_word('глас').
	stop_word('го').
	stop_word('година').
	stop_word('години').
	stop_word('годишен').
	stop_word('д').
	stop_word('да').
	stop_word('дали').
	stop_word('два').
	stop_word('двама').
	stop_word('двамата').
	stop_word('две').
	stop_word('двете').
	stop_word('ден').
	stop_word('днес').
	stop_word('дни').
	stop_word('до').
	stop_word('добра').
	stop_word('добре').
	stop_word('добро').
	stop_word('добър').
	stop_word('докато').
	stop_word('докога').
	stop_word('дори').
	stop_word('досега').
	stop_word('доста').
	stop_word('друг').
	stop_word('друга').
	stop_word('други').
	stop_word('е').
	stop_word('евтин').
	stop_word('едва').
	stop_word('един').
	stop_word('една').
	stop_word('еднаква').
	stop_word('еднакви').
	stop_word('еднакъв').
	stop_word('едно').
	stop_word('екип').
	stop_word('ето').
	stop_word('живот').
	stop_word('за').
	stop_word('забавям').
	stop_word('зад').
	stop_word('заедно').
	stop_word('заради').
	stop_word('засега').
	stop_word('заспал').
	stop_word('затова').
	stop_word('защо').
	stop_word('защото').
	stop_word('и').
	stop_word('из').
	stop_word('или').
	stop_word('им').
	stop_word('има').
	stop_word('имат').
	stop_word('иска').
	stop_word('й').
	stop_word('каза').
	stop_word('как').
	stop_word('каква').
	stop_word('какво').
	stop_word('както').
	stop_word('какъв').
	stop_word('като').
	stop_word('кога').
	stop_word('когато').
	stop_word('което').
	stop_word('които').
	stop_word('кой').
	stop_word('който').
	stop_word('колко').
	stop_word('която').
	stop_word('къде').
	stop_word('където').
	stop_word('към').
	stop_word('лесен').
	stop_word('лесно').
	stop_word('ли').
	stop_word('лош').
	stop_word('м').
	stop_word('май').
	stop_word('малко').
	stop_word('ме').
	stop_word('между').
	stop_word('мек').
	stop_word('мен').
	stop_word('месец').
	stop_word('ми').
	stop_word('много').
	stop_word('мнозина').
	stop_word('мога').
	stop_word('могат').
	stop_word('може').
	stop_word('мокър').
	stop_word('моля').
	stop_word('момента').
	stop_word('му').
	stop_word('н').
	stop_word('на').
	stop_word('над').
	stop_word('назад').
	stop_word('най').
	stop_word('направи').
	stop_word('напред').
	stop_word('например').
	stop_word('нас').
	stop_word('не').
	stop_word('него').
	stop_word('нещо').
	stop_word('нея').
	stop_word('ни').
	stop_word('ние').
	stop_word('никой').
	stop_word('нито').
	stop_word('нищо').
	stop_word('но').
	stop_word('нов').
	stop_word('нова').
	stop_word('нови').
	stop_word('новина').
	stop_word('някои').
	stop_word('някой').
	stop_word('няколко').
	stop_word('няма').
	stop_word('обаче').
	stop_word('около').
	stop_word('освен').
	stop_word('особено').
	stop_word('от').
	stop_word('отгоре').
	stop_word('отново').
	stop_word('още').
	stop_word('пак').
	stop_word('по').
	stop_word('повече').
	stop_word('повечето').
	stop_word('под').
	stop_word('поне').
	stop_word('поради').
	stop_word('после').
	stop_word('почти').
	stop_word('прави').
	stop_word('пред').
	stop_word('преди').
	stop_word('през').
	stop_word('при').
	stop_word('пък').
	stop_word('първата').
	stop_word('първи').
	stop_word('първо').
	stop_word('пъти').
	stop_word('равен').
	stop_word('равна').
	stop_word('с').
	stop_word('са').
	stop_word('сам').
	stop_word('само').
	stop_word('се').
	stop_word('сега').
	stop_word('си').
	stop_word('син').
	stop_word('скоро').
	stop_word('след').
	stop_word('следващ').
	stop_word('сме').
	stop_word('смях').
	stop_word('според').
	stop_word('сред').
	stop_word('срещу').
	stop_word('сте').
	stop_word('съм').
	stop_word('със').
	stop_word('също').
	stop_word('т').
	stop_word('т.н.').
	stop_word('тази').
	stop_word('така').
	stop_word('такива').
	stop_word('такъв').
	stop_word('там').
	stop_word('твой').
	stop_word('те').
	stop_word('тези').
	stop_word('ти').
	stop_word('то').
	stop_word('това').
	stop_word('тогава').
	stop_word('този').
	stop_word('той').
	stop_word('толкова').
	stop_word('точно').
	stop_word('три').
	stop_word('трябва').
	stop_word('тук').
	stop_word('тъй').
	stop_word('тя').
	stop_word('тях').
	stop_word('у').
	stop_word('утре').
	stop_word('харесва').
	stop_word('хиляди').
	stop_word('ч').
	stop_word('часа').
	stop_word('че').
	stop_word('често').
	stop_word('чрез').
	stop_word('ще').
	stop_word('щом').
	stop_word('юмрук').
	stop_word('я').
	stop_word('як').

:- end_object.
