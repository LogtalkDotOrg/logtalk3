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


:- object(stopwords_ko,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the ko language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-ko',
			'Source commit' - 'bb9e03d893c62d13bce6344523d3d9e76aa5f289',
			'Generated entries' - '679.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('!').
	stop_word('"').
	stop_word('$').
	stop_word('%').
	stop_word('&').
	stop_word('''').
	stop_word('(').
	stop_word(')').
	stop_word('*').
	stop_word('+').
	stop_word(',').
	stop_word('-').
	stop_word('.').
	stop_word('...').
	stop_word('0').
	stop_word('1').
	stop_word('2').
	stop_word('3').
	stop_word('4').
	stop_word('5').
	stop_word('6').
	stop_word('7').
	stop_word('8').
	stop_word('9').
	stop_word(';').
	stop_word('<').
	stop_word('=').
	stop_word('>').
	stop_word('?').
	stop_word('@').
	stop_word('\\').
	stop_word('^').
	stop_word('_').
	stop_word('`').
	stop_word('|').
	stop_word('~').
	stop_word('·').
	stop_word('—').
	stop_word('——').
	stop_word('‘').
	stop_word('’').
	stop_word('“').
	stop_word('”').
	stop_word('…').
	stop_word('、').
	stop_word('。').
	stop_word('〈').
	stop_word('〉').
	stop_word('《').
	stop_word('》').
	stop_word('가').
	stop_word('가까스로').
	stop_word('가령').
	stop_word('각').
	stop_word('각각').
	stop_word('각자').
	stop_word('각종').
	stop_word('갖고말하자면').
	stop_word('같다').
	stop_word('같이').
	stop_word('개의치않고').
	stop_word('거니와').
	stop_word('거바').
	stop_word('거의').
	stop_word('것').
	stop_word('것과 같이').
	stop_word('것들').
	stop_word('게다가').
	stop_word('게우다').
	stop_word('겨우').
	stop_word('견지에서').
	stop_word('결과에 이르다').
	stop_word('결국').
	stop_word('결론을 낼 수 있다').
	stop_word('겸사겸사').
	stop_word('고려하면').
	stop_word('고로').
	stop_word('곧').
	stop_word('공동으로').
	stop_word('과').
	stop_word('과연').
	stop_word('관계가 있다').
	stop_word('관계없이').
	stop_word('관련이 있다').
	stop_word('관하여').
	stop_word('관한').
	stop_word('관해서는').
	stop_word('구').
	stop_word('구체적으로').
	stop_word('구토하다').
	stop_word('그').
	stop_word('그들').
	stop_word('그때').
	stop_word('그래').
	stop_word('그래도').
	stop_word('그래서').
	stop_word('그러나').
	stop_word('그러니').
	stop_word('그러니까').
	stop_word('그러면').
	stop_word('그러므로').
	stop_word('그러한즉').
	stop_word('그런 까닭에').
	stop_word('그런데').
	stop_word('그런즉').
	stop_word('그럼').
	stop_word('그럼에도 불구하고').
	stop_word('그렇게 함으로써').
	stop_word('그렇지').
	stop_word('그렇지 않다면').
	stop_word('그렇지 않으면').
	stop_word('그렇지만').
	stop_word('그렇지않으면').
	stop_word('그리고').
	stop_word('그리하여').
	stop_word('그만이다').
	stop_word('그에 따르는').
	stop_word('그위에').
	stop_word('그저').
	stop_word('그중에서').
	stop_word('그치지 않다').
	stop_word('근거로').
	stop_word('근거하여').
	stop_word('기대여').
	stop_word('기점으로').
	stop_word('기준으로').
	stop_word('기타').
	stop_word('까닭으로').
	stop_word('까악').
	stop_word('까지').
	stop_word('까지 미치다').
	stop_word('까지도').
	stop_word('꽈당').
	stop_word('끙끙').
	stop_word('끼익').
	stop_word('나').
	stop_word('나머지는').
	stop_word('남들').
	stop_word('남짓').
	stop_word('너').
	stop_word('너희').
	stop_word('너희들').
	stop_word('네').
	stop_word('넷').
	stop_word('년').
	stop_word('논하지 않다').
	stop_word('놀라다').
	stop_word('누가 알겠는가').
	stop_word('누구').
	stop_word('다른').
	stop_word('다른 방면으로').
	stop_word('다만').
	stop_word('다섯').
	stop_word('다소').
	stop_word('다수').
	stop_word('다시 말하자면').
	stop_word('다시말하면').
	stop_word('다음').
	stop_word('다음에').
	stop_word('다음으로').
	stop_word('단지').
	stop_word('답다').
	stop_word('당신').
	stop_word('당장').
	stop_word('대로 하다').
	stop_word('대하면').
	stop_word('대하여').
	stop_word('대해 말하자면').
	stop_word('대해서').
	stop_word('댕그').
	stop_word('더구나').
	stop_word('더군다나').
	stop_word('더라도').
	stop_word('더불어').
	stop_word('더욱더').
	stop_word('더욱이는').
	stop_word('도달하다').
	stop_word('도착하다').
	stop_word('동시에').
	stop_word('동안').
	stop_word('된바에야').
	stop_word('된이상').
	stop_word('두번째로').
	stop_word('둘').
	stop_word('둥둥').
	stop_word('뒤따라').
	stop_word('뒤이어').
	stop_word('든간에').
	stop_word('들').
	stop_word('등').
	stop_word('등등').
	stop_word('딩동').
	stop_word('따라').
	stop_word('따라서').
	stop_word('따위').
	stop_word('따지지 않다').
	stop_word('딱').
	stop_word('때').
	stop_word('때가 되어').
	stop_word('때문에').
	stop_word('또').
	stop_word('또한').
	stop_word('뚝뚝').
	stop_word('라 해도').
	stop_word('령').
	stop_word('로').
	stop_word('로 인하여').
	stop_word('로부터').
	stop_word('로써').
	stop_word('륙').
	stop_word('를').
	stop_word('마음대로').
	stop_word('마저').
	stop_word('마저도').
	stop_word('마치').
	stop_word('막론하고').
	stop_word('만 못하다').
	stop_word('만약').
	stop_word('만약에').
	stop_word('만은 아니다').
	stop_word('만이 아니다').
	stop_word('만일').
	stop_word('만큼').
	stop_word('말하자면').
	stop_word('말할것도 없고').
	stop_word('매').
	stop_word('매번').
	stop_word('메쓰겁다').
	stop_word('몇').
	stop_word('모').
	stop_word('모두').
	stop_word('무렵').
	stop_word('무릎쓰고').
	stop_word('무슨').
	stop_word('무엇').
	stop_word('무엇때문에').
	stop_word('물론').
	stop_word('및').
	stop_word('바꾸어말하면').
	stop_word('바꾸어말하자면').
	stop_word('바꾸어서 말하면').
	stop_word('바꾸어서 한다면').
	stop_word('바꿔 말하면').
	stop_word('바로').
	stop_word('바와같이').
	stop_word('밖에 안된다').
	stop_word('반대로').
	stop_word('반대로 말하자면').
	stop_word('반드시').
	stop_word('버금').
	stop_word('보는데서').
	stop_word('보다더').
	stop_word('보드득').
	stop_word('본대로').
	stop_word('봐').
	stop_word('봐라').
	stop_word('부류의 사람들').
	stop_word('부터').
	stop_word('불구하고').
	stop_word('불문하고').
	stop_word('붕붕').
	stop_word('비걱거리다').
	stop_word('비교적').
	stop_word('비길수 없다').
	stop_word('비로소').
	stop_word('비록').
	stop_word('비슷하다').
	stop_word('비추어 보아').
	stop_word('비하면').
	stop_word('뿐만 아니라').
	stop_word('뿐만아니라').
	stop_word('뿐이다').
	stop_word('삐걱').
	stop_word('삐걱거리다').
	stop_word('사').
	stop_word('삼').
	stop_word('상대적으로 말하자면').
	stop_word('생각한대로').
	stop_word('설령').
	stop_word('설마').
	stop_word('설사').
	stop_word('셋').
	stop_word('소생').
	stop_word('소인').
	stop_word('솨').
	stop_word('쉿').
	stop_word('습니까').
	stop_word('습니다').
	stop_word('시각').
	stop_word('시간').
	stop_word('시작하여').
	stop_word('시초에').
	stop_word('시키다').
	stop_word('실로').
	stop_word('심지어').
	stop_word('아').
	stop_word('아니').
	stop_word('아니나다를가').
	stop_word('아니라면').
	stop_word('아니면').
	stop_word('아니었다면').
	stop_word('아래윗').
	stop_word('아무거나').
	stop_word('아무도').
	stop_word('아야').
	stop_word('아울러').
	stop_word('아이').
	stop_word('아이고').
	stop_word('아이구').
	stop_word('아이야').
	stop_word('아이쿠').
	stop_word('아하').
	stop_word('아홉').
	stop_word('안 그러면').
	stop_word('않기 위하여').
	stop_word('않기 위해서').
	stop_word('알 수 있다').
	stop_word('알았어').
	stop_word('앗').
	stop_word('앞에서').
	stop_word('앞의것').
	stop_word('야').
	stop_word('약간').
	stop_word('양자').
	stop_word('어').
	stop_word('어기여차').
	stop_word('어느').
	stop_word('어느 년도').
	stop_word('어느것').
	stop_word('어느곳').
	stop_word('어느때').
	stop_word('어느쪽').
	stop_word('어느해').
	stop_word('어디').
	stop_word('어때').
	stop_word('어떠한').
	stop_word('어떤').
	stop_word('어떤것').
	stop_word('어떤것들').
	stop_word('어떻게').
	stop_word('어떻해').
	stop_word('어이').
	stop_word('어째서').
	stop_word('어쨋든').
	stop_word('어쩔수 없다').
	stop_word('어찌').
	stop_word('어찌됏든').
	stop_word('어찌됏어').
	stop_word('어찌하든지').
	stop_word('어찌하여').
	stop_word('언제').
	stop_word('언젠가').
	stop_word('얼마').
	stop_word('얼마 안 되는 것').
	stop_word('얼마간').
	stop_word('얼마나').
	stop_word('얼마든지').
	stop_word('얼마만큼').
	stop_word('얼마큼').
	stop_word('엉엉').
	stop_word('에').
	stop_word('에 가서').
	stop_word('에 달려 있다').
	stop_word('에 대해').
	stop_word('에 있다').
	stop_word('에 한하다').
	stop_word('에게').
	stop_word('에서').
	stop_word('여').
	stop_word('여기').
	stop_word('여덟').
	stop_word('여러분').
	stop_word('여보시오').
	stop_word('여부').
	stop_word('여섯').
	stop_word('여전히').
	stop_word('여차').
	stop_word('연관되다').
	stop_word('연이서').
	stop_word('영').
	stop_word('영차').
	stop_word('옆사람').
	stop_word('예').
	stop_word('예를 들면').
	stop_word('예를 들자면').
	stop_word('예컨대').
	stop_word('예하면').
	stop_word('오').
	stop_word('오로지').
	stop_word('오르다').
	stop_word('오자마자').
	stop_word('오직').
	stop_word('오호').
	stop_word('오히려').
	stop_word('와').
	stop_word('와 같은 사람들').
	stop_word('와르르').
	stop_word('와아').
	stop_word('왜').
	stop_word('왜냐하면').
	stop_word('외에도').
	stop_word('요만큼').
	stop_word('요만한 것').
	stop_word('요만한걸').
	stop_word('요컨대').
	stop_word('우르르').
	stop_word('우리').
	stop_word('우리들').
	stop_word('우선').
	stop_word('우에 종합한것과같이').
	stop_word('운운').
	stop_word('월').
	stop_word('위에서 서술한바와같이').
	stop_word('위하여').
	stop_word('위해서').
	stop_word('윙윙').
	stop_word('육').
	stop_word('으로').
	stop_word('으로 인하여').
	stop_word('으로서').
	stop_word('으로써').
	stop_word('을').
	stop_word('응').
	stop_word('응당').
	stop_word('의').
	stop_word('의거하여').
	stop_word('의지하여').
	stop_word('의해').
	stop_word('의해되다').
	stop_word('의해서').
	stop_word('이').
	stop_word('이 되다').
	stop_word('이 때문에').
	stop_word('이 밖에').
	stop_word('이 외에').
	stop_word('이 정도의').
	stop_word('이것').
	stop_word('이곳').
	stop_word('이때').
	stop_word('이라면').
	stop_word('이래').
	stop_word('이러이러하다').
	stop_word('이러한').
	stop_word('이런').
	stop_word('이럴정도로').
	stop_word('이렇게 많은 것').
	stop_word('이렇게되면').
	stop_word('이렇게말하자면').
	stop_word('이렇구나').
	stop_word('이로 인하여').
	stop_word('이르기까지').
	stop_word('이리하여').
	stop_word('이만큼').
	stop_word('이번').
	stop_word('이봐').
	stop_word('이상').
	stop_word('이어서').
	stop_word('이었다').
	stop_word('이와 같다').
	stop_word('이와 같은').
	stop_word('이와 반대로').
	stop_word('이와같다면').
	stop_word('이외에도').
	stop_word('이용하여').
	stop_word('이유만으로').
	stop_word('이젠').
	stop_word('이지만').
	stop_word('이쪽').
	stop_word('이천구').
	stop_word('이천육').
	stop_word('이천칠').
	stop_word('이천팔').
	stop_word('인 듯하다').
	stop_word('인젠').
	stop_word('일').
	stop_word('일것이다').
	stop_word('일곱').
	stop_word('일단').
	stop_word('일때').
	stop_word('일반적으로').
	stop_word('일지라도').
	stop_word('임에 틀림없다').
	stop_word('입각하여').
	stop_word('입장에서').
	stop_word('잇따라').
	stop_word('있다').
	stop_word('자').
	stop_word('자기').
	stop_word('자기집').
	stop_word('자마자').
	stop_word('자신').
	stop_word('잠깐').
	stop_word('잠시').
	stop_word('저').
	stop_word('저것').
	stop_word('저것만큼').
	stop_word('저기').
	stop_word('저쪽').
	stop_word('저희').
	stop_word('전부').
	stop_word('전자').
	stop_word('전후').
	stop_word('점에서 보아').
	stop_word('정도에 이르다').
	stop_word('제').
	stop_word('제각기').
	stop_word('제외하고').
	stop_word('조금').
	stop_word('조차').
	stop_word('조차도').
	stop_word('졸졸').
	stop_word('좀').
	stop_word('좋아').
	stop_word('좍좍').
	stop_word('주룩주룩').
	stop_word('주저하지 않고').
	stop_word('줄은 몰랏다').
	stop_word('줄은모른다').
	stop_word('중에서').
	stop_word('중의하나').
	stop_word('즈음하여').
	stop_word('즉').
	stop_word('즉시').
	stop_word('지든지').
	stop_word('지만').
	stop_word('지말고').
	stop_word('진짜로').
	stop_word('쪽으로').
	stop_word('차라리').
	stop_word('참').
	stop_word('참나').
	stop_word('첫번째로').
	stop_word('쳇').
	stop_word('총적으로').
	stop_word('총적으로 말하면').
	stop_word('총적으로 보면').
	stop_word('칠').
	stop_word('콸콸').
	stop_word('쾅쾅').
	stop_word('쿵').
	stop_word('타다').
	stop_word('타인').
	stop_word('탕탕').
	stop_word('토하다').
	stop_word('통하여').
	stop_word('툭').
	stop_word('퉤').
	stop_word('틈타').
	stop_word('팍').
	stop_word('팔').
	stop_word('퍽').
	stop_word('펄렁').
	stop_word('하').
	stop_word('하게될것이다').
	stop_word('하게하다').
	stop_word('하겠는가').
	stop_word('하고 있다').
	stop_word('하고있었다').
	stop_word('하곤하였다').
	stop_word('하구나').
	stop_word('하기 때문에').
	stop_word('하기 위하여').
	stop_word('하기는한데').
	stop_word('하기만 하면').
	stop_word('하기보다는').
	stop_word('하기에').
	stop_word('하나').
	stop_word('하느니').
	stop_word('하는 김에').
	stop_word('하는 편이 낫다').
	stop_word('하는것도').
	stop_word('하는것만 못하다').
	stop_word('하는것이 낫다').
	stop_word('하는바').
	stop_word('하더라도').
	stop_word('하도다').
	stop_word('하도록시키다').
	stop_word('하도록하다').
	stop_word('하든지').
	stop_word('하려고하다').
	stop_word('하마터면').
	stop_word('하면 할수록').
	stop_word('하면된다').
	stop_word('하면서').
	stop_word('하물며').
	stop_word('하여금').
	stop_word('하여야').
	stop_word('하자마자').
	stop_word('하지 않는다면').
	stop_word('하지 않도록').
	stop_word('하지마').
	stop_word('하지마라').
	stop_word('하지만').
	stop_word('하하').
	stop_word('한 까닭에').
	stop_word('한 이유는').
	stop_word('한 후').
	stop_word('한다면').
	stop_word('한다면 몰라도').
	stop_word('한데').
	stop_word('한마디').
	stop_word('한적이있다').
	stop_word('한켠으로는').
	stop_word('한항목').
	stop_word('할 따름이다').
	stop_word('할 생각이다').
	stop_word('할 줄 안다').
	stop_word('할 지경이다').
	stop_word('할 힘이 있다').
	stop_word('할때').
	stop_word('할만하다').
	stop_word('할망정').
	stop_word('할뿐').
	stop_word('할수있다').
	stop_word('할수있어').
	stop_word('할줄알다').
	stop_word('할지라도').
	stop_word('할지언정').
	stop_word('함께').
	stop_word('해도된다').
	stop_word('해도좋다').
	stop_word('해봐요').
	stop_word('해서는 안된다').
	stop_word('해야한다').
	stop_word('해요').
	stop_word('했어요').
	stop_word('향하다').
	stop_word('향하여').
	stop_word('향해서').
	stop_word('허').
	stop_word('허걱').
	stop_word('허허').
	stop_word('헉').
	stop_word('헉헉').
	stop_word('헐떡헐떡').
	stop_word('형식으로 쓰여').
	stop_word('혹시').
	stop_word('혹은').
	stop_word('혼자').
	stop_word('훨씬').
	stop_word('휘익').
	stop_word('휴').
	stop_word('흐흐').
	stop_word('흥').
	stop_word('힘입어').
	stop_word('︿').
	stop_word('！').
	stop_word('＃').
	stop_word('＄').
	stop_word('％').
	stop_word('＆').
	stop_word('（').
	stop_word('）').
	stop_word('＊').
	stop_word('＋').
	stop_word('，').
	stop_word('０').
	stop_word('１').
	stop_word('２').
	stop_word('３').
	stop_word('４').
	stop_word('５').
	stop_word('６').
	stop_word('７').
	stop_word('８').
	stop_word('９').
	stop_word('：').
	stop_word('；').
	stop_word('＜').
	stop_word('＞').
	stop_word('？').
	stop_word('＠').
	stop_word('［').
	stop_word('］').
	stop_word('｛').
	stop_word('｜').
	stop_word('｝').
	stop_word('～').
	stop_word('￥').

:- end_object.
