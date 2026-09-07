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


:- object(stopwords_he,
	implements(stop_words_language_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Gene Diaz',
		date is 2026-09-05,
		comment is 'Stop-word facts for the he language code.',
		copyright is 'Copyright (c) 2016 Gene Diaz',
		license is 'MIT',
		remarks is [
			'Source' - 'https://github.com/stopwords-iso/stopwords-he',
			'Source commit' - '79a1e86b6c3800c09210e939ef640b5e4af86d83',
			'Generated entries' - '194.'
		],
		see_also is [stop_words(_, _)]
	]).

	stop_word('אבל').
	stop_word('או').
	stop_word('אולי').
	stop_word('אותה').
	stop_word('אותו').
	stop_word('אותי').
	stop_word('אותך').
	stop_word('אותם').
	stop_word('אותן').
	stop_word('אותנו').
	stop_word('אז').
	stop_word('אחר').
	stop_word('אחרות').
	stop_word('אחרי').
	stop_word('אחריכן').
	stop_word('אחרים').
	stop_word('אחרת').
	stop_word('אי').
	stop_word('איזה').
	stop_word('איך').
	stop_word('אין').
	stop_word('איפה').
	stop_word('איתה').
	stop_word('איתו').
	stop_word('איתי').
	stop_word('איתך').
	stop_word('איתכם').
	stop_word('איתכן').
	stop_word('איתם').
	stop_word('איתן').
	stop_word('איתנו').
	stop_word('אך').
	stop_word('אל').
	stop_word('אלה').
	stop_word('אלו').
	stop_word('אם').
	stop_word('אנחנו').
	stop_word('אני').
	stop_word('אס').
	stop_word('אף').
	stop_word('אצל').
	stop_word('אשר').
	stop_word('את').
	stop_word('אתה').
	stop_word('אתכם').
	stop_word('אתכן').
	stop_word('אתם').
	stop_word('אתן').
	stop_word('באיזומידה').
	stop_word('באמצע').
	stop_word('באמצעות').
	stop_word('בגלל').
	stop_word('בין').
	stop_word('בלי').
	stop_word('במידה').
	stop_word('במקוםשבו').
	stop_word('ברם').
	stop_word('בשביל').
	stop_word('בשעהש').
	stop_word('בתוך').
	stop_word('גם').
	stop_word('דרך').
	stop_word('הוא').
	stop_word('היא').
	stop_word('היה').
	stop_word('היכן').
	stop_word('היתה').
	stop_word('היתי').
	stop_word('הם').
	stop_word('הן').
	stop_word('הנה').
	stop_word('הסיבהשבגללה').
	stop_word('הרי').
	stop_word('ואילו').
	stop_word('ואת').
	stop_word('זאת').
	stop_word('זה').
	stop_word('זות').
	stop_word('יהיה').
	stop_word('יוכל').
	stop_word('יוכלו').
	stop_word('יותרמדי').
	stop_word('יכול').
	stop_word('יכולה').
	stop_word('יכולות').
	stop_word('יכולים').
	stop_word('יכל').
	stop_word('יכלה').
	stop_word('יכלו').
	stop_word('יש').
	stop_word('כאן').
	stop_word('כאשר').
	stop_word('כולם').
	stop_word('כולן').
	stop_word('כזה').
	stop_word('כי').
	stop_word('כיצד').
	stop_word('כך').
	stop_word('ככה').
	stop_word('כל').
	stop_word('כלל').
	stop_word('כמו').
	stop_word('כן').
	stop_word('כפי').
	stop_word('כש').
	stop_word('לא').
	stop_word('לאו').
	stop_word('לאיזותכלית').
	stop_word('לאן').
	stop_word('לבין').
	stop_word('לה').
	stop_word('להיות').
	stop_word('להם').
	stop_word('להן').
	stop_word('לו').
	stop_word('לי').
	stop_word('לכם').
	stop_word('לכן').
	stop_word('למה').
	stop_word('למטה').
	stop_word('למעלה').
	stop_word('למקוםשבו').
	stop_word('למרות').
	stop_word('לנו').
	stop_word('לעבר').
	stop_word('לעיכן').
	stop_word('לפיכך').
	stop_word('לפני').
	stop_word('מאד').
	stop_word('מאחורי').
	stop_word('מאיזוסיבה').
	stop_word('מאין').
	stop_word('מאיפה').
	stop_word('מבלי').
	stop_word('מבעד').
	stop_word('מדוע').
	stop_word('מה').
	stop_word('מהיכן').
	stop_word('מול').
	stop_word('מחוץ').
	stop_word('מי').
	stop_word('מכאן').
	stop_word('מכיוון').
	stop_word('מלבד').
	stop_word('מן').
	stop_word('מנין').
	stop_word('מסוגל').
	stop_word('מעט').
	stop_word('מעטים').
	stop_word('מעל').
	stop_word('מצד').
	stop_word('מקוםבו').
	stop_word('מתחת').
	stop_word('מתי').
	stop_word('נגד').
	stop_word('נגר').
	stop_word('נו').
	stop_word('עד').
	stop_word('עז').
	stop_word('על').
	stop_word('עלי').
	stop_word('עליה').
	stop_word('עליהם').
	stop_word('עליהן').
	stop_word('עליו').
	stop_word('עליך').
	stop_word('עליכם').
	stop_word('עלינו').
	stop_word('עם').
	stop_word('עצמה').
	stop_word('עצמהם').
	stop_word('עצמהן').
	stop_word('עצמו').
	stop_word('עצמי').
	stop_word('עצמם').
	stop_word('עצמן').
	stop_word('עצמנו').
	stop_word('פה').
	stop_word('רק').
	stop_word('שוב').
	stop_word('של').
	stop_word('שלה').
	stop_word('שלהם').
	stop_word('שלהן').
	stop_word('שלו').
	stop_word('שלי').
	stop_word('שלך').
	stop_word('שלכה').
	stop_word('שלכם').
	stop_word('שלכן').
	stop_word('שלנו').
	stop_word('שם').
	stop_word('תהיה').
	stop_word('תחת').

:- end_object.
