%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	logtalk_load(strings(loader)),
	logtalk_load([
		stop_words_language_protocol,
		stop_words_protocol,
		stop_words,
		'languages/stopwords_af',
		'languages/stopwords_ar',
		'languages/stopwords_bg',
		'languages/stopwords_bn',
		'languages/stopwords_br',
		'languages/stopwords_ca',
		'languages/stopwords_cs',
		'languages/stopwords_da',
		'languages/stopwords_de',
		'languages/stopwords_el',
		'languages/stopwords_en',
		'languages/stopwords_eo',
		'languages/stopwords_es',
		'languages/stopwords_et',
		'languages/stopwords_eu',
		'languages/stopwords_fa',
		'languages/stopwords_fi',
		'languages/stopwords_fr',
		'languages/stopwords_ga',
		'languages/stopwords_gl',
		'languages/stopwords_ha',
		'languages/stopwords_he',
		'languages/stopwords_hi',
		'languages/stopwords_hr',
		'languages/stopwords_hu',
		'languages/stopwords_hy',
		'languages/stopwords_id',
		'languages/stopwords_it',
		'languages/stopwords_ja',
		'languages/stopwords_ko',
		'languages/stopwords_ku',
		'languages/stopwords_la',
		'languages/stopwords_lt',
		'languages/stopwords_lv',
		'languages/stopwords_mr',
		'languages/stopwords_ms',
		'languages/stopwords_nl',
		'languages/stopwords_no',
		'languages/stopwords_pl',
		'languages/stopwords_pt',
		'languages/stopwords_ro',
		'languages/stopwords_ru',
		'languages/stopwords_sk',
		'languages/stopwords_sl',
		'languages/stopwords_so',
		'languages/stopwords_st',
		'languages/stopwords_sv',
		'languages/stopwords_sw',
		'languages/stopwords_th',
		'languages/stopwords_tl',
		'languages/stopwords_tr',
		'languages/stopwords_uk',
		'languages/stopwords_ur',
		'languages/stopwords_vi',
		'languages/stopwords_yo',
		'languages/stopwords_zh',
		'languages/stopwords_zu'
	], [
		optimize(on)
	])
)).
