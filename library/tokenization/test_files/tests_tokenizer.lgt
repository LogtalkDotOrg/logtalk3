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


	cover(tokenizer(_, _)).

	test(tokenizer_tokenize_2_empty, deterministic(Tokens == [])) :-
		tokenizer::tokenize("", Tokens).

	test(tokenizer_tokenize_2_whitespace, deterministic(Tokens == [])) :-
		tokenizer::tokenize(" \t\n", Tokens).

	test(tokenizer_tokenize_2_basic, deterministic(Tokens == ["Hello", ",", "world", "!"])) :-
		tokenizer::tokenize("Hello, world!", Tokens).

	test(tokenizer_tokenize_2_contraction_hyphen, deterministic(Tokens == ["Isn't", "well-known", "."])) :-
		tokenizer::tokenize("Isn't well-known.", Tokens).

	test(tokenizer_tokenize_2_abbreviation_number, deterministic(Tokens == ["Mr.", "Smith", "paid", "$", "12,500.25", "."])) :-
		tokenizer::tokenize("Mr. Smith paid $12,500.25.", Tokens).

	test(tokenizer_tokenize_2_signed_exponent_numbers, deterministic(Tokens == ["Values", "-42", ",", "+3.5", "and", "6.02e+23", "."])) :-
		tokenizer::tokenize("Values -42, +3.5 and 6.02e+23.", Tokens).

	test(tokenizer_tokenize_2_malformed_number_separators, deterministic(Tokens == ["1.2", ".", "3"])) :-
		tokenizer::tokenize("1.2.3", Tokens).

	test(tokenizer_tokenize_2_english_abbreviations, deterministic(Tokens == ["Capt.", "Lewis", "met", "Sen.", "Clark", "in", "Jan.", "2024", "with", "Morgan", ",", "Ph.D.", "."])) :-
		tokenizer::tokenize("Capt. Lewis met Sen. Clark in Jan. 2024 with Morgan, Ph.D..", Tokens).

	test(tokenizer_tokenize_2_web, deterministic(Tokens == ["See", "https://example.com/path", ",", "or", "me@example.org", "."])) :-
		tokenizer::tokenize("See https://example.com/path, or me@example.org.", Tokens).

	test(tokenizer_tokenize_2_urls, deterministic(Tokens == ["Fetch", "ftp://ftp.example.com/pub/file.zip", ",", "open", "https://[2001:db8::1]/path?x=1#part", ",", "visit", "www.example.com", ",", "or", "mailto:user@example.com", "."])) :-
		tokenizer::tokenize("Fetch ftp://ftp.example.com/pub/file.zip, open https://[2001:db8::1]/path?x=1#part, visit www.example.com, or mailto:user@example.com.", Tokens).

	test(tokenizer_tokenize_2_email, deterministic(Tokens == ["Email", "user_name@example.com", "."])) :-
		tokenizer::tokenize("Email user_name@example.com.", Tokens).

	test(tokenizer_tokenize_3_without_punctuation, deterministic(Tokens == ["Hello", "world"])) :-
		tokenizer::tokenize("Hello, world!", Tokens, [keep_punctuation(false)]).

	test(tokenizer_tokenize_3_lowercase, deterministic(Tokens == ["hello", "world"])) :-
		tokenizer::tokenize("Hello WORLD", Tokens, [lowercase(true)]).

	test(tokenizer_split_sentences_2_basic, deterministic(Sentences == ["One.", "Two?", "Three!"])) :-
		tokenizer::split_sentences("One. Two? Three!", Sentences).

	test(tokenizer_split_sentences_2_abbreviation_decimal, deterministic(Sentences == ["Dr. Brown arrived at 3.14 p.m.", "She left."])) :-
		tokenizer::split_sentences("Dr. Brown arrived at 3.14 p.m. She left.", Sentences).

	test(tokenizer_split_sentences_2_english_abbreviations, deterministic(Sentences == ["Capt. Lewis met Sen. Clark on Jan. 5.", "They discussed U.K. policy, e.g. trade."])) :-
		tokenizer::split_sentences("Capt. Lewis met Sen. Clark on Jan. 5. They discussed U.K. policy, e.g. trade.", Sentences).

	test(tokenizer_split_sentences_2_protected_terminal, deterministic(Sentences == ["See https://example.com.", "Value 3.14.", "Write me@example.org."])) :-
		tokenizer::split_sentences("See https://example.com. Value 3.14. Write me@example.org.", Sentences).

	test(tokenizer_tokenize_sentences_2_basic, deterministic(Sentences == [["She", "was", "late", "."], ["Really", "?"]])) :-
		tokenizer::tokenize_sentences("She was late. Really?", Sentences).

	test(tokenizer_tokenize_sentences_3_without_punctuation, deterministic(Sentences == [["She", "was", "late"], ["Really"]])) :-
		tokenizer::tokenize_sentences("She was late. Really?", Sentences, [keep_punctuation(false)]).

	test(tokenizer_custom_provider, deterministic(Tokens == ["Custom provider"])) :-
		tiny_tokenizer::tokenize("Custom provider", Tokens).

	test(tokenizer_invalid_representation, error(domain_error(text_representation, unsupported))) :-
		tokenizer(unsupported, english_tokenizer)::tokenize("text", _).

	test(tokenizer_variable_representation, error(instantiation_error)) :-
		tokenizer(_, english_tokenizer)::tokenize("text", _).

	test(tokenizer_variable_options, error(instantiation_error)) :-
		tokenizer::tokenize("text", _, _).

	test(tokenizer_invalid_option, error(domain_error(option, keep_punctuation(maybe)))) :-
		tokenizer::tokenize("text", _, [keep_punctuation(maybe)]).
