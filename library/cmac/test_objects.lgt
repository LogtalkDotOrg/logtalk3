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


:- object(cmac_test_cipher_64,
	implements(block_cipher_prepared_key_protocol)).

	block_size(8).
	key_size(1).

	encrypt_block(Key, Block, EncryptedBlock) :-
		prepare_key(Key, PreparedKey),
		encrypt_prepared_block(PreparedKey, Block, EncryptedBlock).

	decrypt_block(Key, Block, DecryptedBlock) :-
		prepare_key(Key, PreparedKey),
		decrypt_prepared_block(PreparedKey, Block, DecryptedBlock).

	prepare_key([Key], cmac_test_prepared(Key)).

	encrypt_prepared_block(cmac_test_prepared(_), [Byte| Bytes], [EncryptedByte| Bytes]) :-
		EncryptedByte is xor(Byte, 0x80).

	decrypt_prepared_block(PreparedKey, Block, DecryptedBlock) :-
		encrypt_prepared_block(PreparedKey, Block, DecryptedBlock).

:- end_object.


:- object(cmac_test_cipher_unsupported,
	implements(block_cipher_prepared_key_protocol)).

	block_size(12).
	key_size(1).
	encrypt_block(_, _, _).
	decrypt_block(_, _, _).
	prepare_key(_, cmac_test_prepared).
	encrypt_prepared_block(_, _, _).
	decrypt_prepared_block(_, _, _).

:- end_object.
