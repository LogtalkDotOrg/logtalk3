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


:- object(djb2_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'DJB2 32-bit hash function.',
		see_also is [djb2_64, sdbm_32, fnv1a_32]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	hash(Bytes, Hash) :-
		djb2_32(Bytes, 5381, Value),
		word32_hex(Value, Hash).

	new_hash_state(5381).

	update_hash_state(Acc0, Bytes, Acc) :-
		djb2_32(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		word32_hex(Acc, Hash).

	djb2_32([], Acc, Acc).
	djb2_32([Byte| Bytes], Acc0, Acc) :-
		Acc1 is ((Acc0 << 5) + Acc0 + Byte) /\ 0xFFFFFFFF,
		djb2_32(Bytes, Acc1, Acc).

:- end_object.


:- object(sdbm_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'sdbm 32-bit hash function.',
		see_also is [sdbm_64, djb2_32, fnv1a_32]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	hash(Bytes, Hash) :-
		sdbm_32(Bytes, 0, Value),
		word32_hex(Value, Hash).

	new_hash_state(0).

	update_hash_state(Acc0, Bytes, Acc) :-
		sdbm_32(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		word32_hex(Acc, Hash).

	sdbm_32([], Acc, Acc).
	sdbm_32([Byte| Bytes], Acc0, Acc) :-
		Acc1 is (Byte + (Acc0 << 6) + (Acc0 << 16) - Acc0) /\ 0xFFFFFFFF,
		sdbm_32(Bytes, Acc1, Acc).

:- end_object.


:- object(fnv1a_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'FNV-1a 32-bit hash function.',
		see_also is [fnv1a_64, djb2_32, sdbm_32]
	]).

	:- uses(hash_common_32, [
		word32_hex/2, mul32/3
	]).

	hash(Bytes, Hash) :-
		fnv1a_32(Bytes, 0x811C9DC5, Value),
		word32_hex(Value, Hash).

	new_hash_state(0x811C9DC5).

	update_hash_state(Acc0, Bytes, Acc) :-
		fnv1a_32(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		word32_hex(Acc, Hash).

	fnv1a_32([], Acc, Acc).
	fnv1a_32([Byte| Bytes], Acc0, Acc) :-
		Acc1 is xor(Acc0, Byte),
		mul32(Acc1, 0x01000193, Acc2),
		fnv1a_32(Bytes, Acc2, Acc).

:- end_object.


:- object(crc32_reflected(_Polynomial_),
	implements(hash_state_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Parametric reflected CRC-32 hash function using initial value 0xFFFFFFFF and final xor 0xFFFFFFFF.',
		parameters is [
			'Polynomial' - 'Reflected CRC-32 polynomial.'
		],
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32b, crc32c, crc32posix, crc32mpeg2, crc32bzip2, crc32q,
			murmurhash3_x86_32, fnv1a_32
		]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	hash(Bytes, Hash) :-
		crc32_reflected(Bytes, CRC32),
		word32_hex(CRC32, Hash).

	new_hash_state(0xFFFFFFFF).

	update_hash_state(Acc0, Bytes, Acc) :-
		crc32_acc(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		CRC32 is xor(Acc, 0xFFFFFFFF) /\ 0xFFFFFFFF,
		word32_hex(CRC32, Hash).

	crc32_reflected(Bytes, CRC32) :-
		crc32_acc(Bytes, 0xFFFFFFFF, Acc),
		CRC32 is xor(Acc, 0xFFFFFFFF) /\ 0xFFFFFFFF.

	crc32_acc([], Acc, Acc).
	crc32_acc([Byte| Bytes], Acc0, Acc) :-
		Index is xor(Acc0, Byte) /\ 0xFF,
		crc32_table_value(Index, TableValue),
		Acc1 is xor((Acc0 >> 8), TableValue) /\ 0xFFFFFFFF,
		crc32_acc(Bytes, Acc1, Acc).

	crc32_table_value(Index, Value) :-
		RowIndex is Index >> 4,
		crc32_table_row(RowIndex, _Polynomial_, Row),
		!,
		ColumnIndex is (Index /\ 15) + 1,
		arg(ColumnIndex, Row, Value).
	crc32_table_value(Index, Value) :-
		crc32_table_value(8, Index, Value).

	crc32_table_value(0, Value, Value) :-
		!.
	crc32_table_value(Count, Value0, Value) :-
		(	Value0 /\ 1 =:= 1 ->
			Value1 is xor((Value0 >> 1), _Polynomial_)
		;	Value1 is Value0 >> 1
		),
		NextCount is Count - 1,
		crc32_table_value(NextCount, Value1, Value).

	crc32_table_row(0, 0xEDB88320, row(0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA, 0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3, 0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988, 0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91)).
	crc32_table_row(1, 0xEDB88320, row(0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE, 0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7, 0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC, 0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5)).
	crc32_table_row(2, 0xEDB88320, row(0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172, 0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B, 0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940, 0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59)).
	crc32_table_row(3, 0xEDB88320, row(0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116, 0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F, 0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924, 0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D)).
	crc32_table_row(4, 0xEDB88320, row(0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A, 0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433, 0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818, 0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01)).
	crc32_table_row(5, 0xEDB88320, row(0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E, 0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457, 0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C, 0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65)).
	crc32_table_row(6, 0xEDB88320, row(0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2, 0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB, 0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0, 0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9)).
	crc32_table_row(7, 0xEDB88320, row(0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086, 0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F, 0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4, 0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD)).
	crc32_table_row(8, 0xEDB88320, row(0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A, 0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683, 0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8, 0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1)).
	crc32_table_row(9, 0xEDB88320, row(0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE, 0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7, 0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC, 0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5)).
	crc32_table_row(10, 0xEDB88320, row(0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252, 0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B, 0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60, 0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79)).
	crc32_table_row(11, 0xEDB88320, row(0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236, 0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F, 0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04, 0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D)).
	crc32_table_row(12, 0xEDB88320, row(0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A, 0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713, 0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38, 0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21)).
	crc32_table_row(13, 0xEDB88320, row(0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E, 0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777, 0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C, 0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45)).
	crc32_table_row(14, 0xEDB88320, row(0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2, 0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB, 0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0, 0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9)).
	crc32_table_row(15, 0xEDB88320, row(0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6, 0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF, 0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94, 0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D)).
	crc32_table_row(0, 0x82F63B78, row(0x00000000, 0xF26B8303, 0xE13B70F7, 0x1350F3F4, 0xC79A971F, 0x35F1141C, 0x26A1E7E8, 0xD4CA64EB, 0x8AD958CF, 0x78B2DBCC, 0x6BE22838, 0x9989AB3B, 0x4D43CFD0, 0xBF284CD3, 0xAC78BF27, 0x5E133C24)).
	crc32_table_row(1, 0x82F63B78, row(0x105EC76F, 0xE235446C, 0xF165B798, 0x030E349B, 0xD7C45070, 0x25AFD373, 0x36FF2087, 0xC494A384, 0x9A879FA0, 0x68EC1CA3, 0x7BBCEF57, 0x89D76C54, 0x5D1D08BF, 0xAF768BBC, 0xBC267848, 0x4E4DFB4B)).
	crc32_table_row(2, 0x82F63B78, row(0x20BD8EDE, 0xD2D60DDD, 0xC186FE29, 0x33ED7D2A, 0xE72719C1, 0x154C9AC2, 0x061C6936, 0xF477EA35, 0xAA64D611, 0x580F5512, 0x4B5FA6E6, 0xB93425E5, 0x6DFE410E, 0x9F95C20D, 0x8CC531F9, 0x7EAEB2FA)).
	crc32_table_row(3, 0x82F63B78, row(0x30E349B1, 0xC288CAB2, 0xD1D83946, 0x23B3BA45, 0xF779DEAE, 0x05125DAD, 0x1642AE59, 0xE4292D5A, 0xBA3A117E, 0x4851927D, 0x5B016189, 0xA96AE28A, 0x7DA08661, 0x8FCB0562, 0x9C9BF696, 0x6EF07595)).
	crc32_table_row(4, 0x82F63B78, row(0x417B1DBC, 0xB3109EBF, 0xA0406D4B, 0x522BEE48, 0x86E18AA3, 0x748A09A0, 0x67DAFA54, 0x95B17957, 0xCBA24573, 0x39C9C670, 0x2A993584, 0xD8F2B687, 0x0C38D26C, 0xFE53516F, 0xED03A29B, 0x1F682198)).
	crc32_table_row(5, 0x82F63B78, row(0x5125DAD3, 0xA34E59D0, 0xB01EAA24, 0x42752927, 0x96BF4DCC, 0x64D4CECF, 0x77843D3B, 0x85EFBE38, 0xDBFC821C, 0x2997011F, 0x3AC7F2EB, 0xC8AC71E8, 0x1C661503, 0xEE0D9600, 0xFD5D65F4, 0x0F36E6F7)).
	crc32_table_row(6, 0x82F63B78, row(0x61C69362, 0x93AD1061, 0x80FDE395, 0x72966096, 0xA65C047D, 0x5437877E, 0x4767748A, 0xB50CF789, 0xEB1FCBAD, 0x197448AE, 0x0A24BB5A, 0xF84F3859, 0x2C855CB2, 0xDEEEDFB1, 0xCDBE2C45, 0x3FD5AF46)).
	crc32_table_row(7, 0x82F63B78, row(0x7198540D, 0x83F3D70E, 0x90A324FA, 0x62C8A7F9, 0xB602C312, 0x44694011, 0x5739B3E5, 0xA55230E6, 0xFB410CC2, 0x092A8FC1, 0x1A7A7C35, 0xE811FF36, 0x3CDB9BDD, 0xCEB018DE, 0xDDE0EB2A, 0x2F8B6829)).
	crc32_table_row(8, 0x82F63B78, row(0x82F63B78, 0x709DB87B, 0x63CD4B8F, 0x91A6C88C, 0x456CAC67, 0xB7072F64, 0xA457DC90, 0x563C5F93, 0x082F63B7, 0xFA44E0B4, 0xE9141340, 0x1B7F9043, 0xCFB5F4A8, 0x3DDE77AB, 0x2E8E845F, 0xDCE5075C)).
	crc32_table_row(9, 0x82F63B78, row(0x92A8FC17, 0x60C37F14, 0x73938CE0, 0x81F80FE3, 0x55326B08, 0xA759E80B, 0xB4091BFF, 0x466298FC, 0x1871A4D8, 0xEA1A27DB, 0xF94AD42F, 0x0B21572C, 0xDFEB33C7, 0x2D80B0C4, 0x3ED04330, 0xCCBBC033)).
	crc32_table_row(10, 0x82F63B78, row(0xA24BB5A6, 0x502036A5, 0x4370C551, 0xB11B4652, 0x65D122B9, 0x97BAA1BA, 0x84EA524E, 0x7681D14D, 0x2892ED69, 0xDAF96E6A, 0xC9A99D9E, 0x3BC21E9D, 0xEF087A76, 0x1D63F975, 0x0E330A81, 0xFC588982)).
	crc32_table_row(11, 0x82F63B78, row(0xB21572C9, 0x407EF1CA, 0x532E023E, 0xA145813D, 0x758FE5D6, 0x87E466D5, 0x94B49521, 0x66DF1622, 0x38CC2A06, 0xCAA7A905, 0xD9F75AF1, 0x2B9CD9F2, 0xFF56BD19, 0x0D3D3E1A, 0x1E6DCDEE, 0xEC064EED)).
	crc32_table_row(12, 0x82F63B78, row(0xC38D26C4, 0x31E6A5C7, 0x22B65633, 0xD0DDD530, 0x0417B1DB, 0xF67C32D8, 0xE52CC12C, 0x1747422F, 0x49547E0B, 0xBB3FFD08, 0xA86F0EFC, 0x5A048DFF, 0x8ECEE914, 0x7CA56A17, 0x6FF599E3, 0x9D9E1AE0)).
	crc32_table_row(13, 0x82F63B78, row(0xD3D3E1AB, 0x21B862A8, 0x32E8915C, 0xC083125F, 0x144976B4, 0xE622F5B7, 0xF5720643, 0x07198540, 0x590AB964, 0xAB613A67, 0xB831C993, 0x4A5A4A90, 0x9E902E7B, 0x6CFBAD78, 0x7FAB5E8C, 0x8DC0DD8F)).
	crc32_table_row(14, 0x82F63B78, row(0xE330A81A, 0x115B2B19, 0x020BD8ED, 0xF0605BEE, 0x24AA3F05, 0xD6C1BC06, 0xC5914FF2, 0x37FACCF1, 0x69E9F0D5, 0x9B8273D6, 0x88D28022, 0x7AB90321, 0xAE7367CA, 0x5C18E4C9, 0x4F48173D, 0xBD23943E)).
	crc32_table_row(15, 0x82F63B78, row(0xF36E6F75, 0x0105EC76, 0x12551F82, 0xE03E9C81, 0x34F4F86A, 0xC69F7B69, 0xD5CF889D, 0x27A40B9E, 0x79B737BA, 0x8BDCB4B9, 0x988C474D, 0x6AE7C44E, 0xBE2DA0A5, 0x4C4623A6, 0x5F16D052, 0xAD7D5351)).

:- end_object.


:- object(crc32b,
	extends(crc32_reflected(0xEDB88320))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/ISO-HDLC hash function using the reflected polynomial 0xEDB88320, initial value 0xFFFFFFFF, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_reflected(_), crc32c, crc32posix, crc32mpeg2, crc32bzip2, crc32q, murmurhash3_x86_32,
			fnv1a_32
		]
	]).

:- end_object.


:- object(crc32c,
	extends(crc32_reflected(0x82F63B78))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32C/Castagnoli hash function using the reflected polynomial 0x82F63B78, initial value 0xFFFFFFFF, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_reflected(_), crc32b, crc32posix, crc32mpeg2, crc32bzip2, crc32q, murmurhash3_x86_32,
			fnv1a_32
		]
	]).

:- end_object.


:- object(crc32_non_reflected(_Polynomial_, _Initial_, _FinalXor_, _AppendLength_),
	implements(hash_state_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Parametric non-reflected CRC-32 hash function using a canonical polynomial, configurable initial value and final xor value, and optional appended little-endian length bytes.',
		parameters is [
			'Polynomial' - 'Canonical non-reflected CRC-32 polynomial.',
			'Initial' - 'Initial CRC accumulator value.',
			'FinalXor' - 'Final xor value.',
			'AppendLength' - 'Boolean flag controlling whether the message length is appended as little-endian bytes.'
		],
		see_also is [
			crc32_reflected(_), crc32b, crc32c, crc32posix, crc32mpeg2, crc32bzip2, crc32q,
			murmurhash3_x86_32, fnv1a_32
		]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	:- uses(list, [
		length/2
	]).

	hash(Bytes, Hash) :-
		crc32_non_reflected(Bytes, CRC32),
		word32_hex(CRC32, Hash),
		!.

	new_hash_state(state(_Initial_, 0)).

	update_hash_state(state(Acc0, Length0), Bytes, state(Acc, Length)) :-
		crc32_non_reflected_acc(Bytes, Acc0, Acc),
		length(Bytes, BytesLength),
		Length is Length0 + BytesLength.

	final_hash_state(state(Acc0, Length), Hash) :-
		(	_AppendLength_ == true ->
			integer_length_bytes(Length, LengthBytes),
			crc32_non_reflected_acc(LengthBytes, Acc0, Acc1)
		;	Acc1 = Acc0
		),
		CRC32 is xor(Acc1, _FinalXor_) /\ 0xFFFFFFFF,
		word32_hex(CRC32, Hash),
		!.

	crc32_non_reflected(Bytes, CRC32) :-
		crc32_non_reflected_acc(Bytes, _Initial_, Acc0),
		(	_AppendLength_ == true ->
			length_bytes(Bytes, LengthBytes),
			crc32_non_reflected_acc(LengthBytes, Acc0, Acc1)
		;	Acc1 = Acc0
		),
		CRC32 is xor(Acc1, _FinalXor_) /\ 0xFFFFFFFF.

	crc32_non_reflected_acc([], Acc, Acc).
	crc32_non_reflected_acc([Byte| Bytes], Acc0, Acc) :-
		step_byte(Acc0, Byte, Acc1),
		crc32_non_reflected_acc(Bytes, Acc1, Acc).

	step_byte(Acc0, Byte, Acc) :-
		Index is xor(Acc0 >> 24, Byte) /\ 0xFF,
		RowIndex is Index >> 4,
		crc32_table_row(RowIndex, _Polynomial_, Row),
		!,
		ColumnIndex is (Index /\ 15) + 1,
		arg(ColumnIndex, Row, Value),
		Acc is xor(Acc0 << 8, Value) /\ 0xFFFFFFFF.
	step_byte(Acc0, Byte, Acc) :-
		Acc1 is xor(Acc0, (Byte /\ 0xFF) << 24) /\ 0xFFFFFFFF,
		step_bits(8, Acc1, Acc).

	step_bits(0, Acc, Acc) :-
		!.
	step_bits(Count, Acc0, Acc) :-
		(	Acc0 /\ 0x80000000 =:= 0x80000000 ->
			Acc1 is xor((Acc0 << 1), _Polynomial_) /\ 0xFFFFFFFF
		;	Acc1 is (Acc0 << 1) /\ 0xFFFFFFFF
		),
		NextCount is Count - 1,
		step_bits(NextCount, Acc1, Acc).

	length_bytes(Bytes, LengthBytes) :-
		length(Bytes, Length),
		integer_length_bytes(Length, LengthBytes).

	integer_length_bytes(0, []) :-
		!.
	integer_length_bytes(Length, [Byte| Bytes]) :-
		Length > 0,
		Byte is Length /\ 0xFF,
		NextLength is Length >> 8,
		integer_length_bytes(NextLength, Bytes).

	crc32_table_row(0, 0x04C11DB7, row(0x00000000, 0x04C11DB7, 0x09823B6E, 0x0D4326D9, 0x130476DC, 0x17C56B6B, 0x1A864DB2, 0x1E475005, 0x2608EDB8, 0x22C9F00F, 0x2F8AD6D6, 0x2B4BCB61, 0x350C9B64, 0x31CD86D3, 0x3C8EA00A, 0x384FBDBD)).
	crc32_table_row(1, 0x04C11DB7, row(0x4C11DB70, 0x48D0C6C7, 0x4593E01E, 0x4152FDA9, 0x5F15ADAC, 0x5BD4B01B, 0x569796C2, 0x52568B75, 0x6A1936C8, 0x6ED82B7F, 0x639B0DA6, 0x675A1011, 0x791D4014, 0x7DDC5DA3, 0x709F7B7A, 0x745E66CD)).
	crc32_table_row(2, 0x04C11DB7, row(0x9823B6E0, 0x9CE2AB57, 0x91A18D8E, 0x95609039, 0x8B27C03C, 0x8FE6DD8B, 0x82A5FB52, 0x8664E6E5, 0xBE2B5B58, 0xBAEA46EF, 0xB7A96036, 0xB3687D81, 0xAD2F2D84, 0xA9EE3033, 0xA4AD16EA, 0xA06C0B5D)).
	crc32_table_row(3, 0x04C11DB7, row(0xD4326D90, 0xD0F37027, 0xDDB056FE, 0xD9714B49, 0xC7361B4C, 0xC3F706FB, 0xCEB42022, 0xCA753D95, 0xF23A8028, 0xF6FB9D9F, 0xFBB8BB46, 0xFF79A6F1, 0xE13EF6F4, 0xE5FFEB43, 0xE8BCCD9A, 0xEC7DD02D)).
	crc32_table_row(4, 0x04C11DB7, row(0x34867077, 0x30476DC0, 0x3D044B19, 0x39C556AE, 0x278206AB, 0x23431B1C, 0x2E003DC5, 0x2AC12072, 0x128E9DCF, 0x164F8078, 0x1B0CA6A1, 0x1FCDBB16, 0x018AEB13, 0x054BF6A4, 0x0808D07D, 0x0CC9CDCA)).
	crc32_table_row(5, 0x04C11DB7, row(0x7897AB07, 0x7C56B6B0, 0x71159069, 0x75D48DDE, 0x6B93DDDB, 0x6F52C06C, 0x6211E6B5, 0x66D0FB02, 0x5E9F46BF, 0x5A5E5B08, 0x571D7DD1, 0x53DC6066, 0x4D9B3063, 0x495A2DD4, 0x44190B0D, 0x40D816BA)).
	crc32_table_row(6, 0x04C11DB7, row(0xACA5C697, 0xA864DB20, 0xA527FDF9, 0xA1E6E04E, 0xBFA1B04B, 0xBB60ADFC, 0xB6238B25, 0xB2E29692, 0x8AAD2B2F, 0x8E6C3698, 0x832F1041, 0x87EE0DF6, 0x99A95DF3, 0x9D684044, 0x902B669D, 0x94EA7B2A)).
	crc32_table_row(7, 0x04C11DB7, row(0xE0B41DE7, 0xE4750050, 0xE9362689, 0xEDF73B3E, 0xF3B06B3B, 0xF771768C, 0xFA325055, 0xFEF34DE2, 0xC6BCF05F, 0xC27DEDE8, 0xCF3ECB31, 0xCBFFD686, 0xD5B88683, 0xD1799B34, 0xDC3ABDED, 0xD8FBA05A)).
	crc32_table_row(8, 0x04C11DB7, row(0x690CE0EE, 0x6DCDFD59, 0x608EDB80, 0x644FC637, 0x7A089632, 0x7EC98B85, 0x738AAD5C, 0x774BB0EB, 0x4F040D56, 0x4BC510E1, 0x46863638, 0x42472B8F, 0x5C007B8A, 0x58C1663D, 0x558240E4, 0x51435D53)).
	crc32_table_row(9, 0x04C11DB7, row(0x251D3B9E, 0x21DC2629, 0x2C9F00F0, 0x285E1D47, 0x36194D42, 0x32D850F5, 0x3F9B762C, 0x3B5A6B9B, 0x0315D626, 0x07D4CB91, 0x0A97ED48, 0x0E56F0FF, 0x1011A0FA, 0x14D0BD4D, 0x19939B94, 0x1D528623)).
	crc32_table_row(10, 0x04C11DB7, row(0xF12F560E, 0xF5EE4BB9, 0xF8AD6D60, 0xFC6C70D7, 0xE22B20D2, 0xE6EA3D65, 0xEBA91BBC, 0xEF68060B, 0xD727BBB6, 0xD3E6A601, 0xDEA580D8, 0xDA649D6F, 0xC423CD6A, 0xC0E2D0DD, 0xCDA1F604, 0xC960EBB3)).
	crc32_table_row(11, 0x04C11DB7, row(0xBD3E8D7E, 0xB9FF90C9, 0xB4BCB610, 0xB07DABA7, 0xAE3AFBA2, 0xAAFBE615, 0xA7B8C0CC, 0xA379DD7B, 0x9B3660C6, 0x9FF77D71, 0x92B45BA8, 0x9675461F, 0x8832161A, 0x8CF30BAD, 0x81B02D74, 0x857130C3)).
	crc32_table_row(12, 0x04C11DB7, row(0x5D8A9099, 0x594B8D2E, 0x5408ABF7, 0x50C9B640, 0x4E8EE645, 0x4A4FFBF2, 0x470CDD2B, 0x43CDC09C, 0x7B827D21, 0x7F436096, 0x7200464F, 0x76C15BF8, 0x68860BFD, 0x6C47164A, 0x61043093, 0x65C52D24)).
	crc32_table_row(13, 0x04C11DB7, row(0x119B4BE9, 0x155A565E, 0x18197087, 0x1CD86D30, 0x029F3D35, 0x065E2082, 0x0B1D065B, 0x0FDC1BEC, 0x3793A651, 0x3352BBE6, 0x3E119D3F, 0x3AD08088, 0x2497D08D, 0x2056CD3A, 0x2D15EBE3, 0x29D4F654)).
	crc32_table_row(14, 0x04C11DB7, row(0xC5A92679, 0xC1683BCE, 0xCC2B1D17, 0xC8EA00A0, 0xD6AD50A5, 0xD26C4D12, 0xDF2F6BCB, 0xDBEE767C, 0xE3A1CBC1, 0xE760D676, 0xEA23F0AF, 0xEEE2ED18, 0xF0A5BD1D, 0xF464A0AA, 0xF9278673, 0xFDE69BC4)).
	crc32_table_row(15, 0x04C11DB7, row(0x89B8FD09, 0x8D79E0BE, 0x803AC667, 0x84FBDBD0, 0x9ABC8BD5, 0x9E7D9662, 0x933EB0BB, 0x97FFAD0C, 0xAFB010B1, 0xAB710D06, 0xA6322BDF, 0xA2F33668, 0xBCB4666D, 0xB8757BDA, 0xB5365D03, 0xB1F740B4)).
	crc32_table_row(0, 0x814141AB, row(0x00000000, 0x814141AB, 0x83C3C2FD, 0x02828356, 0x86C6C451, 0x078785FA, 0x050506AC, 0x84444707, 0x8CCCC909, 0x0D8D88A2, 0x0F0F0BF4, 0x8E4E4A5F, 0x0A0A0D58, 0x8B4B4CF3, 0x89C9CFA5, 0x08888E0E)).
	crc32_table_row(1, 0x814141AB, row(0x98D8D3B9, 0x19999212, 0x1B1B1144, 0x9A5A50EF, 0x1E1E17E8, 0x9F5F5643, 0x9DDDD515, 0x1C9C94BE, 0x14141AB0, 0x95555B1B, 0x97D7D84D, 0x169699E6, 0x92D2DEE1, 0x13939F4A, 0x11111C1C, 0x90505DB7)).
	crc32_table_row(2, 0x814141AB, row(0xB0F0E6D9, 0x31B1A772, 0x33332424, 0xB272658F, 0x36362288, 0xB7776323, 0xB5F5E075, 0x34B4A1DE, 0x3C3C2FD0, 0xBD7D6E7B, 0xBFFFED2D, 0x3EBEAC86, 0xBAFAEB81, 0x3BBBAA2A, 0x3939297C, 0xB87868D7)).
	crc32_table_row(3, 0x814141AB, row(0x28283560, 0xA96974CB, 0xABEBF79D, 0x2AAAB636, 0xAEEEF131, 0x2FAFB09A, 0x2D2D33CC, 0xAC6C7267, 0xA4E4FC69, 0x25A5BDC2, 0x27273E94, 0xA6667F3F, 0x22223838, 0xA3637993, 0xA1E1FAC5, 0x20A0BB6E)).
	crc32_table_row(4, 0x814141AB, row(0xE0A08C19, 0x61E1CDB2, 0x63634EE4, 0xE2220F4F, 0x66664848, 0xE72709E3, 0xE5A58AB5, 0x64E4CB1E, 0x6C6C4510, 0xED2D04BB, 0xEFAF87ED, 0x6EEEC646, 0xEAAA8141, 0x6BEBC0EA, 0x696943BC, 0xE8280217)).
	crc32_table_row(5, 0x814141AB, row(0x78785FA0, 0xF9391E0B, 0xFBBB9D5D, 0x7AFADCF6, 0xFEBE9BF1, 0x7FFFDA5A, 0x7D7D590C, 0xFC3C18A7, 0xF4B496A9, 0x75F5D702, 0x77775454, 0xF63615FF, 0x727252F8, 0xF3331353, 0xF1B19005, 0x70F0D1AE)).
	crc32_table_row(6, 0x814141AB, row(0x50506AC0, 0xD1112B6B, 0xD393A83D, 0x52D2E996, 0xD696AE91, 0x57D7EF3A, 0x55556C6C, 0xD4142DC7, 0xDC9CA3C9, 0x5DDDE262, 0x5F5F6134, 0xDE1E209F, 0x5A5A6798, 0xDB1B2633, 0xD999A565, 0x58D8E4CE)).
	crc32_table_row(7, 0x814141AB, row(0xC888B979, 0x49C9F8D2, 0x4B4B7B84, 0xCA0A3A2F, 0x4E4E7D28, 0xCF0F3C83, 0xCD8DBFD5, 0x4CCCFE7E, 0x44447070, 0xC50531DB, 0xC787B28D, 0x46C6F326, 0xC282B421, 0x43C3F58A, 0x414176DC, 0xC0003777)).
	crc32_table_row(8, 0x814141AB, row(0x40005999, 0xC1411832, 0xC3C39B64, 0x4282DACF, 0xC6C69DC8, 0x4787DC63, 0x45055F35, 0xC4441E9E, 0xCCCC9090, 0x4D8DD13B, 0x4F0F526D, 0xCE4E13C6, 0x4A0A54C1, 0xCB4B156A, 0xC9C9963C, 0x4888D797)).
	crc32_table_row(9, 0x814141AB, row(0xD8D88A20, 0x5999CB8B, 0x5B1B48DD, 0xDA5A0976, 0x5E1E4E71, 0xDF5F0FDA, 0xDDDD8C8C, 0x5C9CCD27, 0x54144329, 0xD5550282, 0xD7D781D4, 0x5696C07F, 0xD2D28778, 0x5393C6D3, 0x51114585, 0xD050042E)).
	crc32_table_row(10, 0x814141AB, row(0xF0F0BF40, 0x71B1FEEB, 0x73337DBD, 0xF2723C16, 0x76367B11, 0xF7773ABA, 0xF5F5B9EC, 0x74B4F847, 0x7C3C7649, 0xFD7D37E2, 0xFFFFB4B4, 0x7EBEF51F, 0xFAFAB218, 0x7BBBF3B3, 0x793970E5, 0xF878314E)).
	crc32_table_row(11, 0x814141AB, row(0x68286CF9, 0xE9692D52, 0xEBEBAE04, 0x6AAAEFAF, 0xEEEEA8A8, 0x6FAFE903, 0x6D2D6A55, 0xEC6C2BFE, 0xE4E4A5F0, 0x65A5E45B, 0x6727670D, 0xE66626A6, 0x622261A1, 0xE363200A, 0xE1E1A35C, 0x60A0E2F7)).
	crc32_table_row(12, 0x814141AB, row(0xA0A0D580, 0x21E1942B, 0x2363177D, 0xA22256D6, 0x266611D1, 0xA727507A, 0xA5A5D32C, 0x24E49287, 0x2C6C1C89, 0xAD2D5D22, 0xAFAFDE74, 0x2EEE9FDF, 0xAAAAD8D8, 0x2BEB9973, 0x29691A25, 0xA8285B8E)).
	crc32_table_row(13, 0x814141AB, row(0x38780639, 0xB9394792, 0xBBBBC4C4, 0x3AFA856F, 0xBEBEC268, 0x3FFF83C3, 0x3D7D0095, 0xBC3C413E, 0xB4B4CF30, 0x35F58E9B, 0x37770DCD, 0xB6364C66, 0x32720B61, 0xB3334ACA, 0xB1B1C99C, 0x30F08837)).
	crc32_table_row(14, 0x814141AB, row(0x10503359, 0x911172F2, 0x9393F1A4, 0x12D2B00F, 0x9696F708, 0x17D7B6A3, 0x155535F5, 0x9414745E, 0x9C9CFA50, 0x1DDDBBFB, 0x1F5F38AD, 0x9E1E7906, 0x1A5A3E01, 0x9B1B7FAA, 0x9999FCFC, 0x18D8BD57)).
	crc32_table_row(15, 0x814141AB, row(0x8888E0E0, 0x09C9A14B, 0x0B4B221D, 0x8A0A63B6, 0x0E4E24B1, 0x8F0F651A, 0x8D8DE64C, 0x0CCCA7E7, 0x044429E9, 0x85056842, 0x8787EB14, 0x06C6AABF, 0x8282EDB8, 0x03C3AC13, 0x01412F45, 0x80006EEE)).

:- end_object.


:- object(crc32posix,
	extends(crc32_non_reflected(0x04C11DB7, 0x00000000, 0xFFFFFFFF, true))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/POSIX (cksum) hash function using the canonical polynomial 0x04C11DB7, initial value 0x00000000, appended little-endian length bytes, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32_reflected(_), crc32b, crc32c, crc32mpeg2, crc32bzip2,
			crc32q, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(crc32mpeg2,
	extends(crc32_non_reflected(0x04C11DB7, 0xFFFFFFFF, 0x00000000, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/MPEG-2 hash function using the canonical polynomial 0x04C11DB7, initial value 0xFFFFFFFF, and final xor 0x00000000.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32posix, crc32bzip2, crc32q, crc32_reflected(_), crc32b,
			crc32c, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(crc32bzip2,
	extends(crc32_non_reflected(0x04C11DB7, 0xFFFFFFFF, 0xFFFFFFFF, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/BZIP2 hash function using the canonical polynomial 0x04C11DB7, initial value 0xFFFFFFFF, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32mpeg2, crc32posix, crc32q, crc32_reflected(_), crc32b,
			crc32c, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(crc32q,
	extends(crc32_non_reflected(0x814141AB, 0x00000000, 0x00000000, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32Q hash function, also used by AIXM-style formats, using the canonical polynomial 0x814141AB, initial value 0x00000000, and final xor 0x00000000.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32mpeg2, crc32bzip2, crc32posix, crc32_reflected(_), crc32b,
			crc32c, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(murmurhash3_x86_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'MurmurHash3 x86 32-bit hash function with seed 0.',
		see_also is [murmurhash3_x86_128, murmurhash3_x64_128]
	]).

	:- uses(hash_common_32, [
		word32_hex/2, mul32/3, rol32/3
	]).

	:- uses(list, [
		append/3, length/2
	]).

	hash(Bytes, Hash) :-
		murmurhash3_x86_32(Bytes, 0, Value),
		word32_hex(Value, Hash).

	% the state buffers the at-most-3 leftover bytes that do not yet form a
	% complete 4-byte block; Length is the total number of bytes seen so
	% far, needed only at finalization
	new_hash_state(state([], 0, 0)).

	update_hash_state(state(Buffer0, Length0, H0), Bytes, state(Buffer1, Length1, H1)) :-
		append(Buffer0, Bytes, Combined),
		length(Bytes, BytesLength),
		Length1 is Length0 + BytesLength,
		body(Combined, H0, _, H1, Buffer1).

	final_hash_state(state(Buffer, Length, H1), Hash) :-
		tail(Buffer, K1),
		H2 is xor(H1, K1),
		H3 is xor(H2, Length),
		fmix(H3, Value),
		word32_hex(Value, Hash).

	murmurhash3_x86_32(Bytes, Seed, Hash) :-
		length(Bytes, Length),
		body(Bytes, Seed, Length, H1, Tail),
		tail(Tail, K1),
		H2 is xor(H1, K1),
		H3 is xor(H2, Length),
		fmix(H3, Hash).

	body([B0, B1, B2, B3| Bytes], H0, Length, H, Tail) :-
		!,
		byte_order::bytes_to_integer(little, [B0, B1, B2, B3], K0),
		block(K0, H0, H1),
		body(Bytes, H1, Length, H, Tail).
	body(Tail, H, _, H, Tail).

	block(K0, H0, H) :-
		mul32(K0, 0xCC9E2D51, K1),
		rol32(K1, 15, K2),
		mul32(K2, 0x1B873593, K3),
		H1 is xor(H0, K3),
		rol32(H1, 13, H2),
		mul32(H2, 5, H3),
		H is (H3 + 0xE6546B64) /\ 0xFFFFFFFF.

	tail([], 0).
	tail([B0], K1) :-
		!,
		K0 is B0,
		tail_mix(K0, K1).
	tail([B0, B1], K1) :-
		!,
		K0 is B0 \/ (B1 << 8),
		tail_mix(K0, K1).
	tail([B0, B1, B2], K1) :-
		K0 is B0 \/ (B1 << 8) \/ (B2 << 16),
		tail_mix(K0, K1).

	tail_mix(K0, K1) :-
		mul32(K0, 0xCC9E2D51, K2),
		rol32(K2, 15, K3),
		mul32(K3, 0x1B873593, K1).

	fmix(H0, H) :-
		H1 is xor(H0, H0 >> 16),
		mul32(H1, 0x85EBCA6B, H2),
		H3 is xor(H2, H2 >> 13),
		mul32(H3, 0xC2B2AE35, H4),
		H is xor(H4, H4 >> 16) /\ 0xFFFFFFFF.

:- end_object.


:- category(blake2_core(_Key_, _DigestSize_),
	implements([hash_digest_protocol, hash_state_protocol])).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Common BLAKE2 hash function core.',
		parameters is [
			'Key' - 'A list of key bytes. Use the empty list for unkeyed hashing.',
			'DigestSize' - 'Number of digest bytes to generate.'
		]
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(list, [
		append/3, length/2, nth0/3
	]).

	:- protected(blake2_max_digest_size/1).
	:- mode(blake2_max_digest_size(--integer), one).
	:- info(blake2_max_digest_size/1, [
		comment is 'Returns the maximum digest size in bytes for the BLAKE2 variant.',
		argnames is ['MaxDigestSize']
	]).

	:- protected(blake2_max_key_size/1).
	:- mode(blake2_max_key_size(--integer), one).
	:- info(blake2_max_key_size/1, [
		comment is 'Returns the maximum key size in bytes for the BLAKE2 variant.',
		argnames is ['MaxKeySize']
	]).

	:- protected(blake2_block_size/1).
	:- mode(blake2_block_size(--integer), one).
	:- info(blake2_block_size/1, [
		comment is 'Returns the block size in bytes for the BLAKE2 variant.',
		argnames is ['BlockSize']
	]).

	:- protected(blake2_round_count/1).
	:- mode(blake2_round_count(--integer), one).
	:- info(blake2_round_count/1, [
		comment is 'Returns the number of compression rounds for the BLAKE2 variant.',
		argnames is ['RoundCount']
	]).

	:- protected(blake2_initial_state/2).
	:- mode(blake2_initial_state(+integer, --list(integer)), one).
	:- info(blake2_initial_state/2, [
		comment is 'Computes the initial hash state from the parameter word.',
		argnames is ['ParameterWord', 'InitialState']
	]).

	:- protected(blake2_working_vector/4).
	:- mode(blake2_working_vector(+list(integer), +integer, +boolean, --list(integer)), one).
	:- info(blake2_working_vector/4, [
		comment is 'Initializes the compression working vector from the hash state, byte counter, and final block flag.',
		argnames is ['State', 'Total', 'Final', 'WorkingVector']
	]).

	:- protected(blake2_block_words/2).
	:- mode(blake2_block_words(+list(byte), --list(integer)), one).
	:- info(blake2_block_words/2, [
		comment is 'Decodes a block of bytes into the message words used by the compression function.',
		argnames is ['Block', 'MessageWords']
	]).

	:- protected(blake2_g/10).
	:- mode(blake2_g(+integer, +integer, +integer, +integer, +integer, +integer, --integer, --integer, --integer, --integer), one).
	:- info(blake2_g/10, [
		comment is 'Applies the BLAKE2 G mixing function to four scalar working words and two message words.',
		argnames is ['A0', 'B0', 'C0', 'D0', 'X', 'Y', 'A', 'B', 'C', 'D']
	]).

	:- protected(blake2_finalize/4).
	:- mode(blake2_finalize(+list(integer), +list(integer), +integer, --list(integer)), one).
	:- info(blake2_finalize/4, [
		comment is 'Finalizes the hash state by combining it with the compression working vector.',
		argnames is ['HashState', 'WorkingVector', 'Index', 'FinalState']
	]).

	:- protected(blake2_state_bytes/2).
	:- mode(blake2_state_bytes(+list(integer), --list(byte)), one).
	:- info(blake2_state_bytes/2, [
		comment is 'Encodes the hash state words as little-endian bytes.',
		argnames is ['State', 'Bytes']
	]).

	digest(Bytes, DigestBytes) :-
		new_hash_state(State0),
		update_hash_state(State0, Bytes, State1),
		final_digest_state(State1, DigestBytes).

	digest_size(_DigestSize_) :-
		valid_parameters(_).

	block_size(BlockSize) :-
		valid_parameters(_),
		::blake2_block_size(BlockSize).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	% unlike the MD-style hashes, BLAKE2's compression function is given an
	% explicit flag marking whether the block being compressed is the last
	% one, and that flag changes the digest, not just an appended length
	% field; the state must therefore always hold back a full block (up to
	% the algorithm block size) that has not yet been compressed, since a block that
	% currently looks complete may still turn out not to be the last one
	% once more bytes arrive in a later update_hash_state/3 call. Only at
	% final_hash_state/2, when no more bytes can arrive, is the held-back
	% buffer known to be the final block
	new_hash_state(state(Buffer, 0, InitialState)) :-
		valid_parameters(KeyLength),
		blake2_parameter_word(KeyLength, ParameterWord),
		::blake2_initial_state(ParameterWord, InitialState),
		::blake2_block_size(BlockSize),
		initial_buffer(KeyLength, BlockSize, Buffer).

	update_hash_state(state(Buffer0, Total0, State0), Bytes, state(Buffer1, Total1, State1)) :-
		append(Buffer0, Bytes, Combined),
		consume_full_blocks(Combined, Total0, State0, Buffer1, Total1, State1).

	final_hash_state(state(Buffer, Total0, State0), Hash) :-
		valid_parameters(_),
		final_digest_state(state(Buffer, Total0, State0), DigestBytes),
		bytes_hex(DigestBytes, Hash).

	final_digest_state(state(Buffer, Total0, State0), DigestBytes) :-
		::blake2_block_size(BlockSize),
		length(Buffer, BlockLength),
		Total is Total0 + BlockLength,
		pad_block(Buffer, BlockSize, Block),
		compress(State0, Block, Total, true, State),
		::blake2_state_bytes(State, StateBytes),
		take_up_to(_DigestSize_, StateBytes, DigestBytes, _).

	% consumes complete, definitely-not-final blocks from Buffer,
	% leaving between 1 and BlockSize bytes (or 0, only if Buffer started empty
	% and stays empty) as the new held-back buffer
	consume_full_blocks(Buffer, Total0, State0, FinalBuffer, Total, State) :-
		::blake2_block_size(BlockSize),
		take_up_to(BlockSize, Buffer, Block, Rest),
		Rest = [_| _],
		!,
		Total1 is Total0 + BlockSize,
		compress(State0, Block, Total1, false, State1),
		consume_full_blocks(Rest, Total1, State1, FinalBuffer, Total, State).
	consume_full_blocks(Buffer, Total, State, Buffer, Total, State).

	compress(State0, Block, Total, Final, State) :-
		::blake2_block_words(Block, MessageWords),
		Message =.. [words| MessageWords],
		::blake2_working_vector(State0, Total, Final, Working0),
		rounds(0, Message, Working0, Working),
		::blake2_finalize(State0, Working, 0, State).

	rounds(Round, _, Working, Working) :-
		::blake2_round_count(RoundCount),
		Round =:= RoundCount,
		!.
	rounds(Round, MessageWords, Working0, Working) :-
		blake2_sigma(Round, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		arg(S0, MessageWords, M0),
		arg(S1, MessageWords, M1),
		arg(S2, MessageWords, M2),
		arg(S3, MessageWords, M3),
		arg(S4, MessageWords, M4),
		arg(S5, MessageWords, M5),
		arg(S6, MessageWords, M6),
		arg(S7, MessageWords, M7),
		arg(S8, MessageWords, M8),
		arg(S9, MessageWords, M9),
		arg(S10, MessageWords, M10),
		arg(S11, MessageWords, M11),
		arg(S12, MessageWords, M12),
		arg(S13, MessageWords, M13),
		arg(S14, MessageWords, M14),
		arg(S15, MessageWords, M15),
		Working0 = [V0, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15],
		::blake2_g(V0, V4, V8, V12, M0, M1, A0, A4, A8, A12),
		::blake2_g(V1, V5, V9, V13, M2, M3, A1, A5, A9, A13),
		::blake2_g(V2, V6, V10, V14, M4, M5, A2, A6, A10, A14),
		::blake2_g(V3, V7, V11, V15, M6, M7, A3, A7, A11, A15),
		::blake2_g(A0, A5, A10, A15, M8, M9, B0, B5, B10, B15),
		::blake2_g(A1, A6, A11, A12, M10, M11, B1, B6, B11, B12),
		::blake2_g(A2, A7, A8, A13, M12, M13, B2, B7, B8, B13),
		::blake2_g(A3, A4, A9, A14, M14, M15, B3, B4, B9, B14),
		Working1 = [B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15],
		NextRound is Round + 1,
		rounds(NextRound, MessageWords, Working1, Working).

	valid_parameters(KeyLength) :-
		valid_digest_size,
		valid_key(KeyLength).

	valid_digest_size :-
		var(_DigestSize_),
		instantiation_error.
	valid_digest_size :-
		integer(_DigestSize_),
		::blake2_max_digest_size(MaxDigestSize),
		_DigestSize_ >= 1,
		_DigestSize_ =< MaxDigestSize,
		!.
	valid_digest_size :-
		domain_error(blake2_digest_size, _DigestSize_).

	valid_key(KeyLength) :-
		key_length(_Key_, KeyLength),
		::blake2_max_key_size(MaxKeySize),
		(	KeyLength =< MaxKeySize ->
			true
		;	domain_error(blake2_key, _Key_)
		).

	key_length(Key, _) :-
		\+ ground(Key),
		instantiation_error.
	key_length(Key, Length) :-
		key_length_checked(Key, 0, Length),
		!.
	key_length(Key, _) :-
		domain_error(blake2_key, Key).

	key_length_checked([], Length, Length).
	key_length_checked([Byte| Bytes], Length0, Length) :-
		integer(Byte),
		Byte >= 0,
		Byte =< 255,
		Length1 is Length0 + 1,
		key_length_checked(Bytes, Length1, Length).

	blake2_parameter_word(KeyLength, ParameterWord) :-
		ParameterWord is _DigestSize_ + (KeyLength << 8) + 0x01010000.

	initial_buffer(0, _, []) :-
		!.
	initial_buffer(_, BlockSize, Buffer) :-
		pad_block(_Key_, BlockSize, Buffer).

	pad_block(BlockBytes, BlockSize, Block) :-
		length(BlockBytes, Length),
		Padding is BlockSize - Length,
		zeros(Padding, ZeroBytes),
		append(BlockBytes, ZeroBytes, Block).

	zeros(0, []) :-
		!.
	zeros(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zeros(NextCount, Bytes).

	take_up_to(0, Rest, [], Rest) :-
		!.
	take_up_to(_, [], [], []) :-
		!.
	take_up_to(Count, [Byte| Bytes], [Byte| Prefix], Rest) :-
		NextCount is Count - 1,
		take_up_to(NextCount, Bytes, Prefix, Rest).

	blake2_sigma(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16).
	blake2_sigma(1, 15, 11, 5, 9, 10, 16, 14, 7, 2, 13, 1, 3, 12, 8, 6, 4).
	blake2_sigma(2, 12, 9, 13, 1, 6, 3, 16, 14, 11, 15, 4, 7, 8, 2, 10, 5).
	blake2_sigma(3, 8, 10, 4, 2, 14, 13, 12, 15, 3, 7, 6, 11, 5, 1, 16, 9).
	blake2_sigma(4, 10, 1, 6, 8, 3, 5, 11, 16, 15, 2, 12, 13, 7, 9, 4, 14).
	blake2_sigma(5, 3, 13, 7, 11, 1, 12, 9, 4, 5, 14, 8, 6, 16, 15, 2, 10).
	blake2_sigma(6, 13, 6, 2, 16, 15, 14, 5, 11, 1, 8, 7, 4, 10, 3, 9, 12).
	blake2_sigma(7, 14, 12, 8, 15, 13, 2, 4, 10, 6, 1, 16, 5, 9, 7, 3, 11).
	blake2_sigma(8, 7, 16, 15, 10, 12, 4, 1, 9, 13, 3, 14, 8, 2, 5, 11, 6).
	blake2_sigma(9, 11, 3, 9, 5, 8, 7, 2, 6, 16, 12, 10, 15, 4, 13, 14, 1).
	blake2_sigma(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16).
	blake2_sigma(11, 15, 11, 5, 9, 10, 16, 14, 7, 2, 13, 1, 3, 12, 8, 6, 4).

:- end_category.


:- object(blake2s(_Key_, _DigestSize_),
	imports(blake2_core(_Key_, _DigestSize_))).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'BLAKE2s hash function.',
		parameters is [
			'Key' - 'A list of 0 to 32 key bytes. Use the empty list for unkeyed hashing.',
			'DigestSize' - 'Number of digest bytes to generate, from 1 to 32.'
		],
		see_also is [blake2s, blake2b(_, _), md5, sha256]
	]).

	:- uses(hash_common_32, [
		add32/3, ror32/3
	]).

	:- uses(list, [
		nth0/3
	]).

	blake2_max_digest_size(32).

	blake2_max_key_size(32).

	blake2_block_size(64).

	blake2_round_count(10).

	blake2_initial_state(ParameterWord, [H0, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19]) :-
		H0 is xor(0x6A09E667, ParameterWord) /\ 0xFFFFFFFF.

	blake2_working_vector([H0, H1, H2, H3, H4, H5, H6, H7], Total, Final, [H0, H1, H2, H3, H4, H5, H6, H7, 0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, V12, V13, V14, 0x5BE0CD19]) :-
		TotalLow is Total /\ 0xFFFFFFFF,
		TotalHigh is (Total >> 32) /\ 0xFFFFFFFF,
		V12 is xor(0x510E527F, TotalLow) /\ 0xFFFFFFFF,
		V13 is xor(0x9B05688C, TotalHigh) /\ 0xFFFFFFFF,
		(	Final == true -> FinalFlag = 0xFFFFFFFF
		;	FinalFlag = 0x00000000
		),
		V14 is xor(0x1F83D9AB, FinalFlag) /\ 0xFFFFFFFF.

	blake2_g(A0, B0, C0, D0, X, Y, A2, B4, C2, D4) :-
		add32(A0, B0, T0),
		add32(T0, X, A1),
		D1 is xor(D0, A1) /\ 0xFFFFFFFF,
		ror32(D1, 16, D2),
		add32(C0, D2, C1),
		B1 is xor(B0, C1) /\ 0xFFFFFFFF,
		ror32(B1, 12, B2),
		add32(A1, B2, T1),
		add32(T1, Y, A2),
		D3 is xor(D2, A2) /\ 0xFFFFFFFF,
		ror32(D3, 8, D4),
		add32(C1, D4, C2),
		B3 is xor(B2, C2) /\ 0xFFFFFFFF,
		ror32(B3, 7, B4).

	blake2_finalize([], _, _, []).
	blake2_finalize([HashWord| HashWords], Working, Index0, [StateWord| StateWords]) :-
		nth0(Index0, Working, Word0),
		Index8 is Index0 + 8,
		nth0(Index8, Working, Word8),
		StateWord is xor(HashWord, xor(Word0, Word8)) /\ 0xFFFFFFFF,
		Index is Index0 + 1,
		blake2_finalize(HashWords, Working, Index, StateWords).

	blake2_state_bytes(State, DigestBytes) :-
		blake2_state_bytes(State, DigestBytes, []).

	blake2_state_bytes([], Bytes, Bytes).
	blake2_state_bytes([Word| Words], Bytes, Tail) :-
		byte_order::integer_to_bytes(little, 4, Word, Bytes, Rest),
		blake2_state_bytes(Words, Rest, Tail).

	blake2_block_words([], []).
	blake2_block_words([B0, B1, B2, B3| Bytes], [Word| Words]) :-
		byte_order::bytes_to_integer(little, [B0, B1, B2, B3], Word),
		blake2_block_words(Bytes, Words).

:- end_object.


:- object(blake2s,
	extends(blake2s([], 32))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-30,
		comment is 'BLAKE2s hash function.',
		see_also is [blake2s(_, _), blake2b, md5, sha256]
	]).

:- end_object.


:- object(md5,
	implements([hash_digest_protocol, hash_state_protocol])).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'MD5 hash function.',
		see_also is [sha1, sha256]
	]).

	:- uses(hash_common_32, [
		add32/3, add32/5, bytes_hex/2,
		pad_md_tail/5, rol32/3
	]).

	:- uses(list, [
		append/3, length/2, nth0/3, take/4
	]).

	digest(Bytes, DigestBytes) :-
		length(Bytes, Length),
		md5_consume_blocks(Bytes, 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, A0, B0, C0, D0, Tail),
		pad_md_tail(little, Tail, Length, 8, PaddedTail),
		md5_blocks(PaddedTail, A0, B0, C0, D0, A, B, C, D),
		byte_order::integer_to_bytes(little, 4, A, DigestBytes, BytesB),
		byte_order::integer_to_bytes(little, 4, B, BytesB, BytesC),
		byte_order::integer_to_bytes(little, 4, C, BytesC, BytesD),
		byte_order::integer_to_bytes(little, 4, D, BytesD, []).

	digest_size(16).

	block_size(64).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	% the state buffers the at-most-63 leftover bytes that do not yet form a
	% complete 64-byte block; Length is the total number of bytes seen so
	% far, needed only for the MD padding appended to the final, partial
	% block at finalization
	new_hash_state(state([], 0, 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476)).

	update_hash_state(state(Buffer0, Length0, A0, B0, C0, D0), Bytes, state(Buffer1, Length1, A1, B1, C1, D1)) :-
		append(Buffer0, Bytes, Combined),
		length(Bytes, BytesLength),
		Length1 is Length0 + BytesLength,
		md5_consume_blocks(Combined, A0, B0, C0, D0, A1, B1, C1, D1, Buffer1).

	final_hash_state(state(Buffer, Length, A, B, C, D), Hash) :-
		pad_md_tail(little, Buffer, Length, 8, PaddedTail),
		md5_blocks(PaddedTail, A, B, C, D, FA, FB, FC, FD),
		byte_order::integer_to_bytes(little, 4, FA, DigestBytes, BytesB),
		byte_order::integer_to_bytes(little, 4, FB, BytesB, BytesC),
		byte_order::integer_to_bytes(little, 4, FC, BytesC, BytesD),
		byte_order::integer_to_bytes(little, 4, FD, BytesD, []),
		bytes_hex(DigestBytes, Hash).

	md5_consume_blocks(Bytes, A0, B0, C0, D0, A, B, C, D, Tail) :-
		take(64, Bytes, Block, Rest),
		!,
		block_words_le(Block, X),
		md5_rounds(0, X, A0, B0, C0, D0, AA, BB, CC, DD),
		add32(A0, AA, A1),
		add32(B0, BB, B1),
		add32(C0, CC, C1),
		add32(D0, DD, D1),
		md5_consume_blocks(Rest, A1, B1, C1, D1, A, B, C, D, Tail).
	md5_consume_blocks(Bytes, A, B, C, D, A, B, C, D, Bytes).

	md5_blocks([], A, B, C, D, A, B, C, D).
	md5_blocks([Byte| Bytes], A0, B0, C0, D0, A, B, C, D) :-
		take(64, [Byte| Bytes], Block, Rest),
		block_words_le(Block, X),
		md5_rounds(0, X, A0, B0, C0, D0, AA, BB, CC, DD),
		add32(A0, AA, A1),
		add32(B0, BB, B1),
		add32(C0, CC, C1),
		add32(D0, DD, D1),
		md5_blocks(Rest, A1, B1, C1, D1, A, B, C, D).

	md5_rounds(64, _, A, B, C, D, A, B, C, D) :-
		!.
	md5_rounds(I, X, A0, B0, C0, D0, A, B, C, D) :-
		md5_f(I, B0, C0, D0, F),
		md5_g(I, G),
		nth0(G, X, XG),
		md5_k(I, K),
		add32(A0, F, XG, K, T0),
		md5_s(I, S),
		rol32(T0, S, T1),
		add32(B0, T1, B1),
		NextI is I + 1,
		md5_rounds(NextI, X, D0, B1, B0, C0, A, B, C, D).

	md5_f(I, B, C, D, F) :-
		(	I < 16 ->
			F is ((B /\ C) \/ ((\ B) /\ D)) /\ 0xFFFFFFFF
		;	I < 32 ->
			F is ((D /\ B) \/ ((\ D) /\ C)) /\ 0xFFFFFFFF
		;	I < 48 ->
			F is xor(B, xor(C, D)) /\ 0xFFFFFFFF
		;	F is xor(C, (B \/ (\ D))) /\ 0xFFFFFFFF
		).

	md5_g(I, G) :-
		(	I < 16 -> G is I
		;	I < 32 -> G is (5 * I + 1) mod 16
		;	I < 48 -> G is (3 * I + 5) mod 16
		;	G is (7 * I) mod 16
		).

	md5_s( 0,  7).
	md5_s( 1, 12).
	md5_s( 2, 17).
	md5_s( 3, 22).
	md5_s( 4,  7).
	md5_s( 5, 12).
	md5_s( 6, 17).
	md5_s( 7, 22).
	md5_s( 8,  7).
	md5_s( 9, 12).
	md5_s(10, 17).
	md5_s(11, 22).
	md5_s(12,  7).
	md5_s(13, 12).
	md5_s(14, 17).
	md5_s(15, 22).
	md5_s(16,  5).
	md5_s(17,  9).
	md5_s(18, 14).
	md5_s(19, 20).
	md5_s(20,  5).
	md5_s(21,  9).
	md5_s(22, 14).
	md5_s(23, 20).
	md5_s(24,  5).
	md5_s(25,  9).
	md5_s(26, 14).
	md5_s(27, 20).
	md5_s(28,  5).
	md5_s(29,  9).
	md5_s(30, 14).
	md5_s(31, 20).
	md5_s(32,  4).
	md5_s(33, 11).
	md5_s(34, 16).
	md5_s(35, 23).
	md5_s(36,  4).
	md5_s(37, 11).
	md5_s(38, 16).
	md5_s(39, 23).
	md5_s(40,  4).
	md5_s(41, 11).
	md5_s(42, 16).
	md5_s(43, 23).
	md5_s(44,  4).
	md5_s(45, 11).
	md5_s(46, 16).
	md5_s(47, 23).
	md5_s(48,  6).
	md5_s(49, 10).
	md5_s(50, 15).
	md5_s(51, 21).
	md5_s(52,  6).
	md5_s(53, 10).
	md5_s(54, 15).
	md5_s(55, 21).
	md5_s(56,  6).
	md5_s(57, 10).
	md5_s(58, 15).
	md5_s(59, 21).
	md5_s(60,  6).
	md5_s(61, 10).
	md5_s(62, 15).
	md5_s(63, 21).

	md5_k( 0, 0xD76AA478).
	md5_k( 1, 0xE8C7B756).
	md5_k( 2, 0x242070DB).
	md5_k( 3, 0xC1BDCEEE).
	md5_k( 4, 0xF57C0FAF).
	md5_k( 5, 0x4787C62A).
	md5_k( 6, 0xA8304613).
	md5_k( 7, 0xFD469501).
	md5_k( 8, 0x698098D8).
	md5_k( 9, 0x8B44F7AF).
	md5_k(10, 0xFFFF5BB1).
	md5_k(11, 0x895CD7BE).
	md5_k(12, 0x6B901122).
	md5_k(13, 0xFD987193).
	md5_k(14, 0xA679438E).
	md5_k(15, 0x49B40821).
	md5_k(16, 0xF61E2562).
	md5_k(17, 0xC040B340).
	md5_k(18, 0x265E5A51).
	md5_k(19, 0xE9B6C7AA).
	md5_k(20, 0xD62F105D).
	md5_k(21, 0x02441453).
	md5_k(22, 0xD8A1E681).
	md5_k(23, 0xE7D3FBC8).
	md5_k(24, 0x21E1CDE6).
	md5_k(25, 0xC33707D6).
	md5_k(26, 0xF4D50D87).
	md5_k(27, 0x455A14ED).
	md5_k(28, 0xA9E3E905).
	md5_k(29, 0xFCEFA3F8).
	md5_k(30, 0x676F02D9).
	md5_k(31, 0x8D2A4C8A).
	md5_k(32, 0xFFFA3942).
	md5_k(33, 0x8771F681).
	md5_k(34, 0x6D9D6122).
	md5_k(35, 0xFDE5380C).
	md5_k(36, 0xA4BEEA44).
	md5_k(37, 0x4BDECFA9).
	md5_k(38, 0xF6BB4B60).
	md5_k(39, 0xBEBFBC70).
	md5_k(40, 0x289B7EC6).
	md5_k(41, 0xEAA127FA).
	md5_k(42, 0xD4EF3085).
	md5_k(43, 0x04881D05).
	md5_k(44, 0xD9D4D039).
	md5_k(45, 0xE6DB99E5).
	md5_k(46, 0x1FA27CF8).
	md5_k(47, 0xC4AC5665).
	md5_k(48, 0xF4292244).
	md5_k(49, 0x432AFF97).
	md5_k(50, 0xAB9423A7).
	md5_k(51, 0xFC93A039).
	md5_k(52, 0x655B59C3).
	md5_k(53, 0x8F0CCC92).
	md5_k(54, 0xFFEFF47D).
	md5_k(55, 0x85845DD1).
	md5_k(56, 0x6FA87E4F).
	md5_k(57, 0xFE2CE6E0).
	md5_k(58, 0xA3014314).
	md5_k(59, 0x4E0811A1).
	md5_k(60, 0xF7537E82).
	md5_k(61, 0xBD3AF235).
	md5_k(62, 0x2AD7D2BB).
	md5_k(63, 0xEB86D391).

	block_words_le([], []).
	block_words_le([B0, B1, B2, B3| Bytes], [Word| Words]) :-
		byte_order::bytes_to_integer(little, [B0, B1, B2, B3], Word),
		block_words_le(Bytes, Words).

:- end_object.
