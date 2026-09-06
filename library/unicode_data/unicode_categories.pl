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

% Generated Unicode 17.0.0 compatibility view. Do not edit.

:- include(unicode_character_data).

unicode_category(CodePoint, Category) :-
	(	var(CodePoint) ->
		unicode_data_general_category_range(Start, End, SpecificCategory),
		between(Start, End, CodePoint)
	;	unicode_data_general_category_range(Start, End, SpecificCategory),
		CodePoint >= Start, CodePoint =< End,
		!
	),
	unicode_category_convert_(SpecificCategory, Category).

unicode_category_convert_(SpecificCategory, SpecificCategory).
unicode_category_convert_('Lu', 'Lc').
unicode_category_convert_('Ll', 'Lc').
unicode_category_convert_('Lt', 'Lc').
unicode_category_convert_('Cc', 'C').
unicode_category_convert_('Cf', 'C').
unicode_category_convert_('Cn', 'C').
unicode_category_convert_('Co', 'C').
unicode_category_convert_('Cs', 'C').
unicode_category_convert_('Lu', 'L').
unicode_category_convert_('Ll', 'L').
unicode_category_convert_('Lt', 'L').
unicode_category_convert_('Lm', 'L').
unicode_category_convert_('Lo', 'L').
unicode_category_convert_('Mc', 'M').
unicode_category_convert_('Me', 'M').
unicode_category_convert_('Mn', 'M').
unicode_category_convert_('Nd', 'N').
unicode_category_convert_('Nl', 'N').
unicode_category_convert_('No', 'N').
unicode_category_convert_('Pc', 'P').
unicode_category_convert_('Pd', 'P').
unicode_category_convert_('Pe', 'P').
unicode_category_convert_('Pf', 'P').
unicode_category_convert_('Pi', 'P').
unicode_category_convert_('Po', 'P').
unicode_category_convert_('Ps', 'P').
unicode_category_convert_('Sc', 'S').
unicode_category_convert_('Sk', 'S').
unicode_category_convert_('Sm', 'S').
unicode_category_convert_('So', 'S').
unicode_category_convert_('Zl', 'Z').
unicode_category_convert_('Zp', 'Z').
unicode_category_convert_('Zs', 'Z').
