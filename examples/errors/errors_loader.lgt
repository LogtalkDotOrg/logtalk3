%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
	set_logtalk_flag(report, on),
	(logtalk_load(object_redefines_built_in_method) -> true; true),
	(logtalk_load(invalid_clause_head) -> true; true),
	(logtalk_load(unknown_directive) -> true; true),
	(logtalk_load(non_instantiated_directive) -> true; true),
	(logtalk_load(invalid_directive_argument) -> true; true),
	(logtalk_load(unmatched_directive) -> true; true),
	(logtalk_load(category_defines_dynamic_predicate) -> true; true),
	(logtalk_load(control_construct_redefinition) -> true; true),
	(logtalk_load(uses_predicate_repeated) -> true; true),
	(logtalk_load(uses_predicate_conflict) -> true; true),
	(logtalk_load(no_multifile_primary_declaration) -> true; true),
	(logtalk_load(main_include_syntax_error) -> true; true),
	(logtalk_load(main_include_compiler_error_1) -> true; true),
	(logtalk_load(main_include_compiler_error_2) -> true; true)
)).
