%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
	logtalk_load(library(os_loader)),
	logtalk_load(library(basic_types_loader)),
	logtalk_load(library(pairs), [optimize(on)]),
	logtalk_load(modules_diagram_support, [optimize(on)]),
	logtalk_load([
		graph_language_registry,
		graph_language_protocol,
		dot_graph_language
		], [optimize(on)]),
	logtalk_load([
		diagram,
		entity_diagram,
		xref_diagram,
		inheritance_diagram,
		uses_diagram,
		file_diagram,
		file_load_diagram,
		file_dependency_diagram,
		library_diagram,
		library_dependency_diagram,
		diagrams
		], [optimize(on)])
)).
