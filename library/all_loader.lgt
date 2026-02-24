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


:- initialization(
	logtalk_load([
		types(loader),
		arbitrary(loader),
		strings(loader),
		string_distance(loader),
		stemming(loader),
		subsequences(loader),
		os(loader),
		process(loader),
		sockets(loader),
		dates(loader),
		datalog(loader),
		events(loader),
		dependents(loader),
		dictionaries(loader),
		deques(loader),
		nested_dictionaries(loader),
		heaps(loader),
		queues(loader),
		sets(loader),
		graphs(loader),
		hierarchies(loader),
		meta(loader),
		loops(loader),
		random(loader),
		statistics(loader),
		ids(loader),
		intervals(loader),
		logging(loader),
		meta_compiler(loader),
		assignvars(loader),
		hook_flows(loader),
		hook_objects(loader),
		java(loader),
		redis(loader),
		memcached(loader),
		recorded_database(loader),
		optionals(loader),
		options(loader),
		expecteds(loader),
		validations(loader),
		expand_library_alias_paths(loader),
		edcg(loader),
		dif(loader),
		coroutining(loader),
		zippers(loader),
		reader(loader),
		term_io(loader),
		timeout(loader),
		gensym(loader),
		genint(loader),
		git(loader),
		grammars(loader),
		csv(loader),
		tsv(loader),
		json(loader),
		json_rpc(loader),
		json_lines(loader),
		json_schema(loader),
		json_ld(loader),
		yaml(loader),
		toon(loader),
		classifier_protocols(loader),
		c45(loader),
		random_forest(loader),
		ada_boost(loader),
		knn(loader),
		naive_bayes(loader),
		nearest_centroid(loader),
		isolation_forest(loader),
		cbor(loader),
		ccsds(loader),
		avro(loader),
		protobuf(loader),
		base32(loader),
		base58(loader),
		base64(loader),
		base85(loader),
		url(loader),
		ulid(loader),
		uuid(loader),
		html(loader),
		format(loader),
		union_find(loader),
		mutations(loader),
		listing(loader),
		amqp(loader),
		stomp(loader),
		linda(loader),
		command_line_options(loader),
		simulated_annealing(loader),
		mcp_server(loader),
		cloning,
		counters,
		streamvars
	], [
		optimize(on)
	])
).
