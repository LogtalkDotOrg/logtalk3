%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(order,
	imports((descriptors, proto_hierarchy))).

:- end_object.



	:- object(falconiforms,
		extends(order)).

		order(falconiforms).
		eats(meat).
		feet(curved_talons).
		bill(sharp_hooked).

	:- end_object.


		:- object(falcon,
			extends(falconiforms)).

			family(falcon).
			wings(long_pointed).
			head(large).
			tail(narrow_at_tip).

		:- end_object.


			:- object(peregrine_falcon,
				extends(falcon)).

				eats(birds).

			:- end_object.


			:- object(sparrow_hawk,
				extends(falcon)).

				eats(insects).

			:- end_object.


		:- object(vulture,
			extends(falconiforms)).

			family(vulture).
			feed(scavange).
			wings(broad).

		:- end_object.


			:- object(california_condor,
				extends(vulture)).

				flight_profile(flat).

			:- end_object.


			:- object(turkey_vulture,
				extends(vulture)).

				flight_profile(v_shaped).

			:- end_object.



	:- object(passerformes,
		extends(order)).

		order(passerformes).
		feet(one_long_backward_toe).

	:- end_object.


		:- object(flycatcher,
			extends(passerformes)).

			family(flycatcher).
			bill(flat).
			eats(flying_insects).

		:- end_object.


			:- object(ash_throated_flycatcher,
				extends(flycatcher)).

				throat(white).

			:- end_object.


			:- object(great_crested_flycatcher,
				extends(flycatcher)).

				tail(long_rusty).

			:- end_object.


		:- object(swallow,
			extends(passerformes)).

			family(swallow).
			wings(long_pointed).
			tail(forked).
			bill(short).

		:- end_object.


			:- object(barn_swallow,
				extends(swallow)).

				tail(forked).

			:- end_object.


			:- object(cliff_swallow,
				extends(swallow)).

				tail(square).

			:- end_object.


			:- object(purple_martin,
				extends(swallow)).

				color(dark).

			:- end_object.



	:- object(tubenose,
		extends(order)).

		order(tubenose).
		nostrils(external_tubular).
		live(at_sea).
		bill(hooked).

	:- end_object.


		:- object(fulmar,
			extends(tubenose)).

			size(medium).
			flight(flap_glide).

		:- end_object.


		:- object(albatross,
			extends(tubenose)).

			family(albatross).
			size(large).
			wings(long_narrow).

		:- end_object.


			:- object(black_footed_albatross,
				extends(albatross)).

				color(dark).

			:- end_object.


			:- object(laysan_albatross,
				extends(albatross)).

				color(white).

			:- end_object.



	:- object(waterfowl,
		extends(order)).

		order(waterfowl).
		feet(webbed).
		bill(flat).

	:- end_object.


		:- object(duck,
			extends(waterfowl)).

			family(duck).
			feed(on_water_surface).
			flight(agile).

		:- end_object.


			:- object(female_mallard,
				extends(duck)).

				voice(quack).
				color(mottled_brown).

			:- end_object.


			:- object(male_mallard,
				extends(duck)).

				voice(quack).
				head(green).

			:- end_object.


			:- object(pintail,
				extends(duck)).

				voice(short_whistle).

			:- end_object.


		:- object(goose,
			extends(waterfowl)).

			family(goose).
			size(plump).
			flight(powerful).

		:- end_object.


			:- object(canada_goose,
				extends(goose)).

				head(black).
				cheek(white).

			:- end_object.


			:- object(snow_goose,
				extends(goose)).

				color(white).

			:- end_object.


		:- object(swan,
			extends(waterfowl)).

			family(swan).
			neck(long).
			color(white).
			flight(ponderous).

		:- end_object.


			:- object(trumpeter_swan,
				extends(swan)).

				voice(loud_trumpeting).

			:- end_object.


			:- object(whistling_swan,
				extends(swan)).

				voice(muffled_musical_whistle).

			:- end_object.
