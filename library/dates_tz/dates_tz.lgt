%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(dates_tz,
	implements(dates_tz_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-08,
		comment is 'Zone-aware date-time conversion bridging the ``dates`` and ``tzif`` libraries.',
		see_also is [dates_tz_protocol, date, tzif]
	]).

	utc_to_local_tz(UTCDateTime, Zone, LocalDateTime) :-
		tzif::offset(Zone, UTCDateTime, OffsetSeconds),
		date::add_duration(UTCDateTime, OffsetSeconds, LocalDateTime).

	local_to_utc_tz(LocalDateTime, Zone, UTCDateTime) :-
		tzif::local_offset(Zone, LocalDateTime, OffsetSeconds),
		date::subtract_duration(LocalDateTime, OffsetSeconds, UTCDateTime).

	local_to_utc_tz_with_resolution(LocalDateTime, Zone, ResolutionMode, UTCDateTime) :-
		tzif::local_offset_with_resolution(Zone, LocalDateTime, ResolutionMode, OffsetSeconds),
		date::subtract_duration(LocalDateTime, OffsetSeconds, UTCDateTime).

	convert_zones(LocalDateTime, FromZone, ToZone, ResultDateTime) :-
		local_to_utc_tz(LocalDateTime, FromZone, UTCDateTime),
		utc_to_local_tz(UTCDateTime, ToZone, ResultDateTime).

	convert_zones_with_resolution(LocalDateTime, FromZone, ResolutionMode, ToZone, ResultDateTime) :-
		local_to_utc_tz_with_resolution(LocalDateTime, FromZone, ResolutionMode, UTCDateTime),
		utc_to_local_tz(UTCDateTime, ToZone, ResultDateTime).

:- end_object.
