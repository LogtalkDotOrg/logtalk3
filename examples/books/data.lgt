%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


% the raw data in this example are books, some of them sold with extras, and
% with some of extras weighting enough for their weight to be declared:

% book(Name, Author, Year)
book('The Philosopher''s Stone', 'J. K. Rowling', 1997).
book('The Chamber of Secrets',   'J. K. Rowling', 1998).
book('The Prisoner of Azkaban',  'J. K. Rowling', 1999).
book('The Goblet of Fire',       'J. K. Rowling', 2000).
book('The Order of the Phoenix', 'J. K. Rowling', 2003).
book('The Half-Blood Prince',    'J. K. Rowling', 2005).
book('The Deathly Hallows',      'J. K. Rowling', 2007).

% extra(Book, Kind)
extra('The Philosopher''s Stone', quidditch_set).
extra('The Chamber of Secrets',   map).
extra('The Half-Blood Prince',    audio_cd).
extra('The Deathly Hallows',      horcrux_set).

% weight(Extra, Weight)
weight(quidditch_set, 2000).
weight(horcrux_set,   1000).
