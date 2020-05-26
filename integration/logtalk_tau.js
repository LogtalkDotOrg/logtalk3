////////////////////////////////////////////////////////////////////////////
//
//  Integration file for Tau Prolog
//  Last updated on May 26, 2020
//
//  This file is part of Logtalk <https://logtalk.org/>  
//  Copyright 1998-2020 José Antonio Riaza Valverde and  
//  Paulo Moura <pmoura@logtalk.org>
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//  
//      http://www.apache.org/licenses/LICENSE-2.0
//  
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//
////////////////////////////////////////////////////////////////////////////


var readlineSync = require("readline-sync");
var pl = require("tau-prolog/modules/core.js");
require("tau-prolog/modules/format.js")(pl);
require("tau-prolog/modules/js.js")(pl);
require("tau-prolog/modules/lists.js")(pl);
require("tau-prolog/modules/os.js")(pl);
require("tau-prolog/modules/random.js")(pl);
require("tau-prolog/modules/statistics.js")(pl);

var max_answers = process.argv[2] || 100;
var session = pl.create(50000);
var compose = (f,g) => x => f(g(x));
var show = x => console.log(pl.format_answer(x, session));
const LOGTALKHOME = process.env.LOGTALKHOME;
var repl = function() {
    var query = readlineSync.question("?- ", {keepWhitespace: true}) + "\n";
    session.query(query, {success: function(_goal) {
        session.answers(function(answer) {
            show(answer);
            if(answer === false || pl.type.is_error(answer))
                repl();
        }, max_answers);
    }, error: compose(repl, show)});
};
session.consult(`${LOGTALKHOME}/adapters/tau.pl`, {success: function() {
    session.consult(`${LOGTALKHOME}/paths/paths.pl`, {success: function() {
        session.consult(`${LOGTALKHOME}/core/core.pl`, {success: function() {
            repl();
        }, error: show});
    }, error: show});
}, error: show});
