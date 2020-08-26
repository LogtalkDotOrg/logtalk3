////////////////////////////////////////////////////////////////////////////
//
//  Integration file for Tau Prolog
//  Last updated on Aug 26, 2020
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

const LOGTALKHOME = process.env.LOGTALKHOME;

var options = {};
var args = process.argv.slice(2);
for(var i = 0; i < args.length; i++) {
    if((args[i] === "-g" || args[i] === "--goal") && args[i+1]) {
        options.goal = args[i+1];
        i++;
    }
}

var session = pl.create(50000);
const compose = (f,g) => x => f(g(x));
const show = function(answer) {
    console.log(pl.format_answer(answer, session, {quoted: true}));
};
const action = function(callback) {
    var input = readlineSync.question("", {keepWhitespace: true}).trim();
    switch(input) {
        // next answer
        case ";":
            session.answer(next_answer(callback));
            break;
        // break
        case "":
            callback();
            break;
        // help
        case "h":
            console.log("Actions: ");
            console.log(";\tredo");
            console.log("RET\tbreak");
            console.log("h\thelp");
            action(callback);
            break;
        // unknown
        default:
            console.log("Unknown action: " + input + " (h for help)");
            action(callback);
            break;
    }
};
const next_answer = function(callback) {
    return function(answer) {
        show(answer);
        if(answer === false || pl.type.is_error(answer))
            callback();
        else
            action(callback);
    };
};
const query = function(goal, callback) {
    if(goal.trim().length === 0) {
        callback();
        return;
    }
    session.query(goal, {success: function(_goal) {
        session.answer(next_answer(callback));
    }, error: compose(repl, show)});
};
const repl = function() {
    var goal = readlineSync.question("?- ", {keepWhitespace: true}) + "\n";
    query(goal, repl);
};
session.consult(`${LOGTALKHOME}/adapters/tau.pl`, {success: function() {
    session.consult(`${LOGTALKHOME}/paths/paths.pl`, {success: function() {
        session.consult(`${LOGTALKHOME}/core/core.pl`, {success: function() {
            if(options.goal)
                query(options.goal, repl);
            else
                repl();
        }, error: show});
    }, error: show});
}, error: show});
