###########################################################################
#
#  This file is part of Logtalk <https://logtalk.org/>
#  Copyright 2021 Paul Brown <pbrown@optimusprime.ai>
#  SPDX-License-Identifier: Apache-2.0
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
###########################################################################


package require json

namespace eval ::lgt {

variable logtalk 0
set install_dir [file dirname [file normalize [info script]]]
# the definition of the "load_cmd" variable depends on the Logtalk setup
# and operating-system; the default definition that follows assumes a POSIX
# system where the user called the "logtalk_backend_select" script to define
# a "logtalk" script to run Logtalk with the user preferred backend; an
# alternative in POSIX systems, is to replace "logtalk" above with the name
# of a specific integration script, e.g. "gplgt" or "tplgt.sh"; on Windows,
# the command used by an integration shortcut that was created during Logtalk
# installation must be used with proper character escaping and absolute paths
# as exemplified below
variable load_cmd "logtalk"
# variable load_cmd "gplgt"
# variable load_cmd "tplgt.sh"
# variable load_cmd "C:\\\\Program\\ Files\\\\swipl\\\\bin\\\\swipl.exe -s \"C:/Program Files (x86)/Logtalk/integration/logtalk_swi.pl\""

proc connect {} {
	# Open as file
	set lgt::logtalk [open "|$lgt::load_cmd" w+]
	# Load loader
	puts $lgt::logtalk "logtalk_load('[file join $lgt::install_dir loader.lgt]'), tkinter::go."
	flush $lgt::logtalk
	discard_to_ready
	# Log connected
	puts "Logtalk connected"
}

proc connect_to {loader} {
	# Open as file
	set lgt::logtalk [open "|$lgt::load_cmd" w+]
	# Load loader
	puts $lgt::logtalk "logtalk_load(\['[file join $lgt::install_dir loader.lgt]', $loader\]), tkinter::go."
	flush $lgt::logtalk
	discard_to_ready
	# Log connected
	puts "Logtalk connected"
}

proc discard_to_ready {} {
	set line ""
	while {$line != "LOGTALK READY"} {
		gets $lgt::logtalk line
		# Uncomment the next line for debugging
		# puts $line
	}
}

proc disconnect {} {
	puts $lgt::logtalk "halt."
	flush $lgt::logtalk
	# catch to ignore unflushed output
	catch {close $lgt::logtalk}
	set lgt::logtalk 0
	# Log disconnected
	puts "Logtalk disconnected"
}

proc query {query} {
	# query
	puts $lgt::logtalk "$query."
	flush $lgt::logtalk

	# response
	gets $lgt::logtalk user_output
	set ans [json::json2dict $user_output]

	# respond
	return $ans
}

}
