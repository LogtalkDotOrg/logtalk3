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
variable install_dir [pwd]
variable load_cmd "logtalk"

proc connect {} {
	# Open as file
	set lgt::logtalk [open "|$lgt::load_cmd" w+]
	# Load loader
	puts $lgt::logtalk "logtalk_load('$lgt::install_dir/loader.lgt'), tkinter::go."
	flush $lgt::logtalk
	discard_to_ready
	# Log connected
	puts "Logtalk connected"
}

proc connect_to {loader} {
	# Open as file
	set lgt::logtalk [open "|$lgt::load_cmd" w+]
	# Load loader
	puts $lgt::logtalk "logtalk_load(\['$lgt::install_dir/loader.lgt', $loader\]), tkinter::go."
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
