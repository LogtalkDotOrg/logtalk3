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


package require Tk

wm title . "Symbolic Expression Differentiation"

source "query.tcl"

# Window layout
grid [ttk::frame .c -padding "3 3 12 12"] -column 0 -row 0 -sticky wnes
grid [ttk::frame .statusbar -relief sunken -height 64 -padding "3"] -column 0 -row 1 -sticky wes
grid columnconfigure	. 0 -weight 1
grid rowconfigure		. 0 -weight 1

# .c contents
# Headings
grid [ttk::label .c.time_lbl -text "Expression" -font TkHeadingFont] -column 1 -row 0 -columnspan 2 -sticky nwe
# Input
grid [ttk::label .c.expression_lbl -text "Expression" -font TkHeadingFont] -column 1 -row 1
grid [ttk::entry .c.expression_entry -width 32 -textvariable expression] -column 1 -row 1
grid [ttk::button .c.calc -text "Differentiate" -command calculate] -column 2 -row 1
# Results
grid [ttk::label .c.result_lbl -text "Result" -font TkHeadingFont] -column 1 -row 2 -columnspan 2 -sticky nwe
grid [ttk::label .c.result_result -width 32 -textvariable result -relief ridge -padding "3" -anchor e] -column 1 -row 3
# Some nice padding
foreach w [winfo children .c] {grid configure $w -padx 5 -pady 5}
# Status Bar
grid [ttk::label .statusbar.status_lbl -text "Status:"] -column 0 -row 0 -sticky ws
grid [ttk::label .statusbar.status_msg -textvariable status] -column 1 -row 0 -sticky wes

# Setup
# Default values
set ::expression "2*x**3 + x**2 - 4*x"
set ::status "loading"
# Focus on first input
focus .c.expression_entry
# Connect Logtalk
lgt::connect_to { symdiff(loader) }
set ::status "connected"
# Override closing window so we can disconnect Logtalk
wm protocol . WM_DELETE_WINDOW on_close

proc on_close {} {
	# When closing the window disconnect Logtalk, then destroy the window
	lgt::disconnect
	set ::status "disconnected"
	destroy .
}

proc calculate {} {
	# Get the vars and send it to Logtalk to solve
	set ::status "differentiating"
	set query "($::expression)::diff(Diff), Diff::simplify(Simplified)"
	# puts $query
	set response [lgt::query $query]
	# Log response
	puts $response
	set ::status "solved"
	switch [dict get $response status] {
		"success" {show_results [dict get $response unifications]}
		"error" {on_error [dict get $response error]"}
		"fail" {on_fail}
	}
}

proc show_results {unifications} {
	# Update the text variables to show the unifications
	set ::result "[dict get $unifications Simplified]"
}

proc on_error {error_msg} {
	# Update status
	set ::status "Error -> $error_msg"
	clear_results
}

proc on_fail {} {
	# Update status
	set ::status "Failed"
	clear_results
}

proc clear_results {} {
	# Clear any set results
	set ::result ""
}
