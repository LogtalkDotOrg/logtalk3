#############################################################################
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
##
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##
##       http://www.apache.org/licenses/LICENSE-2.0
##
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
##
#############################################################################


#Requires -Version 7.3

$output = [Console]::OpenStandardOutput()
$errorOutput = [Console]::OpenStandardError()
$outputBytes = [Text.Encoding]::ASCII.GetBytes('0123456789abcdef0123456789abcdef')
$errorBytes = [Text.Encoding]::ASCII.GetBytes('fedcba9876543210fedcba9876543210')
for ($count = 0; $count -lt 4096; $count++) {
	$output.Write($outputBytes, 0, $outputBytes.Length)
	$errorOutput.Write($errorBytes, 0, $errorBytes.Length)
}
$output.Flush()
$errorOutput.Flush()
