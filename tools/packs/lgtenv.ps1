#############################################################################
## 
##   Packs virtual environment script
##   Last updated on March 18, 2025
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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

[CmdletBinding()]
param(
	[Parameter()]
	[String]$d,
	[Switch]$c,
	[String]$p,
	[Switch]$v,
	[Switch]$h
)

Function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output "$myName 0.5"
}

Function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output ""
	Write-Output "This script creates a packs virtual environment in the current directory or in a"
	Write-Output "specified directory by writing or appending to a .env file. It requires Set-PsEnv"
	Write-Output "to be installed."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName [-d results] [-c] [-p packs]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -d directory where to create the virtual environment"
	Write-Output "     (absolute path; default is the current directory)"
	Write-Output "  -c create directory if it does not exist"
	Write-Output "  -p packs sub-directory"
	Write-Output "     (relative path; default is is the current directory)"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

Function Confirm-Parameters() {

	if ($v -eq $true) {
		Write-Script-Version
		Exit 0
	}

	if ($h -eq $true) {
		Write-Usage-Help
		Exit 0
	}
}

# default argument values

$base = $pwd

Confirm-Parameters

if ($d -eq "") {
	$directory = $base
} elseif (!(Test-Path $d -PathType Container)) {
	if ($c -eq $true) {
		$directory = $d
		New-Item -Path $d -ItemType directory > $null
	} else {
		Write-Output "Error: directory $d does not exist."
		Exit 1
	}
} else {
	$directory = $d
}

if ($p -eq "") {
	$packs = $directory
} elseif (!(Test-Path (Join-Path $directory $p) -PathType Container)) {
	$packs = Join-Path $directory $p
	New-Item -Path (Join-Path $directory $p) -ItemType directory > $null
} else {
	$packs = Join-Path $directory $p
}

if (!(Get-Command "Set-PsEnv" -ErrorAction SilentlyContinue)) {
  Write-Output "Error: Set-PsEnv is not installed."
  Exit 1
}

Add-Content -Path $directory\.env -Value "LOGTALKPACKS=$packs"
Set-PsEnv
Exit 0
