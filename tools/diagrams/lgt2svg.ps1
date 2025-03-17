#############################################################################
## 
##   DOT diagram files to SVG files conversion script 
##   Last updated on March 17, 2025
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 2022-2024 Paulo Moura <pmoura@logtalk.org>
##   Copyright 2022 Hans N. Beck
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
	[String]$c = "dot", 
	[String]$l = "elk", 
	[String]$a = "", 
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output "$myName 0.12"
}

function Get-Logtalkhome {
	if ($null -eq $env:LOGTALKHOME) 
	{
		Write-Output "The environment variable LOGTALKHOME should be defined first, pointing"
		Write-Output "to your Logtalk installation directory!"
		Write-Output "Trying the default locations for the Logtalk installation..."
		
		$DEFAULTPATHS = [string[]](
			"C:\Program Files (x86)\Logtalk",
			"C:\Program Files\Logtalk",
			"%LOCALAPPDATA%\Logtalk"
		)
		
		# Checking all default paths
		foreach ($DEFAULTPATH in $DEFAULTPATHS) { 
			Write-Output "Looking for: $DEFAULTPATH"
			if (Test-Path $DEFAULTPATH) {
				Write-Output "... using Logtalk installation found at $DEFAULTPATH"
				$env:LOGTALKHOME = $DEFAULTPATH
				break
			}
		}
	}
	# At the end LOGTALKHOME was set already or now is set
}

function Get-Logtalkuser {
	if ($null -eq $env:LOGTALKUSER) {
		Write-Output "After the script completion, you must set the environment variable"
		Write-Output "LOGTALKUSER pointing to %USERPROFILE%\Documents\Logtalk."
		$env:LOGTALKUSER = "$env:USERPROFILE\Documents\Logtalk"
	}
	# At the end LOGTALKUSER was set already or now is set
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output "This script converts .d2 and .dot files in the current directory to SVG files"
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName [-c command] [-a arguments]"
	Write-Output "  $myName [-l layout] [-a arguments]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -c Graphviz command (dot, circo, fdp, or neato; default is $c)"
	Write-Output "  -l d2 layout (dagre, elk, or tala; default is $l)"
	Write-Output "  -a additional arguments wrapped as a string to be passed to the converter command (no default)"
	Write-Output "  -v print version"
	Write-Output "  -h print help"
	Write-Output ""
}

function Confirm-Parameters() {

	if ($h -eq $true) {
		Write-Usage-Help
		Exit 0
	}

	if ($v -eq $true) {
		Write-Script-Version
		Exit 0
	}

	if ($c -ne "dot" -and $c -ne "circo" -and $c -ne "fdp" -and $c -ne "neato") {
	Write-Output "Error! Unknown Graphviz command: $c"
		Start-Sleep -Seconds 2
		Exit 1
	}

	if ($l -ne "dagre" -and $l -ne "elk" -and $l -ne "tala") {
	Write-Output "Error! Unknown d2 layout: $l"
		Start-Sleep -Seconds 2
		Exit 1
	}

}

###################### here it starts ############################ 

Confirm-Parameters

Get-Logtalkhome

# Check for existence
if (Test-Path $env:LOGTALKHOME) {
	$output = "Found LOGTALKHOME at: $env:LOGTALKHOME"
	Write-Output $output
} else {
	Write-Output "... unable to locate Logtalk installation directory!"
	Start-Sleep -Seconds 2
	Exit 1
}

Get-Logtalkuser

# Check for existence
if (Test-Path $env:LOGTALKUSER) {
	if (!(Test-Path $env:LOGTALKUSER/VERSION.txt)) {
		Write-Output "Cannot find version information in the Logtalk user directory at %LOGTALKUSER%!"
		Write-Output "Creating an up-to-date Logtalk user directory..."
		logtalk_user_setup
	} else {
		$system_version = Get-Content $env:LOGTALKHOME/VERSION.txt
		$user_version = Get-Content $env:LOGTALKUSER/VERSION.txt
		if ($user_version -lt $system_version) {
			Write-Output "Logtalk user directory at %LOGTALKUSER% is outdated: "
			Write-Output "    $user_version < $system_version"
			Write-Output "Creating an up-to-date Logtalk user directory..."
			logtalk_user_setup
		}
	}
} else {
	Write-Output "Cannot find %LOGTALKUSER% directory! Creating a new Logtalk user directory"
	Write-Output "by running the logtalk_user_setup shell script:"
	logtalk_user_setup
}

$d2_failed_flag = $false
$dot_failed_flag = $false

$d2_count = Get-ChildItem -Path . -Filter *.d2 | Measure-Object | ForEach-Object{$_.Count}
$dot_count = Get-ChildItem -Path . -Filter *.dot | Measure-Object | ForEach-Object{$_.Count}

# Copy CSS file once if needed
if ($d2_count -gt 0 -or $dot_count -gt 0) {
	Copy-Item -Path "$env:LOGTALKUSER\tools\diagrams\diagrams.css" -Destination .
}

if ($d2_count -gt 0) {
	Write-Output "Converting .d2 files to .svg files ..."
	Get-ChildItem -Path . -Filter *.d2 | 
	Foreach-Object {
		Write-Host -NoNewline "  converting $($_.Name)"
		if ($a -ne "") {
			& d2 --layout $l (-Split $a) $_.Name "$($_.BaseName).svg"
		} else {
			& d2 --layout $l $_.Name "$($_.BaseName).svg"
		}
		if ($?) {
			Write-Output " done"
		} else {
			$d2_failed_flag = $true
			Write-Output " failed"
		}
	}
}

if ($dot_count -gt 0) {
	Write-Output "Converting .dot files to .svg files ..."
	Get-ChildItem -Path . -Filter *.dot | 
	Foreach-Object {
		Write-Host -NoNewline "  converting $($_.Name)"
		$converted = $false
		$counter = 24
		
		# Retry logic to handle Graphviz's random crashes
		while (-not $converted -and $counter -gt 0) {
			try {
				if ($a -ne "") {
					& $c -q -Tsvg -Gfontnames=svg -o "$($_.BaseName).svg" (-Split $a) $_.Name
				} else {
					& $c -q -Tsvg -Gfontnames=svg -o "$($_.BaseName).svg" $_.Name
				}
				
				# Check if command succeeded
				if ($?) {
					$converted = $true
				}
			}
			catch {
				# Continue on error
			}
			
			$counter--
			Write-Host -NoNewline "."
		}
		
		# Report conversion status
		if (-not $converted) {
			$dot_failed_flag = $true
			Write-Output " failed"
		} else {
			Write-Output " done"
		}
	}
}

if ($d2_count -eq 0 -and $dot_count -eq 0) {
	Write-Output "No .d2 or .dot files exist in the current directory!"
	Write-Output ""
	Exit 0
} elseif (-not $d2_failed_flag -and -not $dot_failed_flag) {
	Write-Output "Conversion done."
	Write-Output ""
	Exit 0
} else {
	Write-Output "One or more files could not be converted!"
	Write-Output ""
	Exit 1
}
