#############################################################################
## 
##   DOT diagram files to SVG files conversion script 
##   Last updated on April 8, 2022
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 2022 Hans N. Beck and Paulo Moura <pmoura@logtalk.org>
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

[CmdletBinding()]
param(
	[Parameter()]
	[String]$c = "dot", 
	[Switch]$v,
	[Switch]$h
)

function Get-ScriptVersion {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 0.7")
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
		# One possibility is using HOME environment
		if (-not ($null -eq $env:HOME)) {
			$DEFAULTPATHS += $env:HOME + '\logtalk' #TODO really correct for windows?
		}
		
		# Checking all possibilites
		foreach ($DEFAULTPATH in $DEFAULTPATHS) { 
			Write-Output ("Looking for: " + $DEFAULTPATH)
			if (Test-Path $DEFAULTPATH) {
				Write-Output ("... using Logtalk installation found at " + $DEFAULTPATH)
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
		$env:LOGTALKUSER = "%USERPROFILE%\Documents\Logtalk"
	}
	# At the end LOGTALKUSER was set already or now is set
}

function Get-Usage() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output "This script converts all Graphviz .dot files"
	Write-Output "in the current directory to SVG files"
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ($myName + " [-c command] [--% arguments]")
	Write-Output ($myName + " -v")
	Write-Output ($myName + " -h")
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -v print version"
	Write-Output "  -c Graphviz command (valid values are dot, circo, fdp and neato; default is $c)"
	Write-Output "  --% addtional arguments to be passed to the Graphviz command (no default)"
	Write-Output "  -h print help"
	Write-Output ""
}

function Check-Parameters() {

	if ($h -eq $true) {
		Get-Usage
		Exit
	}

	if ($v -eq $true) {
		Get-ScriptVersion
		Exit
	}

	if ($c -ne "dot" -and $c -ne "circo" -and $c -ne "fdp" -and $c -ne "neato") {
	Write-Output ("Error! Unknown Graphviz command: " + $c)
		Start-Sleep -Seconds 2
		Exit
	}

}

###################### here it starts ############################ 

Check-Parameters

Get-Logtalkhome

# Check for existence
if (Test-Path $env:LOGTALKHOME) {
	$output = "Found LOGTALKHOME at: " + $env:LOGTALKHOME
	Write-Output $output
} else {
	Write-Output "... unable to locate Logtalk installation directory!"
	Start-Sleep -Seconds 2
	Exit
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

Write-Output "Converting .dot files to .svg files ..."
$count = Get-ChildItem -Path . -Filter *.dot | Measure-Object | %{$_.Count}

if ($count -gt 0) {
	Copy-Item -Path ($env:LOGTALKUSER + '\tools\diagrams\zoom.png') -Destination .
	Copy-Item -Path ($env:LOGTALKUSER + '\tools\diagrams\diagrams.css') -Destination .
	Get-ChildItem -Path . -Filter *.dot | 
	Foreach-Object {
		Write-Host -NoNewline ("  converting " + $_.Name)
		$converted = 1
		$counter = 16
		While (($converted -eq 1) -and ($counter -gt 0)) {
			if ($args.Count -gt 2 -and $args[$args.Count-2] -eq "--%") {
				& $c -q -Tsvg -Gfontnames=svg -o ($_.BaseName + ".svg") $args[$args.Count-1] $_.Name
			} elseif ($args.Count -eq 2 -and $args[0] -eq "--%") {
				& $c -q -Tsvg -Gfontnames=svg -o ($_.BaseName + ".svg") $args[$args.Count-1] $_.Name
			} else {
				& $c -q -Tsvg -Gfontnames=svg -o ($_.BaseName + ".svg") $_.Name
			}
			if ($?) {
				$converted = 0
			}
			$counter--
			Write-Host -NoNewline "."
		}
		if ($counter -eq 0) {
			Write-Output " failed"
		} else {
			Write-Output " done"
		}
	}
	Write-Output "Conversion done"
	Write-Output ""
} else {
	Write-Output "No .dot files exist in the current directory!"
	Write-Output ""
}
