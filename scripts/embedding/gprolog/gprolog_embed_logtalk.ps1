
#############################################################################
## 
##   This script creates a new GNU Prolog top-level interpreter
##   that embeds Logtalk and optionally a Logtalk application
## 
##   Last updated on March 17, 2025
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 2022 Hans N. Beck
##   SPDX-FileCopyrightText: 2022 Paulo Moura <pmoura@logtalk.org>
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
	[Switch]$c, 
	[Switch]$x, 
	[String]$d = $pwd,
	[String]$t,
	[String]$n = "application",
	[String]$p = "$env:LOGTALKHOME\paths\paths.pl",
	[String]$s = "$env:LOGTALKHOME\scripts\embedding\settings-embedding-sample.lgt", 
	[String]$l,
	[String]$g = "true",
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output "$myName 0.17"
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
		$env:LOGTALKUSER = "%USERPROFILE%\Documents\Logtalk"
	}
	# At the end LOGTALKUSER was set already or now is set
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output "This script creates a new GNU Prolog top-level interpreter that embeds the"
	Write-Output "Logtalk compiler and runtime and an optional application from an application"
	Write-Output "source code given its loader file."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName [-c] [-d directory] [-t tmpdir] [-n name] [-p paths] [-s settings] [-l loader]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -c compile library alias paths in paths and settings files"
	Write-Output "  -d directory for generated QLF files (absolute path; default is current directory)"
	Write-Output "  -t temporary directory for intermediate files (absolute path; default is an auto-created directory)"
	Write-Output "  -n name of the generated saved state (default is application)"
	Write-Output "  -p library paths file (absolute path; default is $p)"
	Write-Output "  -s settings file (absolute path or 'none'; default is $s)"
	Write-Output "  -l loader file for the application (absolute path)"
	Write-Output "  -v print version of $myName"
	Write-Output "  -h help"
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

	if (-not(Test-Path $p)) { # cannot be ""
		Write-Output "The $p library paths file does not exist!"
		Start-Sleep -Seconds 2
		Exit 1
	}

	if (($s -ne "") -and ($s -ne "none") -and (-not(Test-Path $s))) {
		Write-Output "The $s settings file does not exist!"
		Start-Sleep -Seconds 2
		Exit 1
	}

	if (($l -ne "") -and (-not(Test-Path $l))) {
		Write-Output "The $loader loader file does not exist!"
		Start-Sleep -Seconds 2
		Exit 1
	}

	if ($t -eq "") {
		$t = "$pwd\tmp"
	}

	if (-not (Test-Path $d)) {
		try {
			New-Item -Path $d -ItemType Directory > $null
		} catch {
			Write-Output "Could not create destination directory at $d!"
			Start-Sleep -Seconds 2
			Exit 1
		}
	}

	if (-not (Test-Path $t)) {
		try {
			New-Item -Path $t -ItemType Directory > $null
		} catch {
			Write-Output "Could not create temporary directory at $t!"
			Start-Sleep -Seconds 2
			Exit 1
		}
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

$env:LINEDIT = 'gui=no'

Push-Location
Set-Location $t

Copy-Item -Path "$env:LOGTALKHOME\adapters\gnu.pl" -Destination .
Copy-Item -Path "$env:LOGTALKHOME\core\core.pl" -Destination .
$ScratchDirOption = ", scratch_directory('$($t.Replace('\','/'))')"

$GoalParam = "logtalk_compile([core(expanding), core(monitoring), core(forwarding), core(user), core(logtalk), core(core_messages)], [optimize(on)$ScratchDirOption]), halt"
gplgt --query-goal $GoalParam

if ($c -eq $true) {
	$GoalParam = "logtalk_load(expand_library_alias_paths(loader)),logtalk_compile('$($p.Replace('\','/')),[hook(expand_library_alias_paths)$ScratchDirOption]),halt"
	gplgt --query-goal $GoalParam
} else {
	Copy-Item -Path $p -Destination "$t\paths_lgt.pl"
}

if (($s -eq "") -or ($s -eq "none")) {
	Set-Content -Path settings_lgt.pl -Value ""
} elseif ($c -eq $true) {
	$GoalParam = "logtalk_load(expand_library_alias_paths(loader)),logtalk_compile('$($s.Replace('\','/')),[hook(expand_library_alias_paths),optimize(on)$ScratchDirOption]), halt"
	gplgt --query-goal $GoalParam
} else {
	$GoalParam = "logtalk_compile('$($s.Replace('\','/')),[optimize(on)$ScratchDirOption]), halt" 
	gplgt --query-goal $GoalParam
}

if ($l -ne "") {
	try {
		New-Item -Path "$t/application" -ItemType Directory > $null
		Push-Location "$t/application"
	} catch {
		Write-Output "Could not create temporary directory at $t/application!"
		Start-Sleep -Seconds 2
		Exit 
	}

	if ($s -ne "") {
		Copy-Item -Path $s -Destination .
	}

	$GoalParam = "set_logtalk_flag(clean,off), set_logtalk_flag(scratch_directory,'$($t.Replace('\', '/'))/application'), logtalk_load('$($l.Replace('\', '/'))'), halt" 
	gplgt --query-goal $GoalParam

	Pop-Location
}

if ($s -ne "") {
	(Get-Content gnu.pl) -replace 'settings_file, allow', 'settings_file, deny' | Set-Content gnu.pl
}

if ($args.Count -gt 2 -and $args[$args.Count-2] -eq "--%") {
	gplc (-Split $args[$args.Count-1]) -o "$d/$n" gnu.pl $(ls expanding*_lgt.pl | ForEach-Object {$_.FullName}) $(ls monitoring*_lgt.pl | ForEach-Object {$_.FullName}) $(ls forwarding*_lgt.pl | ForEach-Object {$_.FullName})  $(ls user*_lgt.pl | ForEach-Object {$_.FullName}) $(ls logtalk*_lgt.pl | ForEach-Object {$_.FullName}) $(ls core_messages*_lgt.pl | ForEach-Object {$_.FullName}) $(ls application/*.pl | sort LastWriteTime | ForEach-Object {$_.FullName}) $(ls settings*_lgt.pl | ForEach-Object {$_.FullName}) core.pl $(ls paths*_lgt.pl | ForEach-Object {$_.FullName})
} else {
	gplc -o "$d/$n" gnu.pl $(ls expanding*_lgt.pl | ForEach-Object {$_.FullName}) $(ls monitoring*_lgt.pl | ForEach-Object {$_.FullName}) $(ls forwarding*_lgt.pl | ForEach-Object {$_.FullName})  $(ls user*_lgt.pl | ForEach-Object {$_.FullName}) $(ls logtalk*_lgt.pl | ForEach-Object {$_.FullName}) $(ls core_messages*_lgt.pl | ForEach-Object {$_.FullName}) $(ls application/*.pl | sort LastWriteTime | ForEach-Object {$_.FullName}) $(ls settings*_lgt.pl | ForEach-Object {$_.FullName}) core.pl $(ls paths*_lgt.pl | ForEach-Object {$_.FullName})
}

Pop-Location

try {
	Remove-Item $t -Force -Recurse
} catch {
	Write-Output "Error occurred at cleanup"
}
