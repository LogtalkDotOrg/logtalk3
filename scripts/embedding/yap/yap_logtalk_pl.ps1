
#############################################################################
## 
##   This script creates a YAP logtalk.pl file with the Logtalk compiler
##   and runtime and optionally an application.pl file with a Logtalk
##   application
## 
##   Last updated on April 7, 2022
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
	[Switch]$c, 
	[Switch]$x, 
	[String]$d = $pwd,
	[String]$t,
	[String]$n = "application",
	[String]$p = ($env:LOGTALKHOME + '\paths\paths_core.pl'),
	[String]$s, 
	[String]$l,
	[String]$g = "true",
	[Switch]$v,
	[Switch]$h
)

function Get-ScriptVersion {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 0.14")
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

	Write-Output "This script creates a YAP logtalk.pl file with the Logtalk compiler and"
	Write-Output "runtime and an optional application.pl file from an application source"
	Write-Output "code given its loader file. It can also generate a standalone saved state."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ($myName + " [-c] [-x] [-d directory] [-t tmpdir] [-n name] [-p paths] [-k hooks] [-s settings] [-l loader] [-g goal]")
	Write-Output ($myName + " -v")
	Write-Output ($myName + " -h")
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -c compile library alias paths in paths and settings files"
	Write-Output "  -x also generate a standalone saved state"
	Write-Output "  -d directory for generated QLF files (absolute path; default is current directory)"
	Write-Output "  -t temporary directory for intermediate files (absolute path; default is an auto-created directory)"
	Write-Output "  -n name of the generated saved state (default is application)"
	Write-Output ("  -p library paths file (absolute path; default is " + $p + ")")
	Write-Output ("  -k hooks file (absolute path; default is " + $k + ")")
	Write-Output "  -s settings file (absolute path)"
	Write-Output "  -l loader file for the application (absolute path)"
	Write-Output "  -g startup goal for the saved state in canonical syntax (default is true)"
	Write-Output ("  -v print version of " +  $myName)
	Write-Output "  -h help"
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

	if (-not(Test-Path $p)) { # cannot be ""
		Write-Output ("The " + $p + " library paths file does not exist!")
		Start-Sleep -Seconds 2
		Exit
	}

	if (-not(Test-Path $k)) { # cannot be ""
		Write-Output ("The " + $k + " hooks file does not exist!")
		Start-Sleep -Seconds 2
		Exit
	}

	if (($s -ne "") -and (-not(Test-Path $s))) {
	Write-Output ("The " + $s + " settings file does not exist!")
		Start-Sleep -Seconds 2
		Exit
	}

	if (($l -ne "") -and (-not(Test-Path $l))) {
		Write-Output ("The " + $loader + " loader file does not exist!")
		Start-Sleep -Seconds 2
		Exit
	}

	if ($t -eq "") {
		$t = "$pwd\tmp"
	}

	if (-not (Test-Path $d)) {
		try {
			New-Item $d -ItemType Directory
		} catch {
			Write-Output ("Could not create destination directory! at " + $d)
			Start-Sleep -Seconds 2
			Exit 
		}
	}

	if (-not (Test-Path $t)) {
		try {
			New-Item $t -ItemType Directory
		} catch {
			Write-Output ("Could not create temporary directory! at " + $t)
			Start-Sleep -Seconds 2
			Exit 
		}
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

Push-Location
Set-Location $t

Copy-Item -Path ($env:LOGTALKHOME + '\adapters\yap.pl') -Destination .
Copy-Item -Path ($env:LOGTALKHOME + '\core\core.pl') -Destination .
$ScratchDirOption = ", scratch_directory('" + $t.Replace('\','/') + "')"

$GoalParam = "logtalk_compile([core(expanding), core(monitoring), core(forwarding), core(user), core(logtalk), core(core_messages)], [optimize(on)" + $ScratchDirOption + "]), halt."
yaplgt -g $GoalParam 

if ($c -eq $true) {
	$GoalParam = "logtalk_load(library(expand_library_alias_paths_loader)),logtalk_compile('" + $p.Replace('\','/') + "',[hook(expand_library_alias_paths)" + $ScratchDirOption + "]),halt."
	yaplgt -g $GoalParam
} else {
	Copy-Item -Path $p -Destination ($t + '\paths_lgt.pl')
}

if ($s -eq "") {
	Get-Content -Path yap.pl,
		paths_*.pl,
		expanding*_lgt.pl,
		monitoring*_lgt.pl,
		forwarding*_lgt.pl,
		user*_lgt.pl,
		logtalk*_lgt.pl,
		core_messages_*lgt.pl,
		core.pl,
		$k | Set-Content logtalk.pl
} else {
	if ($c -eq $true) {
		$GoalParam = "logtalk_load(library(expand_library_alias_paths_loader)),logtalk_compile('" + $s.Replace('\','/') + "',[hook(expand_library_alias_paths),optimize(on)" + $ScratchDirOption + "]), halt."
	} else {
		$GoalParam = "logtalk_compile('" + $s.Replace('\','/') + "',[optimize(on)" + $ScratchDirOption + "]), halt." 
	}
	yaplgt -g $GoalParam
	Get-Content -Path yap.pl,
		paths_*.pl,
		expanding*_lgt.pl,
		monitoring*_lgt.pl,
		forwarding*_lgt.pl,
		user*_lgt.pl,
		logtalk*_lgt.pl,
		core_messages*_lgt.pl,
		settings*_lgt.pl,
		core.pl,
		$k | Set-Content $d/logtalk.pl
}

if ($l -ne "") {
	try {
		New-Item $t/application -ItemType Directory
		Push-Location $t/application
	} catch {
		Write-Output ("Could not create temporary directory! at " + $t + "/application")
		Start-Sleep -Seconds 2
		Exit 
	}

	$GoalParam = "consult('" + $d.Replace('\', '/') +  "/logtalk.pl'), set_logtalk_flag(clean,off), set_logtalk_flag(scratch_directory,'" + $t.Replace('\', '/') + "/application'), logtalk_load('" + $l.Replace('\', '/')  + "'), halt." 

	yap -g $GoalParam
	Get-Item *.pl | 
		Sort-Object -Property @{Expression = "LastWriteTime"; Descending = $false} |
		Get-Content |
		Set-Content $d/application.pl

	Pop-Location
}

if ($x -eq $true) {
	Set-Location $d
	if ($l -ne "") {
		$GoalParam = "consult(['logtalk.pl','application.pl']), save_program('" + $n + "', " + $g + "), halt."
		yap -g $GoalParam
	} else {
		$GoalParam = "consult(['logtalk.pl']), save_program('" + $n + "', '" + $g + "'), halt."
		yap -g $GoalParam
	}
}

Pop-Location

try {
	Remove-Item $t -Confirm -Recurse
} catch {
	Write-Output ("Error occurred at clean-up")
}
