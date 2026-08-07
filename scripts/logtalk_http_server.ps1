#############################################################################
##
##   Local HTTP server script
##   Last updated on August 7, 2026
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

[CmdletBinding()]
param(
	[Parameter()]
	[String]$p,
	[String]$n = "8080",
	[Parameter(Position = 0)]
	[String]$Path = $PWD.Path,
	[Switch]$v,
	[Switch]$h
)

Function Write-Script-Version {
	$myName = Split-Path -Path $MyInvocation.ScriptName -Leaf
	Write-Output "$myName 1.0"
}

Function Write-Usage-Help {
	$myName = Split-Path -Path $MyInvocation.ScriptName -Leaf

	Write-Output ""
	Write-Output "This script starts a local HTTP server for a directory."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName -p prolog [-n port] [directory]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Required arguments:"
	Write-Output "  -p backend Prolog compiler"
	Write-Output "     (valid values are eclipse, sicstus, swi, trealla, and xvm)"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -n server port (default is 8080)"
	Write-Output "  directory to serve (default is the current directory)"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

Function ConvertTo-PrologAtom {
	param(
		[Parameter(Mandatory = $true)]
		[String]$Text
	)

	return $Text.Replace("'", "''")
}

Function Resolve-DocumentRoot {
	param(
		[Parameter(Mandatory = $true)]
		[String]$Path
	)

	$resolvedPath = (Resolve-Path -LiteralPath $Path).ProviderPath
	$root = [System.IO.Path]::GetPathRoot($resolvedPath)
	$current = Get-Item -Force -LiteralPath $root
	$relativePath = $resolvedPath.Substring($root.Length)
	$components = $relativePath.Split([System.IO.Path]::DirectorySeparatorChar, [System.StringSplitOptions]::RemoveEmptyEntries)
	foreach ($component in $components) {
		$item = Get-Item -Force -LiteralPath (Join-Path $current.FullName $component)
		$target = $item.ResolveLinkTarget($true)
		if ($null -ne $target) {
			$current = Get-Item -Force -LiteralPath (Resolve-DocumentRoot $target.FullName)
		} else {
			$current = $item
		}
	}
	return $current.FullName
}

if ($v) {
	Write-Script-Version
	Exit 0
}

if ($h) {
	Write-Usage-Help
	Exit 0
}

if ([String]::IsNullOrEmpty($p)) {
	Write-Error "Error! Backend Prolog compiler not specified!"
	Write-Usage-Help
	Exit 1
}

$port = 0
if (-not [Int32]::TryParse($n, [ref]$port) -or $port -lt 1 -or $port -gt 65535) {
	Write-Error "Error! Invalid server port: $n"
	Exit 1
}

if (-not (Test-Path -LiteralPath $Path -PathType Container)) {
	Write-Error "Error! Directory does not exist: $Path"
	Exit 1
}
$documentRoot = Resolve-DocumentRoot $Path

$dot = ""
switch ($p) {
	"eclipse" {
		$logtalk = "eclipselgt"
		$goalOptions = @("-e")
	}
	"sicstus" {
		$logtalk = "sicstuslgt"
		$goalOptions = @("--noinfo", "--goal")
		$dot = "."
	}
	"swi" {
		$logtalk = "swilgt"
		$goalOptions = @("-q", "-g")
	}
	"trealla" {
		$logtalk = "tplgt"
		$goalOptions = @("-q", "-g")
	}
	"xvm" {
		$logtalk = "xvmlgt"
		$goalOptions = @("-q", "-g")
		$dot = "."
	}
	default {
		Write-Error "Error! Unsupported backend Prolog compiler: $p"
		Write-Usage-Help
		Exit 1
	}
}

if ($null -eq (Get-Command $logtalk -ErrorAction SilentlyContinue)) {
	Write-Error "Error! $logtalk integration script not found."
	Write-Error "       Check that its directory is in your execution path."
	Exit 1
}

$homeDriver = if ([String]::IsNullOrEmpty($env:LOGTALKHOME)) {
	$null
} else {
	Join-Path $env:LOGTALKHOME "scripts/logtalk_http_server.lgt"
}

if ($null -ne $homeDriver -and (Test-Path -LiteralPath $homeDriver -PathType Leaf)) {
	$driver = $homeDriver
} else {
	$driver = Join-Path $PSScriptRoot "logtalk_http_server.lgt"
}

if (-not (Test-Path -LiteralPath $driver -PathType Leaf)) {
	Write-Error "Error! Logtalk HTTP server driver not found: $driver"
	Exit 1
}

$documentRootAtom = ConvertTo-PrologAtom $documentRoot
$driverAtom = ConvertTo-PrologAtom $driver
$goal = "set_logtalk_flag(report,warnings),logtalk_load(http_server(loader)),logtalk_load(http_static_files(loader)),logtalk_load('$driverAtom',[scratch_directory('$LOGTALKUSER/scratch')]),logtalk_http_server::serve($port,'$documentRootAtom'),halt$dot"

& $logtalk @goalOptions $goal
Exit $LASTEXITCODE
