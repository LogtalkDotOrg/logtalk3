/////////////////////////////////////////////////////////////////////////////
//
//   DOT diagram files to SVG files conversion script  
//   Last updated on August 4, 2021
//
//   This file is part of Logtalk <https://logtalk.org/>  
//   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
//   SPDX-License-Identifier: Apache-2.0
//   
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
//
/////////////////////////////////////////////////////////////////////////////


if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}


if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}


var WshShell = new ActiveXObject("WScript.Shell");

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

var logtalk_home;
var logtalk_user;

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (WshSystemEnv.Item("LOGTALKUSER"))
	logtalk_user = WshSystemEnv.Item("LOGTALKUSER");
else if (WshUserEnv.Item("LOGTALKUSER"))
	logtalk_user = WshUserEnv.Item("LOGTALKUSER")
else {
	WScript.Echo("Error! The environment variable LOGTALKUSER must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

FSObject.CopyFile(logtalk_user + "\\tools\\diagrams\\zoom.png", WshShell.CurrentDirectory + "\\zoom.png");
FSObject.CopyFile(logtalk_user + "\\tools\\diagrams\\diagrams.css", WshShell.CurrentDirectory + "\\diagrams.css");

WScript.Echo("");
WScript.Echo("Converting .dot files to .svg files ...");

var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
	var dot_file = files.item().name;
	if (FSObject.GetExtensionName(dot_file) == "dot") {
		WScript.Echo("  converting " + dot_file);
		var svg_file = WshShell.CurrentDirectory + "\\" + FSObject.GetBaseName(dot_file) + ".svg";
		WshShell.Run("dot.exe -Tsvg -o\"" + svg_file + "\" \"" + dot_file + "\"", true);
	}
}

WScript.Echo("Conversion done");
WScript.Echo("");

WScript.Quit(0);


function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Graphviz .dot files");
	WScript.Echo("in the current directory to SVG files");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Quit(1);
}
