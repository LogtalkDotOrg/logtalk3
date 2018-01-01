/////////////////////////////////////////////////////////////////////////////
//
//   XML documenting files to plain text conversion script 
//   Last updated on July 5, 2016
//
//   This file is part of Logtalk <http://logtalk.org/>  
//   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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

var WshShell = new ActiveXObject("WScript.Shell");

var directory = WshShell.CurrentDirectory;

var processor = "msxsl";
// var processor = "xsltproc";
// var processor = "xalan";
// var processor = "sabcmd";

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

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

var entity_xslt = logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk_entity_to_txt.xsl";
var index_xslt = logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk_index_to_txt.xsl";

var d_arg = "";
var p_arg = "";

if (WScript.Arguments.Named.Exists("d"))
	d_arg = WScript.Arguments.Named.Item("d");

if (WScript.Arguments.Named.Exists("p"))
	p_arg = WScript.Arguments.Named.Item("p");

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (d_arg != "" && !FSObject.FolderExists(d_arg)) {
	WScript.Echo("Error! directory does not exists: " + d_arg);
	WScript.Echo("");
	usage_help();
} else if (d_arg != "")
	directory = d_arg;

if (p_arg != "" && p_arg != "msxsl" && p_arg != "xsltproc" && p_arg != "xalan" && p_arg != "sabcmd") {
	WScript.Echo("Error! Unsupported XSLT processor:" + p_arg);
	WScript.Echo("");
	usage_help();
} else if (p_arg != "")
	processor = p_arg;

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk_entity.dtd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_entity.dtd", WshShell.CurrentDirectory + "\\logtalk_entity.dtd");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk_index.dtd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_index.dtd", WshShell.CurrentDirectory + "\\logtalk_index.dtd");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\custom.ent")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\custom.ent", WshShell.CurrentDirectory + "\\custom.ent");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk_entity.xsd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_entity.xsd", WshShell.CurrentDirectory + "\\logtalk_entity.xsd");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk_index.xsd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_index.xsd", WshShell.CurrentDirectory + "\\logtalk_index.xsd");
}

if (!FSObject.FileExists(directory + "\\logtalk.css")) {
	FSObject.CopyFile(logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk.css", directory + "\\logtalk.css");
}

WScript.Echo("");
WScript.Echo("converting XML files...");

var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
	var file = files.item().name;
	var xslt;
	if (FSObject.GetExtensionName(file) == "xml") {
		WScript.Echo("  converting " + file);
		var txt_file = directory + "\\" + FSObject.GetBaseName(file) + ".txt";
		if (file == "library_index.xml" || file == "directory_index.xml" || file == "entity_index.xml" || file == "predicate_index.xml") {
			xslt = index_xslt;
		} else {
			xslt = entity_xslt;
		}
		switch (processor) {
			case "msxsl" :
				WshShell.Run("msxsl -o \"" + txt_file + "\" \"" + file + "\" \"" + xslt + "\"", true);
				break;
			case "xsltproc" :
				WshShell.Run("xsltproc -o \"" + txt_file + "\" \"" + xslt + "\" \"" + file + "\"", true);
				break;
			case "xalan" :
				WshShell.Run("xalan -o \"" + txt_file + "\" \"" + file + "\" \"" + xslt + "\"", true);
				break;
			case "sabcmd" :
				WshShell.Run("sabcmd \"" + xslt + "\" \"" + file + "\" \"" + txt_file + "\"", true);
				break;
		}
	}
}

WScript.Echo("conversion done");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk XML files documenting files in the");
	WScript.Echo("current directory to text files");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " [/d:directory] [/p:processor]");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  d - output directory for the generated files (default is " + directory + ")");
	WScript.Echo("  p - XSLT processor (msxsl, xsltproc, xalan, or sabcmd; default is " + processor + ")");
	WScript.Echo("");
	WScript.Quit(1);
}
