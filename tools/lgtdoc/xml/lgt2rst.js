/////////////////////////////////////////////////////////////////////////////
//
//   XML documenting files to reStructuredText files conversion script
//
//   Last updated on April 12, 2022
//
//   This file is part of Logtalk <https://logtalk.org/>  
//   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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

var WshShell = new ActiveXObject("WScript.Shell");

var directory = WshShell.CurrentDirectory;

var sphinx = false;
var make_html = false;

var index_file = "index.rst"
var index_title = "Documentation index"

var processor = "msxsl";
// var processor = "xsltproc";
// var processor = "xalan";
// var processor = "sabcmd";
// var processor = "saxon";

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

var entity_xslt = logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk_entity_to_rst.xsl";
var index_xslt = logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk_index_to_rst.xsl";

var d_arg = "";
var i_arg = "";
var t_arg = "";
var p_arg = "";

if (WScript.Arguments.Named.Exists("d"))
	d_arg = WScript.Arguments.Named.Item("d");

if (WScript.Arguments.Named.Exists("i"))
	i_arg = WScript.Arguments.Named.Item("i");

if (WScript.Arguments.Named.Exists("t"))
	t_arg = WScript.Arguments.Named.Item("t");

if (WScript.Arguments.Named.Exists("p"))
	p_arg = WScript.Arguments.Named.Item("p");

if (WScript.Arguments.Named.Exists("s"))
	sphinx = true;

if (WScript.Arguments.Named.Exists("m"))
	make_html = true;

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (d_arg != "" && !FSObject.FolderExists(d_arg)) {
	WScript.Echo("Error! directory does not exists: " + d_arg);
	WScript.Echo("");
	usage_help();
} else if (d_arg != "")
	directory = d_arg;

if (i_arg != "")
	index_file=i_arg;

if (t_arg != "")
	index_title=t_arg;

if (p_arg != "" && p_arg != "msxsl" && p_arg != "xsltproc" && p_arg != "xalan" && p_arg != "sabcmd" && p_arg != "saxon") {
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
WScript.Echo("Converting XML files to reStructuredText files...");

var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
	var file = files.item().name;
	var xslt;
	if (FSObject.GetExtensionName(file) == "xml") {
		WScript.Echo("  converting " + file);
		var rst_file = directory + "\\" + FSObject.GetBaseName(file) + ".rst";
		if (file == "library_index.xml" || file == "directory_index.xml" || file == "entity_index.xml" || file == "predicate_index.xml") {
			xslt = index_xslt;
		} else {
			xslt = entity_xslt;
		}
		switch (processor) {
			case "msxsl" :
				WshShell.Run("msxsl -o \"" + rst_file + "\" \"" + file + "\" \"" + xslt + "\"", true);
				break;
			case "xsltproc" :
				WshShell.Run("xsltproc -o \"" + rst_file + "\" \"" + xslt + "\" \"" + file + "\"", true);
				break;
			case "xalan" :
				WshShell.Run("xalan -o \"" + rst_file + "\" \"" + file + "\" \"" + xslt + "\"", true);
				break;
			case "sabcmd" :
				WshShell.Run("sabcmd \"" + xslt + "\" \"" + file + "\" \"" + rst_file + "\"", true);
				break;
			case "saxon" :
				WshShell.Run("java net.sf.saxon.Transform -o:\"" + rst_file + "\" -s:\"" + file + "\" -xsl:\"" + xslt + "\"", true);
				break;
		}
	}
}

WScript.Echo("conversion done");
WScript.Echo("");
WScript.Echo("generating index file...");

index_file = directory + "\\" + index_file;
create_index_file();

WScript.Echo("index file generated");
WScript.Echo("");

if (sphinx) {
	WScript.Echo("");
	WScript.Echo("running sphinx-quickstart ...");
	WScript.Echo("");
	FSObject.MoveFile(WshShell.CurrentDirectory + "\\index.rst", WshShell.CurrentDirectory + "\\index.backup");
	WshShell.Run("sphinx-quickstart --templatedir=\"$LOGTALKUSER\\tools\\lgtdoc\\xml\"", 1, true);
	if (!FSObject.FolderExists(WshShell.CurrentDirectory + "\\_static")) {
		FSObject.CreateFolder(WshShell.CurrentDirectory + "\\_static");
	}
	if (!FSObject.FolderExists(WshShell.CurrentDirectory + "\\_static\\css")) {
		FSObject.CreateFolder(WshShell.CurrentDirectory + "\\_static\\css");
	}
	FSObject.CopyFile(logtalk_user + "\\tools\\lgtdoc\\xml\\css\\sphinx\\custom.css", WshShell.CurrentDirectory + "\\_static\\css\\custom.css");
	FSObject.DeleteFile(WshShell.CurrentDirectory + "\\index.rst");
	FSObject.MoveFile(WshShell.CurrentDirectory + "\\index.backup", WshShell.CurrentDirectory + "\\index.rst");
	WScript.Echo("");
}

if (sphinx && make_html) {
	WScript.Echo("");
	WScript.Echo("running make html ...");
	WScript.Echo("");
	WshShell.Run("make html", 1, true);
	WScript.Echo("");
	
}

WScript.Echo("all done");
WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk XML files documenting files in the");
	WScript.Echo("current directory to reStructuredText files for use with Sphinx");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " [/d:directory] [/i:index] [/t:title] [/p:processor] [/s] [/m]");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  d - output directory for the generated files (default is " + directory + ")");
	WScript.Echo("  i - name of the index file (default is " + index_file + ")");
	WScript.Echo("  t - title to be used in the index file (default is " + index_title + ")");
	WScript.Echo("  p - XSLT processor (msxsl, xsltproc, xalan, sabcmd, or saxon; default is " + processor + ")");
	WScript.Echo("  s - run sphinx-quickstart script");
	WScript.Echo("  m - run make html (requires also using the -s option)");
	WScript.Echo("");
	WScript.Quit(1);
}

function create_index_file() {

	var f = FSObject.CreateTextFile(index_file, true);

	f.WriteLine("# " + index_title);
	f.WriteLine("");

	var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

	if (FSObject.FileExists(WshShell.CurrentDirectory + "\\directory_index.xml")) {
		f.WriteLine(".. toctree::");
		f.WriteLine("   :maxdepth: 3");
		f.WriteLine("   :caption: Contents:");
		f.WriteLine("");
		f.WriteLine("   library_index");
		f.WriteLine("   directory_index");
		f.WriteLine("   entity_index");
		f.WriteLine("   predicate_index");
		f.WriteLine("");
		f.WriteLine("Indices and tables");
		f.WriteLine("==================");
		f.WriteLine("");
		f.WriteLine("* :ref:\`genindex\`");
		f.WriteLine("* :ref:\`search\`");
	} else {
		for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
			var file = files.item().name;
			if (FSObject.GetExtensionName(file) == "xml") {
				var rst_file = FSObject.GetBaseName(file) + ".rst";
				WScript.Echo("  indexing " + html_file);
				var index = FSObject.GetBaseName(file).lastIndexOf("_");
				var pars = FSObject.GetBaseName(file).slice(index+1);
				var entity = FSObject.GetBaseName(file).slice(0, index);
				if (pars == 0)
					f.WriteLine("* [" + entity + "](" + rst_file + ")");
				else
					f.WriteLine("* [" + entity + "/" + pars + "](" + rst_file + ")");
			}
		}
	}
	f.WriteLine("");

	var today = new Date();
	var year  = today.getFullYear();
	var month = today.getMonth() + 1;
	if (month < 10)
		month = "0" + month;
	var day = today.getDate();
	if (day < 10)
		day = "0" + day;
	strToday = year + "/" + month + "/" + day;
	var hours = today.getHours();
	if (hours < 10)
		hours = "0" + hours;
	var mins = today.getMinutes();
	if (mins < 10)
		mins = "0" + mins;
	var secs = today.getSeconds();
	if (secs < 10)
		secs = "0" + secs;
	strTime = hours + ":" + mins + ":" + secs;
	f.WriteLine("Generated on " + strToday + " - " + strTime);

	f.Close();
}
