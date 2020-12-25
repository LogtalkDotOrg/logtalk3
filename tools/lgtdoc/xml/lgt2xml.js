/////////////////////////////////////////////////////////////////////////////
//
//   XML documenting files to XML conversion script 
//   Last updated on July 5, 2016
//
//   This file is part of Logtalk <https://logtalk.org/>  
//   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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

var format = "xhtml";
// var format = "html";

var directory = WshShell.CurrentDirectory;

var index_file = "index.html";
var index_title = "Documentation index";

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

var f_arg = "";
var i_arg = "";
var t_arg = "";

if (WScript.Arguments.Named.Exists("f"))
	f_arg = WScript.Arguments.Named.Item("f");

if (WScript.Arguments.Named.Exists("i"))
	i_arg = WScript.Arguments.Named.Item("i");

if (WScript.Arguments.Named.Exists("t"))
	t_arg = WScript.Arguments.Named.Item("t");

if (f_arg != "" && f_arg != "xhtml" && f_arg != "html") {
	WScript.Echo("Error! Unsupported output format: " + f_arg);
	WScript.Echo("");
	usage_help();
} else if (f_arg != "")
	format = f_arg;

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (i_arg != "")
	index_file=i_arg;

if (t_arg != "")
	index_title=t_arg;

if (!FSObject.FileExists(directory + "\\logtalk_entity.dtd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_entity.dtd", directory + "\\logtalk_entity.dtd");
}

if (!FSObject.FileExists(directory + "\\logtalk_index.dtd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_index.dtd", directory + "\\logtalk_index.dtd");
}

if (!FSObject.FileExists(directory + "\\custom.ent")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\custom.ent", directory + "\\custom.ent");
}

if (!FSObject.FileExists(directory + "\\logtalk_entity.xsd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_entity.xsd", directory + "\\logtalk_entity.xsd");
}

if (!FSObject.FileExists(directory + "\\logtalk_index.xsd")) {
	FSObject.CopyFile(logtalk_home + "\\tools\\lgtdoc\\xml\\logtalk_index.xsd", directory + "\\logtalk_index.xsd");
}

if (!FSObject.FileExists(directory + "\\logtalk.css")) {
	FSObject.CopyFile(logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk.css", directory + "\\logtalk.css");
}

if (!FSObject.FileExists(directory + "\\logtalk_entity_to_xml.xsl")) {
	FSObject.CopyFile(logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk_entity_to_xml.xsl", directory + "\\logtalk_entity_to_xml.xsl");
}

if (!FSObject.FileExists(directory + "\\logtalk_index_to_xml.xsl")) {
	FSObject.CopyFile(logtalk_user + "\\tools\\lgtdoc\\xml\\logtalk_index_to_xml.xsl", directory + "\\logtalk_index_to_xml.xsl");
}

WScript.Echo("");
WScript.Echo("generating index file...");

create_index_file();

WScript.Echo("index file generated");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script generates an index for all the Logtalk XML files");
	WScript.Echo("documenting files in the current directory");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " [/f:format] [/i:index] [/t:title]");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  f - format of the index file (either xhtml or html; default is " + format + ")");
	WScript.Echo("  i - name of the index file (default is " + index_file + ")");
	WScript.Echo("  t - title to be used in the index file (default is " + index_title + ")");
	WScript.Echo("");
	WScript.Quit(1);
}

function create_index_file() {

	var f = FSObject.CreateTextFile(index_file, true);

	switch (format) {
		case "xhtml" :
			f.WriteLine("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
			f.WriteLine("<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>");
			f.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
			f.WriteLine("<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">");
			break;
		case "html" :
			f.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">");
			f.WriteLine("<html>");
			break;
	}

	f.WriteLine("<head>");
	f.WriteLine("    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>");
	f.WriteLine("    <title>" + index_title + "</title>");
	f.WriteLine("    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>");
	f.WriteLine("</head>");
	f.WriteLine("<body>");
	f.WriteLine("<h1>" + index_title + "</h1>");
	f.WriteLine("<ul>");

	var files = new Enumerator(FSObject.GetFolder(directory).Files);

	if (FSObject.FileExists(WshShell.CurrentDirectory + "\\directory_index.xml")) {
		f.WriteLine("    <li><a href=\"library_index.xml\">Library index</a></li>");
		f.WriteLine("    <li><a href=\"directory_index.xml\">Directory index</a></li>");
		f.WriteLine("    <li><a href=\"entity_index.xml\">Entity index</a></li>");
		f.WriteLine("    <li><a href=\"predicate_index.xml\">Predicate index</a></li>");
	} else {
		for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
			var file = files.item().name;
			if (FSObject.GetExtensionName(file) == "xml") {
				WScript.Echo("  indexing " + file);
				var index = FSObject.GetBaseName(file).lastIndexOf("_");
				var pars = FSObject.GetBaseName(file).slice(index+1);
				var entity = FSObject.GetBaseName(file).slice(0, index);
				if (pars == 0)
					f.WriteLine("    <li><a href=\"" + file + "\">" + entity + "</a></li>");
				else
					f.WriteLine("    <li><a href=\"" + file + "\">" + entity + "/" + pars + "</a></li>");
        	
			}
		}
	}

	f.WriteLine("</ul>");

	var today = new Date();
	var year  = today.getFullYear();
	var month = today.getMonth() + 1;
	if (month < 10)
        month = "0" + month;
	day   = today.getDate();
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
	f.WriteLine("<p>Generated on " + strToday + " - " + strTime + "</p>");

	f.WriteLine("</body>");
	f.WriteLine("</html>");

	f.Close();
}
