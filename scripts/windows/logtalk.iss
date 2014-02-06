; This file is part of Logtalk <http://logtalk.org/>  
; Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
; 
; Logtalk is free software. You can redistribute it and/or modify it under
; the terms of the FSF GNU General Public License 3  (plus some additional
; terms per section 7).        Consult the `LICENSE.txt` file for details.

#define MyAppName "Logtalk"
#define MyAppPublisher "Logtalk.org"
#define MyAppURL "http://logtalk.org"
#define MyAppUrlName "Logtalk Web Site.url"
#define MyAppRegURL "http://logtalk.org/regform.html"
#define MyAppRegUrlName "Logtalk Registration.url"

#define MyBaseDir "C:\lgt3git"
#define MyAppVer FileRead(FileOpen(MyBaseDir + "\VERSION.txt"))

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppName} {#MyAppVer}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
ChangesEnvironment=yes
ChangesAssociations=yes
DefaultDirName={pf}\Logtalk
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile={#MyBaseDir}\scripts\windows\LICENSE.rtf
InfoBeforeFile={#MyBaseDir}\scripts\windows\README.rtf
; SetupIconFile={#MyBaseDir}\scripts\windows\logtalk.ico
OutputBaseFilename=logtalk-{#MyAppVer}
Compression=lzma
SolidCompression=yes
PrivilegesRequired=none

VersionInfoTextVersion={#MyAppVer}
VersionInfoCopyright=© Paulo Moura, Copyright (c) 1998-2014

AllowRootDirectory=yes
UninstallFilesDir="{userdocs}\Logtalk uninstaller"

MinVersion=0,5.0

[Types]
Name: "full"; Description: "Full installation"
Name: "base"; Description: "Base system installation"
Name: "user"; Description: "User files installation"
Name: "prolog"; Description: "Prolog integration shortcuts"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "base"; Description: "Base system (compiler/runtime, Prolog integration files, manuals)"; Types: full base custom; Flags: disablenouninstallwarning
Name: "user"; Description: "User files (libraries, examples, and other support files)"; Types: full user custom; Flags: checkablealone disablenouninstallwarning
Name: "user\backup"; Description: "Backup current Logtalk user folder"; Types: full user custom; Flags: disablenouninstallwarning
Name: "prolog"; Description: "Prolog integration (back-end compiler support)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\bp"; Description: "B-Prolog integration (version 7.8 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\cxprolog"; Description: "CxProlog integration (version 0.97.7 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\eclipse"; Description: "ECLiPSe integration (version 6.1#143 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\gprolog"; Description: "GNU Prolog integration (version 1.4.2 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\lean"; Description: "Lean Prolog Prolog integration (version 3.8.4 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\quintus"; Description: "Quintus Prolog integration (version 3.3 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\sicstus"; Description: "SICStus Prolog integration (version 4.1.0 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\swicon"; Description: "SWI-Prolog (console) integration (version 6.0.0 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\swiwin"; Description: "SWI-Prolog (window) integration (version 6.0.0 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\xsb"; Description: "XSB integration (version 3.4.1 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\xsbmt"; Description: "XSB-MT integration (version 3.4.1 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\yap"; Description: "YAP (console) integration (version 6.3.4 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning
Name: "prolog\yap"; Description: "YAP (window) integration (version 6.3.2 or later)"; Types: full prolog custom; Flags: disablenouninstallwarning

[Tasks]
Name: registration; Description: "&Register {#MyAppName} (opens a web page; requires an Internet connection)"; Components: base
Name: shortcut; Description: "&Create a desktop shortcut to the Logtalk user folder"; Components: user

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Messages]
BeveledLabel={#MyAppName} {#MyAppVer} © Paulo Moura, Copyright (c) 1998-2014

[Dirs]
Name: {code:GetLgtUserDir}; Components: user; Flags: uninsneveruninstall
Name: "{userdocs}\Logtalk uninstaller"

[Files]
Source: "{#MyBaseDir}\*"; Excludes: ".*"; DestDir: "{app}"; Components: base; Flags: ignoreversion recursesubdirs createallsubdirs

Source: "{#MyBaseDir}\contributions\*"; Excludes: ".*"; DestDir: "{code:GetLgtUserDir}\contributions"; Components: user; Flags: ignoreversion recursesubdirs createallsubdirs uninsneveruninstall
Source: "{#MyBaseDir}\docs\*"; Excludes: ".*"; DestDir: "{code:GetLgtUserDir}\docs"; Components: user; Flags: ignoreversion recursesubdirs createallsubdirs uninsneveruninstall
Source: "{#MyBaseDir}\examples\*"; Excludes: ".*"; DestDir: "{code:GetLgtUserDir}\examples"; Components: user; Flags: ignoreversion recursesubdirs createallsubdirs uninsneveruninstall
Source: "{#MyBaseDir}\library\*"; Excludes: ".*"; DestDir: "{code:GetLgtUserDir}\library"; Components: user; Flags: ignoreversion recursesubdirs createallsubdirs uninsneveruninstall
Source: "{#MyBaseDir}\scratch\*"; Excludes: ".*"; DestDir: "{code:GetLgtUserDir}\scratch"; Components: user; Flags: ignoreversion recursesubdirs createallsubdirs uninsneveruninstall
Source: "{#MyBaseDir}\tools\*"; Excludes: ".*"; DestDir: "{code:GetLgtUserDir}\tools"; Components: user; Flags: ignoreversion recursesubdirs createallsubdirs uninsneveruninstall

Source: "{#MyBaseDir}\loader-sample.lgt"; DestDir: "{code:GetLgtUserDir}"; DestName: "loader-sample.lgt"; Components: user; Flags: ignoreversion uninsneveruninstall
Source: "{#MyBaseDir}\settings-sample.lgt"; DestDir: "{code:GetLgtUserDir}"; DestName: "settings-sample.lgt"; Components: user; Flags: ignoreversion uninsneveruninstall
Source: "{#MyBaseDir}\VERSION.txt"; DestDir: "{code:GetLgtUserDir}"; DestName: "VERSION.txt"; Components: user; Flags: ignoreversion uninsneveruninstall

Source: "{#MyBaseDir}\scripts\*.bat"; DestDir: "{win}"; Components: base; Flags: ignoreversion; Check: IsAdminLoggedOn
Source: "{#MyBaseDir}\tools\lgtdoc\xml\*.bat"; DestDir: "{win}"; Components: base; Flags: ignoreversion; Check: IsAdminLoggedOn

[INI]
Filename: "{app}\{#MyAppUrlName}"; Section: "InternetShortcut"; Key: "URL"; String: "{#MyAppURL}"; Components: base
Filename: "{app}\{#MyAppRegUrlName}"; Section: "InternetShortcut"; Key: "URL"; String: "{#MyAppRegURL}"; Components: base

[InstallDelete]
Type: files; Name: "{app}\adapters\*.xwam"
Type: files; Name: "{app}\integration\*.xwam"
Type: files; Name: "{app}\paths\*.xwam"

[Icons]
Name: "{group}\Bibliography"; Filename: "{app}\BIBLIOGRAPHY.bib"; Components: base
Name: "{group}\User and Reference Manuals"; Filename: "{app}\manuals\index.html"; Components: base
Name: "{group}\License"; Filename: "{app}\LICENSE.txt"; Components: base
Name: "{group}\Release Notes"; Filename: "{app}\RELEASE_NOTES.md"; Components: base
Name: "{group}\Read Me"; Filename: "{app}\README.md"; Components: base
Name: "{group}\Customization instructions"; Filename: "{app}\CUSTOMIZE.md"; Components: base
Name: "{group}\Default settings"; Filename: "%LOGTALKUSER%\settings-sample.lgt"; Components: base
Name: "{group}\Entity documentation"; Filename: "%LOGTALKUSER%\docs\index.html"; Components: base

Name: "{group}\Web Site"; Filename: "{#MyAppUrl}"; Components: base

Name: "{group}\Logtalk - B-Prolog"; Filename: "{code:GetBPExePath}"; Parameters: "-i ""$LOGTALKHOME/integration/logtalk_bp.pl"""; Comment: "Runs Logtalk with B-Prolog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\bp; Flags: createonlyiffileexists

Name: "{group}\Logtalk - CxProlog"; Filename: "{code:GetCxExePath}"; Parameters: "--script ""%LOGTALKHOME%\\integration\\logtalk_cx.pl"""; Comment: "Runs Logtalk with CxProlog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\cxprolog; Flags: createonlyiffileexists

Name: "{group}\Logtalk - ECLiPSe"; Filename: "{code:GetEclipseExePath}"; Parameters: "-L iso -b ""$LOGTALKHOME/integration/logtalk_eclipse.pl"""; Comment: "Runs Logtalk with ECLiPSe"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\eclipse; Flags: createonlyiffileexists

Name: "{group}\Logtalk - GNU Prolog"; Filename: "{code:GetGPExePath}"; Parameters: "--init-goal ""['$LOGTALKHOME/integration/logtalk_gp.pl']"""; Comment: "Runs Logtalk with GNU Prolog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\gprolog; Flags: createonlyiffileexists

Name: "{group}\Logtalk - Lean Prolog"; Filename: "{code:GetLeanPrologExePath}"; Parameters: """['$LOGTALKHOME/integration/logtalk_lean']"""; Comment: "Runs Logtalk with Lean Prolog"; WorkingDir: "%CD%"; Components: prolog\lean; Flags: createonlyiffileexists

Name: "{group}\Logtalk - Quintus Prolog"; Filename: "{code:GetQuintusExePath}"; Parameters: "+l ""%LOGTALKHOME%\integration\logtalk_quintus.pl"" +z ""$LOGTALKHOME"" ""$LOGTALKUSER"""; Comment: "Runs Logtalk with Quintus Prolog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\quintus; Flags: createonlyiffileexists

Name: "{group}\Logtalk - SICStus Prolog"; Filename: "{code:GetSICStusExePath}"; Parameters: "-l ""%LOGTALKHOME%\integration\logtalk_sicstus.pl"""; Comment: "Runs Logtalk with SICStus Prolog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\sicstus; Flags: createonlyiffileexists

Name: "{group}\Logtalk - SWI-Prolog (console)"; Filename: "{code:GetSWIConExePath}"; Parameters: "-s ""%LOGTALKHOME%\integration\logtalk_swi.pl"""; Comment: "Runs Logtalk with SWI-Prolog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\swicon; Flags: createonlyiffileexists

Name: "{group}\Logtalk - SWI-Prolog (window)"; Filename: "{code:GetSWIWinExePath}"; Parameters: "-s ""%LOGTALKHOME%\integration\logtalk_swi.pl"""; Comment: "Runs Logtalk with SWI-Prolog"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\swiwin; Flags: createonlyiffileexists

Name: "{group}\Logtalk - XSB"; Filename: "{code:GetXSBExePath}"; Parameters: "-l -e ""['%LOGTALKHOME%\\integration\\logtalk_xsb.pl']."""; Comment: "Runs Logtalk with XSB (first time may require running as administrator)"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\xsb; Flags: createonlyiffileexists

Name: "{group}\Logtalk - XSB-MT"; Filename: "{code:GetXSBMTExePath}"; Parameters: "-l -e ""['%LOGTALKHOME%\\integration\\logtalk_xsbmt.pl']."""; Comment: "Runs Logtalk with XSB-MT (first time may require running as administrator)"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\xsbmt; Flags: createonlyiffileexists

Name: "{group}\Logtalk - YAP (console)"; Filename: "{code:GetYAPConExePath}"; Parameters: "-l ""$LOGTALKHOME/integration/logtalk_yap.pl"""; Comment: "Runs Logtalk with YAP"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\yap; Flags: createonlyiffileexists

Name: "{group}\Logtalk - YAP (window)"; Filename: "{code:GetYAPWinExePath}"; Parameters: "-l ""$LOGTALKHOME/integration/logtalk_yap.pl"""; Comment: "Runs Logtalk with YAP"; WorkingDir: "%LOGTALKUSER%"; Components: prolog\yap; Flags: createonlyiffileexists

Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"; Components: base

Name: "{code:GetLgtUserDir}\Bibliography"; Filename: "{app}\BIBLIOGRAPHY.bib"; Components: user
Name: "{code:GetLgtUserDir}\License"; Filename: "{app}\LICENSE.txt"; Components: user
Name: "{code:GetLgtUserDir}\Release Notes"; Filename: "{app}\RELEASE_NOTES.md"; Components: user
Name: "{code:GetLgtUserDir}\Read Me"; Filename: "{app}\README.md"; Components: user
Name: "{code:GetLgtUserDir}\Customization instructions"; Filename: "{app}\CUSTOMIZE.md"; Components: user
Name: "{code:GetLgtUserDir}\adapters"; Filename: "{app}\adapters"; Components: user
Name: "{code:GetLgtUserDir}\manuals"; Filename: "{app}\manuals"; Components: user
Name: "{code:GetLgtUserDir}\paths"; Filename: "{app}\paths"; Components: user
Name: "{code:GetLgtUserDir}\coding"; Filename: "{app}\coding"; Components: user

Name: "{userdesktop}\Logtalk user files"; Filename: "{code:GetLgtUserDir}"; Components: user; Tasks: shortcut

[Registry]
; admin users
Root: HKLM; Subkey: "Software\Logtalk"; ValueType: string; ValueName: "Version"; ValueData: "{#MyAppVer}"; Components: base; Flags: deletevalue uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: expandsz; ValueName: "LOGTALKHOME"; ValueData: "{app}"; Components: base; Flags: deletevalue uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKCU; Subkey: "Environment"; ValueType: expandsz; ValueName: "LOGTALKUSER"; ValueData: "{code:GetLgtUserDir}"; Flags: deletevalue uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKLM; Subkey: "SOFTWARE\Classes\MIME\Database\Content Type\text/x-logtalk"; Components: base; Flags: uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKLM; Subkey: "SOFTWARE\Classes\MIME\Database\Content Type\text/x-logtalk"; ValueType: string; ValueName: "Extension"; ValueData: ".lgt"; Components: base; Flags: deletevalue uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKLM; Subkey: "SOFTWARE\Classes\MIME\Database\Content Type\text/x-logtalk"; ValueType: string; ValueName: "Extension"; ValueData: ".logtalk"; Components: base; Flags: deletevalue uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".lgt"; ValueType: string; ValueName: ""; ValueData: "LogtalkSourceFile"; Components: base; Flags: uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".lgt"; ValueType: string; ValueName: "Content Type"; ValueData: "text/x-logtalk"; Components: base; Flags: uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".logtalk"; ValueType: string; ValueName: ""; ValueData: "LogtalkSourceFile"; Components: base; Flags: uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".logtalk"; ValueType: string; ValueName: "Content Type"; ValueData: "text/x-logtalk"; Components: base; Flags: uninsdeletevalue; Check: IsAdminLoggedOn
Root: HKCR; Subkey: "LogtalkSourceFile"; ValueType: string; ValueName: ""; ValueData: "Logtalk source file"; Components: base; Flags: uninsdeletekey; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".md"; ValueType: string; ValueName: ""; ValueData: "txtfile"; Components: base; Flags: uninsdeletevalue createvalueifdoesntexist; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".md"; ValueType: string; ValueName: "Content Type"; ValueData: "text/plain"; Components: base; Flags: uninsdeletevalue createvalueifdoesntexist; Check: IsAdminLoggedOn
Root: HKCR; Subkey: ".md"; ValueType: string; ValueName: "PerceivedType"; ValueData: "text"; Components: base; Flags: uninsdeletevalue createvalueifdoesntexist; Check: IsAdminLoggedOn
; non-admin users
Root: HKCU; Subkey: "Software\Logtalk"; ValueType: string; ValueName: "Version"; ValueData: "{#MyAppVer}"; Components: base; Flags: deletevalue uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "Environment"; ValueType: expandsz; ValueName: "LOGTALKHOME"; ValueData: "{app}"; Components: base; Flags: deletevalue uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "Environment"; ValueType: expandsz; ValueName: "LOGTALKUSER"; ValueData: "{code:GetLgtUserDir}"; Flags: deletevalue uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\MIME\Database\Content Type\text/x-logtalk"; Components: base; Flags: uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\MIME\Database\Content Type\text/x-logtalk"; ValueType: string; ValueName: "Extension"; ValueData: ".lgt"; Components: base; Flags: deletevalue uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\MIME\Database\Content Type\text/x-logtalk"; ValueType: string; ValueName: "Extension"; ValueData: ".logtalk"; Components: base; Flags: deletevalue uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.lgt"; ValueType: string; ValueName: ""; ValueData: "LogtalkSourceFile"; Components: base; Flags: uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.lgt"; ValueType: string; ValueName: "Content Type"; ValueData: "text/x-logtalk"; Components: base; Flags: uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.logtalk"; ValueType: string; ValueName: ""; ValueData: "LogtalkSourceFile"; Components: base; Flags: uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.logtalk"; ValueType: string; ValueName: "Content Type"; ValueData: "text/x-logtalk"; Components: base; Flags: uninsdeletevalue; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\LogtalkSourceFile"; ValueType: string; ValueName: ""; ValueData: "Logtalk source file"; Components: base; Flags: uninsdeletekey; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.md"; ValueType: string; ValueName: ""; ValueData: "txtfile"; Components: base; Flags: uninsdeletevalue createvalueifdoesntexist; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.md"; ValueType: string; ValueName: "Content Type"; ValueData: "text/plain"; Components: base; Flags: uninsdeletevalue createvalueifdoesntexist; Check: not IsAdminLoggedOn
Root: HKCU; Subkey: "SOFTWARE\Classes\.md"; ValueType: string; ValueName: "PerceivedType"; ValueData: "text"; Components: base; Flags: uninsdeletevalue createvalueifdoesntexist; Check: not IsAdminLoggedOn

[Run]
Filename: "{app}\README.md"; Description: "Open the README.md file"; Components: base; Flags: postinstall shellexec skipifsilent
Filename: "{app}\{#MyAppRegUrlName}"; Components: base; Flags: shellexec nowait; Tasks: registration

[UninstallDelete]
Type: filesandordirs; Name: "{app}"; Components: base
Type: filesandordirs; Name: "{group}"; Components: base

[Code]
var
  LgtUserDirPage: TInputDirWizardPage;
  WarningPage, ErrorPage: TOutputMsgWizardPage;
  Explanation, Warning, Error, BackupFolder: String;

function GetLgtUserDir(Param: String): String;
begin
  Result := LgtUserDirPage.Values[0]
end;

function GetCurrentDate(Param: String): String;
begin
  Result := GetDateTimeString('dddddd tt', '-', '-')
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  NewFolder: String;
begin
  NewFolder := LgtUserDirPage.Values[0];
  if (CurStep = ssInstall) and DirExists(NewFolder) and (pos('backup', WizardSelectedComponents(False)) > 0) then begin
    BackupFolder := NewFolder + '-backup-' + GetDateTimeString('yyyy-mm-dd-hhnnss', '-', ':');
    RenameFile(NewFolder, BackupFolder)
  end
  else if (CurStep = ssPostInstall) then begin
    if FileExists(BackupFolder + '\settings.lgt') then
      FileCopy(BackupFolder + '\settings.lgt', NewFolder + '\settings.lgt', False);
    if FileExists(BackupFolder + '\settings.logtalk') then
      FileCopy(BackupFolder + '\settings.logtalk', NewFolder + '\settings.logtalk', False)
  end
end;

function BPExePath: String;
var
  BPDIR: String;
begin
  if RegQueryStringValue(HKLM, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment\', 'BPDIR', BPDIR) then
    if FileExists(BPDIR + '\bp.bat') then
      Result := BPDIR + '\bp.bat'
    else if FileExists(BPDIR + '\bp.exe') then
      Result := BPDIR + '\bp.exe'
    else
      Result := 'prolog_compiler_not_installed'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetBPExePath(Param: String): String;
var
  Warning: String;
begin
  Result := BPExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect B-Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
    MsgBox(Warning, mbError, MB_OK);
  end
end;

function CxExePath: String;
var
  CxDir: String;
begin
  if RegQueryStringValue(HKLM, 'SOFTWARE\CxProlog\', 'CXPROLOG_DIR', CxDir) then
    Result := CxDir + '\cxprolog.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetCxExePath(Param: String): String;
var
  Warning: String;
begin
  Result := CxExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect CxProlog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function EclipseExePath: String;
var
  ECLIPSEDIR: String;
begin
  if IsWin64 then
    if RegQueryStringValue(HKLM64, 'Software\IC-Parc\Eclipse\6.1\', 'ECLIPSEDIR', ECLIPSEDIR) then
        Result := ECLIPSEDIR + '\lib\x86_64_nt\eclipse.exe'
    else if RegQueryStringValue(HKLM32, 'Software\IC-Parc\Eclipse\6.1\', 'ECLIPSEDIR', ECLIPSEDIR) then
        Result := ECLIPSEDIR + '\lib\i386_nt\eclipse.exe'
    else
      Result := 'prolog_compiler_not_installed'
  else if RegQueryStringValue(HKLM, 'Software\IC-Parc\Eclipse\6.1\', 'ECLIPSEDIR', ECLIPSEDIR) then
         Result := ECLIPSEDIR + '\lib\i386_nt\eclipse.exe'
       else
         Result := 'prolog_compiler_not_installed'
end;

function GetEclipseExePath(Param: String): String;
var
  Warning: String;
begin
  Result := EclipseExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect ECLiPSe Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
    MsgBox(Warning, mbError, MB_OK);
  end
end;

function GPExePath: String;
var
  RootPath: String;
begin
  if RegQueryStringValue(HKCU, 'Software\GnuProlog\', 'RootPath', RootPath) then
    Result := RootPath + '\bin\gprolog.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetGPExePath(Param: String): String;
var
  Warning: String;
begin
  Result := GPExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect GNU Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
    MsgBox(Warning, mbError, MB_OK);
  end
end;

function LeanPrologExePath: String;
begin
  if FileExists(ExpandConstant('{pf}') + '\LeanProlog\lprolog.bat') then
    Result := ExpandConstant('{pf}') + '\LeanProlog\lprolog.bat'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetLeanPrologExePath(Param: String): String;
var
  Warning: String;
begin
  Result := LeanPrologExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect Lean Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
    MsgBox(Warning, mbError, MB_OK);
  end
end;

function QuintusExePath: String;
var
  SP_PATH: String;
begin
  if FileExists(ExpandConstant('{pf}') + '\Quintus Prolog 3.5\bin\ix86\qpwin.exe') then
    Result := ExpandConstant('{pf}') + '\Quintus Prolog 3.5\bin\ix86\qpwin.exe'
  else if FileExists(ExpandConstant('{pf}') + '\Quintus Prolog 3.4\bin\ix86\qpwin.exe') then
    Result := ExpandConstant('{pf}') + '\Quintus Prolog 3.4\bin\ix86\qpwin.exe'
  else if FileExists(ExpandConstant('{pf}') + '\Quintus Prolog 3.3\bin\ix86\qpwin.exe') then
    Result := ExpandConstant('{pf}') + '\Quintus Prolog 3.3\bin\ix86\qpwin.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetQuintusExePath(Param: String): String;
var
  Warning: String;
begin
  Result := QuintusExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect Quintus Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function SICStusExePath: String;
var
  SP_PATH: String;
begin
  if IsWin64 then
    if RegQueryStringValue(HKLM64, 'Software\SICS\SICStus4.2_x86-win32-nt-4\', 'SP_PATH', SP_PATH) or
       RegQueryStringValue(HKLM32, 'Software\SICS\SICStus4.2_x86-win32-nt-4\', 'SP_PATH', SP_PATH) or
       RegQueryStringValue(HKLM32, 'Software\SICS\SICStus4.1_x86-win32-nt-4\', 'SP_PATH', SP_PATH)
    then
      Result := SP_PATH + '\bin\spwin.exe'  
    else
      Result := 'prolog_compiler_not_installed'
  else if RegQueryStringValue(HKLM, 'Software\SICS\SICStus4.2_x86-win32-nt-4\', 'SP_PATH', SP_PATH) then
    Result := SP_PATH + '\bin\spwin.exe'
  else if RegQueryStringValue(HKLM, 'Software\SICS\SICStus4.1_x86-win32-nt-4\', 'SP_PATH', SP_PATH) then
    Result := SP_PATH + '\bin\spwin.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetSICStusExePath(Param: String): String;
var
  Warning: String;
begin
  Result := SICStusExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect SICStus Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function SWIConExePath: String;
var
  Home: String;
begin
  if IsWin64 then
    if RegQueryStringValue(HKLM64, 'Software\SWI\Prolog64\', 'home', Home) or
       RegQueryStringValue(HKLM64, 'Software\SWI\Prolog\', 'home', Home) or
       RegQueryStringValue(HKLM32, 'Software\SWI\Prolog64\', 'home', Home) or
       RegQueryStringValue(HKLM32, 'Software\SWI\Prolog\', 'home', Home)
    then
      if FileExists(Home + '\bin\plcon.exe') then
        Result := Home + '\bin\plcon.exe'
      else if FileExists(Home + '\bin\swipl.exe') then
        Result := Home + '\bin\swipl.exe'
      else
        Result := 'prolog_compiler_not_installed'
  else
    if RegQueryStringValue(HKLM, 'Software\SWI\Prolog64\', 'home', Home) or
       RegQueryStringValue(HKLM, 'Software\SWI\Prolog\', 'home', Home)
    then
      if FileExists(Home + '\bin\plcon.exe') then
        Result := Home + '\bin\plcon.exe'
      else if FileExists(Home + '\bin\swipl.exe') then
        Result := Home + '\bin\swipl.exe'
      else
        Result := 'prolog_compiler_not_installed'
end;

function GetSWIConExePath(Param: String): String;
var
  Warning: String;
begin
  Result := SWIConExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect SWI-Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK)
  end
end;

function SWIWinExePath: String;
var
  Home: String;
begin
  if IsWin64 then
    if RegQueryStringValue(HKLM64, 'Software\SWI\Prolog64\', 'home', Home) or
       RegQueryStringValue(HKLM64, 'Software\SWI\Prolog\', 'home', Home) or
       RegQueryStringValue(HKLM32, 'Software\SWI\Prolog64\', 'home', Home) or
       RegQueryStringValue(HKLM32, 'Software\SWI\Prolog\', 'home', Home)
    then
      if FileExists(Home + '\bin\plwin.exe') then
        Result := Home + '\bin\plwin.exe'
      else if FileExists(Home + '\bin\swipl-win.exe') then
        Result := Home + '\bin\swipl-win.exe'
      else
        Result := 'prolog_compiler_not_installed'
  else
    if RegQueryStringValue(HKLM, 'Software\SWI\Prolog64\', 'home', Home) or
       RegQueryStringValue(HKLM, 'Software\SWI\Prolog\', 'home', Home)
    then
      if FileExists(Home + '\bin\plwin.exe') then
        Result := Home + '\bin\plwin.exe'
      else if FileExists(Home + '\bin\swipl-win.exe') then
        Result := Home + '\bin\swipl-win.exe'
      else
        Result := 'prolog_compiler_not_installed'
end;

function GetSWIWinExePath(Param: String): String;
var
  Warning: String;
begin
  Result := SWIWinExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect SWI-Prolog installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK)
  end
end;

function XSBExePath: String;
var
  XSB_DIR: String;
begin
  if RegQueryStringValue(HKLM, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment\', 'XSB_DIR', XSB_DIR) then
    if IsWin64 and FileExists(XSB_DIR + '\config\x64-pc-windows\bin\xsb.exe') then
      Result := XSB_DIR + '\config\x64-pc-windows\bin\xsb.exe'
    else if FileExists(XSB_DIR + '\config\x86-pc-windows\bin\xsb.exe') then
      Result := XSB_DIR + '\config\x86-pc-windows\bin\xsb.exe'
    else if FileExists(XSB_DIR + '\config\i686-pc-cygwin\bin\xsb.exe') then
      Result := XSB_DIR + '\config\i686-pc-cygwin\bin\xsb.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetXSBExePath(Param: String): String;
var
  Warning: String;
begin
  Result := XSBExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect XSB installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function XSBMTExePath: String;
var
  XSB_DIR: String;
begin
  if RegQueryStringValue(HKLM, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment\', 'XSB_DIR', XSB_DIR) then
    if IsWin64 and FileExists(XSB_DIR + '\config\x64-pc-windows-mt\bin\xsb.exe') then
      Result := XSB_DIR + '\config\x64-pc-windows-mt\bin\xsb.exe'
    else if FileExists(XSB_DIR + '\config\x86-pc-windows-mt\bin\xsb.exe') then
      Result := XSB_DIR + '\config\x86-pc-windows-mt\bin\xsb.exe'
    else if FileExists(XSB_DIR + '\config\i686-pc-cygwin-mt\bin\xsb.exe') then
      Result := XSB_DIR + '\config\i686-pc-cygwin-mt\bin\xsb.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetXSBMTExePath(Param: String): String;
var
  Warning: String;
begin
  Result := XSBMTExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect XSB-MT installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function YAPConExePath: String;
var
  Home: String;
begin
  if IsWin64 then
    if RegQueryStringValue(HKLM64, 'Software\YAP\Prolog64\', 'home', Home) then
      Result := Home + '\bin\yap.exe'
    else if RegQueryStringValue(HKLM32, 'Software\YAP\Prolog64\', 'home', Home) then
      Result := Home + '\bin\yap.exe'
    else
      Result := 'prolog_compiler_not_installed'
  else if RegQueryStringValue(HKLM, 'Software\YAP\Prolog\', 'home', Home) then
    Result := Home + '\bin\yap.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetYAPConExePath(Param: String): String;
var
  Warning: String;
begin
  Result := YAPConExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect YAP installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function YAPWinExePath: String;
var
  Home: String;
begin
  if IsWin64 then
    if RegQueryStringValue(HKLM64, 'Software\YAP\Prolog64\', 'home', Home) then
      Result := Home + '\bin\yap-win.exe'
    else if RegQueryStringValue(HKLM32, 'Software\YAP\Prolog64\', 'home', Home) then
      Result := Home + '\bin\yap-win.exe'
    else
      Result := 'prolog_compiler_not_installed'
  else if RegQueryStringValue(HKLM, 'Software\YAP\Prolog\', 'home', Home) then
    Result := Home + '\bin\yap-win.exe'
  else
    Result := 'prolog_compiler_not_installed'
end;

function GetYAPWinExePath(Param: String): String;
var
  Warning: String;
begin
  Result := YAPWinExePath;
  if Result = 'prolog_compiler_not_installed' then
  begin
    Warning := 'Failed to detect YAP installation.' + Chr(13) + 'Logtalk integration shortcut not created.';
  MsgBox(Warning, mbError, MB_OK);
  end
end;

function NoBackEndPrologCompilerInstalled: Boolean;
begin
    Result :=
      (BPExePath = 'prolog_compiler_not_installed') and
      (CxExePath = 'prolog_compiler_not_installed') and
      (EclipseExePath = 'prolog_compiler_not_installed') and
      (GPExePath = 'prolog_compiler_not_installed') and
      (LeanPrologExePath = 'prolog_compiler_not_installed') and
      (SICStusExePath = 'prolog_compiler_not_installed') and
      (SWIConExePath = 'prolog_compiler_not_installed') and
      (SWIWinExePath = 'prolog_compiler_not_installed') and
      (XSBExePath = 'prolog_compiler_not_installed') and
      (XSBMTExePath = 'prolog_compiler_not_installed') and
      (YAPConExePath = 'prolog_compiler_not_installed') and
      (YAPWinExePath = 'prolog_compiler_not_installed')
end;

procedure InitializeWizard;
var
  Version, InstalledVersion: String;
  LOGTALKHOME, LOGTALKUSER: String;
begin
  if IsAdminLoggedOn then
    if RegQueryStringValue(HKLM, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'LOGTALKHOME', LOGTALKHOME) then
      WizardForm.DirEdit.Text := LOGTALKHOME
    else
      WizardForm.DirEdit.Text := ExpandConstant('{pf}') + '\Logtalk'
  else if RegQueryStringValue(HKCU, 'Environment', 'LOGTALKHOME', LOGTALKHOME) then
    WizardForm.DirEdit.Text := LOGTALKHOME
  else
    WizardForm.DirEdit.Text := ExpandConstant('{localappdata}') + '\Logtalk';
  Explanation := 'Select the folder in which Setup should install Logtalk user files, then click Next.'
                 + Chr(13) + Chr(13)
                 + 'These files allows each user to independently customize Logtalk and to freely modify the provided libraries, programming examples, and other supporting files.'
                 + Chr(13) + Chr(13)
                 + 'A copy of these files must exist in the user home folder in order to use the Logtalk-Prolog integration scripts available from the Start Menu program group "Logtalk".'
                 + Chr(13) + Chr(13)
                 + 'Addtional end-users may use this installer to make a copy of these files on their home folders after a full installation of Logtalk.';
  LgtUserDirPage := CreateInputDirPage(wpSelectDir,
    'Select folder for Logtalk user files', 'Where should Logtalk user files be installed?',
    Explanation,
    False, 'New Folder');
  LgtUserDirPage.Add('');
  if RegQueryStringValue(HKCU, 'Environment', 'LOGTALKUSER', LOGTALKUSER) then
    LgtUserDirPage.Values[0] := LOGTALKUSER
  else 
    LgtUserDirPage.Values[0] := ExpandConstant('{userdocs}') + '\Logtalk';
  if not IsAdminLoggedOn and RegQueryStringValue(HKLM, 'Software\Logtalk', 'Version', Version) then
  begin
    Warning := 'You are running this installer from a non-administrative account.'
               + Chr(13) + Chr(13)
               + 'The base Logtalk system is already installed by an administrator. You may simply setup Logtalk for you as an end-user by choosing to install only the user-level files. In alternative, you may perform a full installation.'
               + Chr(13) + Chr(13)
               + 'If Logtalk is already set for you, this installer will make a backup copy of your current files (if you choose the same installation folder) and will restore all user files to their default, pristine state.';
    WarningPage := CreateOutputMsgPage(wpWelcome, 'Information', 'Please read the following important information before continuing.', Warning);
    WizardForm.TypesCombo.ItemIndex := 2;
    WizardForm.TypesCombo.OnChange(WizardForm.TypesCombo)
  end;
  if RegQueryStringValue(HKCU, 'Software\Logtalk', 'Version', Version) then
    InstalledVersion := Version
  else if RegQueryStringValue(HKLM, 'Software\Logtalk', 'Version', Version) then
    InstalledVersion := Version
  else if RegQueryStringValue(HKCU, 'Environment', 'LOGTALKHOME', LOGTALKHOME) and DirExists(LOGTALKHOME) then
    InstalledVersion := 'no_installed'
  else if RegQueryStringValue(HKLM, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'LOGTALKHOME', LOGTALKHOME) and DirExists(LOGTALKHOME) then
    InstalledVersion := 'no_installed'
  else
    InstalledVersion := 'no_installed';
  if (CompareStr(InstalledVersion, 'no_installed') <> 0) and (CompareStr(InstalledVersion, ExpandConstant('{#MyAppVer}')) < 0) then
  begin
    Warning := 'Your Logtalk user directory is outdated: ' + InstalledVersion + ' < ' + ExpandConstant('{#MyAppVer}')
               + Chr(13) + Chr(13)
               + 'You must updade your Logtalk user folder by performing a full installation.'
               + Chr(13) + Chr(13)
               + 'All aditional Logtalk users on your computer must also use this installer to update their Logtalk user folders.';
    WarningPage := CreateOutputMsgPage(wpWelcome, 'Warning', 'Logtalk user folder update required.', Warning)
  end;
  if NoBackEndPrologCompilerInstalled then
  begin
    Error := 'No compatible Prolog compiler found!'
             + Chr(13) + Chr(13)
             + 'Logtalk requires a compatible Prolog compiler to be installed in order to run. Logtalk uses a Prolog compiler as a back-end compiler.'
             + Chr(13) + Chr(13)
             + 'You must rerun the Logtalk installer after installing a compatible Prolog compiler.';
    ErrorPage := CreateOutputMsgPage(wpSelectDir, 'Warning', 'No compatible Prolog compiler found!', Error)
  end
end;
