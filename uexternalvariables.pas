unit uExternalVariables;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uObjects, uContainers;

type

  TExtVarInfo = class
  public
    BelongsToProgram: string;
    VariableName: string;
    constructor Create(const InProgram, Name: string);
  end;

type

  { TUsedProgram }

  TUsedProgram = record
    Timestamp: longint;
    Name: string;
    FullPath: string;
    Variables: TVariablesTable;
    class operator = (a, b: TUsedProgram): Boolean;
    procedure Free;
  end;

type
  TUsedProgramsList = specialize TGenericHashTable<string, TUsedProgram>;


function UsedProgram(const ProgramName, ProgramFullPath: string): TUsedProgram;

procedure FetchUsedPrograms(Code: TStrings; const CurrentDir: string; var Programs: TUsedProgramsList);

procedure ProcessUsedProgram(var TheProgram: TUsedProgram);

implementation

uses uSettings, regexpr, Forms, FileUtil, uVariableFetcher;

var
  UsedProgramRegex: TRegexpr;


function UsedProgram(const ProgramName, ProgramFullPath: string): TUsedProgram;
var
  TheFile: longint;
begin
  TheFile := -1;
  try
    with Result do
    begin
      Name := ProgramName;
      FullPath := ProgramFullPath;
      TheFile := FileOpen(ProgramFullPath, fmOpenRead + fmShareDenyNone);
      Variables := TVariablesTable.Create;
      Variables.Sorted := True;
      Variables.OnCompare := @KeyCompare;
    end;
  finally
    if TheFile > 0 then
      FileClose(TheFile);
  end;
end;

function UsedProgramDummy(const ProgramName: string): TUsedProgram;
begin
  Result.Name := ProgramName;
end;

function CheckExists(const ProgramName, CurrentDir: string; out ActualPath: string): boolean;
var
  Aux: string;
begin
  Result := False;
  Aux := ChangeFileExt(Settings.GetValue('kspfolder', '') + PathDelim + 'Plugins' + PathDelim + 'PluginData' +
    PathDelim + 'Archive' + PathDelim + ProgramName, '.txt');

  if FileExists(Aux) then
  begin
    ActualPath := Aux;
    Result := True;
  end
  else if FileExists(ChangeFileExt(CurrentDir + PathDelim + ProgramName, '.txt')) then
  begin
    ActualPath := ChangeFileExt(CurrentDir + PathDelim + ProgramName, '.txt');
    Result := True;
  end;

end;

procedure FetchUsedPrograms(Code: TStrings; const CurrentDir: string; var Programs: TUsedProgramsList);
var
  Line, ActualPath: string;
  AuxList: TUsedProgramsList;
  Key: string;
  Aux: TUsedProgram;

begin
  AuxList := TUsedProgramsList.Create;
  for Line in Code do
    if UsedProgramRegex.Exec(Line) then
      if CheckExists(UsedProgramRegex.Match[2], CurrentDir, ActualPath) then
        AuxList[UsedProgramRegex.Match[2]] := UsedProgram(UsedProgramRegex.Match[2], ActualPath);

  AuxList.PrepareIterator;
  while AuxList.GetNextKey(Key) do
    begin
    if Programs.Contains(Key) then
      begin
      Aux := Programs[Key];
      Aux.Variables := TVariablesTable.Create;
      Aux.Variables.Assign(Programs[Key].Variables);
      AuxList[Key] := Aux;
      end;
    end;

  Programs.Clear;
  Programs.AddFrom(AuxList);

  Application.ProcessMessages;
end;


procedure ProcessUsedProgram(var TheProgram: TUsedProgram);
var TheFile: longint;
    Code: TStrings;
    Stream: TFileStream;
begin
  TheFile := -1;
  Code := TStringList.Create;
  Stream := nil;
  try
    try
      TheFile :=  FileOpen(TheProgram.FullPath, fmOpenRead or fmShareDenyNone);
      if TheProgram.Timestamp <>  FileGetDate(TheFile) then
      begin
        Stream := TFileStream.Create(Utf8ToSys(TheProgram.FullPath), fmOpenRead or fmShareDenyNone);
        Code.LoadFromStream(Stream);
        FetchVariables(Code, TheProgram.Variables, True);
        TheProgram.Timestamp := FileGetDate(TheFile);
      end;
    except
    end;
  finally
    if Assigned(Stream) then
      Stream.Free;
    Code.Free;
    if TheFile > 0 then
      FileClose(TheFile);
  end;
end;

class operator TUsedProgram.=(a, b: TUsedProgram): Boolean;
begin
  Result := UpperCase(a.Name) = UpperCase(b.Name);
end;

procedure TUsedProgram.Free;
begin
  Variables.Free;
end;

{ ExtVarInfo }

constructor TExtVarInfo.Create(const InProgram, Name: string);
begin
  BelongsToProgram := InProgram;
  VariableName := Name;
end;

initialization
  UsedProgramRegex := TRegExpr.Create;
  UsedProgramRegex.Expression := '(?i)(run\s)([a-z,0-9_]+)';

finalization
  UsedProgramRegex.Free;

end.
