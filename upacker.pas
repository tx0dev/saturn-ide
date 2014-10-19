unit uPacker;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Clipbrd, uExternalVariables;

procedure PackToFile(const Code: TStrings; const Destination: string; RemoveComments, RemoveTrailingSpaces: boolean;
  Tags: TStrings; out OriginalSize, PackedSize: integer);

procedure PackToArchive(const Code: TStrings; const ProgramName: string; RemoveComments, RemoveTrailingSpaces: boolean;
  Tags: TStrings; out OriginalSize, PackedSize: integer);

procedure PackToClipboard(const Code: TStrings; RemoveComments, RemoveTrailingSpaces: boolean;
  Tags: TStrings; out OriginalSize, PackedSize: integer);

procedure RunDeepCollisionScan(Code: TStrings; const FileName: string; var TextToShow: string);

procedure SyntaxCheck(out Messages: string);

implementation

uses uSettings, regexpr, FileUtil, uObjects, uVariableFetcher, fgl, uSyntaxCheck, uMarkup, uMain;


type TVarInfo = record
  VarName, ProgamName: string;
  CallChain: string;
  class operator = (a, b: TVarInfo): Boolean;
end;

type TVarInfoList = specialize TFPGList<TVarInfo>;


var
  Digits: array[1..3] of byte = (Ord('a'), Ord('a') - 1, Ord('a') - 1);
  CommentsRegex, VariableDeclRegex: TRegexpr; // Regex to look for comments and variables

procedure IncDigit(Index: integer);
begin
  if Index > 3 then
    exit;
  if Digits[Index] = Ord('z') then
  begin
    Digits[Index] := Ord('a');
    IncDigit(Index + 1);
  end
  else
    Inc(Digits[Index]);
end;

{function GetNewCombination: string;
var
  D: byte;
begin
  Result := '';
  for D in Digits do
    Result := Result + chr(D);
  IncDigit(1);
  Result := LeftStr(Result, Pos(chr(Ord('a') - 1), Result) - 1);
  Result := FilterKnownIdentifiers(Result);
end;

function GetNewVariableName: string;
begin
  repeat
    Result := GetNewCombination;
  until Result <> '';
end;}


function VarInfo(const VarName, ProgName, CallChain: string): TVarInfo;
begin
  Result.VarName := VarName;
  Result.ProgamName := ProgName;
  Result.CallChain := CallChain + ' > ' + ProgName;
end;

class operator TVarInfo.=(a, b: TVarInfo): Boolean;
begin
  Result := UpperCase(a.VarName) = UpperCase(b.VarName);
end;

function FilterComments(const Line: string; RemoveComments: boolean): string;
begin
  Result := Line;
  if not RemoveComments then
    exit;
  if CommentsRegex.Exec(Line) then
    Result := LeftStr(Line, CommentsRegex.MatchPos[0] - 1);
end;

function TrimLine(const Line: string; RemoveTrailingSpaces: boolean): string;
begin
  Result := TrimRight(Line);
  if RemoveTrailingSpaces then
    Result := TrimLeft(Result);
end;

function FilterTag(const Line: string; Expr: TRegExpr; var Filtering: boolean): string;
begin
  if Expr = nil then
  begin
    Result := Line;
    exit;
  end;
  if Filtering then
  begin
    if Trim(Line) = '' then
      Filtering := False;
    Result := '';
  end
  else
  begin
    if Expr.Exec(Line) then
    begin
      Filtering := True;
      Result := '';
    end
    else
      Result := Line;
  end;
end;

function BuildTagList(const Tags: TStrings): TRegexpr;
var
  Aux, Tag: string;
begin
  Result := nil;
  Aux := '(?i)';
  for Tag in Tags do
    Aux := Aux + '(\s)*(//' + Tag + '(\s|$))|';
  if Aux <> '(?i)' then
  begin
    Result := TRegExpr.Create;
    Result.Expression := Copy(Aux, 1, Length(aux) - 1);
  end;
end;

procedure SyntaxCheck(out Messages: string);
var Errors: TMarkupList;
    Error: TMarkupRecord;
begin
  Errors := TMarkupList.Create;
  uSyntaxCheck.SyntaxCheck(frmMain.ActiveEditor, Errors);

  Messages := '';
  Errors.PrepareIterator;
  while Errors.GetNextItem(Error) do
    Messages := Messages + 'Error at line/col (' + IntToStr(Error.StartPoint.Y) + ',' + IntToStr(Error.StartPoint.X) +
                '): ' +  Error.Message + LineEnding;
  Errors.Free;
end;

function Pack(const Code: TStrings; RemoveComments, RemoveTrailingSpaces: boolean; Tags: TStrings;
  out OriginalSize, PackedSize: integer): TStrings;
var
  Idx: integer;
  Line: string;
  Filtering: boolean;
  TagsToRemove: TRegExpr;
begin
  Result := TStringList.Create;
  OriginalSize := 0;
  PackedSize := 0;
  Filtering := False;
  TagsToRemove := BuildTagList(Tags);
  try
    for Idx := 0 to Code.Count - 1 do
    begin
      Line := Code[Idx];
      OriginalSize := OriginalSize + Length(Line);
      Line := FilterTag(Line, TagsToRemove, Filtering);
      if Filtering then
        continue;
      Line := FilterComments(Line, RemoveComments);
      Line := TrimLine(Line, RemoveTrailingSpaces);
      Result.Add(Line);
      PackedSize := PackedSize + Length(Line);
    end;
  finally
    TagsToRemove.Free;
  end;
end;

procedure PackToFile(const Code: TStrings; const Destination: string; RemoveComments, RemoveTrailingSpaces: boolean;
  Tags: TStrings; out OriginalSize, PackedSize: integer);
var
  OutputCode: TStrings;
begin
  OutputCode := Pack(Code, RemoveComments, RemoveTrailingSpaces, Tags, OriginalSize, PackedSize);
  OutputCode.SaveToFile(UTF8ToSys(Destination));
end;

procedure PackToArchive(const Code: TStrings; const ProgramName: string; RemoveComments, RemoveTrailingSpaces: boolean;
  Tags: TStrings; out OriginalSize, PackedSize: integer);
begin
  if not DirectoryExists(Settings.GetValue('kspfolder', '')) then
    raise Exception.Create('KSP folder couldn''t be found. Please go to Settings and make sure the value is correct.')
  else if ProgramName = '' then
    raise Exception.Create('You need to specify a destination name or check the "Same name" checkbox in the Pack Confirguration window.')
  else
    PackToFile(Code, ChangeFileExt(Settings.GetValue('kspfolder', '') + PathDelim + 'Plugins' + PathDelim +
      'PluginData' + PathDelim + 'Archive' + PathDelim + ProgramName, '.txt'),
      RemoveComments, RemoveTrailingSpaces, Tags, OriginalSize, PackedSize);
end;

procedure PackToClipboard(const Code: TStrings; RemoveComments, RemoveTrailingSpaces: boolean;
  Tags: TStrings; out OriginalSize, PackedSize: integer);
var Aux: string;
begin
  Aux := Pack(Code, RemoveComments, RemoveTrailingSpaces, Tags, OriginalSize, PackedSize).Text;
  Clipboard.AsText := Copy(Aux, 0, Length(Aux) - Length(LineEnding));
end;

procedure ProcessProgram(const FileName: string; var VarList: TVarInfoList; AlreadyProcessed: TStringList; const CallChain: string);
var Stream: TFileStream;
    Code: TStringList;
    ProgName: string;
    UsedPrograms: TUsedProgramsList;
    AuxVarList : TVariablesTable;
    Idx: Integer;
begin
  if AlreadyProcessed.IndexOf(FileName) >= 0 then exit;
  AlreadyProcessed.Add(FileName);
  Stream := nil;
  Code := TStringList.Create;
  UsedPrograms := TUsedProgramsList.Create;
  AuxVarList := TVariablesTable.Create;
  AuxVarList.Sorted := True;
  AuxVarList.OnCompare := @KeyCompare;
  try
    try
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      Code.LoadFromStream(Stream);
      FetchVariables(Code, AuxVarList, False);
      for Idx := 0 to AuxVarList.Count - 1 do
        VarList.Add(VarInfo(AuxVarList.Keys[Idx], ExtractFileNameOnly(FileName), CallChain));
      FetchUsedPrograms(Code, ExtractFileDir(FileName), UsedPrograms);
      UsedPrograms.PrepareIterator;
      while UsedPrograms.GetNextKey(ProgName) do
        ProcessProgram(UsedPrograms[ProgName].FullPath, VarList, AlreadyProcessed, CallChain + ' > ' + ExtractFileNameOnly(FileName));
    except
    end;
  finally
    if Assigned(Stream) then
      Stream.Free;
    Code.Free;
    UsedPrograms.Free;
  end;
end;


procedure RunDeepCollisionScan(Code: TStrings; const FileName: string; var TextToShow: string);
var UsedPrograms: TUsedProgramsList;
    ProgName: string;
    Variables: TVariablesTable;
    ExternalVars: TVarInfoList;
    AlreadyProcessed: TStringList;
    Idx, Aux: integer;

begin
  Variables := TVariablesTable.Create;
  Variables.Sorted := True;
  Variables.OnCompare := @KeyCompare;
  ExternalVars := TVarInfoList.Create;
  UsedPrograms := TUsedProgramsList.Create;
  AlreadyProcessed := TStringList.Create;
  AlreadyProcessed.Sorted := True;
  try
    FetchVariables(Code, Variables, True);
    FetchUsedPrograms(Code, ExtractFileDir(FileName), UsedPrograms);
    UsedPrograms.PrepareIterator;
    while UsedPrograms.GetNextKey(ProgName) do
      ProcessProgram(UsedPrograms[ProgName].FullPath, ExternalVars, AlreadyProcessed, ExtractFileNameOnly(FileName));

    for Idx := 0 to Variables.Count - 1 do
      begin
      Aux := ExternalVars.IndexOf(VarInfo(Variables.Keys[Idx], '', ''));
      if  Aux >= 0 then
        TextToShow := TextToShow + '"' + Variables.Keys[Idx] + '" in ' + ExternalVars.Items[Aux].ProgamName +
                      ' by calls: ' +  ExternalVars.Items[Aux].CallChain + LineEnding;
      end;

    //MessageDlg('Variable collision check', 'The following variables were found to collide with others:' + LineEnding + Results.Text, mtInformation, [mbOk], 0);

  finally
    Variables.Free;
    UsedPrograms.Free;
    ExternalVars.Free;
    AlreadyProcessed.Free;
  end;
end;


initialization

  CommentsRegex := TRegexpr.Create;
  CommentsRegex.Expression := '//.*';

  VariableDeclRegex := TRegexpr.Create;
  VariableDeclRegex.Expression := '(?i)(set)(.*)(to)(.*)(x)';

finalization
  CommentsRegex.Free;

end.
