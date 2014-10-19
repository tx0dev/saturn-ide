unit uVariableFetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uObjects;

procedure FetchVariables(const SourceCode: TStrings; out VariablesTable: TVariablesTable; ClearList: boolean);

function GetListOfIdentifiers(const Line: string; StartFrom: integer; DiscardFirstIdent: boolean): TStringList;

implementation

uses regexpr, Forms;

var
  ListVarDeclRegex, ForVarDeclRegex, VarDeclRegex: TRegexpr;
  ParamDeclRegex: TRegexpr;


procedure GetParams(const Text: string; out List: TStringList);
begin
  {%H-}List.Clear;
  List.Delimiter := ',';
  List.DelimitedText := Text;
end;


procedure FetchVariables(const SourceCode: TStrings; out VariablesTable: TVariablesTable; ClearList: boolean);
var
  Line, Param: string;
  Identifiers: TStringList;
  Aux: TIdentifierClass;
begin
  Identifiers := TStringList.Create;
  if ClearList then
    {%H-}VariablesTable.Clear;
  for Line in SourceCode do
  begin
    if VarDeclRegex.Exec(Line) then
    begin
      repeat
        Application.ProcessMessages;
        try
          Identifiers := GetListOfIdentifiers(VarDeclRegex.Match[7], length(VarDeclRegex.Match[7]), False);
          VariablesTable[VarDeclRegex.Match[3]] := GetIdentifierType(Identifiers, VariablesTable, aux);
        except
        end;
      until not VarDeclRegex.ExecNext;
    end
    else if ForVarDeclRegex.Exec(Line) then
    begin
      VariablesTable[ForVarDeclRegex.Match[3]] := otUnknown;
    end
    else if ListVarDeclRegex.Exec(Line) then
    begin
      VariablesTable[ListVarDeclRegex.Match[7]] := otList;
    end
    else if ParamDeclRegex.Exec(Line) then
    begin
      GetParams(ParamDeclRegex.Match[2], Identifiers);
      for Param in Identifiers do
        VariablesTable[Param] := otUnknown;
    end;
  end;
end;


function GetListOfIdentifiers(const Line: string; StartFrom: integer; DiscardFirstIdent: boolean): TStringList;

  procedure SearchMatchingPar(const Line: string; var Idx: integer);
  var
    ParCounter: integer;
  begin
    ParCounter := 0;
    repeat
      if Line[Idx] = ')' then
        Inc(ParCounter)
      else if Line[Idx] = '(' then
        Dec(ParCounter);
      Dec(Idx);
    until (Idx = 0) or (ParCounter = 0);
  end;

  procedure ConsumeSeps(const Line: string; var Idx: integer);
  begin
    if (Idx > 0) and (Line[Idx] = ':') then
      Dec(Idx);
    if (Idx > 0) and (Line[Idx] = ')') then
      SearchMatchingPar(Line, Idx);
  end;

var
  Idx: integer;
  Identif: string;

begin
  Result := TStringList.Create;
  Idx := StartFrom;

  if (Idx > 0) and ((Line[Idx] = ':') or (not DiscardFirstIdent and (Line[Idx] = ')'))) then
    ConsumeSeps(Line, Idx)
  else if (Idx > 0) and (Line[Idx] in IdentChars) and DiscardFirstIdent then
  begin
    //consume last word
    while (Idx >= 1) and (Line[Idx] in IdentChars) do
      Dec(Idx);
    ConsumeSeps(Line, Idx);
  end;

  Identif := '';
  if Idx >= 1 then
  begin
    while (Idx >= 1) and (Line[Idx] in (IdentChars + [':'])) do
    begin
      if Line[Idx] in IdentChars then
        Identif := Line[Idx] + Identif
      else
      begin
        Result.Insert(0, Identif);
        Identif := '';
      end;
      Dec(Idx);
    end;
    if Identif <> '' then
      Result.Insert(0, Identif);
  end;
end;


initialization
  VarDeclRegex := TRegexpr.Create;
  VarDeclRegex.Expression := '(?i)(set)(\s+)([a-z0-9_]+)(\s+)(to)(\s+)([a-z0-9_:",\(\)]+)';

  ForVarDeclRegex := TRegexpr.Create;
  ForVarDeclRegex.Expression := '(?i)(for)(\s+)([a-z0-9_]+)(\s+)(in)(\s+)([a-z0-9_:",\(\)]+)';

  ListVarDeclRegex := TRegexpr.Create;
  ListVarDeclRegex.Expression := '(?i)(list)(\s+)([a-z0-9_]+)(\s+)(in)(\s+)([a-z0-9_:",\(\)]+)';

  ParamDeclRegex := TRegexpr.Create;
  ParamDeclRegex.Expression := '(?i)(declare parameter\s+)(.+)(\.)';


finalization
  VarDeclRegex.Free;
  ForVarDeclRegex.Free;
  ListVarDeclRegex.Free;
  ParamDeclRegex.Free;

end.
