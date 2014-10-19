unit uSyntaxCheckingErrors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTokens;

type
  ESyntaxError = class(Exception);
  EStopException = class(Exception); //Used as a way to stop the checker... ugly as hell, I know.



type TSyntaxErrorType = (eExpected,  //Expected X but found Y.
                         eUnknown // ttUnknown token found.
);


procedure Error_Expected(Expected: TTokenTypeSet; const Found: TToken);
procedure Error_Expected(Expected: TKeywordTypeSet; const Found: TToken);
procedure Error_Expected(Expected: TCommandTypeSet; const Found: TToken);
procedure Error_Expected(Expected: TSymbolTypeSet; const Found: TToken);

procedure Error_Expected(Expected: TTokenType; const Found: TToken);
procedure Error_Expected(Expected: TKeywordType; const Found: TToken);
procedure Error_Expected(Expected: TCommandType; const Found: TToken);
procedure Error_Expected(Expected: TSymbolType; const Found: TToken);

procedure Error_Expected(const Expected: string; const Found: TToken);



implementation

function GetTokenName(const Token: TToken): string;
begin
  case Token.TokenType of
    ttSymbol: Result := '"' + SymbolTypeNames[Token.SymbolType] + '"';
    ttKeyword: Result := GetKeywordTypeName(Token.KeywordType);
    ttCommand: Result := GetCommandTypeName(Token.CommandType);
    else
      Result := TokenTypeNames[Token.TokenType];
  end;
end;

procedure Error_Expected(Expected: TTokenTypeSet; const Found: TToken);
var
  Str: string;
  TT: TTokenType;
begin
  Str := '';
  for TT in Expected do
    Str := Str + TokenTypeNames[TT] + ', ';
  SetLength(Str, Length(Str) - 2);
  raise ESyntaxError.Create('Error. Expected [' + Str + '], got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TKeywordTypeSet; const Found: TToken);
var
  Str: string;
  KT: TKeywordType;
begin
  Str := '';
  for KT in Expected do
    Str := Str + GetKeywordTypeName(KT) + ', ';
  SetLength(Str, Length(Str) - 2);
  raise ESyntaxError.Create('Error. Expected [' + Str + '], got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TCommandTypeSet; const Found: TToken);
var
  Str: string;
  CT: TCommandType;
begin
  Str := '';
  for CT in Expected do
    Str := Str + GetCommandTypeName(CT) + ', ';
  SetLength(Str, Length(Str) - 2);
  raise ESyntaxError.Create('Error. Expected [' + Str + '], got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TSymbolTypeSet; const Found: TToken);
var
  Str: string;
  ST: TSymbolType;
begin
  Str := '';
  for ST in Expected do
    Str := Str + '"' +SymbolTypeNames[ST] + '", ';
  SetLength(Str, Length(Str) - 2);
  raise ESyntaxError.Create('Error. Expected [' + Str + '], got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TTokenType; const Found: TToken);
begin
  raise ESyntaxError.Create('Error. Expected ' + TokenTypeNames[Expected] + ', got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TKeywordType; const Found: TToken);
begin
  raise ESyntaxError.Create('Error. Expected ' + GetKeywordTypeName(Expected) + ', got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TCommandType; const Found: TToken);
begin
  raise ESyntaxError.Create('Error. Expected ' + GetCommandTypeName(Expected) + ', got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(Expected: TSymbolType; const Found: TToken);
begin
  raise ESyntaxError.Create('Error. Expected "' + SymbolTypeNames[Expected] + '", got ' + GetTokenName(Found) + ' instead.');
end;

procedure Error_Expected(const Expected: string; const Found: TToken);
begin
  raise ESyntaxError.Create('Error. Expected ' + Expected + ', got ' + GetTokenName(Found) + ' instead.');
end;

end.
