unit uTokens;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

{ TODO : Add kLegs}

uses
  Classes, SysUtils, uContainers;

type
  TTokenType = (ttCommand, ttComment, ttFunction, ttIdentifier, ttInteger, ttKeyword, ttReal, ttString, ttSymbol, ttSystemVar,
    ttSpace, ttUnknown, ttEOL, ttNone {<-- special token returned by TEditorLines when no more tokens remain});


type TCommandType = (cAdd, cBatch, cBreak, cClearscreen, cCopy, cDeclare, cDelete, cDeploy, cEdit, cFor, cIf, cList, cLock,
       cLog, cOn, cPrint, cRCS, cReboot, cRemove, cRename, cRun, cSet, cShutdown, cStage, cSwitch, cToggle,
       cUnlock, cUnset, cUntil, cWait, cWhen, cUnknown);

type TKeywordType = (kAbort, kAG, kAll, kAnd, kAt, kBodies, kBrakes, kBy, kChutes, kElements, kElse, kEngines, kFile,
       kFiles, kFrom, kGear, kHeading, kIn, kLights, kList, kNextnode, kNot, kOff, kOr, kParameter, kParameters, { TODO : does "parameters" really exist? }
       kParts, kResources, kSAS, kSensors, kTargets, kThen, kTo, kVolume, kVolumes, kTrue, kFalse, kUnknown);


type TSymbolType = (sDot, sComma, sParOpen, sParClose, sSlash, sPlus, sMinus, sAsterisk, sBraceOpen, sBraceClose, sColon,
                   sCircumflex, sNotEqual, sEqual, sGreaterThan, sSmallerThan,
                   sGreaterOrEqualThan, sSmallerOrEqualThan, sHash, sSquareOpen, sSquareClose,
                   sUnknown);

type TCommandTypeSet = set of TCommandType;
     TKeywordTypeSet = set of TKeywordType;
     TTokenTypeSet = set of TTokenType;
     TSymbolTypeSet = set of TSymbolType;

type

{ TTokenPoint }

 TTokenPoint = record
  X, Y: integer;
  class operator =(const p1, p2: TTokenPoint): boolean;
end;

type

{ TToken }

 TToken = record
  Text: string;
  TokenType: TTokenType;
  CommandType: TCommandType;
  KeywordType: TKeywordType;
  SymbolType: TSymbolType;
  Position: TTokenPoint;
  class operator = (t1, t2: TToken): boolean;
end;

type TEditorTokenLine = specialize TGenericList<TToken>;

type

  { TEditorLines }

  TEditorLines = class
  private
    type TEditorTokenLineList = specialize TGenericList<TEditorTokenLine>;
  private
    FLines: TEditorTokenLineList;
    IterLineIndex: integer;
    function GetLine(Index: integer): TEditorTokenLine;
    procedure SetLine(Index: integer; AValue: TEditorTokenLine);
    procedure GrowTo(Index: Integer);
  public
    constructor Create;
    property Lines[Index: integer]: TEditorTokenLine read GetLine write SetLine; default;
    procedure PrepareIterator;
    procedure PrepareIterator(FromLine: integer); overload;
    function GetNextToken(out Token: TToken): boolean;
    procedure AdvanceLine;
    function GetCurrentLine: integer;
  end;


function Token(const Text: string; TokenType: TTokenType; CommandType: TCommandType; KeywordType: TKeywordType;
               SymbolType: TSymbolType;  const Position: TTokenPoint): TToken;
function TokenPoint(X, Y: integer): TTokenPoint;
function GetKeywordTypeName(KwdType: TKeywordType): string;
function GetCommandTypeName(CmdType: TCommandType): string;

const TokenTypeNames : array[TTokenType] of string = ('COMMAND', 'COMMENT', 'FUNCTION', 'IDENTIFIER', 'INTEGER', 'KEYWORD', 'REAL',
        'STRING', 'SYMBOL', 'SYSTEM VARIABLE', 'SPACE', 'UNKNOWN', 'EOL', 'END OF FILE');

const SymbolTypeNames : array[TSymbolType] of string = ('.', ',', '(', ')', '/', '+', '-', '*', '{', '}', ':', '^', '!=',
                                                        '=', '>', '<', '>=', '<=', '#', '[', ']', '???');


implementation

uses typinfo;

function Token(const Text: string; TokenType: TTokenType; CommandType: TCommandType; KeywordType: TKeywordType;
               SymbolType: TSymbolType;  const Position: TTokenPoint): TToken;
begin
  Result.Text := Text;
  Result.TokenType := TokenType;
  Result.CommandType := CommandType;
  Result.KeywordType := KeywordType;
  Result.SymbolType := SymbolType;
  Result.Position := Position;
end;

function TokenPoint(X, Y: integer): TTokenPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function GetKeywordTypeName(KwdType: TKeywordType): string;
begin
  Result:= UpperCase(GetEnumName(TypeInfo(TKeywordType), ord(KwdType)));
  Delete(Result, 1, 1);
end;


function GetCommandTypeName(CmdType: TCommandType): string;
begin
  Result:= UpperCase(GetEnumName(TypeInfo(TCommandType), ord(CmdType)));
  Delete(Result, 1, 1);
end;

{ TTokenPoint }

class operator TTokenPoint. = (const p1, p2: TTokenPoint): boolean;
begin
  Result := p1 = p2;
end;

{ TToken }

class operator TToken. = (t1, t2: TToken): boolean;
begin
  Result := (t1.TokenType = t2.TokenType) and (t1.Text = t2.Text);
end;

{ TEditorLines }

function TEditorLines.GetLine(Index: integer): TEditorTokenLine;
begin
  if Index > FLines.ItemCount - 1 then
  begin
    GrowTo(Index);
    Result := FLines[Index];
  end
  else if Index <= FLines.ItemCount then
  begin
    Result := FLines[Index];
  end;
end;

procedure TEditorLines.SetLine(Index: integer; AValue: TEditorTokenLine);
begin
  if Index > FLines.ItemCount - 1 then
  begin
    GrowTo(Index);
    FLines[Index] := AValue;
  end
  else if Index <= FLines.ItemCount then
  begin
    FLines[Index] :=  AValue;
  end;
end;

procedure TEditorLines.GrowTo(Index: Integer);
var Idx, Start: Integer;
begin
  Start := FLines.ItemCount;
  for Idx := Start to Index do
    FLines.Add(TEditorTokenLine.Create(10, 10));
end;

constructor TEditorLines.Create;
begin
  FLines := TEditorTokenLineList.Create(100, 100);
end;

procedure TEditorLines.PrepareIterator;
begin
  PrepareIterator(0);
end;

procedure TEditorLines.PrepareIterator(FromLine: integer);
begin
  IterLineIndex := FromLine;
  if FLines.ItemCount > FromLine then
    FLines[FromLine].PrepareIterator;
end;

function TEditorLines.GetNextToken(out Token: TToken): boolean;
begin
  if IterLineIndex >= FLines.ItemCount then
  begin
    Token.TokenType := ttNone;
    Result := False;
  end
  else
  begin
    if FLines[IterLineIndex].GetNextItem(Token) then
      Result := True
    else
    begin
      Inc(IterLineIndex);
      if IterLineIndex < FLines.ItemCount then
        FLines[IterLineIndex].PrepareIterator;
      Result := GetNextToken(Token);
    end;
  end;
end;

procedure TEditorLines.AdvanceLine;
begin
  PrepareIterator(IterLineIndex + 1);
end;

function TEditorLines.GetCurrentLine: integer;
begin
  Result := IterLineIndex;
end;



end.
