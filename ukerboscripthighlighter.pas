unit uKerboscriptHighlighter;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, SynEditHighlighter, SynEditMiscClasses, Graphics, uEditorFile, Dialogs, uTokens, jsonConf,
  SynHighlighterAny{$ifdef DEBUG}, LazLogger {$endif};

type

  { TKerboscriptHighlighter }

  TKerboscriptHighlighter = class(TSynCustomHighlighter)
  private
    {(*}
    type TProcTableProc = procedure of object;
    {*)}
  private
    fEditor: TEditorFile;
    PosStart, PosEnd: integer; //Position in current line
    PosBack: Integer; //Used in processing a nummer with scientific notation, declared here for speed
    CurrentLineIndex: integer;
    CurrentLine: PChar;
    CurrentTokenType: TTokenType;
    KeywordType: TKeywordType;
    CommandType: TCommandType;
    SymbolType: TSymbolType;
    ProcTable: array[#0..#255] of TProcTableProc;

    // For Proc[X]
    CurStrLen: integer;
    AuxIdentPtr: PChar;
    CurToken: string;

    fFont: TFont;
    fExtraCharSpacing: integer;
    fTabWidth: integer;
    fOldStyleCursor: boolean;
    fAutocompleteInUppercase: boolean;
    fBracketMatch: TSynSelectedColor;

    fCommentAttr: TSynHighlighterAttributes;
    fFunctionAttr: TSynHighlighterAttributes;
    fIdentifierAttr: TSynHighlighterAttributes;
    fKeywordAttr: TSynHighlighterAttributes;
    fNumberAttr: TSynHighlighterAttributes;
    fSpaceAttr: TSynHighlighterAttributes;
    fStringAttr: TSynHighlighterAttributes;
    fSymbolAttr: TSynHighlighterAttributes;
    fSystemVariableAttr: TSynHighlighterAttributes;
    fUnknownAttr: TSynHighlighterAttributes;

    procedure CreateProcTable;

    procedure SymbolCharStep; inline;
    procedure ProcIdentifier;
    procedure ProcSymbol;
    procedure ProcSpace;
    procedure ProcUnknown;
    procedure ProcNumber;
    procedure ProcEOL;
    procedure ProcComment;
    procedure ProcString;
    // Compares the parameter with the current string using the IdentCharCompTable for fast case in-sensitive match.
    function StrComp(const TheString: string): boolean;
    procedure SetKW(KwdType: TKeywordType); inline;

    procedure ReadSectionFromJSON(const Data: TJSONConfig; const SectionName: string; Attributes: TSynHighlighterAttributes);
    procedure WriteSectionFromJSON(const Data: TJSONConfig; const SectionName: string; Attributes: TSynHighlighterAttributes);

    procedure ResetToBlankDefaults;
    procedure ResetToOldtimerDefaults;
    procedure ResetToSaturnDefaults;
    procedure ResetToClearDefault;

    procedure SetAttributes(var Destination: TSynHighlighterAttributes; const AttrName: string;
      Foreground, Background: TColor; FontStyle: TFontStyles);

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;
    procedure Next; override;
    function GetEol: boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenPos: integer; override;
    function GetTokenKind: integer; override;
    procedure ResetToDefaults(const SchemeName: string);
    procedure SetEditor(Editor: TEditorFile);

    property CommentAttr: TSynHighlighterAttributes read fCommentAttr write fCommentAttr;
    property FunctionAttr: TSynHighlighterAttributes read fFunctionAttr write fFunctionAttr;
    property IdentifierAttr: TSynHighlighterAttributes read fIdentifierAttr write fIdentifierAttr;
    property KeywordAttr: TSynHighlighterAttributes read fKeywordAttr write fKeywordAttr;
    property NumberAttr: TSynHighlighterAttributes read fNumberAttr write fNumberAttr;
    property SpaceAttr: TSynHighlighterAttributes read fSpaceAttr write fSpaceAttr;
    property StringAttr: TSynHighlighterAttributes read fStringAttr write fStringAttr;
    property SymbolAttr: TSynHighlighterAttributes read fSymbolAttr write fSymbolAttr;
    property SystemVariableAttr: TSynHighlighterAttributes read fSystemVariableAttr write fSystemVariableAttr;
    property ExtraCharSpacing: integer read fExtraCharSpacing write fExtraCharSpacing;
    property BracketMatchColor: TSynSelectedColor read fBracketMatch write fBracketMatch;
    property Font: TFont read fFont write fFont;
    property TabWidth: integer read fTabWidth write fTabWidth;
    property OldStyleCursor: boolean read fOldStyleCursor write fOldStyleCursor;
    property AutocompleteInUppercase: boolean read fAutocompleteInUppercase write fAutocompleteInUppercase;

    procedure LoadFromJSON(const Data: TJSONConfig; const SchemeName: string);
    procedure WriteToJSON(const Data: TJSONConfig; const SchemeName: string);
    procedure Assign(Source: TPersistent); override;

  end;


implementation

uses uObjects;

var
  IdentCharTable: array [#0..#255] of bytebool; //Table for quick checking if char is in identif char set.
  IdentCharCompTable: array [#0..#255] of smallint; //Table to quick compare two chars case insensitive.
  FirstIdentCharSet: set of char = ['a'..'z', 'A'..'Z'];
  IdentCharSet: set of char = ['a'..'z', 'A'..'Z', '_', '0'..'9'];
  SpaceCharTable: array[#0..#255] of bytebool;
  SpaceCharSet: set of char = [#1..#9, #11, #12, #14..#32];
  SymbolCharSet: set of char = ['.', ',', '(', ')', '+', '-', '*', '{', '}', ':', '^', '<', '>', '=', '!', '/', '#', '[', ']'];
  SingleCharSymbols: set of char = ['.', ',', '(', ')', '+', '-', '*', '{', '}', ':', '^', '#', '[', ']'];
  SymbolCharTable: array[#0..#255] of bytebool;
  SymbolTypeTable: array[#0..#255] of TSymbolType;
  NumberCharTable: array[#0..#255] of bytebool; //Table for quick checking if char is in number char set.
  NumberCharSet: set of char = ['0'..'9'];

{ TKerboscriptHighlighter }

constructor TKerboscriptHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ResetToDefaults('Saturn');
  CreateProcTable;
  fEditor := nil;
end;

procedure TKerboscriptHighlighter.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  CurrentLine := PChar(NewValue);
  CurrentLineIndex := LineNumber;
  if fEditor <> nil then
  begin
    //0.3
    fEditor.TokenLines[CurrentLineIndex].Clear;
    if LineNumber < fEditor.LineIndex_Syntax then
      fEditor.LineIndex_Syntax := LineNumber;
  end;
  PosEnd := 0;
  Next;
end;

procedure TKerboscriptHighlighter.Next;
begin
  PosStart := PosEnd;
  ProcTable[CurrentLine[PosEnd]];
  //0.3
  if fEditor <> nil then
    fEditor.TokenLines[CurrentLineIndex].Add(
      Token(Copy(CurrentLine, PosStart + 1, PosEnd - PosStart), CurrentTokenType, CommandType, KeywordType,
      SymbolType, TokenPoint(PosStart + 1, CurrentLineIndex))
      );
end;

function TKerboscriptHighlighter.GetEol: boolean;
begin
  Result := CurrentTokenType = ttEOL;
end;

procedure TKerboscriptHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := PosEnd - PosStart;
  TokenStart := CurrentLine + PosStart;
end;

function TKerboscriptHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case CurrentTokenType of
    ttCommand: Result := fKeywordAttr;
    ttComment: Result := fCommentAttr;
    ttFunction: Result := fFunctionAttr;
    ttIdentifier: Result := fIdentifierAttr;
    ttKeyword: Result := fKeywordAttr;
    ttInteger, ttReal: Result := fNumberAttr;
    ttString: Result := fStringAttr;
    ttSymbol: Result := fSymbolAttr;
    ttSystemVar: Result := fSystemVariableAttr;
    ttSpace: Result := fSpaceAttr;
    else
      Result := fUnknownAttr;
  end;
end;

function TKerboscriptHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttr;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttr;
    SYN_ATTR_KEYWORD: Result := fKeywordAttr;
    SYN_ATTR_STRING: Result := fStringAttr;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttr;
    else
      Result := nil;
  end;
end;

function TKerboscriptHighlighter.GetToken: string;
begin
  Result := copy(CurrentLine, PosStart + 1, PosEnd - PosStart);
end;

function TKerboscriptHighlighter.GetTokenPos: integer;
begin
  Result := PosStart;
end;

function TKerboscriptHighlighter.GetTokenKind: integer;
begin
  Result := Ord(CurrentTokenType);
end;

procedure TKerboscriptHighlighter.SetEditor(Editor: TEditorFile);
begin
  fEditor := Editor;
  fEditor.LineIndex_HL := High(fEditor.LineIndex_HL);
end;


procedure TKerboscriptHighlighter.CreateProcTable;
var
  Idx: char;
begin
  for Idx := #0 to #255 do
    if Idx in FirstIdentCharSet then
      ProcTable[Idx] := @ProcIdentifier
    else if Idx in SymbolCharSet then
      ProcTable[Idx] := @ProcSymbol
    else if Idx in SpaceCharSet then
      ProcTable[Idx] := @ProcSpace
    else if Idx in ['0'..'9'] then
      ProcTable[Idx] := @ProcNumber
    else if Idx = #0 then
      ProcTable[Idx] := @ProcEOL
    else if Idx = '"' then
      ProcTable[Idx] := @ProcString
    else
      ProcTable[Idx] := @ProcUnknown;
end;

procedure TKerboscriptHighlighter.SymbolCharStep;
begin
  while SymbolCharTable[CurrentLine[PosEnd]] do
    Inc(PosEnd);
  CurStrLen := PosEnd - PosStart;
  AuxIdentPtr := CurrentLine + PosStart;
end;

procedure TKerboscriptHighlighter.ProcIdentifier;
begin
  while IdentCharTable[CurrentLine[PosEnd]] do
    Inc(PosEnd);
  CurToken := Copy(CurrentLine, PosStart + 1, PosEnd - PosStart);
  if Commands.Contains(CurToken) then
  begin
    CommandType := Commands[CurToken];
    CurrentTokenType := ttCommand;
    if (CommandType = cStage) and (CurrentLine[PosEnd] = ':') then
      CurrentTokenType := ttSystemVar
    else if (CommandType = cList) and (CurrentLine[PosEnd] = '(') then
      CurrentTokenType := ttFunction { TODO : Are you sure? When could a command be a function too?}
  end else if Keywords.Contains(CurToken) then
  begin
    KeywordType := Keywords[CurToken];
    CurrentTokenType := ttKeyword;
  end
  else if ((CurrentTokenType <> ttSymbol) or (SymbolType <> sColon)) and ((KnownSystemVariables.IndexOf(CurToken) >= 0)) then
    CurrentTokenType := ttSystemVar
  else if ((CurrentTokenType <> ttSymbol) or (SymbolType <> sColon)) and ((KnownSystemFunctions.IndexOf(CurToken) >= 0)) then
    CurrentTokenType := ttFunction
  else
    CurrentTokenType := ttIdentifier;
end;


procedure TKerboscriptHighlighter.ProcSymbol;
begin
  CurrentTokenType := ttSymbol;
  if CurrentLine[PosEnd] in SingleCharSymbols then
  begin
    SymbolType := SymbolTypeTable[CurrentLine[PosEnd]];
    Inc(PosEnd);
    if SymbolType = sMinus then
    begin
      if (CurrentLine[PosEnd] = '.') and (NumberCharTable[CurrentLine[PosEnd + 1]]) then //Number in the form -.123
      begin
        Inc(PosEnd);
        ProcNumber;
      end
      else if NumberCharTable[CurrentLine[PosEnd]] then //Number in the form -123 or -123.123
        ProcNumber;
    end
    else if (SymbolType = sDot) and (NumberCharTable[CurrentLine[PosEnd]]) then //Number in the form .123
      ProcNumber;
  end
  else
  begin
    Inc(PosEnd);
    case CurrentLine[PosEnd - 1] of
      '/': if CurrentLine[PosEnd] = '/' then
          ProcComment
        else
          SymbolType := sSlash;
      '=':
      begin
        if CurrentLine[PosEnd] = '=' then
          Inc(PosEnd);
        SymbolType := sEqual;
      end;
      '<': if CurrentLine[PosEnd] = '=' then
        begin
          Inc(PosEnd);
          SymbolType := sSmallerOrEqualThan;
        end
        else
          SymbolType := sSmallerThan;
      '>': if CurrentLine[PosEnd] = '=' then
        begin
          Inc(PosEnd);
          SymbolType := sGreaterOrEqualThan;
        end
        else
          SymbolType := sGreaterThan;
      '!': if CurrentLine[PosEnd] = '=' then
        begin
          Inc(PosEnd);
          SymbolType := sNotEqual;
        end
        else
          CurrentTokenType := ttUnknown;
      else
        CurrentTokenType := ttUnknown;
    end;//case
  end;
end;

procedure TKerboscriptHighlighter.ProcSpace;
begin
  Inc(PosEnd);
  while SpaceCharTable[CurrentLine[PosEnd]] do
    Inc(PosEnd);
  CurrentTokenType := ttSpace;
end;

procedure TKerboscriptHighlighter.ProcUnknown;
begin
  Inc(PosEnd);
  while (CurrentLine[PosEnd] in [#128..#191]) or //continued utf8 subcode
    ((CurrentLine[PosEnd] <> #0) and (ProcTable[CurrentLine[PosEnd]] = @ProcUnknown)) do
    Inc(PosEnd);
  CurrentTokenType := ttUnknown;
end;

procedure TKerboscriptHighlighter.ProcNumber;
begin
  Inc(PosEnd);
  //Consume 0..9
  while NumberCharTable[CurrentLine[PosEnd]] do
    Inc(PosEnd);
  CurrentTokenType := ttInteger;
  //Consume dot and numbers if present
  if (CurrentLine[PosEnd] = '.') and (NumberCharTable[CurrentLine[PosEnd + 1]]) then
  begin
    Inc(PosEnd);
    CurrentTokenType := ttReal;
    //Consume 0..9
    while NumberCharTable[CurrentLine[PosEnd]] do
      Inc(PosEnd);
  end;
  //Consumes E+-0..9 if present
  if (IdentCharCompTable[CurrentLine[PosEnd]] = IdentCharCompTable['E']) then
  begin
    CurrentTokenType := ttReal;
    PosBack := PosEnd;
    Inc(PosEnd);
    if CurrentLine[PosEnd] in ['+', '-'] then
    begin
      Inc(PosEnd);
      if  NumberCharTable[CurrentLine[PosEnd]] then
        while NumberCharTable[CurrentLine[PosEnd]] do
          Inc(PosEnd)
      else
        PosEnd := PosBack; //Wrong number format, "E" will be considered an identifier and catched later
    end
    else
      PosEnd := PosBack; //Wrong number format, "E" will be considered an identifier and catched later
  end;
end;

procedure TKerboscriptHighlighter.ProcEOL;
begin
  CurrentTokenType := ttEOL;
end;

procedure TKerboscriptHighlighter.ProcComment;
begin
  //  Inc(PosEnd);
  while CurrentLine[PosEnd] <> #0 do
    Inc(PosEnd);
  CurrentTokenType := ttComment;
end;

procedure TKerboscriptHighlighter.ProcString;
begin
  Inc(PosEnd);
  while not (CurrentLine[PosEnd] in [#0, '"']) do
    Inc(PosEnd);
  if CurrentLine[PosEnd] = '"' then
    Inc(PosEnd);
  CurrentTokenType := ttString;
end;

// Compares the parameter with the current string using the IdentCharCompTable for fast case-insensitve match.
function TKerboscriptHighlighter.StrComp(const TheString: string): boolean;
var
  Idx: integer;
  Aux: PChar;
begin
  Aux := AuxIdentPtr;
  if Length(TheString) = CurStrLen then
  begin
    Result := True;
    for Idx := 1 to CurStrLen do
    begin
      if IdentCharCompTable[Aux^] <> IdentCharCompTable[TheString[Idx]] then
      begin
        Result := False;
        break;
      end;
      Inc(Aux);
    end;
  end
  else
    Result := False;
end;

procedure TKerboscriptHighlighter.SetKW(KwdType: TKeywordType); inline;
begin
  KeywordType := KwdType;
  CurrentTokenType := ttKeyword;
end;

procedure TKerboscriptHighlighter.LoadFromJSON(const Data: TJSONConfig; const SchemeName: string);
begin
  ResetToDefaults('Saturn');
  Data.OpenKey('highlighters/' + SchemeName, True);
  ReadSectionFromJSON(Data, 'comment', fCommentAttr);
  ReadSectionFromJSON(Data, 'function', fFunctionAttr);
  ReadSectionFromJSON(Data, 'identifier', fIdentifierAttr);
  ReadSectionFromJSON(Data, 'keyword', fKeywordAttr);
  ReadSectionFromJSON(Data, 'number', fNumberAttr);
  ReadSectionFromJSON(Data, 'space', fSpaceAttr);
  ReadSectionFromJSON(Data, 'string', fStringAttr);
  ReadSectionFromJSON(Data, 'symbol', fSymbolAttr);
  ReadSectionFromJSON(Data, 'systemvariable', fSystemVariableAttr);
  fBracketMatch.Foreground := Data.GetValue('backetmatch/foreground', fBracketMatch.Foreground);
  fBracketMatch.Background := Data.GetValue('backetmatch/foreground', fBracketMatch.Background);
  fBracketMatch.Style := TFontStyles(Data.GetValue('bracketmatch/style', longint(fBracketMatch.Style)));
  fFont.Name := Data.GetValue('font/name', fFont.Name);
  fFont.Size := Data.GetValue('font/size', fFont.Size);
  fFont.Quality := TFontQuality(Data.GetValue('font/quality', longint(fFont.Quality)));
  fExtraCharSpacing := Data.GetValue('font/extracharspacing', fExtraCharSpacing);
  fTabWidth := Data.GetValue('tabwidth', 2);
  Data.CloseKey;
end;

procedure TKerboscriptHighlighter.ReadSectionFromJSON(const Data: TJSONConfig; const SectionName: string;
  Attributes: TSynHighlighterAttributes);
begin
  Attributes.Foreground := Data.GetValue(SectionName + '/foreground', Attributes.Foreground);
  Attributes.Background := Data.GetValue(SectionName + '/background', Attributes.Background);
  Attributes.Style := TFontStyles(Data.GetValue(SectionName + '/style', longint(Attributes.Style)));
end;

procedure TKerboscriptHighlighter.WriteSectionFromJSON(const Data: TJSONConfig; const SectionName: string;
  Attributes: TSynHighlighterAttributes);
begin
  Data.SetValue(SectionName + '/foreground', Attributes.Foreground);
  Data.SetValue(SectionName + '/background', Attributes.Background);
  Data.SetValue(SectionName + '/style', longint(Attributes.Style));
end;

procedure TKerboscriptHighlighter.SetAttributes(var Destination: TSynHighlighterAttributes;
  const AttrName: string; Foreground, Background: TColor; FontStyle: TFontStyles);
begin
  Destination := TSynHighlighterAttributes.Create(AttrName);
  Destination.Background := Background;
  Destination.Foreground := Foreground;
  Destination.Style := FontStyle;
end;

procedure TKerboscriptHighlighter.WriteToJSON(const Data: TJSONConfig; const SchemeName: string);
begin
  Data.OpenKey('highlighters/' + SchemeName, True);
  WriteSectionFromJSON(Data, 'comment', fCommentAttr);
  WriteSectionFromJSON(Data, 'function', fFunctionAttr);
  WriteSectionFromJSON(Data, 'identifier', fIdentifierAttr);
  WriteSectionFromJSON(Data, 'keyword', fKeywordAttr);
  WriteSectionFromJSON(Data, 'number', fNumberAttr);
  WriteSectionFromJSON(Data, 'space', fSpaceAttr);
  WriteSectionFromJSON(Data, 'string', fStringAttr);
  WriteSectionFromJSON(Data, 'symbol', fSymbolAttr);
  WriteSectionFromJSON(Data, 'systemvariable', fSystemVariableAttr);
  Data.SetValue('backetmatch/foreground', fBracketMatch.Foreground);
  Data.SetValue('backetmatch/foreground', fBracketMatch.Background);
  Data.SetValue('bracketmatch/style', longint(fBracketMatch.Style));
  Data.SetValue('font/name', fFont.Name);
  Data.SetValue('font/size', fFont.Size);
  Data.SetValue('font/quality', longint(fFont.Quality));
  Data.SetValue('font/extracharspacing', fExtraCharSpacing);
  Data.SetValue('tabwidth', fTabWidth);
  Data.CloseKey;
end;

procedure TKerboscriptHighlighter.Assign(Source: TPersistent);
var
  SourceHighlighter: TKerboscriptHighlighter;
begin
  if Source is TKerboscriptHighlighter then
  begin
    SourceHighlighter := Source as TKerboscriptHighlighter;
    fFont.Assign(SourceHighlighter.fFont);
    fExtraCharSpacing := SourceHighlighter.fExtraCharSpacing;
    fBracketMatch.Assign(SourceHighlighter.fBracketMatch);
    fTabWidth := SourceHighlighter.fTabWidth;
    fOldStyleCursor := SourceHighlighter.fOldStyleCursor;
    fAutocompleteInUppercase := SourceHighlighter.fAutocompleteInUppercase;

    fCommentAttr.Assign(SourceHighlighter.fCommentAttr);
    fFunctionAttr.Assign(SourceHighlighter.fFunctionAttr);
    fIdentifierAttr.Assign(SourceHighlighter.fIdentifierAttr);
    fKeywordAttr.Assign(SourceHighlighter.fKeywordAttr);
    fNumberAttr.Assign(SourceHighlighter.fNumberAttr);
    fSpaceAttr.Assign(SourceHighlighter.fSpaceAttr);
    fStringAttr.Assign(SourceHighlighter.fStringAttr);
    fSymbolAttr.Assign(SourceHighlighter.fSymbolAttr);
    fSystemVariableAttr.Assign(SourceHighlighter.fSystemVariableAttr);
  end;
  inherited Assign(Source);
end;


procedure TKerboscriptHighlighter.ResetToDefaults(const SchemeName: string);
begin
  if SchemeName = 'Old-Timer' then
    ResetToOldtimerDefaults
  else if SchemeName = 'Clear' then
    ResetToClearDefault
  else if SchemeName = '' then
    ResetToBlankDefaults
  else
    ResetToSaturnDefaults;

  fFont := TFont.Create;
  {$ifdef Linux}
  fFont.Name := 'Monospace';
  {$endif}
  {$ifdef Windows}
  fFont.Name := 'Courier New';
  {$endif}
  {$ifdef Darwin}
  fFont.Name := 'Monaco';
  {$endif}
  fFont.Size := 10;
  fFont.Quality := fqDraft;

  fExtraCharSpacing := 0;
  fTabWidth := 2;
end;

procedure TKerboscriptHighlighter.ResetToBlankDefaults;
begin
  SetAttributes(fCommentAttr, 'comment', clYellow, clNone, []);
  SetAttributes(fFunctionAttr, 'function', clDefault, clNone, []);
  SetAttributes(fIdentifierAttr, 'identifier', clDefault, clNone, []);
  SetAttributes(fKeywordAttr, 'keyword', clDefault, clNone, []);
  SetAttributes(fNumberAttr, 'number', clDefault, clNone, []);
  SetAttributes(fSpaceAttr, 'space', clNone, clDefault, []);
  SetAttributes(fStringAttr, 'string', clDefault, clNone, []);
  SetAttributes(fSymbolAttr, 'symbol', clDefault, clNone, []);
  SetAttributes(fSystemVariableAttr, 'systemvariable', clDefault, clNone, []);

  fBracketMatch := TSynSelectedColor.Create;
  fBracketMatch.Foreground := clNone;
  fBracketMatch.Background := fSpaceAttr.Background;
  fBracketMatch.Style := [fsUnderline];

  fFont := TFont.Create;
  {$ifdef Linux}
  fFont.Name := 'Monospace';
  {$endif}
  {$ifdef Windows}
  fFont.Name := 'Courier New';
  {$endif}
  {$ifdef Darwin}
  fFont.Name := 'Monaco';
  {$endif}
  fFont.Size := 10;
  fFont.Quality := fqDraft;

  fExtraCharSpacing := 0;
  fTabWidth := 2;
  fOldStyleCursor := False;
end;

procedure TKerboscriptHighlighter.ResetToOldtimerDefaults;
begin
  SetAttributes(fCommentAttr, 'comment', 2534984, clNone, [fsBold]);
  SetAttributes(fFunctionAttr, 'function', 2534984, clNone, [fsBold]);
  SetAttributes(fIdentifierAttr, 'identifier', 2534984, clNone, [fsBold]);
  SetAttributes(fKeywordAttr, 'keyword', 2534984, clNone, [fsBold]);
  SetAttributes(fNumberAttr, 'number', 2534984, clNone, [fsBold]);
  SetAttributes(fSpaceAttr, 'space', clNone, 2960685, [fsBold]);
  SetAttributes(fStringAttr, 'string', 2534984, clNone, [fsBold]);
  SetAttributes(fSymbolAttr, 'symbol', 2534984, clNone, [fsBold]);
  SetAttributes(fSystemVariableAttr, 'systemvariable', 2534984, clNone, [fsBold]);
  SetAttributes(fUnknownAttr, 'unknown', clRed, clNone, [fsBold]);

  fBracketMatch := TSynSelectedColor.Create;
  fBracketMatch.Foreground := clNone;
  fBracketMatch.Background := clNone;
  fBracketMatch.Style := [];
  fOldStyleCursor := True;
end;

procedure TKerboscriptHighlighter.ResetToSaturnDefaults;
begin
  SetAttributes(fCommentAttr, 'comment', 10209435, clNone, [fsItalic]);
  SetAttributes(fFunctionAttr, 'function', 4554932, clNone, [fsBold]);
  SetAttributes(fIdentifierAttr, 'identifier', 12632256, clNone, []);
  SetAttributes(fKeywordAttr, 'keyword', 2399012, clNone, [fsBold]);
  SetAttributes(fNumberAttr, 'number', 16745215, clNone, []);
  SetAttributes(fSpaceAttr, 'space', clNone, 2960685, []);
  SetAttributes(fStringAttr, 'string', 4227327, clNone, []);
  SetAttributes(fSymbolAttr, 'symbol', 16744448, clNone, []);
  SetAttributes(fSystemVariableAttr, 'systemvariable', 10944511, clNone, [fsBold]);
  SetAttributes(fUnknownAttr, 'unknown', clRed, clNone, []);

  fBracketMatch := TSynSelectedColor.Create;
  fBracketMatch.Foreground := clNone;
  fBracketMatch.Background := clNone;
  fBracketMatch.Style := [fsUnderline];
  fOldStyleCursor := True;
end;

procedure TKerboscriptHighlighter.ResetToClearDefault;
begin
  SetAttributes(fCommentAttr, 'comment', 3187581, clNone, [fsItalic]);
  SetAttributes(fFunctionAttr, 'function', 3696530, clNone, [fsBold]);
  SetAttributes(fIdentifierAttr, 'identifier', 4276545, clNone, []);
  SetAttributes(fKeywordAttr, 'keyword', 8404992, clNone, [fsBold]);
  SetAttributes(fNumberAttr, 'number', 4868833, clNone, []);
  SetAttributes(fSpaceAttr, 'space', clNone, 16580350, []);
  SetAttributes(fStringAttr, 'string', 15361005, clNone, []);
  SetAttributes(fSymbolAttr, 'symbol', 11753728, clNone, []);
  SetAttributes(fSystemVariableAttr, 'systemvariable', 16744448, clNone, [fsBold]);
  SetAttributes(fUnknownAttr, 'unknown', clRed, clNone, []);

  fBracketMatch := TSynSelectedColor.Create;
  fBracketMatch.Foreground := clRed;
  fBracketMatch.Background := clNone;
  fBracketMatch.Style := [fsBold];
  fOldStyleCursor := False;
end;

var
  Idx, Aux: char;

initialization

  for Idx := #0 to #255 do
  begin
    IdentCharTable[Idx] := Idx in IdentCharSet;
    NumberCharTable[Idx] := Idx in NumberCharSet;
    SpaceCharTable[Idx] := Idx in SpaceCharSet;
    SymbolCharTable[Idx] := Idx in SymbolCharSet;

    Aux := UpCase(Idx);
    if Idx in ['A'..'Z', 'a'..'z'] then
      IdentCharCompTable[Idx] := Ord(Aux) - Ord('A')
    else
      IdentCharCompTable[Idx] := Ord(Aux);

    SymbolTypeTable[Idx] := sUnknown;
  end;

  SymbolTypeTable['.'] := sDot;
  SymbolTypeTable[','] := sComma;
  SymbolTypeTable['('] := sParOpen;
  SymbolTypeTable[')'] := sParClose;
  SymbolTypeTable['/'] := sSlash;
  SymbolTypeTable['+'] := sPlus;
  SymbolTypeTable['-'] := sMinus;
  SymbolTypeTable['*'] := sAsterisk;
  SymbolTypeTable['{'] := sBraceOpen;
  SymbolTypeTable['}'] := sBraceClose;
  SymbolTypeTable[':'] := sColon;
  SymbolTypeTable['^'] := sCircumflex;
  SymbolTypeTable['<'] := sSmallerThan;
  SymbolTypeTable['>'] := sGreaterThan;
  SymbolTypeTable['='] := sEqual;
  SymbolTypeTable['#'] := sHash;
  SymbolTypeTable['['] := sSquareOpen;
  SymbolTypeTable[']'] := sSquareClose;

end.
