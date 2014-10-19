unit uSyntaxCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTokens, typinfo, Dialogs, uEditorFile, uMarkup{$ifdef DEBUG}, LazLogger {$endif};

procedure SyntaxCheck(Editor: TEditorFile; Errors: TMarkupList); //Errors assigned when running from the program packer.

implementation

uses Forms, uMain, uSyntaxCheckingErrors;

var Token: TToken;
    PrevTokenEndPos: TTokenPoint;
    Tokens: TEditorLines;
    CommandProcTable: array[TCommandType] of procedure;
    LastOneWasABlock: boolean; //used as a work around to the fact the the dot is optional after a {}.

    //Used en block proc.
    CurrentEditor: TEditorFile;
    ErrorsFound: TMarkupList;

procedure _Function; forward;
procedure IdentifierOrSysVar; forward;
procedure Block; forward;
procedure Statement; forward;
procedure Expression; forward;
procedure NextToken; forward; inline;

function TokenIs(TknType: TTokenType): boolean; forward;

procedure SyntaxCheck(Editor: TEditorFile; Errors: TMarkupList);
begin
  Tokens := Editor.TokenLines;
  Tokens.PrepareIterator;
  NextToken;

  CurrentEditor := Editor;
  ErrorsFound := Errors;
  while not TokenIs(ttNone) do
  begin
    Application.ProcessMessages;
    try
      Statement;
    except
      on e: ESyntaxError do
        begin
        if Errors <> nil then
          Errors.Add(MarkupRecord(PrevTokenEndPos, TokenPoint(Token.Position.X + Length(Token.Text), Token.Position.Y), e.Message))
        else
          Editor.AddMarkup(PrevTokenEndPos, TokenPoint(Token.Position.X + Length(Token.Text), Token.Position.Y), e.Message);
        Tokens.AdvanceLine;
        NextToken;
        end;
    end;
  end;
end;

procedure NextToken; inline;
var GotToken: boolean;
begin
  Application.ProcessMessages;
  if frmMain.OkToClose then //We're shutting down, stop any activity
    raise EStopException.Create('');
  repeat
    PrevTokenEndPos.Y := Token.Position.Y;
    PrevTokenEndPos.X := Token.Position.X + Length(Token.Text);
    GotToken := Tokens.GetNextToken(Token);
  until not GotToken or not ((Token.TokenType in  [ttSpace, ttComment, ttEOL]));
  //while (Tokens.GetNextToken(Token)) and ((Token.TokenType in  [ttSpace, ttComment, ttEOL])) do;
end;

function TokenIs(TknType: TTokenType): boolean; inline;
begin
  Result := Token.TokenType = TknType;
end;

function TokenIsAny(TknSet: TTokenTypeSet): boolean; inline;
begin
  Result := Token.TokenType in TknSet;
end;

function TokenIsCmd(Cmd: TCommandType): boolean; inline;
begin
  Result := TokenIs(ttCommand) and (Token.CommandType = Cmd);
end;

function TokenIsAnyCmd(Cmds: TCommandTypeSet): boolean; inline;
begin
  Result := TokenIs(ttCommand) and (Token.CommandType in Cmds);
end;

function TokenIsKwd(Kwd: TKeywordType): boolean; inline;
begin
  Result := TokenIs(ttKeyword) and (Token.KeywordType = Kwd);
end;

function TokenIsAnyKwd(KwdSet: TKeywordTypeSet): boolean; inline;
begin
  Result := TokenIs(ttKeyword) and (Token.KeywordType in KwdSet);
end;

function TokenIsSym(Sym: TSymbolType): boolean; inline;
begin
  Result := TokenIs(ttSymbol) and (Token.SymbolType = Sym);
end;

function TokenIsAnySym(SymSet: TSymbolTypeSet): boolean; inline;
begin
  Result := TokenIs(ttSymbol) and (Token.SymbolType in SymSet);
end;

function TokenIsAny(TknSet: TTokenTypeSet; KwdSet: TKeywordTypeSet; SymSet: TSymbolTypeSet): boolean; inline;
begin
  Result := TokenIsAny(TknSet) or
            (TokenIs(ttKeyword) and TokenIsAnyKwd(KwdSet)) or
            (TokenIs(ttSymbol) and TokenIsAnySym(SymSet));
end;

// V Matchers ----------------------------------------------------------------------------------------------------------

procedure MatchTkn(const What: TTokenType);
begin
  if TokenIs(What) then
    NextToken
  else
    Error_Expected([What], Token);
end;

procedure MatchAnyTkn(const What: TTokenTypeSet);
begin
  if Token.TokenType in What then
    NextToken
  else
    Error_Expected(What, Token);
end;

procedure MatchCmd(const Cmd: TCommandType);
begin
  if TokenIsCmd(Cmd) then
    NextToken
  else
    Error_Expected([ttCommand], Token);
end;

procedure MatchAnyCmd(const Cmds: TCommandTypeSet);
begin
  if TokenIs(ttCommand) and TokenIsAnyCmd(Cmds) then
    NextToken
  else
    Error_Expected(Cmds, Token);
end;

procedure MatchKwd(const Kwd: TKeywordType);
begin
  if TokenIsKwd(Kwd) then
    NextToken
  else
    Error_Expected(Kwd, Token);
end;

procedure MatchAnyKwd(const Kwds: TKeywordTypeSet);
begin
  if TokenIs(ttKeyword) and TokenIsAnyKwd(Kwds) then
    NextToken
  else
    Error_Expected(Kwds, Token);
end;

procedure MatchSym(const What: TSymbolType);
begin
  if TokenIsSym(What) then
    NextToken
  else
    Error_Expected(What, Token);
end;

procedure MatchAny(const Tkns: TTokenTypeSet; const Kwds: TKeywordTypeSet; const Cmds: TCommandTypeSet; const Syms: TSymbolTypeSet);
begin
  if TokenIsAny(Tkns) or TokenIsAnyKwd(Kwds) or TokenIsAnyCmd(Cmds) or TokenIsAnySym(Syms) then
    NextToken
  else
    Error_Expected('[IDENTIFIER, "NEXTNODE"]', Token);
end;

// ^ Matchers ----------------------------------------------------------------------------------------------------------


// V main parser procs -------------------------------------------------------------------------------------------------

procedure Factor;
begin
  //NextToken;
  if TokenIsAny([ttInteger, ttReal, ttIdentifier, ttString, ttFunction, ttSystemVar], [kNot, kTrue, kFalse], [sPlus, sMinus, sParOpen]) then
  begin
    if TokenIsAny([ttIdentifier, ttSystemVar]) then
      IdentifierOrSysVar
    else if TokenIsSym(sParOpen) then
    begin
      MatchSym(sParOpen);
      Expression;
      MatchSym(sParClose);
    end
    else if TokenIs(ttFunction) then
      _Function
    else
      NextToken;
  end
  else
    Error_Expected('value', Token);
end;

procedure Term;
begin
  Factor;
  while TokenIsSym(sCircumflex) do
  begin
    NextToken;
    Factor;
  end;
end;

procedure Multiplication;
begin
  Term;
  while TokenIsSym(sAsterisk) do
  begin
    NextToken;
    Term;
  end;
end;

procedure Division;
begin
  Multiplication;
  while TokenIsSym(sSlash) do
  begin
    NextToken;
    Multiplication;
  end;
end;

procedure MathExpr;
begin
  Division;
  while TokenIsAnySym([sPlus, sMinus]) do
  begin
    NextToken;
    Division;
  end;
end;

procedure Relation;
begin
  MathExpr;
  while TokenIsAnySym([sEqual, sGreaterThan, sGreaterOrEqualThan, sSmallerThan, sSmallerOrEqualThan]) do
  begin
    NextToken;
    MathExpr;
  end;
end;

procedure AndExpression;
begin
  Relation;
  while TokenIsKwd(kAnd) do
  begin
    NextToken;
    Relation;
  end;
end;

procedure Expression;
begin
  AndExpression;
  while TokenIsKwd(kOr) do
  begin
    NextToken;
    AndExpression;
  end;
end;

procedure ArrayWithSqrBrackets;
begin
  MatchSym(sSquareOpen);
  Expression;
  MatchSym(sSquareClose);
end;

procedure ArrayWithHash;
begin
  NextToken;
  MatchAnyTkn([ttIdentifier, ttInteger]);
  while TokenIsSym(sHash) do
  begin
    NextToken;
    MatchAnyTkn([ttIdentifier, ttInteger]);
  end;
end;

procedure IdentifierOrSysVar;
begin
  MatchAnyTkn([ttIdentifier, ttSystemVar]);
  if TokenIsSym(sSquareOpen) then
    ArrayWithSqrBrackets
  else if TokenIsSym(sHash) then
    ArrayWithHash;
  while TokenIsSym(sColon) do
  begin
    NextToken;
    MatchTkn(ttIdentifier);
  end;
end;

procedure Statement;
begin
  case Token.TokenType of
    //ttFunction: _Function;
    ttCommand: CommandProcTable[Token.CommandType];
    //ttKeyword: KeywordProcTable[Token.KeywordType];
    ttEOL: NextToken;
  else
    Error_Expected(ttCommand, Token);
  end;
  if not LastOneWasABlock or TokenIsSym(sDot) then
    MatchSym(sDot);
  LastOneWasABlock := False;
end;

procedure ArgList;
begin
  Expression;
  while TokenIsSym(sComma) do
  begin
    NextToken;
    Expression;
  end;
end;

procedure _Function;
begin
  MatchTkn(ttFunction);
  MatchSym(sParOpen);
  ArgList;
  MatchSym(sParClose);
end;

procedure Block;
begin
  MatchSym(sBraceOpen);
  while not TokenIsSym(sBraceClose) and not TokenIs(ttNone) do
    try
      Statement;
    except
       on e: ESyntaxError do
         begin
         if ErrorsFound <> nil then
            ErrorsFound.Add(MarkupRecord(PrevTokenEndPos, TokenPoint(Token.Position.X + Length(Token.Text), Token.Position.Y), e.Message))
         //ErrorsFound.Add(MarkupRecord(Token.Position, TokenPoint(Token.Position.X + Length(Token.Text), Token.Position.Y), e.Message))
          else
            CurrentEditor.AddMarkup(PrevTokenEndPos, TokenPoint(Token.Position.X + Length(Token.Text), Token.Position.Y), e.Message);
            //CurrentEditor.AddMarkup(Token.Position, TokenPoint(Token.Position.X + Length(Token.Text), Token.Position.Y), e.Message);
         Tokens.AdvanceLine;
         NextToken;
         end;
     end;
  MatchSym(sBraceClose);
  LastOneWasABlock := True;
end;

// ^ main parser procs -------------------------------------------------------------------------------------------------

// Command proccessing -------------------------------------------------------------------------------------------------

procedure Proc_Add;
begin
  NextToken;
  Expression;
end;

procedure Proc_Batch;
begin
  NextToken;
end;

procedure Proc_Break;
begin
  NextToken;
end;

procedure Proc_Clearscreen;
begin
  NextToken;
end;

procedure Proc_Copy;
begin
  NextToken;
  MatchAnyKwd([kFrom, kTo]);
  MatchAnyTkn([ttIdentifier, ttInteger]);
end;

procedure Proc_Declare;
begin
  NextToken;
  if TokenIsKwd(kParameter) then
    MatchKwd(kParameter);
  MatchTkn(ttIdentifier);
  while TokenIsSym(sComma) do
  begin
    NextToken;
    MatchTkn(ttIdentifier);
  end;
end;

procedure Proc_Delete;
begin
  NextToken;
  MatchTkn(ttIdentifier);
  if TokenIsKwd(kFrom) then
  begin
    NextToken;
    MatchAnyTkn([ttIdentifier, ttInteger]);
  end;
end;

procedure Proc_Deploy;
begin
  NextToken;
end;

procedure Proc_Edit;
begin
  NextToken;
  MatchTkn(ttIdentifier);
end;

procedure Proc_For;
begin
  NextToken;
  MatchTkn(ttIdentifier);
  MatchKwd(kIn);
  IdentifierOrSysVar;
  Block;
end;

procedure Proc_If;
begin
  NextToken;
  Expression;
  Block;
  if TokenIsKwd(kElse) then
  begin
    NextToken;
    Block;
  end;
end;

procedure Proc_List;
begin
  NextToken;
  if not TokenIsSym(sDot) then
  begin
    MatchAnyKwd([kBodies, kTargets, kResources, kParts, kEngines, kSensors, kElements]);
    MatchKwd(kIn);
    IdentifierOrSysVar;
  end;
end;

procedure Proc_Lock;
begin
  NextToken;
  MatchAnyTkn([ttIdentifier, ttSystemVar]);
  if TokenIsKwd(kTo) then
  begin
    NextToken;
    Expression;
  end;
end;

procedure Proc_Log;
begin
  NextToken;
  Expression;
  MatchKwd(kTo);
  IdentifierOrSysVar;
end;

procedure Proc_On;
begin
  NextToken;
  IdentifierOrSysVar;
  Block;
end;

procedure Proc_Print;
begin
  NextToken;
  Expression;
  if TokenIsKwd(kAt) then
  begin
    NextToken;
    MatchSym(sParOpen);
    MatchTkn(ttInteger);
    MatchSym(sComma);
    MatchTkn(ttInteger);
    MatchSym(sParClose);
  end;
end;

procedure Proc_RCS;
begin
  NextToken;
  MatchAny([], [kOff], [cOn], []);
end;

procedure Proc_Reboot;
begin
  NextToken;
end;

procedure Proc_Remove;
begin
  NextToken;
  if not TokenIsSym(sDot) then
    MatchAny([ttIdentifier], [kNextnode], [], []);
end;

procedure Proc_Rename;
begin
  NextToken;
  if TokenIsAnyKwd([kVolume, kFile]) then
    NextToken;
  MatchAnyTkn([ttIdentifier, ttInteger]);
  MatchKwd(kTo);
  MatchTkn(ttIdentifier);
end;

procedure Proc_Run;
begin
  NextToken;
  MatchTkn(ttIdentifier);
  if TokenIsSym(sParOpen) then
  begin
    NextToken;
    ArgList;
    MatchSym(sParClose);
  end;
  if TokenIsCmd(cOn) then
  begin
    NextToken;
    MatchAnyTkn([ttIdentifier, ttInteger]);
  end;
end;

procedure Proc_Set;
begin
  NextToken;
  IdentifierOrSysVar;
  MatchKwd(kTo);
  Expression;
end;

procedure Proc_Shutdown;
begin
  NextToken;
end;

procedure Proc_Stage;
begin
  NextToken;
end;

procedure Proc_Switch;
begin
  NextToken;
  MatchKwd(kTo);
  MatchAnyTkn([ttIdentifier, ttInteger]);
end;

procedure Proc_Toggle;
begin
  NextToken;
  MatchAny([ttIdentifier, ttSystemVar], [kAG], [], []);
end;

procedure Proc_Unlock;
begin
  NextToken;
  MatchAny([ttIdentifier], [kAll], [], []);
end;

procedure Proc_Unset;
begin
  NextToken;
  MatchAny([ttIdentifier], [kAll], [], []);
end;

procedure Proc_Until;
begin
  NextToken;
  Expression;
  Block;
end;

procedure Proc_Wait;
begin
  NextToken;
  if TokenIsCmd(cUntil) then
    MatchCmd(cUntil);
  Expression;
end;

procedure Proc_When;
begin
  NextToken;
  Expression;
  MatchKwd(kThen);
  Block;
end;

procedure Proc_Unknown;
begin
  raise ESyntaxError.Create('Unknown token in stream. This is a bug. Please be a pal and contact Saturn''s author. :)');
end;

initialization
  CommandProcTable[cAdd] := @Proc_Add;
  CommandProcTable[cBatch] := @Proc_Batch;
  CommandProcTable[cBreak] := @Proc_Break;
  CommandProcTable[cClearscreen] := @Proc_Clearscreen;
  CommandProcTable[cCopy] := @Proc_Copy;
  CommandProcTable[cDeclare] := @Proc_Declare;
  CommandProcTable[cDelete] := @Proc_Delete;
  CommandProcTable[cDeploy] := @Proc_Deploy;
  CommandProcTable[cEdit] := @Proc_Edit;
  CommandProcTable[cFor] := @Proc_For;
  CommandProcTable[cIf] := @Proc_If;
  CommandProcTable[cList] := @Proc_List;
  CommandProcTable[cLock] := @Proc_Lock;
  CommandProcTable[cLog] := @Proc_Log;
  CommandProcTable[cOn] := @Proc_On;
  CommandProcTable[cPrint] := @Proc_Print;
  CommandProcTable[cRCS] := @Proc_RCS;
  CommandProcTable[cReboot] := @Proc_Reboot;
  CommandProcTable[cRemove] := @Proc_Remove;
  CommandProcTable[cRename] := @Proc_Rename;
  CommandProcTable[cRun] := @Proc_Run;
  CommandProcTable[cSet] := @Proc_Set;
  CommandProcTable[cShutdown] := @Proc_Shutdown;
  CommandProcTable[cStage] := @Proc_Stage;
  CommandProcTable[cSwitch] := @Proc_Switch;
  CommandProcTable[cToggle] := @Proc_Toggle;
  CommandProcTable[cUnlock] := @Proc_Unlock;
  CommandProcTable[cUnset] := @Proc_Unset;
  CommandProcTable[cUntil] := @Proc_Until;
  CommandProcTable[cWait] := @Proc_Wait;
  CommandProcTable[cWhen] := @Proc_When;
  CommandProcTable[cUnknown] := @Proc_Unknown;

{
  KeywordProcTable[kAbort] := @Proc_SimpleKwd;
  KeywordProcTable[kAdd] := @Proc_Add;
  KeywordProcTable[kAG] := @Proc_SimpleKwd;
  KeywordProcTable[kAll] := @Proc_SimpleKwd;
  KeywordProcTable[kAt] := @Proc_SimpleKwd;
  KeywordProcTable[kBodies] := @Proc_SimpleKwd;
  KeywordProcTable[kBrakes] := @Proc_Brakes;
  KeywordProcTable[kBreak] := @NextToken;
  KeywordProcTable[kClearscreen] := @NextToken;
  KeywordProcTable[kCopy] := @Proc_Copy;
  KeywordProcTable[kDeclare] := @Proc_Declare;
  KeywordProcTable[kDelete] := @Proc_Delete;
  KeywordProcTable[kEdit] := @Proc_Edit;
  KeywordProcTable[kEngines] := @Proc_SimpleKwd;
  KeywordProcTable[kFile] := @Proc_SimpleKwd;
  KeywordProcTable[kFrom] := @Proc_SimpleKwd;
  KeywordProcTable[kGear] := @Proc_Gear;
  KeywordProcTable[kHeading] := @Proc_Heading;
  KeywordProcTable[kIf] := @Proc_If;
  KeywordProcTable[kLights] := @Proc_Lights;
  KeywordProcTable[kList] := @Proc_List;
  KeywordProcTable[kLock] := @Proc_Lock;
  KeywordProcTable[kLog] := @Proc_Log;
  KeywordProcTable[kOff] := @Proc_SimpleKwd;
  KeywordProcTable[kOn] := @Proc_On;
  KeywordProcTable[kParameter] := @Proc_SimpleKwd;
  KeywordProcTable[kParameters] := @Proc_SimpleKwd;
  KeywordProcTable[kParts] := @Proc_SimpleKwd;
  KeywordProcTable[kPrint] := @Proc_Print;
  KeywordProcTable[kRCS] := @Proc_RCS;
  KeywordProcTable[kReboot] := @NextToken;//@Proc_Reboot;
  KeywordProcTable[kRemove] := @Proc_Remove;
  KeywordProcTable[kRename] := @Proc_Rename;
  KeywordProcTable[kResources] := @Proc_SimpleKwd;
  KeywordProcTable[kRun] := @Proc_Run;
  KeywordProcTable[kSAS] := @Proc_SAS;
  KeywordProcTable[kSensors] := @Proc_SimpleKwd;
  KeywordProcTable[kSet] := @Proc_Set;
  KeywordProcTable[kShutdown] := @NextToken;
  KeywordProcTable[kStage] := @NextToken;
  KeywordProcTable[kSwitch] := @Proc_Switch;
  KeywordProcTable[kTargets] := @Proc_SimpleKwd;
  KeywordProcTable[kThen] := @Proc_SimpleKwd;
  KeywordProcTable[kTo] := @Proc_SimpleKwd;
  KeywordProcTable[kToggle] := @Proc_Toggle;
  KeywordProcTable[kUnlock] := @Proc_Unlock;
  KeywordProcTable[kUntil] := @Proc_Until;
  KeywordProcTable[kVolume] := @Proc_SimpleKwd;
  KeywordProcTable[kVolumes] := @Proc_SimpleKwd;
  KeywordProcTable[kWait] := @Proc_Wait;
  KeywordProcTable[kWhen] := @Proc_When;
  KeywordProcTable[kUnknown] := @Proc_Unknown;}
end.
