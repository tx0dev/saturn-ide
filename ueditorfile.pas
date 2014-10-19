unit uEditorFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, SynEdit, Controls, uObjects, uExternalVariables, FileUtil, uTokens,SynEditMarkup,
  uMarkup;

type

  { TMySynEdit }

  TMySynEdit = class(TSynEdit)
  private
    function GetMarkupManager: TSynEditMarkupManager;
  public
    property MarkupMgr: TSynEditMarkupManager read GetMarkupManager;

  end;


type

  { TEditorFile }

  TEditorFile = class(TTabSheet)
  private
    Markup: TMarkup;
    fFileName: string;
    fUntitled: boolean;
    fEditor: TMySynEdit;
    fTimestamp: longint;
    fDontAskReloadForTimestamp: longint;
    CurrentlyUpdating: boolean;
    procedure SetFileName(AValue: string);
    procedure OnChangeEvent(Sender: TObject);
    function GetTimestamp: longint;
  public
    //First line the syntax checker processed that didn't have any errors.
    LineIndex_Syntax: integer;
    //First line the highlighter parsed
    LineIndex_HL: integer;
    UntitledIndex: integer;
    IsFromArchive: boolean;
    KnownVariables: TVariablesTable;
    UsedPrograms: TUsedProgramsList;
    TokenLines: TEditorLines;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read fFileName write SetFileName;
    property Untitled: boolean read fUntitled;
    procedure SaveFile;
    procedure SaveFileAs(const SaveAs: string);
    procedure ReadFile(const FileToRead: string);
    procedure Reload;
    procedure OnShowEvent(Sender: TObject);
    procedure OnEnterEvent(Sender: TObject);
    procedure OnShowHintEvent(Sender: TObject; HintInfo: PHintInfo);
    procedure OnClickEvent(Sender: TObject);
    property Editor: TMySynEdit read fEditor;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    function ExternallyChanged: boolean;
    function AskToReload: boolean;
    procedure SetDontAskReloadForTimestamp;
    procedure ClearMarkups;
    procedure AddMarkup(const StartPoint, EndPoint: TTokenPoint; const Message: string);
    procedure StartSyntaxCheck;
    procedure FinishSyntaxCheck;
  end;

implementation

{ TEditorFile }

uses uKerboscriptHighlighter, uMain, Graphics, SynGutterLineNumber, SynEditKeyCmds, uVariableFetcher,
  Dialogs, Menus, SynEditPointClasses, uSettings, SynEditTypes, Forms;

{ TMySynEdit }

function TMySynEdit.GetMarkupManager: TSynEditMarkupManager;
begin
  Result := TSynEditMarkupManager(GetMarkupMgr);
end;

procedure TEditorFile.SetFileName(AValue: string);
begin
  fFileName := AValue;
  Caption := ExtractFileName(AValue);
  OnShow(self);
end;

procedure TEditorFile.OnChangeEvent(Sender: TObject);
begin
  Caption := '*' + ExtractFileName(FileName);
end;

function TEditorFile.GetTimestamp: longint;
var
  TheFile: longint;
begin
  TheFile := -1;
  try
    TheFile := FileOpen(Utf8ToSys(FileName), fmOpenRead + fmShareDenyNone);
    Result := FileGetDate(TheFile);
  finally
    if TheFile > 0 then
      FileClose(TheFile);
  end;
end;

constructor TEditorFile.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CurrentlyUpdating := True;
  OnShow := @OnShowEvent;
  parent := frmMain.pcMDIFaker;
  fEditor := TMySynEdit.Create(self);
  fEditor.Parent := self;
  fEditor.Align := alClient;
  fEditor.Highlighter := frmMain.Highlighter;
  TKerboscriptHighlighter(fEditor.Highlighter).SetEditor(Self);
  fEditor.BracketMatchColor.Assign(TKerboscriptHighlighter(fEditor.Highlighter).BracketMatchColor);
  fEditor.Options := fEditor.Options + [eoKeepCaretX, eoEnhanceHomeKey, eoBracketHighlight,
    eoGroupUndo, eoTabsToSpaces, eoTrimTrailingSpaces, eoHideRightMargin] - [eoScrollPastEol, eoSmartTabs];
  fEditor.Color := TKerboscriptHighlighter(fEditor.Highlighter).SpaceAttr.Background;

  fEditor.Font.Assign(frmMain.seHidden.Font);
  fEditor.ExtraCharSpacing := frmMain.seHidden.ExtraCharSpacing;
  fEditor.OnStatusChange := @frmMain.seHiddenStatusChange;
  fEditor.OnReplaceText := @frmMain.seHiddenReplaceText;
  fEditor.OnChange := @OnChangeEvent;
  fEditor.OnEnter := @OnEnterEvent;
  fEditor.OnShowHint := @OnShowHintEvent;
  fEditor.OnClick := @OnClickEvent;
  fEditor.ShowHint := True;
  TSynGutterLineNumber(fEditor.Gutter.Parts.Part[1]).ZeroStart := True;
  fEditor.TabWidth := Settings.GetValue('editor/tabwidth', 2);
  fEditor.PopupMenu := frmMain.popmEditor;

  fEditor.Keystrokes.Delete(fEditor.Keystrokes.FindCommand(ecInsertLine)); //Ctrl-N
  fEditor.Keystrokes.Delete(Editor.Keystrokes.FindCommand(ecPageBottom)); //Ctrl-PgDwn
  fEditor.Keystrokes.Delete(Editor.Keystrokes.FindCommand(ecPageTop)); //Ctrl-PgUp


  fEditor.BookMarkOptions.BookmarkImages := frmMain.imlstBookmarks;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker0)].Command := ecToggleMarker0;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker1)].Command := ecToggleMarker1;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker2)].Command := ecToggleMarker2;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker3)].Command := ecToggleMarker3;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker4)].Command := ecToggleMarker4;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker5)].Command := ecToggleMarker5;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker6)].Command := ecToggleMarker6;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker7)].Command := ecToggleMarker7;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker8)].Command := ecToggleMarker8;
  fEditor.Keystrokes[fEditor.Keystrokes.FindCommand(ecSetMarker9)].Command := ecToggleMarker9;

  if Settings.GetValue('editor/oldcursor', True) then
    fEditor.InsertCaret := ctHorizontalLine;
  fUntitled := True;
  fEditor.Modified := False;
  KnownVariables := TVariablesTable.Create;
  KnownVariables.Sorted := True;
  KnownVariables.OnCompare := @KeyCompare;

  UsedPrograms := TUsedProgramsList.Create;

  frmMain.SynCompletion1.AddEditor(fEditor);
  CurrentlyUpdating := False;

  TokenLines := TEditorLines.Create;

  Markup := TMarkup.Create(fEditor);
  Markup.MarkupInfo.FrameEdges := sfeBottom;
  Markup.MarkupInfo.FrameStyle := slsWaved;
  Markup.MarkupInfo.FrameColor := clred;
  Markup.MarkupInfo.Foreground := clnone;
  Markup.MarkupInfo.Background := clnone;
  fEditor.MarkupMgr.AddMarkUp(Markup);

  LineIndex_Syntax := 0;

end;

destructor TEditorFile.Destroy;
begin
  CurrentlyUpdating := True;
  KnownVariables.Free;
  UsedPrograms.Free;
  inherited Destroy;
end;

procedure TEditorFile.SaveFile;
begin
  SaveFileAs(FileName);
end;

procedure TEditorFile.SaveFileAs(const SaveAs: string);
begin
  CurrentlyUpdating := True;
  try
    fEditor.Lines.SaveToFile(SaveAs);
    FileName := SaveAs;
    fTimestamp := GetTimestamp;
    fEditor.Modified := False;
    fUntitled := False;
    UntitledIndex := -1;
    Caption := ExtractFileName(FileName);
  finally
    CurrentlyUpdating := False;
  end;
end;

procedure TEditorFile.ReadFile(const FileToRead: string);
begin
  CurrentlyUpdating := True;
  try
    FileName := FileToRead;
    fEditor.Modified := False;
    fUntitled := False;
    UntitledIndex := -1;
    fEditor.Lines.LoadFromFile(FileToRead);
    fTimestamp := GetTimestamp;
    fDontAskReloadForTimestamp := fTimestamp;

  finally
    CurrentlyUpdating := False;
  end;
end;

procedure TEditorFile.Reload;
begin
  if not Untitled then
    ReadFile(FileName);
end;

procedure TEditorFile.OnShowEvent(Sender: TObject);
begin
  frmMain.StatusBar1.Panels[3].Text := FileName;
  if Editor <> nil then
    frmMain.StatusBar1.Panels[1].Text := 'Length: ' + IntToStr(Length(Editor.Text) -
      (Length(sLineBreak) * Editor.Lines.Count));
end;

procedure TEditorFile.OnEnterEvent(Sender: TObject);
begin
  if AskToReload and frmMain.AskToReloadFile then
    Reload;
end;

procedure TEditorFile.OnShowHintEvent(Sender: TObject; HintInfo: PHintInfo);
var Aux: TPoint;
begin
  Aux := Editor.PixelsToLogicalPos(hintinfo^.CursorPos);
  {if Editor.GetWordAtRowCol(Aux) = '' then
     hintinfo^.HintStr := ''
  else}
  begin
    Dec(Aux.Y);
    hintinfo^.HintStr := Markup.GetTextForMarkup(Aux);
  end;
end;

procedure TEditorFile.OnClickEvent(Sender: TObject);
begin
  {Editor.Hint := Markup.GetTextForMarkup(Point(Editor.CaretX, Editor.CaretY - 1));
  Application.ActivateHint(Mouse.CursorPos);}
end;

procedure TEditorFile.Cut;
begin
  fEditor.CutToClipboard;
end;

procedure TEditorFile.Copy;
begin
  fEditor.CopyToClipboard;
end;

procedure TEditorFile.Paste;
begin
  fEditor.PasteFromClipboard;
end;

function TEditorFile.ExternallyChanged: boolean;
begin
  Result := not Untitled and (fTimestamp <> GetTimestamp);
end;

function TEditorFile.AskToReload: boolean;
begin
  Result := not CurrentlyUpdating and ExternallyChanged and (GetTimestamp <> fDontAskReloadForTimestamp);
end;

procedure TEditorFile.SetDontAskReloadForTimestamp;
begin
  fDontAskReloadForTimestamp := GetTimestamp;
end;

procedure TEditorFile.ClearMarkups;
begin
  Markup.ClearMarks;
end;

procedure TEditorFile.AddMarkup(const StartPoint, EndPoint: TTokenPoint; const Message: string);
begin
  Markup.AddMark(StartPoint, EndPoint, Message);
end;


procedure TEditorFile.StartSyntaxCheck;
begin
  Markup.BeginUpdates;
end;

procedure TEditorFile.FinishSyntaxCheck;
begin
  if Markup.FinishedUpdates then
     Editor.Refresh;
end;

end.
