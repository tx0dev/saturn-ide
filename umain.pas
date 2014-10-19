unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, SynPluginSyncroEdit,
  SynEditHighlighter, SynCompletion, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, uEditorFile, LCLType, ExtCtrls, SynEditTypes, uObjects,
  SynEditKeyCmds, SynEditMarkupHighAll, uExternalVariables,
  uKerboscriptHighlighter;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ImageList1: TImageList;
    emiCut: TMenuItem;
    emiCopy: TMenuItem;
    imlstBookmarks: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    emiPaste: TMenuItem;
    emiOpenProgramAtCursor: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    miToolsPanel: TMenuItem;
    miReadme: TMenuItem;
    miOpenProgramAtCursor: TMenuItem;
    miUndindent: TMenuItem;
    miIndent: TMenuItem;
    miSelectColumn: TMenuItem;
    miSelectNormal: TMenuItem;
    MenuItem5: TMenuItem;
    miComments: TMenuItem;
    miGoToLine: TMenuItem;
    miSource: TMenuItem;
    miSelectAll: TMenuItem;
    emiSelectAll: TMenuItem;
    MenuItem2: TMenuItem;
    miRedo: TMenuItem;
    miUndo: TMenuItem;
    miExportBBCode: TMenuItem;
    miExportHTML: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miSearchReplace: TMenuItem;
    MenuItem6: TMenuItem;
    miPackCurrent: TMenuItem;
    miPackConfigs: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miSaveToArchive: TMenuItem;
    miOpenFromArchive: TMenuItem;
    miAbout: TMenuItem;
    miPaste: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miCloseOthers: TMenuItem;
    miCloseAll: TMenuItem;
    miClose: TMenuItem;
    miExit: TMenuItem;
    miSeparator: TMenuItem;
    miSaveAs: TMenuItem;
    miPacker: TMenuItem;
    miSave: TMenuItem;
    miOpen: TMenuItem;
    miSettings: TMenuItem;
    mmMainMenu: TMainMenu;
    miTools: TMenuItem;
    miFile: TMenuItem;
    miFileNew: TMenuItem;
    OpenDialog1: TOpenDialog;
    pcTools: TPageControl;
    pcMDIFaker: TPageControl;
    popmFileMRU: TPopupMenu;
    popmPacker: TPopupMenu;
    popmArchiveMRU: TPopupMenu;
    popmEditor: TPopupMenu;
    ReplaceDialog1: TReplaceDialog;
    SaveDialog1: TSaveDialog;
    seHidden: TSynEdit;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    ToolButton6: TToolButton;
    tvVariables: TTreeView;
    tsVariables: TTabSheet;
    tmrBackgroundWork: TTimer;
    ToolBar1: TToolBar;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    tbSaveAs: TToolButton;
    ToolButton1: TToolButton;
    tbClose: TToolButton;
    tbCloseAll: TToolButton;
    tbCloseOthers: TToolButton;
    ToolButton2: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    tbPacker: TToolButton;
    ToolButton3: TToolButton;
    tbOpenFromArchive: TToolButton;
    tbSaveToArchive: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    btUndo: TToolButton;
    tbRedo: TToolButton;
    procedure emiCopyClick(Sender: TObject);
    procedure emiCutClick(Sender: TObject);
    procedure emiOpenProgramAtCursorClick(Sender: TObject);
    procedure emiPasteClick(Sender: TObject);
    procedure emiSelectAllClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miCommentsClick(Sender: TObject);
    procedure miGoToLineClick(Sender: TObject);
    procedure miIndentClick(Sender: TObject);
    procedure miOpenFromArchiveClick(Sender: TObject);
    procedure miOpenProgramAtCursorClick(Sender: TObject);
    procedure miReadmeClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miSearchReplaceClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miCloseAllClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miCloseOthersClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miExportBBCodeClick(Sender: TObject);
    procedure miExportHTMLClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miNextClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miPackConfigsClick(Sender: TObject);
    procedure miPackConfigsClickToPack(Sender: TObject);
    procedure miPackCurrentClick(Sender: TObject);
    procedure miPackerClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miPreviousClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSaveToArchiveClick(Sender: TObject);
    procedure miSelectColumnClick(Sender: TObject);
    procedure miSelectNormalClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    procedure miToolsPanelClick(Sender: TObject);
    procedure miUndindentClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miFileMRUClick(Sender: TObject);
    procedure miArchiveMRUClick(Sender: TObject);
    procedure pcMDIFakerCloseTabClicked(Sender: TObject);
    procedure seHiddenEnter(Sender: TObject);
    procedure seHiddenReplaceText(Sender: TObject; const {%H-}ASearch, {%H-}AReplace: string;
    {%H-}Line, {%H-}Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure seHiddenStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function SynCompletion1PaintItem(const {%H-}AKey: string; ACanvas: TCanvas; X, Y: integer;
    {%H-}Selected: boolean; Index: integer): boolean;
    procedure SynCompletion1SearchPosition(var APosition: integer);
    procedure tmrBackgroundWorkTimer(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure tvVariablesCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; {%H-}State: TCustomDrawState;
      var DefaultDraw: boolean);
    procedure tvVariablesMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: integer);
  private
    UntitledPool: set of byte;
    CheckSyntax: boolean;
    function GetNextUntitled: byte;
    procedure RemoveUntitled(Number: integer);
    procedure SetTitle;
    function OfferSaveFile(Editor: TEditorFile): integer;
    procedure LoadPackConfigs;
    procedure AddToMRU(const TheFile: string; const Section: string);
    procedure AddFileToMRU(const TheFile: string);
    procedure AddFileToArchiveMRU(const TheFile: string);
    procedure OpenFile(const Thefile: string; IsFromArchive: boolean);
    procedure CloseFile(Editor: TEditorFile);
    function GetActiveEditor: TEditorFile;

    //Adds all knwon identifers to the autocompletion list
    procedure AddAllKownIdentifiers;
    procedure AddToAutocomplete(ToAdd: string; KeepCase: boolean);
    procedure OpenFilesFromCmdLine;
    procedure SaveListOpenFiles;
    procedure OpenPreviousFiles;
    function FileIndex(const TheFile: string): integer;
    //Checks variable collisions
    procedure AddVariablesFromUsedPrograms;
    procedure UpdateVariablesList(Editor: TEditorFile);
    function VariableCollides(const VarName: string; UsedPrograms: TUsedProgramsList; out InProgram: string): boolean;
  public
    OkToClose: boolean;
    Title: string;
    SynMarkup: TSynEditMarkupHighlightAllCaret;
    Highlighter: TKerboscriptHighlighter;
    procedure FindReplace(const ToFind, ReplaceWith: string; Options: TSynSearchOptions);
    procedure UpdateStatusBar(Changes: TSynStatusChanges; IgnoreChanges: boolean);
    function AskToReloadFile: boolean;
    property ActiveEditor: TEditorFile read GetActiveEditor;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

uses uSettings, ufSettings, vinfo, uPacker, uPackConfigurations, uAbout,
  uExporter, uSearchReplace, Clipbrd, uVariableFetcher, StrUtils, SynEditPointClasses, lclintf, uSyntaxCheck;

function TfrmMain.GetNextUntitled: byte;
var
  idx: byte;
begin
  for idx := 0 to 255 do
    if not (idx in untitledPool) then
    begin
      Result := idx;
      break;
    end;
end;

procedure TfrmMain.RemoveUntitled(Number: integer);
begin
  untitledPool := untitledPool - [Number];
end;

procedure TfrmMain.SetTitle;
begin
  try
    uAbout.VersionInfo := TVersionInfo.Create;
    VersionInfo.Load(HINSTANCE);
    Title := 'Saturn kOS IDE v' + IntToStr(VersionInfo.FixedInfo.FileVersion[0]) + '.' +
      IntToStr(VersionInfo.FixedInfo.FileVersion[1]);
  except
    Title := 'Saturn kOS IDE';
  end;
end;

procedure TfrmMain.LoadPackConfigs;
var
  NewItem: TMenuItem;
  Configs: TStringList;
  Config: string;
  Idx: integer;
  CheckedTitle: string;
begin
  //Remove all items except the separator and Pack config items
  for Idx := 0 to popmPacker.Items.Count - 3 do
  begin
    if popmPacker.Items[0].Checked then
      CheckedTitle := popmPacker.Items[0].Caption;
    popmPacker.Items[0].Free;
  end;

  Configs := TStringList.Create;
  Settings.EnumSubKeys('packerconfigs', Configs);
  Configs.Sort;

  for Config in Configs do
  begin
    NewItem := TMenuItem.Create(popmPacker);
    NewItem.Caption := Config;
    NewItem.OnClick := @miPackConfigsClickToPack;
    NewItem.AutoCheck := True;
    NewItem.RadioItem := True;
    NewItem.GroupIndex := 1;
    popmPacker.Items.Add(NewItem);
  end;
  NewItem := popmPacker.Items.Find(CheckedTitle);
  if Assigned(NewItem) then
    NewItem.Checked := True;

  //Re-order to keep separator and PackConfig at bottom
  popmPacker.Items[0].MenuIndex := popmPacker.Items.Count - 1;
  popmPacker.Items[0].MenuIndex := popmPacker.Items.Count - 1;
end;

procedure TfrmMain.emiPasteClick(Sender: TObject);
begin
  ActiveEditor.Editor.PasteFromClipboard;
end;

procedure TfrmMain.AddToMRU(const TheFile: string; const Section: string);
var
  Item: TMenuItem;
  Idx: integer;
  MRUMenu: TPopupMenu;
begin
  if Section = 'filemru/' then
    MRUMenu := popmFileMRU
  else
    MRUMenu := popmArchiveMRU;
  Item := MRUMenu.Items.Find(TheFile);
  if Item <> nil then
    Item.MenuIndex := 0
  else
  begin
    Item := TMenuItem.Create(MRUMenu);
    Item.Caption := TheFile;
    Item.OnClick := @miFileMRUClick;
    MRUMenu.Items.Add(Item);
    if MRUMenu.Items.Count > 15 then
      for Idx := MRUMenu.Items.Count - 1 downto 14 do
        MRUMenu.Items[Idx].Free;
  end;
  for Idx := 0 to MRUMenu.Items.Count - 1 do
    Settings.SetValue(Section + IntToStr(Idx), MRUMenu.Items[Idx].Caption);
end;

procedure TfrmMain.AddFileToMRU(const TheFile: string);
begin
  AddToMRU(TheFile, 'filemru/');
end;

procedure TfrmMain.AddFileToArchiveMRU(const TheFile: string);
begin
  AddToMRU(TheFile, 'archivemru/');
end;

procedure TfrmMain.OpenFile(const TheFile: string; IsFromArchive: boolean);
var
  NewTab: TEditorFile;
begin
  NewTab := nil;
  try
    if FileIndex(TheFile) >= 0 then
      pcMDIFaker.ActivePageIndex := FileIndex(TheFile)
    else
    begin
      tmrBackgroundWork.Enabled := False;
      if (ActiveEditor <> nil) and ActiveEditor.Untitled and not ActiveEditor.Editor.Modified then
        NewTab := ActiveEditor
      else
        NewTab := TEditorFile.Create(pcMDIFaker);
      NewTab.ReadFile(TheFile);

      pcMDIFaker.ActivePage := NewTab;
      NewTab.IsFromArchive := IsFromArchive;
      UpdateStatusBar([], True);
      tmrBackgroundWork.Enabled := True;
    end;
    if ActiveEditor <> nil then
      ActiveEditor.Editor.SetFocus;

  except
    on e: Exception do
    begin
      MessageDlg('Saturn', E.Message, mtError, [mbOK], 0);
      if NewTab <> nil then
        NewTab.Free;
      if ActiveEditor = nil then
        miFileNewClick(miFileNew);
    end;
  end;
end;

procedure TfrmMain.CloseFile(Editor: TEditorFile);
begin
  tmrBackgroundWork.Enabled := False;
  if not Editor.Untitled then
    if Editor.IsFromArchive then
      AddFileToArchiveMRU(Editor.FileName)
    else
      AddFileToMRU(Editor.FileName);

  Editor.Free;
  if ActiveEditor = nil then
    miFileNewClick(miFileNew);
  tmrBackgroundWork.Enabled := True;
end;

function TfrmMain.OfferSaveFile(Editor: TEditorFile): integer;
begin
  try
    Result := MessageDlg('Saturn', 'Save file ' + Editor.FileName + '?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if Result = mrYes then
    begin
      if Editor.Untitled then
      begin
        if SaveDialog1.Execute then
          Editor.SaveFileAs(SaveDialog1.FileName)
        else
          Result := mrCancel;
      end
      else
        Editor.SaveFile;
    end;

  except
    on e: Exception do
      MessageDlg('Saturn', e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.UpdateStatusBar(Changes: TSynStatusChanges; IgnoreChanges: boolean);
begin
  StatusBar1.Panels[0].Text := '   ' + IntToStr(ActiveEditor.Editor.CaretY - 1) + ':' +
    IntToStr(ActiveEditor.Editor.CaretX);
  StatusBar1.Panels[1].Text := 'Length: ' + IntToStr(Length(ActiveEditor.Editor.Text) -
    (Length(sLineBreak) * ActiveEditor.Editor.Lines.Count));
  if not IgnoreChanges and not (scInsertMode in Changes) then
    StatusBar1.Panels[2].Text := 'INS'
  else if not IgnoreChanges then
    StatusBar1.Panels[2].Text := 'DEL';
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  tmrBackgroundWork.Enabled := False;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  tmrBackgroundWork.Enabled := False;
  SaveListOpenFiles;
  miCloseAllClick(miCloseAll);
  if Settings.GetValue('window/remember', True) then
  begin
    Settings.SetValue('window/x', Left);
    Settings.SetValue('window/y', Top);
    Settings.SetValue('window/height', Height);
    Settings.SetValue('window/width', Width);
    Settings.SetValue('window/ismaximized', WindowState = wsMaximized);
  end;
  Settings.SetValue('showtoolspanel', miToolsPanel.Checked);
  Settings.SetValue('splitterleft', Splitter1.GetSplitterPosition);
  CanClose := OkToClose;
  tmrBackgroundWork.Enabled := not CanClose;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
{var
  Idx: integer;}
begin
  UntitledPool := [];

  Application.HintHidePause := 10000;

  Highlighter := TKerboscriptHighlighter.Create(Self);

  {for Idx := 0 to KnownSystemVariables.Count - 1 do
    saHighlighter.Constants.Add(UpperCase(KnownSystemVariables.Keys[Idx]));

  for Idx := 0 to KnownSystemFunctions.Count - 1 do
    saHighlighter.Objects.Add(UpperCase(KnownSystemFunctions.Keys[Idx]));

//  saHighlighter.KeyWords.AddStrings(Keywords);
   }
  {$IFDEF DEBUG}
  ToolButton6.Visible := True;
  {$ENDIF}
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  MRUFile: string;
  MRUList: TStrings;
begin
  SetTitle;
  Caption := Title;
  ReadSettings(Highlighter, frmMain);
  LoadPackConfigs;
  if ParamCount > 0 then
    OpenFilesFromCmdLine;
  if Settings.GetValue('editor/rememberopenfiles', False) then
    OpenPreviousFiles;

  CheckSyntax := Settings.GetValue('editor/realtimesyntaxcheck', True);

  if ActiveEditor = nil then
    miFileNewClick(miFileNew);
  MRUList := TStringList.Create;
  Settings.EnumValues('filemru', MRUList);
  for MRUFile in MRUList do
    AddFileToMRU(Settings.GetValue('filemru/' + MRUFile, ''));

  miToolsPanel.Checked := Settings.GetValue('showtoolspanel', False);
  Splitter1.SetSplitterPosition(Settings.GetValue('splitterleft', frmMain.Width - 200));
  pcTools.Visible := miToolsPanel.Checked;
  tmrBackgroundWork.Enabled := True;
end;


procedure TfrmMain.miCommentsClick(Sender: TObject);
var
  Idx: integer;

begin
  if AnsiStartsStr('//', TrimLeft(ActiveEditor.Editor.LineText)) then
  begin
    for Idx := ActiveEditor.Editor.BlockBegin.Y - 1 to ActiveEditor.Editor.BlockEnd.Y - 1 do
      if Pos('//', TrimLeft(ActiveEditor.Editor.Lines[Idx])) = 1 then
        ActiveEditor.Editor.Lines[Idx] := StringReplace(ActiveEditor.Editor.Lines[Idx], '//', '', []);
  end
  else
    for Idx := ActiveEditor.Editor.BlockBegin.Y - 1 to ActiveEditor.Editor.BlockEnd.Y - 1 do
      ActiveEditor.Editor.Lines[Idx] := '//' + ActiveEditor.Editor.Lines[Idx];
end;

procedure TfrmMain.miGoToLineClick(Sender: TObject);
var
  Value: string;
  TargetLine: integer;
begin
  Value := '';
  if InputQuery('Go To Line', 'Line number:', Value) then
    if TryStrToInt(Value, TargetLine) then
      ActiveEditor.Editor.CaretY := TargetLine + 1;
end;


procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  ActiveEditor.Editor.Highlighter.GetToken;
  try
    frmAbout := TfrmAbout.Create(Self);
    frmAbout.ShowModal;
  finally
    frmAbout.Free;
  end;
end;

procedure TfrmMain.miCloseAllClick(Sender: TObject);
var
  idx: integer;
  res: integer;
  Editor: TEditorFile;
  ToClose: array of TEditorFile;
  Original: TEditorFile;
begin
  tmrBackgroundWork.Enabled := False;
  OkToClose := True; //In the event this is queried by the program shutting down.
  if Sender = miCloseOthers then //Comes from Close all others
    Original := ActiveEditor
  else
    Original := nil;
  SetLength(ToClose, 0);
  for idx := 0 to pcMDIFaker.PageCount - 1 do
  begin
    Editor := TEditorFile(pcMDIFaker.Page[idx]);
    if (Original <> nil) or (Original <> Editor) then
    begin
      if TEditorFile(pcMDIFaker.Page[idx]).Editor.Modified then
      begin
        pcMDIFaker.ActivePageIndex := idx;
        res := OfferSaveFile(Editor);
        if res = mrYes then
        begin
          SetLength(ToClose, Length(ToClose) + 1);
          ToClose[Length(ToClose) - 1] := Editor;
        end
        else if res = mrCancel then
        begin
          SetLength(ToClose, 0);
          OkToClose := False;
          break;
        end
        else if res = mrNo then
        begin
          SetLength(ToClose, Length(ToClose) + 1);
          ToClose[Length(ToClose) - 1] := Editor;
        end;
      end
      else
      begin
        SetLength(ToClose, Length(ToClose) + 1);
        ToClose[Length(ToClose) - 1] := Editor;
      end;
    end;
  end;

  for idx := 0 to Length(ToClose) - 1 do
  begin
    UntitledPool := UntitledPool - [ToClose[idx].UntitledIndex];
    CloseFile(ToClose[idx]);
  end;
  untitledPool := [];
  tmrBackgroundWork.Enabled := True;
end;


procedure TfrmMain.miCloseClick(Sender: TObject);
begin
  if not ActiveEditor.Editor.Modified then
    CloseFile(ActiveEditor)
  else
  if OfferSaveFile(ActiveEditor) <> mrCancel then
  begin
    UntitledPool := UntitledPool - [ActiveEditor.UntitledIndex];
    CloseFile(ActiveEditor);
  end;
end;

procedure TfrmMain.miCloseOthersClick(Sender: TObject);
begin
  miCloseAllClick(miCloseOthers);
end;

procedure TfrmMain.miCopyClick(Sender: TObject);
begin
  ActiveEditor.Editor.CopyToClipboard;
end;

procedure TfrmMain.miCutClick(Sender: TObject);
begin
  ActiveEditor.Editor.CutToClipboard;
end;

procedure TfrmMain.miPasteClick(Sender: TObject);
begin
  ActiveEditor.Editor.PasteFromClipboard;
end;


procedure TfrmMain.miSelectAllClick(Sender: TObject);
begin
  ActiveEditor.Editor.SelectAll;
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  frmMain.Close;
end;

procedure TfrmMain.miExportBBCodeClick(Sender: TObject);
var
  Exporter: TExporterBB;
  Stream: TMemoryStream;
begin
  Exporter := TExporterBB.Create(self);
  try
    Stream := TMemoryStream.Create;
    Exporter.Color := TKerboscriptHighlighter(ActiveEditor.Editor.Highlighter).SpaceAttr.Background;
    Exporter.UseBackground := True;
    Exporter.Highlighter := ActiveEditor.Editor.Highlighter;
    Exporter.ExportAll(ActiveEditor.Editor.Lines);
    Exporter.SaveToStream(Stream);
    Stream.Position := 0;
    with TStringList.Create do
    begin
      LoadFromStream(Stream);
      Clipboard.AsText := Text;
    end;
  finally
    Exporter.Free;
  end;
end;

procedure TfrmMain.miExportHTMLClick(Sender: TObject);
var
  Exporter: TExporterHTML;
  Stream: TMemoryStream;
begin
  Exporter := TExporterHTML.Create(self);
  try
    Stream := TMemoryStream.Create;
    Exporter.Color := TKerboscriptHighlighter(ActiveEditor.Editor.Highlighter).SpaceAttr.Background;
    Exporter.UseBackground := True;
    Exporter.CreateHTMLFragment := True;
    Exporter.Highlighter := ActiveEditor.Editor.Highlighter;
    Exporter.ExportAll(ActiveEditor.Editor.Lines);
    Exporter.SaveToStream(Stream);
    Stream.Position := 0;
    with TStringList.Create do
    begin
      LoadFromStream(Stream);
      Clipboard.AsText := Text;
    end;
  finally
    Exporter.Free;
  end;
end;

procedure TfrmMain.miFileNewClick(Sender: TObject);
var
  NewTab: TEditorFile;
  nextUntitled: byte;
begin
  tmrBackgroundWork.Enabled := False;
  nextUntitled := GetNextUntitled;
  NewTab := TEditorFile.Create(pcMDIFaker);
  NewTab.FileName := 'untitled - ' + IntToStr(nextUntitled);
  NewTab.UntitledIndex := nextUntitled;
  untitledPool := untitledPool + [nextUntitled];
  pcMDIFaker.ActivePage := NewTab;
  NewTab.Editor.SetFocus;
  SynCompletion1.AddEditor(NewTab.Editor);
  UpdateStatusBar([], True);
  tmrBackgroundWork.Enabled := True;
end;

procedure TfrmMain.miNextClick(Sender: TObject);
begin
  if pcMDIFaker.ActivePageIndex = pcMDIFaker.PageCount - 1 then
    pcMDIFaker.ActivePageIndex := 0
  else
    pcMDIFaker.ActivePageIndex := pcMDIFaker.ActivePageIndex + 1;
end;

procedure TfrmMain.miOpenClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute then
      OpenFile(OpenDialog1.FileName, False);

  except
    on e: Exception do
      MessageDlg('Saturn', e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.miPackConfigsClick(Sender: TObject);
begin
  miPackerClick(miPacker);
end;

procedure TfrmMain.miPackConfigsClickToPack(Sender: TObject);
begin
  tmrBackgroundWork.Enabled := False;
  frmPackConfigs := TfrmPackConfigs.Create(self);
  try
    frmPackConfigs.LoadConfigs;
    frmPackConfigs.lbxConfigs.ItemIndex := frmPackConfigs.lbxConfigs.Items.IndexOf(TMenuItem(Sender).Caption);
    frmPackConfigs.LoadConfigValues(TMenuItem(Sender).Caption);
    frmPackConfigs.btnPackClick(frmPackConfigs.btnPack);
    //frmPackConfigs.ShowModal;
    //LoadPackConfigs;
  finally
    frmPackConfigs.Free;
    tmrBackgroundWork.Enabled := True;
  end;
end;

procedure TfrmMain.miPackCurrentClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  for Item in popmPacker.Items do
    if Item.Checked then
      Item.Click;
end;

procedure TfrmMain.miPackerClick(Sender: TObject);
begin
  frmPackConfigs := TfrmPackConfigs.Create(self);
  try
    frmPackConfigs.LoadConfigs;
    frmPackConfigs.ShowModal;
    LoadPackConfigs;
  finally
    frmPackConfigs.Free;
  end;
end;


procedure TfrmMain.miPreviousClick(Sender: TObject);
begin
  if pcMDIFaker.ActivePageIndex = 0 then
    pcMDIFaker.ActivePageIndex := pcMDIFaker.PageCount - 1
  else
    pcMDIFaker.ActivePageIndex := pcMDIFaker.ActivePageIndex - 1;
end;

procedure TfrmMain.miRedoClick(Sender: TObject);
begin
  ActiveEditor.Editor.Redo;
end;

procedure TfrmMain.miSaveAsClick(Sender: TObject);
begin
  try
    if SaveDialog1.Execute then
      ActiveEditor.SaveFileAs(SaveDialog1.FileName);
  except
    on e: Exception do
      MessageDlg('Saturn', e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.miSaveClick(Sender: TObject);
begin
  try
    if ActiveEditor.Untitled then
    begin
      if SaveDialog1.Execute then
        ActiveEditor.SaveFileAs(SaveDialog1.FileName);
    end
    else
      ActiveEditor.SaveFile;

  except
    on e: Exception do
      MessageDlg('Saturn', e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.miSaveToArchiveClick(Sender: TObject);
var
  folder: string;
begin
  try
    folder := Settings.GetValue('kspfolder', '') + PathDelim + 'Plugins' + PathDelim + 'PluginData' +
      PathDelim + 'Archive';
    if not DirectoryExists(folder) then
      raise Exception.Create('KSP folder not found. Please go to settings and make sure it''s set.');

    SaveDialog1.InitialDir := folder;
    SaveDialog1.FileName := ActiveEditor.FileName;
    if Savedialog1.Execute then
      ActiveEditor.SaveFileAs(SaveDialog1.FileName);

  except
    on e: Exception do
      MessageDlg('Saturn', e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.miSelectColumnClick(Sender: TObject);
begin
  ActiveEditor.Editor.ExecuteCommand(ecColumnSelect, '', nil);
end;

procedure TfrmMain.miSelectNormalClick(Sender: TObject);
begin
  ActiveEditor.Editor.ExecuteCommand(ecNormalSelect, '', nil);
end;

procedure TfrmMain.miSettingsClick(Sender: TObject);
var
  idx: integer;
begin
  frmSettings := TfrmSettings.Create(self);
  try
    if frmSettings.ShowModal = mrOk then
    begin
      Highlighter.Assign(TKerboscriptHighlighter(frmSettings.cbxScheme.Items.Objects[frmSettings.cbxScheme.ItemIndex]));
      for idx := 0 to pcMDIFaker.PageCount - 1 do
      begin
        TEditorFile(pcMDIFaker.Page[Idx]).Editor.Highlighter.Assign(Highlighter);
        TEditorFile(pcMDIFaker.Page[Idx]).Editor.BracketMatchColor.Assign(Highlighter.BracketMatchColor);
        TEditorFile(pcMDIFaker.Page[Idx]).Editor.Color := Highlighter.SpaceAttr.Background;
        TEditorFile(pcMDIFaker.Page[Idx]).Editor.Font.Assign(Highlighter.Font);
        TEditorFile(pcMDIFaker.Page[Idx]).Editor.ExtraCharSpacing := Highlighter.ExtraCharSpacing;
        TEditorFile(pcMDIFaker.Page[Idx]).Editor.TabWidth := Highlighter.TabWidth;

        if Highlighter.OldStyleCursor then
          TEditorFile(pcMDIFaker.Page[Idx]).Editor.InsertCaret := ctHorizontalLine
        else
          TEditorFile(pcMDIFaker.Page[Idx]).Editor.InsertCaret := ctVerticalLine;

        TEditorFile(pcMDIFaker.Page[Idx]).Editor.Refresh;
      end;

      Settings.SetValue('window/maximized', frmSettings.rbMaximized.Checked);
      Settings.SetValue('window/remember', frmSettings.rbRemember.Checked);
      Settings.SetValue('window/x', Left);
      Settings.SetValue('window/y', Top);
      Settings.SetValue('window/height', Height);
      Settings.SetValue('window/width', Width);

      Settings.SetValue('kspfolder', frmSettings.dedtKSPFolder.Directory);

      Settings.SetValue('editor/rememberopenfiles', frmSettings.chkRememberOpenFiles.Checked);
      Settings.SetValue('editor/realtimesyntaxcheck', frmSettings.chkSyntax.Checked);

    end;
  finally
    frmSettings.Free;
  end;
end;

procedure TfrmMain.miToolsPanelClick(Sender: TObject);
begin
  pcTools.Visible := miToolsPanel.Checked;
end;

procedure TfrmMain.miUndindentClick(Sender: TObject);
begin
  ActiveEditor.Editor.ExecuteCommand(ecBlockUnindent, '', nil);
end;

procedure TfrmMain.miIndentClick(Sender: TObject);
begin
  ActiveEditor.Editor.ExecuteCommand(ecBlockIndent, '', nil);
end;

procedure TfrmMain.miOpenFromArchiveClick(Sender: TObject);
var
  Folder: string;
begin
  try
    Folder := Settings.GetValue('kspfolder', '') + PathDelim + 'Plugins' + PathDelim + 'PluginData' +
      PathDelim + 'Archive';
    if not DirectoryExists(Folder) then
      MessageDlg('Save to archive', 'KSP folder not found. Please go to settings and make sure it''s set.',
        mtError, [mbOK], 0)
    else
    begin
      OpenDialog1.InitialDir := Folder;
      if OpenDialog1.Execute then
        OpenFile(OpenDialog1.FileName, True);
    end;

  except
    on e: Exception do
      MessageDlg('Saturn', e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.miOpenProgramAtCursorClick(Sender: TObject);
begin
  emiOpenProgramAtCursorClick(emiOpenProgramAtCursor);
end;

procedure TfrmMain.miReadmeClick(Sender: TObject);
begin
  OpenUrl('Readme.html');
end;

procedure TfrmMain.miUndoClick(Sender: TObject);
begin
  ActiveEditor.Editor.Undo;
end;

procedure TfrmMain.miFileMRUClick(Sender: TObject);
begin
  Openfile(TMenuItem(Sender).Caption, False);
end;

procedure TfrmMain.miArchiveMRUClick(Sender: TObject);
begin
  OpenFile(TMenuItem(Sender).Caption, True);
end;

procedure TfrmMain.pcMDIFakerCloseTabClicked(Sender: TObject);
begin
  miCloseClick(miClose);
end;

procedure TfrmMain.seHiddenEnter(Sender: TObject);
begin
  if ActiveEditor.AskToReload and AskToReloadFile then
    ActiveEditor.Reload;
end;

procedure TfrmMain.seHiddenStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ActiveEditor.AskToReload and AskToReloadFile then
    ActiveEditor.Reload;
  UpdateStatusBar(Changes, False);
 { if (saHighlighter.Constants.Count > 0) and (saHighlighter.Constants[0] <>
    UpperCase(ActiveEditor.Editor.GetWordAtRowCol(ActiveEditor.Editor.CaretXY))) then
  begin
    saHighlighter.Constants.Clear;
    ActiveEditor.Editor.Refresh;
  end
  else
  begin
    timer1.Enabled := False;
    timer1.Interval := 1500;
    timer1.Enabled := True;
  end;}
end;

procedure TfrmMain.AddToAutocomplete(ToAdd: string; KeepCase: boolean);
begin
  if (SynCompletion1.CurrentString = '') or (pos(lowercase(SynCompletion1.CurrentString), lowercase(ToAdd)) = 1) then
    if not KeepCase and Settings.GetValue('editor/autocompleteuppercase', False) then
      SynCompletion1.ItemList.Add(UpperCase(ToAdd))
    else if not KeepCase then
      SynCompletion1.ItemList.Add(LowerCase(ToAdd))
    else
      SynCompletion1.ItemList.Add(ToAdd);
end;

procedure TfrmMain.OpenFilesFromCmdLine;
var
  Idx: integer;
begin
  for Idx := 1 to ParamCount do
    if ParamStr(Idx)[1] <> '-' then
      OpenFile(ParamStr(Idx), AnsiStartsStr(LowerCase(Settings.GetValue('kspfolder', '')), LowerCase(ParamStr(Idx))));
end;

procedure TfrmMain.SaveListOpenFiles;
var
  Idx: integer;
begin
  Settings.DeletePath('openfiles');
  Settings.SetValue('openfilesindex', pcMDIFaker.ActivePageIndex);
  for Idx := 0 to pcMDIFaker.PageCount - 1 do
    if not TEditorFile(pcMDIFaker.Page[Idx]).Untitled then
      Settings.SetValue('openfiles/' + IntToStr(Idx), TEditorFile(pcMDIFaker.Page[Idx]).FileName);
end;

procedure TfrmMain.OpenPreviousFiles;
var
  Items: TStringList;
  Item: string;
begin
  Items := TStringList.Create;
  Settings.EnumValues('openfiles', Items);
  for Item in Items do
    OpenFile(Settings.GetValue('openfiles/' + Item, ''), AnsiStartsStr(Settings.GetValue('kspfolder', ''),
      Settings.GetValue('openfiles/' + Item, '')));
  pcMDIFaker.ActivePageIndex := Settings.GetValue('openfilesindex', 0);
end;

function TfrmMain.FileIndex(const TheFile: string): integer;
var
  Idx: integer;
begin
  Result := -1;
  for Idx := 0 to pcMDIFaker.PageCount - 1 do
  begin
    if AnsiCompareFileName(TEditorFile(pcMDIFaker.Page[Idx]).FileName, TheFile) = 0 then
    begin
      Result := Idx;
      exit;
    end;
  end;
end;

procedure TfrmMain.AddVariablesFromUsedPrograms;
var
  Aux: TUsedProgram;
  ProgName: string;
begin
  FetchUsedPrograms(ActiveEditor.Editor.Lines, ExtractFileDir(ActiveEditor.FileName), ActiveEditor.UsedPrograms);
  if pcTools.Visible then
  begin
    ActiveEditor.UsedPrograms.PrepareIterator;
    while ActiveEditor.UsedPrograms.GetNextKey(ProgName) do
    begin
      Aux := ActiveEditor.UsedPrograms[ProgName];
      ProcessUsedProgram(Aux);
      ActiveEditor.UsedPrograms[ProgName] := Aux;
    end;
  end;
end;

procedure TfrmMain.UpdateVariablesList(Editor: TEditorFile);
var
  Idx: integer;
  ProgramNode: TTreeNode;
  InProgram: string;
begin
  if not pcTools.Visible then
    exit;

  tvVariables.Items.Clear;
  ProgramNode := tvVariables.Items.Add(nil, ExtractFileNameOnly(Editor.FileName));
  for Idx := 0 to Editor.KnownVariables.Count - 1 do
    if VariableCollides(Editor.KnownVariables.Keys[Idx], Editor.UsedPrograms, InProgram) then
      tvVariables.Items.AddChildObject(ProgramNode, Editor.KnownVariables.Keys[Idx],
        TExtVarInfo.Create(InProgram, Editor.KnownVariables.Keys[Idx]))
    else
      tvVariables.Items.AddChild(ProgramNode, Editor.KnownVariables.Keys[Idx]);
  ProgramNode.Expand(False);
end;

function TfrmMain.VariableCollides(const VarName: string; UsedPrograms: TUsedProgramsList; out InProgram: string): boolean;
var
  ProgName: string;
begin
  Result := False;
  UsedPrograms.PrepareIterator;
  while UsedPrograms.GetNextKey(ProgName) do
    if UsedPrograms[ProgName].Variables.IndexOf(VarName) >= 0 then
    begin
      Result := True;
      InProgram := ProgName;
      exit;
    end;

end;

function TfrmMain.AskToReloadFile: boolean;
begin
  Result := MessageDlg('Saturn', '"' + ActiveEditor.FileName + '"' + ' was modified outside of Saturn.' +
    LineEnding + LineEnding + 'Do you want to reload it? (you''ll loose your changes)', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
  ActiveEditor.SetDontAskReloadForTimestamp;
end;


function TfrmMain.SynCompletion1PaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer;
  Selected: boolean; Index: integer): boolean;
var
  ItemType: TObjectType;
  IdList: TStringList;
  IdentClass: TIdentifierClass;
  PreffixWidth, TextWidth: integer;
begin
  IdList := GetListOfIdentifiers(ActiveEditor.Editor.LineText, ActiveEditor.Editor.CaretX - 1, True);
  IdList.Add(frmMain.SynCompletion1.ItemList[Index]);
  ItemType := GetIdentifierType(IdList, ActiveEditor.KnownVariables, IdentClass);

  ACanvas.Font.Style := [fsItalic];
  PreffixWidth := ACanvas.TextWidth('(W) ');

  ACanvas.Font.Style := [fsBold];
  TextWidth := ACanvas.TextWidth(SynCompletion1.ItemList[Index]);

  ACanvas.Font.Style := [fsBold];
  ACanvas.TextOut(X + 2 + PreffixWidth, Y, SynCompletion1.ItemList[Index]);

  if (ItemType <> otUnknown) or not (IdentClass in [icKeyword, icCommand]) then
  begin
    if Selected then
      ACanvas.Font.Color := clHighlightText
    else
      ACanvas.Font.Color := not SynCompletion1.TheForm.Color;//clSilver;

    ACanvas.Font.Style := [];
    ACanvas.TextOut(X + 2 + PreffixWidth + TextWidth + 5, Y, GetTypeName(ItemType));
  end;

  ACanvas.Font.Style := [];

  //  if IdList.Count = 1 then
  case IdentClass of
    icFunction:
    begin
      if Selected then
        ACanvas.Font.Color := clHighlightText
      else
        ACanvas.Font.Color := $00004080;
      ACanvas.TextOut(X + 2, Y, '(f)');
    end;

    icSystemVariable, icUserVariable:
    begin
      if Selected then
        ACanvas.Font.Color := clHighlightText
      else
        ACanvas.Font.Color := clRed;
      ACanvas.TextOut(X + 2, Y, '(v)');
    end;

    icKeyword, icCommand:
    begin
      if Selected then
        ACanvas.Font.Color := clHighlightText
      else
        ACanvas.Font.Color := clGreen;
      ACanvas.TextOut(X + 2, Y, '(k)');
    end;

    else
    begin
      if Selected then
        ACanvas.Font.Color := clHighlightText
      else
        ACanvas.Font.Color := clDefault;
      ACanvas.TextOut(X + 2, Y, '(?)');
    end;
  end;

  IdList.Free;
  Result := True;
end;



procedure TfrmMain.SynCompletion1SearchPosition(var APosition: integer);
var
  Identifiers: TStringList;
  Suffixes: TStringList;
  Suffix: string;
  idclass: TIdentifierClass;
begin
  SynCompletion1.ItemList.Clear;
  TStringList(SynCompletion1.ItemList).Sorted := True;
  Identifiers := GetListOfIdentifiers(ActiveEditor.Editor.LineText, ActiveEditor.Editor.CaretX - 1, True);
  Suffixes := GetSuffixes(Identifiers, ActiveEditor.KnownVariables);
  if (Suffixes.Count = 0) and ((Identifiers.Count = 0) or
    (GetIdentifierType(Identifiers, ActiveEditor.KnownVariables, idclass) <> otUnknown)) and
    ((ActiveEditor.Editor.CaretX > 1) and not (ActiveEditor.Editor.LineText[ActiveEditor.Editor.CaretX - 1] in
    SepChars)) then

    AddAllKownIdentifiers;
  for Suffix in Suffixes do
    AddToAutocomplete(Suffix, False);

  if SynCompletion1.ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;

  Suffixes.Free;
  Identifiers.Free;
  SynCompletion1.TheForm.Invalidate;
end;

procedure TfrmMain.tmrBackgroundWorkTimer(Sender: TObject);
begin
  tmrBackgroundWork.Enabled := False;
  if ActiveEditor = nil then
    exit;
  FetchVariables(ActiveEditor.Editor.Lines, ActiveEditor.KnownVariables, True);
  Application.ProcessMessages;
  UpdateVariablesList(ActiveEditor);
  AddVariablesFromUsedPrograms;
  Application.ProcessMessages;
  if CheckSyntax then
  begin
    ActiveEditor.ClearMarkups;
    ActiveEditor.StartSyntaxCheck;
    SyntaxCheck(ActiveEditor, nil);
    Application.ProcessMessages;
    ActiveEditor.FinishSyntaxCheck;
    ActiveEditor.Refresh;
  end;
  tmrBackgroundWork.Enabled := True;
end;

procedure TfrmMain.ToolButton6Click(Sender: TObject);
{var r: string;
    t: TToken;}
begin
  ActiveEditor.ClearMarkups;
  ActiveEditor.StartSyntaxCheck;
  Caption := '>';Application.ProcessMessages;
  SyntaxCheck(ActiveEditor, nil);
  Caption := Caption + '<';
  Application.ProcessMessages;
  ActiveEditor.FinishSyntaxCheck;
  ActiveEditor.Refresh
end;


procedure TfrmMain.tvVariablesCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: boolean);
begin
  if (Node.Data <> nil) then
  begin
    TTreeView(Sender).Canvas.Font.Style := [fsBold];
    TTreeView(Sender).Canvas.Font.Color := $002638D9;
  end
  else
  begin
    TTreeView(Sender).Canvas.Font.Color := clDefault;
    TTreeView(Sender).Canvas.Font.Style := [];
  end;
  defaultdraw := True;
end;

procedure TfrmMain.tvVariablesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  Node: TTreeNode;
begin
  Node := tvVariables.GetNodeAt(X, Y);
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    tvVariables.Hint := 'Collides with variable in program "' + TExtVarInfo(Node.Data).BelongsToProgram + '".';
    tvVariables.ShowHint := True;
  end
  else
  begin
    tvVariables.Hint := '';
    tvVariables.ShowHint := False;
  end;
end;

function TfrmMain.GetActiveEditor: TEditorFile;
begin
  Result := TEditorFile(pcMDIFaker.ActivePage);
end;

procedure TfrmMain.AddAllKownIdentifiers;
var
  Idx: integer;
  Key: string;
begin
  //Known system functions
  for Idx := 0 to KnownSystemFunctions.Count - 1 do
    AddToAutocomplete(KnownSystemFunctions.Keys[Idx], False);

  //Known system variables
  for Idx := 0 to KnownSystemVariables.Count - 1 do
    AddToAutocomplete(KnownSystemVariables.Keys[Idx], False);

  //Add keywords
  Keywords.PrepareIterator;
  while Keywords.GetNextKey(Key) do
    AddToAutocomplete(LowerCase(Key), False);

  //Add commands
  Commands.PrepareIterator;
  while Commands.GetNextKey(Key) do
    AddToAutocomplete(LowerCase(Key), False);

  //Variables in active editor
  for Idx := 0 to ActiveEditor.KnownVariables.Count - 1 do
    AddToAutocomplete(ActiveEditor.KnownVariables.Keys[Idx], True);
end;


procedure TfrmMain.miSearchReplaceClick(Sender: TObject);
begin
  if (ActiveEditor.Editor.SelText <> '') and (ActiveEditor.Editor.BlockBegin.Y = ActiveEditor.Editor.BlockEnd.Y) then
    frmSearchReplace.SetOptions(ActiveEditor.Editor.SelText)
  else
    frmSearchReplace.SetOptions('');
  frmSearchReplace.Show;
  frmSearchReplace.cbxSearch.SetFocus;
end;

procedure TfrmMain.FindReplace(const ToFind, ReplaceWith: string; Options: TSynSearchOptions);
begin
  if ActiveEditor.Editor.SearchReplaceEx(ToFind, ReplaceWith, Options, ActiveEditor.Editor.CaretXY) = 0 then
    ShowMessage('Not found.');
end;

procedure TfrmMain.seHiddenReplaceText(Sender: TObject; const ASearch, AReplace: string;
  Line, Column: integer; var ReplaceAction: TSynReplaceAction);
var
  Res: integer;
begin
  Res := MessageDlg('Replace', 'Replace this occurrence?', mtConfirmation, [mbYes, mbNo, mbYesToAll, mbCancel], 0);
  case Res of
    mrYes: ReplaceAction := raReplace;
    mrNo: ReplaceAction := raSkip;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrCancel: ReplaceAction := raCancel;
  end;
end;

procedure TfrmMain.emiCutClick(Sender: TObject);
begin
  ActiveEditor.Editor.CutToClipboard;
end;

procedure TfrmMain.emiOpenProgramAtCursorClick(Sender: TObject);
var
  ProgramToOpen, ArchiveFolder, FileFolder: string;
  OpenedAnything: boolean;

begin
  ProgramToOpen := ActiveEditor.Editor.GetWordAtRowCol(ActiveEditor.Editor.CaretXY) + '.txt';
  ArchiveFolder := Settings.GetValue('kspfolder', '') + PathDelim + 'Plugins' + PathDelim +
    'PluginData' + PathDelim + 'Archive';
  FileFolder := ExtractFilePath(ActiveEditor.FileName);

  OpenedAnything := False;

  if FileExists(ArchiveFolder + PathDelim + ProgramToOpen) then
  begin
    OpenFile(ArchiveFolder + PathDelim + ProgramToOpen, True);
    OpenedAnything := True;
  end;

  if FileExists(FileFolder + PathDelim + ProgramToOpen) then
  begin
    OpenFile(FileFolder + PathDelim + ProgramToOpen, False);
    OpenedAnything := True;
  end;

  if not OpenedAnything then
    MessageDlg('Open File at Cursor', 'Couldn''t find a file called "' + ProgramToOpen +
      '" on the current directory or the Archive.', mtError, [mbOK], 0);
end;

procedure TfrmMain.emiSelectAllClick(Sender: TObject);
begin
  ActiveEditor.Editor.SelectAll;
end;

procedure TfrmMain.emiCopyClick(Sender: TObject);
begin
  ActiveEditor.Editor.CopyToClipboard;
end;


end.
