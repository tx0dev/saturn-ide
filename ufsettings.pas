unit ufSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ColorBox, Buttons, EditBtn, Spin, ExtCtrls,
  SynEditHighlighter, SynEdit, typinfo, uKerboscriptHighlighter, jsonConf;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btnFont: TBitBtn;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    chkSyntax: TCheckBox;
    chkMonoFonts: TCheckBox;
    chkRememberOpenFiles: TCheckBox;
    chkOldStyleCursor: TCheckBox;
    chkAutocompleteUppercase: TCheckBox;
    clrbForeground: TColorBox;
    clrbBackground: TColorBox;
    cbxScheme: TComboBox;
    dedtKSPFolder: TDirectoryEdit;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbxTypes: TListBox;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    rbRemember: TRadioButton;
    rbMaximized: TRadioButton;
    SaveDialog1: TSaveDialog;
    seSample: TSynEdit;
    seTabWidth: TSpinEdit;
    spinCharSep: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    tbtnCopy: TToolButton;
    tbtnNew: TToolButton;
    tbtnDelete: TToolButton;
    tBtnImport: TToolButton;
    tbtnExport: TToolButton;
    tbtnExportAll: TToolButton;
    tbtnResetToDefault: TToolButton;
    procedure btnFontClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbxSchemeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OptionChange(Sender: TObject);
    procedure lbxTypesSelectionChange(Sender: TObject; User: boolean);
    procedure seSampleClick(Sender: TObject);
    procedure seSampleShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure spinCharSepChange(Sender: TObject);
    procedure tbtnCopyClick(Sender: TObject);
    procedure tbtnDeleteClick(Sender: TObject);
    procedure tbtnExportAllClick(Sender: TObject);
    procedure tbtnExportClick(Sender: TObject);
    procedure tBtnImportClick(Sender: TObject);
    procedure tbtnNewClick(Sender: TObject);
    procedure tbtnResetToDefaultClick(Sender: TObject);
  private
    FreezeChange: boolean;
    function PackAttributes: TSynHighlighterAttributes;
    procedure UnpackAttributes(Attributes: TSynHighlighterAttributes);
    procedure LoadScheme;
    procedure ImportScheme(const SchemeName: string; Data: TJSONConfig);
  public
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

{ TfrmSettings }


uses uSettings, uTokens, SynEditPointClasses;

const
  DefaultCount: integer = 3;

procedure TfrmSettings.lbxTypesSelectionChange(Sender: TObject; User: boolean);
var
  Highlighter: TKerboscriptHighlighter;
begin
  FreezeChange := User;
  Highlighter := TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]);
  if lbxTypes.GetSelectedText = 'Comment' then
    UnpackAttributes(Highlighter.CommentAttr)
  else if lbxTypes.GetSelectedText = 'Function' then
    UnpackAttributes(Highlighter.FunctionAttr)
  else if lbxTypes.GetSelectedText = 'Identifier' then
    UnpackAttributes(Highlighter.IdentifierAttr)
  else if lbxTypes.GetSelectedText = 'Keyword' then
    UnpackAttributes(Highlighter.KeywordAttr)
  else if lbxTypes.GetSelectedText = 'Number' then
    UnpackAttributes(Highlighter.NumberAttr)
  else if lbxTypes.GetSelectedText = 'String' then
    UnpackAttributes(Highlighter.StringAttr)
  else if lbxTypes.GetSelectedText = 'White space' then
    UnpackAttributes(Highlighter.SpaceAttr)
  else if lbxTypes.GetSelectedText = 'Symbol' then
    UnpackAttributes(Highlighter.SymbolAttr)
  else if lbxTypes.GetSelectedText = 'System variable' then
    UnpackAttributes(Highlighter.SystemVariableAttr)
  else if lbxTypes.GetSelectedText = 'Matched bracket' then
  begin
    cbBold.Checked := fsBold in Highlighter.BracketMatchColor.Style;
    cbUnderline.Checked := fsUnderline in Highlighter.BracketMatchColor.Style;
    cbItalic.Checked := fsItalic in Highlighter.BracketMatchColor.Style;
    clrbForeground.Selected := Highlighter.BracketMatchColor.FrameColor;
    clrbBackground.Selected := Highlighter.BracketMatchColor.Background;
  end;
  FreezeChange := False;
end;

procedure TfrmSettings.seSampleClick(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
  Token: string;
  TokenType, Start: integer;
begin
  if seSample.GetHighlighterAttriAtRowColEx(seSample.CaretXY, Token, TokenType, Start, Attr) then
    case TTokenType(TokenType) of
      ttComment: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('Comment');
      ttFunction: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('Function');
      ttIdentifier: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('Identifier');
      ttInteger, ttReal: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('Number');
      ttKeyword: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('Keyword');
      ttString: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('String');
      ttSymbol: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('Symbol');
      ttSystemVar: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('System variable');
      ttSpace: lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('White space');
    end
  else
    lbxTypes.ItemIndex := lbxTypes.Items.IndexOf('White space');
end;

procedure TfrmSettings.seSampleShowHint(Sender: TObject; HintInfo: PHintInfo);
var s: string;
begin
  s := sesample.GetWordAtRowCol(seSample.PixelsToLogicalPos(hintinfo^.CursorPos));
  hintinfo^.HintStr := s;
  //sesample.
end;

procedure TfrmSettings.spinCharSepChange(Sender: TObject);
begin
  seSample.ExtraCharSpacing := spinCharSep.Value;
end;

procedure TfrmSettings.tbtnCopyClick(Sender: TObject);
var
  NewName: string;
  NewHL: TKerboscriptHighlighter;
begin
  NewName := 'Copy of ' + cbxScheme.Text;
  if InputQuery('Copy scheme', 'Name of the new scheme', NewName) and (NewName <> '') then
    if cbxScheme.Items.IndexOf(NewName) >= 0 then
      MessageDlg('Saturn', 'A scheme with this name already exists.', mtError, [mbYes], 0)
    else
    begin
      NewHL := TKerboscriptHighlighter.Create(Self);
      NewHL.Assign(TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]));
      NewHL.WriteToJSON(Settings, NewName);
      cbxScheme.AddItem(NewName, NewHL);
      cbxScheme.ItemIndex := cbxScheme.Items.Count - 1;
      cbxSchemeChange(cbxScheme);
    end;
end;

procedure TfrmSettings.tbtnDeleteClick(Sender: TObject);
begin
  if MessageDlg('Saturn', 'Are you sure you want to delete ' + cbxScheme.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Settings.DeletePath('/highlighters/' + cbxScheme.Text);
    Settings.Flush;
    cbxScheme.ItemIndex := cbxScheme.ItemIndex - 1;
    cbxScheme.Items.Delete(cbxScheme.ItemIndex + 1);
    cbxSchemeChange(cbxScheme);
    btnSaveClick(btnSave);
  end;
end;

procedure TfrmSettings.tbtnExportAllClick(Sender: TObject);
var
  Idx: integer;
  Data: TJSONConfig;
begin
  Data := TJSONConfig.Create(nil);
  try
    try
      if SaveDialog1.Execute then
      begin
        Data.Filename := SaveDialog1.FileName;
        for Idx := 0 to cbxScheme.Items.Count - 1 do
          TKerboscriptHighlighter(cbxScheme.Items.Objects[Idx]).WriteToJSON(Data, cbxScheme.Items[Idx]);
      end;
    except
      on e: Exception do
        MessageDlg('Import Scheme', 'Error importing scheme' + LineEnding + e.Message, mtError, [mbOK], 0);
    end;
  finally
    Data.Flush;
    Data.Free;
  end;
end;

procedure TfrmSettings.tbtnExportClick(Sender: TObject);
var
  Data: TJSONConfig;
begin
  Data := TJSONConfig.Create(nil);
  try
    try
      if SaveDialog1.Execute then
      begin
        Data.Filename := SaveDialog1.FileName;
        TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]).WriteToJSON(Data, cbxScheme.Text);
      end;
    except
      on e: Exception do
        MessageDlg('Import Scheme', 'Error importing scheme' + LineEnding + e.Message, mtError, [mbOK], 0);
    end;
  finally
    Data.Flush;
    Data.Free;
  end;
end;

procedure TfrmSettings.tBtnImportClick(Sender: TObject);
var
  Data: TJSONConfig;
  Schemes: TStringList;
  Scheme: string;
begin
  if OpenDialog1.Execute then
  begin
    try
      try
        Data := TJSONConfig.Create(nil);
        Data.Filename := OpenDialog1.FileName;
        Schemes := TStringList.Create;
        Data.EnumSubKeys('highlighters', Schemes);
        for Scheme in Schemes do
          ImportScheme(Scheme, Data);

        cbxSchemeChange(cbxScheme);
      except
        on e: Exception do
          MessageDlg('Import Scheme', 'Error importing scheme' + LineEnding + e.Message, mtError, [mbOK], 0);
      end;
    finally
      Data.Free;
    end;
  end;
end;

procedure TfrmSettings.tbtnNewClick(Sender: TObject);
var
  NewName: string;
  NewHL: TKerboscriptHighlighter;
begin
  NewName := 'New Scheme';
  if InputQuery('Clone scheme', 'Name of the new scheme', NewName) and (NewName <> '') then
    if cbxScheme.Items.IndexOf(NewName) >= 0 then
      MessageDlg('Saturn', 'A scheme with this name already exists.', mtError, [mbYes], 0)
    else
    begin
      NewHL := TKerboscriptHighlighter.Create(Self);
      NewHL.ResetToDefaults('');
      NewHL.WriteToJSON(Settings, NewName);
      cbxScheme.AddItem(NewName, NewHL);
      cbxScheme.ItemIndex := cbxScheme.Items.Count - 1;
      cbxSchemeChange(cbxScheme);
    end;
end;

procedure TfrmSettings.tbtnResetToDefaultClick(Sender: TObject);
begin
  if MessageDlg('Reset to default', 'Are you sure you want to reset the scheme "' + cbxScheme.Text +
    '" to its default values?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]).ResetToDefaults(cbxScheme.Text);
    cbxSchemeChange(cbxScheme);
  end;
end;

function TfrmSettings.PackAttributes: TSynHighlighterAttributes;
var
  ft: TFontStyles;
begin
  Result := TSynHighlighterAttributes.Create('Reserved', 'Reserved');
  ft := [];
  if cbBold.Checked then
    ft := ft + [fsBold];
  if cbUnderline.Checked then
    ft := ft + [fsUnderline];
  if cbItalic.Checked then
    ft := ft + [fsItalic];
  Result.Style := ft;
  Result.Foreground := clrbForeground.Selected;
  Result.Background := clrbBackground.Selected;
end;

procedure TfrmSettings.UnpackAttributes(Attributes: TSynHighlighterAttributes);
begin
  cbBold.Checked := fsBold in Attributes.Style;
  cbUnderline.Checked := fsUnderline in Attributes.Style;
  cbItalic.Checked := fsItalic in Attributes.Style;
  clrbForeground.Selected := Attributes.Foreground;
  clrbBackground.Selected := Attributes.Background;
end;

procedure TfrmSettings.LoadScheme;
var
  HL: TKerboscriptHighlighter;
begin
  HL := TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]);
  seSample.Highlighter := HL;
  seSample.Color := HL.SpaceAttr.Background;
  seSample.ExtraCharSpacing := HL.ExtraCharSpacing;
  seSample.BracketMatchColor.Assign(HL.BracketMatchColor);
  seSample.Font.Assign(HL.Font);
  seSample.Invalidate;
  if HL.OldStyleCursor then
    seSample.InsertCaret := ctHorizontalLine
  else
    seSample.InsertCaret := ctVerticalLine;
  lbxTypes.ItemIndex := 0;
  lbxTypesSelectionChange(lbxTypes, False);
  btnFont.Caption := seSample.Font.Name + ' ' + IntToStr(seSample.Font.Size) + 'pt.';
  spinCharSep.Value := seSample.ExtraCharSpacing;
  chkAutocompleteUppercase.Checked := HL.AutocompleteInUppercase;
  chkOldStyleCursor.Checked := HL.OldStyleCursor;
  seTabWidth.Value := HL.TabWidth;
end;

procedure TfrmSettings.ImportScheme(const SchemeName: string; Data: TJSONConfig);
var
  NewHL: TKerboscriptHighlighter;
  IndexOfItem: integer;
begin
  IndexOfItem := cbxScheme.Items.IndexOf(SchemeName);
  if (IndexOfItem < 0) or (MessageDlg('Import Scheme', 'You already have a scheme with the name "' +
    SchemeName + LineEnding + '". Would you like to overwrite it?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    NewHL := TKerboscriptHighlighter.Create(Self);
    NewHL.LoadFromJSON(Data, SchemeName);
    if IndexOfItem >= 0 then
    begin
      TKerboscriptHighlighter(cbxScheme.Items.Objects[IndexOfItem]).Free;
      cbxScheme.Items.Objects[IndexOfItem] := NewHL;
      cbxScheme.ItemIndex := IndexOfItem;
    end
    else
    begin
      cbxScheme.AddItem(SchemeName, NewHL);
      cbxScheme.ItemIndex := cbxScheme.Items.Count - 1;
    end;
    NewHL.WriteToJSON(Settings, SchemeName);
  end;
end;

procedure TfrmSettings.OptionChange(Sender: TObject);
var
  Highlighter: TKerboscriptHighlighter;
begin
  if FreezeChange then
    exit;
  Highlighter := TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]);
  if lbxTypes.GetSelectedText = 'Comment' then
    Highlighter.CommentAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'Identifier' then
    Highlighter.IdentifierAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'Keyword' then
    Highlighter.KeywordAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'Number' then
    Highlighter.NumberAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'String' then
    Highlighter.StringAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'Symbol' then
    Highlighter.SymbolAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'Function' then
    Highlighter.FunctionAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'White space' then
  begin
    Highlighter.SpaceAttr := PackAttributes;
    seSample.Color := Highlighter.SpaceAttr.Background;
  end
  else if lbxTypes.GetSelectedText = 'System variable' then
    Highlighter.SystemVariableAttr := PackAttributes
  else if lbxTypes.GetSelectedText = 'Matched bracket' then
  begin
    seSample.BracketMatchColor.Style := [];
    if cbBold.Checked then
      seSample.BracketMatchColor.Style := seSample.BracketMatchColor.Style + [fsBold];
    if cbUnderline.Checked then
      seSample.BracketMatchColor.Style := seSample.BracketMatchColor.Style + [fsUnderline];
    if cbItalic.Checked then
      seSample.BracketMatchColor.Style := seSample.BracketMatchColor.Style + [fsItalic];
    seSample.BracketMatchColor.FrameColor := clrbForeground.Selected;
    seSample.BracketMatchColor.Background := clrbBackground.Selected;
  end;

  seSample.Refresh;
end;

procedure TfrmSettings.btnFontClick(Sender: TObject);
begin
  if chkMonoFonts.Checked then
    FontDialog1.Options := FontDialog1.Options + [fdFixedPitchOnly]
  else
    FontDialog1.Options := FontDialog1.Options - [fdFixedPitchOnly];
  FontDialog1.Font.Assign(seSample.Font);
  if FontDialog1.Execute then
  begin
    seSample.Font.Assign(FontDialog1.Font);
    btnFont.Caption := FontDialog1.Font.Name + ' ' + IntToStr(FontDialog1.Font.Size) + 'pt.';
    seSample.Refresh;
  end;
end;

procedure TfrmSettings.btnSaveClick(Sender: TObject);
begin
  TKerboscriptHighlighter(cbxScheme.Items.Objects[cbxScheme.ItemIndex]).WriteToJSON(Settings, cbxScheme.Text);
  Settings.SetValue('highlighters/inuse', cbxScheme.Text);
end;

procedure TfrmSettings.cbxSchemeChange(Sender: TObject);
begin
  LoadScheme;
  tbtnDelete.Enabled := cbxScheme.ItemIndex >= DefaultCount;
  tbtnResetToDefault.Enabled := cbxScheme.ItemIndex < DefaultCount;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);

  procedure LoadDefault(const SchemeName: string; Index: integer);
  var
    Aux: TKerboscriptHighlighter;
  begin
    Aux := TKerboscriptHighlighter.Create(Self);
    if Index < 0 then
    begin
      Aux.LoadFromJSON(Settings, SchemeName);
      cbxScheme.AddItem(SchemeName, Aux);
    end
    else
    begin
      Aux.ResetToDefaults(SchemeName);
      cbxScheme.Items.InsertObject(Index, SchemeName, Aux);
    end;
  end;

var
  Schemes: TStringList;
  Scheme: string;
  OldTimerDone, SaturnDone, ClearDone: boolean;

begin

  rbMaximized.Checked := Settings.GetValue('window/maximized', False);
  rbRemember.Checked := not rbMaximized.Checked;
  dedtKSPFolder.Directory := Settings.GetValue('kspfolder', '');
  chkRememberOpenFiles.Checked := Settings.GetValue('editor/rememberopenfiles', False);
  chkSyntax.Checked := Settings.GetValue('editor/realtimesyntaxcheck', True);

  Schemes := TStringList.Create;
  Settings.EnumSubKeys('highlighters', Schemes);

  OldTimerDone := False;
  SaturnDone := False;
  ClearDone := False;
  for Scheme in Schemes do
  begin
    LoadDefault(Scheme, -1);
    OldTimerDone := OldTimerDone or (Scheme = 'Old-Timer');
    SaturnDone := SaturnDone or (Scheme = 'Saturn');
    ClearDone := ClearDone or (Scheme = 'Clear');
  end;
  Schemes.Free;

  if not SaturnDone then
    LoadDefault('Saturn', 0);
  if not ClearDone then
    LoadDefault('Clear', 1);
  if not OldTimerDone then
    LoadDefault('Old-Timer', 2);

  cbxScheme.ItemIndex := cbxScheme.Items.IndexOf(Settings.GetValue('highlighters/inuse', 'Saturn'));
  cbxSchemeChange(cbxScheme);

  FreezeChange := False;
  PageControl1.ActivePageIndex := 0;
end;

end.
