unit uPackConfigurations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, EditBtn;

type

  { TfrmPackConfigs }

  TfrmPackConfigs = class(TForm)
    btnClose: TBitBtn;
    btnNewConf: TBitBtn;
    btnDelConf: TBitBtn;
    btnPack: TBitBtn;
    btnSave: TBitBtn;
    btnAddTag: TButton;
    btnRemoveTag: TButton;
    cbDestination: TComboBox;
    chkCollision: TCheckBox;
    chkRemoveTag: TCheckBox;
    chkArchiveSameName: TCheckBox;
    chkRemoveComments: TCheckBox;
    chkRemoveLeadingSpaces: TCheckBox;
    fedtFile: TFileNameEdit;
    fedtArchive: TFileNameEdit;
    Label2: TLabel;
    Label3: TLabel;
    lbxConfigs: TListBox;
    lbxTags: TListBox;
    pnlConfiguration: TPanel;
    procedure btnAddTagClick(Sender: TObject);
    procedure btnDelConfClick(Sender: TObject);
    procedure btnNewConfClick(Sender: TObject);
    procedure btnPackClick(Sender: TObject);
    procedure btnRemoveTagClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbDestinationChange(Sender: TObject);
    procedure chkArchiveSameNameChange(Sender: TObject);
    procedure chkCollisionChange(Sender: TObject);
    procedure chkRefactorVariablesChange(Sender: TObject);
    procedure chkRemoveCommentsChange(Sender: TObject);
    procedure chkRemoveLeadingSpacesChange(Sender: TObject);
    procedure chkRemoveTagChange(Sender: TObject);
    procedure fedtArchiveChange(Sender: TObject);
    procedure fedtFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxConfigsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure lbxTagsSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    procedure ShowResults(const ProgramName, Destination: string; OriginalSize, PackedSize: integer; Collisions: boolean;
      const CollisionText, SyntaxCheckText: string);
  public
    procedure LoadConfigValues(const ConfigName: string);
    procedure LoadConfigs;
  end;

var
  frmPackConfigs: TfrmPackConfigs;

implementation

{$R *.lfm}

{ TfrmPackConfigs }

uses uSettings, uPacker, uMain, uEditorFile, strutils, uObjects, uVariableFetcher, uPackerResults;

procedure TfrmPackConfigs.btnSaveClick(Sender: TObject);
var
  Idx: integer;
begin
  Settings.OpenKey('packerconfigs/' + lbxConfigs.GetSelectedText, True);
  Settings.SetValue('destination/type', cbDestination.Caption);
  if cbDestination.Text = 'Archive' then
  begin
    Settings.SetValue('destination/Archive/samename', chkArchiveSameName.Checked);
    Settings.SetValue('destination/Archive/filename', fedtArchive.FileName);
  end
  else if cbDestination.Text = 'File' then
    Settings.SetValue('destination/File/filename', fedtFile.FileName);

  Settings.SetValue('removecomments', chkRemoveComments.Checked);
  Settings.SetValue('removetrailingspaces', chkRemoveLeadingSpaces.Checked);

  Settings.DeletePath('removetags');
  Settings.SetValue('removetags/enabled', chkRemoveTag.Checked);
  for Idx := 0 to lbxTags.Items.Count - 1 do
    Settings.SetValue('removetags/tags/' + IntToStr(Idx), lbxTags.Items[Idx]);

  Settings.SetValue('collisioncheck', chkCollision.Checked);

  Settings.CloseKey;
  btnSave.Enabled := False;
end;

procedure TfrmPackConfigs.cbDestinationChange(Sender: TObject);
begin
  btnSave.Enabled := True;
  fedtFile.Visible := cbDestination.Text = 'File';
  chkArchiveSameName.Visible := cbDestination.Text = 'Archive';
  fedtArchive.Visible := cbDestination.Text = 'Archive';
  fedtArchive.Enabled := not chkArchiveSameName.Checked;
end;

procedure TfrmPackConfigs.chkArchiveSameNameChange(Sender: TObject);
begin
  fedtArchive.Enabled := not chkArchiveSameName.Checked;
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.chkCollisionChange(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.chkRefactorVariablesChange(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.chkRemoveCommentsChange(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.chkRemoveLeadingSpacesChange(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.chkRemoveTagChange(Sender: TObject);
begin
  lbxTags.Enabled := chkRemoveTag.Checked;
  btnAddTag.Enabled := chkRemoveTag.Checked;
  btnRemoveTag.Enabled := chkRemoveTag.Checked;
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.fedtArchiveChange(Sender: TObject);
begin
  fedtArchive.FileName := ExtractFileNameOnly(fedtArchive.FileName);
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.fedtFileChange(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.FormShow(Sender: TObject);
begin
  btnSave.Enabled := False;
end;

procedure TfrmPackConfigs.lbxConfigsSelectionChange(Sender: TObject; User: boolean);
begin
  pnlConfiguration.Enabled := lbxConfigs.ItemIndex >= 0;
  btnPack.Enabled := lbxConfigs.ItemIndex >= 0;
  LoadConfigValues(lbxConfigs.GetSelectedText);
  btnDelConf.Enabled := lbxConfigs.ItemIndex >= 0;
  btnSave.Enabled := False;
end;

procedure TfrmPackConfigs.lbxTagsSelectionChange(Sender: TObject; User: boolean);
begin
  btnRemoveTag.Enabled := lbxTags.ItemIndex >= 0;
end;

procedure TfrmPackConfigs.ShowResults(const ProgramName, Destination: string; OriginalSize, PackedSize: integer; Collisions: boolean;
  const CollisionText, SyntaxCheckText: string);
begin
  frmPackerResults := TfrmPackerResults.Create(nil);
  try
    frmPackerResults.Label1.Caption := 'Packed ' + ProgramName + ' into ' + Destination + '. Original size: ' + IntToStr(OriginalSize) +
                                       ', packed: ' + IntToStr(PackedSize) + '.';
    if Collisions then
      frmPackerResults.Memo1.Text := CollisionText;
    if Length(SyntaxCheckText) > 0 then
      frmPackerResults.Memo1.Text := frmPackerResults.Memo1.Text + SyntaxCheckText;
    if Length(frmPackerResults.Memo1.Text) = 0 then
      frmPackerResults.AdjustNoCollisions;
    frmPackerResults.ShowModal;
  finally
    frmPackerResults.Free;
  end;
end;


procedure TfrmPackConfigs.btnNewConfClick(Sender: TObject);
var
  New: string;
begin
  New := '';
  if InputQuery('New configuration', 'Name:', New) then
    if lbxConfigs.Items.IndexOf(New) >= 0 then
      MessageDlg('New configuration', 'A configuration by that name already exists.', mtError, [mbOK], 0)
    else
    begin
      lbxConfigs.AddItem(New, nil);
      lbxConfigs.ItemIndex := lbxConfigs.Items.IndexOf(New);
      lbxConfigsSelectionChange(lbxConfigs, False);
      btnSaveClick(btnSave);
      btnPack.Enabled := True;
    end;
end;

procedure TfrmPackConfigs.btnDelConfClick(Sender: TObject);
begin
  if MessageDlg('Program packer', 'Are you sure you want to delete this configuration?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Settings.DeleteValue('packerconfigs/' + lbxConfigs.GetSelectedText);
    lbxConfigs.Items.Delete(lbxConfigs.ItemIndex);
    lbxConfigs.ItemIndex := lbxConfigs.Items.Count - 1;
    lbxConfigsSelectionChange(lbxConfigs, False);
  end;
end;

procedure TfrmPackConfigs.btnAddTagClick(Sender: TObject);
var
  TagName: string;
begin
  TagName := '';
  if InputQuery('Remove lines after', 'Tag name', TagName) then
    if AnsiStartsStr('//', TagName) then
      lbxTags.AddItem(RightStr(TagName, length(TagName) - 2), nil)
    else
      lbxTags.AddItem(TagName, nil);
  lbxTagsSelectionChange(lbxTags, False);
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.btnPackClick(Sender: TObject);
var
  Code: TStrings;
  rc, rts: boolean;
  OriginalSize, PackedSize: integer;
  DestinationStr, Aux: string;
  CollisionCheckText, SyntaxCheckText: string;

begin
  Cursor := crHourGlass;
  try
    try
    Code := TEditorFile(frmMain.pcMDIFaker.ActivePage).Editor.Lines;
    rc := chkRemoveComments.Checked;
    rts := chkRemoveLeadingSpaces.Checked;
    if cbDestination.Text = 'Archive' then
    begin
      if chkArchiveSameName.Checked then
        Aux := ExtractFileNameOnly(TEditorFile(frmMain.pcMDIFaker.ActivePage).FileName)

      else
        Aux := ExtractFileNameOnly(fedtArchive.FileName);
      PackToArchive(Code, Aux, rc, rts, lbxTags.Items, OriginalSize, PackedSize);
      DestinationStr := 'archive (' + Aux + ')';
    end
    else if cbDestination.Text = 'Clipboard' then
    begin
      PackToClipboard(Code, rc, rts, lbxTags.Items, OriginalSize, PackedSize);
      DestinationStr := 'clipboard';
    end
    else if cbDestination.Text = 'File' then
    begin
      PackToFile(Code, fedtFile.Text, rc, rts, lbxTags.Items, OriginalSize, PackedSize);
      DestinationStr := fedtFile.Text;
    end
    else if cbDestination.Text = 'Sub-assembly' then
    begin
    end
    else if cbDestination.Text = 'Vessel' then
    begin
    end;

    CollisionCheckText := '';
    if chkCollision.Checked then
      RunDeepCollisionScan(Code, TEditorFile(frmMain.pcMDIFaker.ActivePage).FileName, CollisionCheckText);

    SyntaxCheck(SyntaxCheckText);

    ShowResults(ExtractFileNameOnly(TEditorFile(frmMain.pcMDIFaker.ActivePage).FileName), DestinationStr,
      OriginalSize, PackedSize, chkCollision.Checked, CollisionCheckText, SyntaxCheckText);
  except
    on e: Exception do
      MessageDlg('Program Packer', E.Message, mtError, [mbOk], 0);
  end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TfrmPackConfigs.btnRemoveTagClick(Sender: TObject);
begin
  lbxTags.Items.Delete(lbxTags.ItemIndex);
  lbxTagsSelectionChange(lbxTags, False);
  btnSave.Enabled := True;
end;

procedure TfrmPackConfigs.LoadConfigValues(const ConfigName: string);
var
  Tags: TStringList;
  xTag: string;
begin
  Settings.OpenKey('packerconfigs/' + ConfigName, True);
  cbDestination.ItemIndex := cbDestination.Items.IndexOf(Settings.GetValue('destination/type', 'Archive'));
  cbDestinationChange(cbDestination);
  chkRemoveComments.Checked := Settings.GetValue('removecomments', False);
  chkRemoveLeadingSpaces.Checked := Settings.GetValue('removetrailingspaces', True);
  if Settings.GetValue('destination/type', 'Archive') = 'Archive' then
  begin
    chkArchiveSameName.Checked := Settings.GetValue('destination/Archive/samename', False);
    fedtArchive.FileName := Settings.GetValue('destination/Archive/filename', '');
    fedtArchive.Enabled := not chkArchiveSameName.Checked;
    fedtArchive.Visible := True;
  end
  else if Settings.GetValue('destination/type', 'Archive') = 'File' then
  begin
    fedtFile.FileName := Settings.GetValue('destination/File/filename', '');
    fedtFile.Visible := True;
    fedtFile.Enabled := True;
  end;

  chkRemoveTag.Checked := Settings.GetValue('removetags/enabled', False);
  Tags := TStringList.Create;
  Settings.EnumValues('removetags/tags', Tags);
  for xTag in Tags do
    lbxTags.Items.Add(Settings.GetValue('removetags/tags/' + xTag, ''));
  chkRemoveTagChange(chkRemoveTag);

  chkCollision.Checked := Settings.GetValue('collisioncheck', False);

  Settings.CloseKey;
end;

procedure TfrmPackConfigs.LoadConfigs;
var
  Configs: TStrings;
  Config: string;
begin
  Configs := TStringList.Create;
  Settings.EnumSubKeys('/packerconfigs/', Configs);
  for Config in Configs do
    lbxConfigs.AddItem(Config, nil);
  if Configs.Count > 0 then
  begin
    lbxConfigs.ItemIndex := 0;
    LoadConfigValues(lbxConfigs.Items[0]);
    btnSave.Enabled := False;
  end;
  fedtArchive.InitialDir := Settings.GetValue('kspfolder', '') + PathDelim + 'Plugins' + PathDelim +
    'PluginData' + PathDelim + 'Archive';
end;

end.
