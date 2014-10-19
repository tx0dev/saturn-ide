unit uSearchReplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, FileCtrl, SynEditTypes;

type

  { TfrmSearchReplace }

  TfrmSearchReplace = class(TForm)
    btnClose: TBitBtn;
    btnFind: TBitBtn;
    chkPrompt: TCheckBox;
    chkOptRegexMultiline: TCheckBox;
    chkOptRegex: TCheckBox;
    chkOptWholeWords: TCheckBox;
    chkOptMatchCase: TCheckBox;
    chkReplace: TCheckBox;
    cbxReplace: TComboBox;
    cbxSearch: TFilterComboBox;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    rbDirBackward: TRadioButton;
    rbDirForward: TRadioButton;
    rbScopeDocument: TRadioButton;
    rbScopeSelection: TRadioButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure cbxSearchChange(Sender: TObject);
    procedure cbxSearchKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure chkReplaceChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure rbOriginCursorChange(Sender: TObject);
  private
    NewSearch: boolean;
    function PackOptions: TSynSearchOptions;
  public
    procedure SetOptions(const ToFind: string);
  end;

var
  frmSearchReplace: TfrmSearchReplace;

implementation

{$R *.lfm}

uses uSettings, uMain, Variants, Math, LCLType;

{ TfrmSearchReplace }

procedure TfrmSearchReplace.chkReplaceChange(Sender: TObject);
begin
  cbxReplace.Enabled := chkReplace.Checked;
  if chkReplace.Checked then
    btnFind.Caption := 'Replace'
  else
    btnFind.Caption := 'Find';
end;

procedure TfrmSearchReplace.FormActivate(Sender: TObject);
begin
  AlphaBlendValue := 255;
end;

procedure TfrmSearchReplace.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Idx: integer;
begin
  for Idx := 0 to Min(9, cbxSearch.Items.Count - 1) do
    Settings.SetValue('findreplace/searchedterms/' + IntToStr(Idx), cbxSearch.Items[Idx]);

  for Idx := 0 to Min(9, cbxReplace.Items.Count - 1) do
    Settings.SetValue('findreplace/replacedterms/' + IntToStr(Idx), cbxReplace.Items[Idx]);
end;

procedure TfrmSearchReplace.FormCreate(Sender: TObject);
var
  Item: string;
  Items: TStringList;
begin
  Items := TStringList.Create;

  Settings.EnumValues('findreplace/searchedterms', Items);
  for Item in Items do
    cbxSearch.Items.Add(Settings.GetValue('findreplace/searchedterms/' + Item, ''));

  Settings.EnumValues('findreplace/replacedterms', Items);
  for Item in Items do
    cbxReplace.Items.Add(Settings.GetValue('findreplace/searchedterms/' + Item, ''));

  Items.Free;
end;

procedure TfrmSearchReplace.FormDeactivate(Sender: TObject);
begin
  AlphaBlendValue := 150;
end;

procedure TfrmSearchReplace.rbOriginCursorChange(Sender: TObject);
begin
  NewSearch := True;
end;


function TfrmSearchReplace.PackOptions: TSynSearchOptions;
begin
  Result := [];
  if rbScopeSelection.Checked then
    Result := Result + [ssoSelectedOnly];

  if rbDirBackward.Checked then
    Result := Result + [ssoBackwards];

  if chkOptMatchCase.Checked then
    Result := Result + [ssoMatchCase];

  if chkOptRegex.Checked then
    Result := Result + [ssoRegExpr];

  if chkOptRegexMultiline.Checked then
    Result := Result + [ssoRegExprMultiLine];

  if chkOptWholeWords.Checked then
    Result := Result + [ssoWholeWord];

  if chkReplace.Checked then
    Result := Result + [ssoReplace];

  if chkPrompt.Checked then
    Result := Result + [ssoPrompt];

  if not NewSearch then
    Result := Result + [ssoFindContinue];
end;


procedure TfrmSearchReplace.btnFindClick(Sender: TObject);
begin
  if cbxSearch.Items.IndexOf(cbxSearch.Text) < 0 then
    cbxSearch.Items.Insert(0, cbxSearch.Text);

  frmMain.FindReplace(cbxSearch.Text, cbxReplace.Text, PackOptions);

  NewSearch := False;
end;

procedure TfrmSearchReplace.cbxSearchChange(Sender: TObject);
begin
  NewSearch := True;
end;

procedure TfrmSearchReplace.cbxSearchKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    btnFind.SetFocus;
    btnFindClick(btnFind);
  end;
end;

procedure TfrmSearchReplace.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSearchReplace.SetOptions(const ToFind: string);
begin
  chkOptMatchCase.Checked := False;
  chkOptWholeWords.Checked := False;
  chkOptRegex.Checked := False;
  chkOptRegexMultiline.Checked := False;
  rbScopeDocument.Checked := True;
  rbDirForward.Checked := True;
  cbxSearch.Text := ToFind;
  chkReplace.Checked := False;
  chkPrompt.Checked := True;
  chkReplaceChange(chkReplace);
end;

end.
