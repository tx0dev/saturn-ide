unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, vinfo;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblVersion: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

  VersionInfo: TVersionInfo;

implementation

{$R *.lfm}

uses lclintf;

{ TfrmAbout }

procedure TfrmAbout.Label3Click(Sender: TObject);
begin
  OpenURL('http://www.famfamfam.com/lab/icons/silk/');
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  lblVersion.Caption := 'Version ' + IntToStr(VersionInfo.FixedInfo.FileVersion[0]) + '.' +
    IntToStr(VersionInfo.FixedInfo.FileVersion[1]) + '.' +
    IntToStr(VersionInfo.FixedInfo.FileVersion[2]) + '.' + IntToStr(VersionInfo.FixedInfo.FileVersion[3]);
end;

procedure TfrmAbout.Label2Click(Sender: TObject);
begin
    OpenURL('http://forum.kerbalspaceprogram.com/threads/62834-Saturn-yet-another-kOS-IDE');
end;

end.

