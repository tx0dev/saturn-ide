unit uPackerResults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfrmPackerResults }

  TfrmPackerResults = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
  private
    { private declarations }
  public
    procedure AdjustNoCollisions;
    { public declarations }
  end;

var
  frmPackerResults: TfrmPackerResults;

implementation

{$R *.lfm}

{ TfrmPackerResults }


procedure TfrmPackerResults.AdjustNoCollisions;
begin
  Height := Height - Memo1.Height - Label2.Height;
  Button1.Top := Button1.Top - Memo1.Height - Label2.Height;
  Memo1.Visible := False;
  Label2.Visible := False;
end;

end.

