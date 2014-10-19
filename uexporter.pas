unit uExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditExport, SynExportHTML, Graphics;

type
  TExporterHTML = class(TSynExporterHTML)
  protected
    function GetHeader: string; override;
    function GetFooter: string; override;
    function ColorToHTML(AColor: TColor): string;
  end;


type
  TExporterBB = class(TSynCustomExporter)
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute({%H-}BackgroundChanged, {%H-}ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    function GetFooter: string; override;
    function GetHeader: string; override;
    function GetFormatName: string; override;
    procedure FormatNewLine; override;
    function ColorToHTML(AColor: TColor): string;
  end;


implementation

{ TExporterBB }

procedure TExporterBB.FormatAfterLastAttribute;
begin
  if fsUnderline in fLastStyle then
    AddData('[/U]');
  if fsItalic in fLastStyle then
    AddData('[/I]');
  if fsBold in fLastStyle then
    AddData('[/B]');
  if fLastFG <> fFont.Color then
    AddData('[/COLOR]');
end;

procedure TExporterBB.FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
  FontStylesChanged: TFontStyles);
begin
  if BackgroundChanged or ForegroundChanged or (FontStylesChanged <> []) then
  begin
    if fsUnderline in fLastStyle then
      AddData('[/U]');
    if fsItalic in fLastStyle then
      AddData('[/I]');
    if fsBold in fLastStyle then
      AddData('[/B]');
  end;
  if (BackgroundChanged or ForegroundChanged) and (fLastFG <> fFont.Color) then
    AddData('[/COLOR]');
end;

procedure TExporterBB.FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
  FontStylesChanged: TFontStyles);
begin
  if (BackgroundChanged or ForegroundChanged) and (fLastFG <> fFont.Color) then
    AddData('[COLOR="' + ColorToHtml(fLastFG) + '"]');
  if BackgroundChanged or ForegroundChanged or (FontStylesChanged <> []) then
  begin
    if fsBold in fLastStyle then
      AddData('[B]');
    if fsItalic in fLastStyle then
      AddData('[I]');
    if fsUnderline in fLastStyle then
      AddData('[U]');
  end;
end;

procedure TExporterBB.FormatBeforeFirstAttribute(BackgroundChanged, ForegroundChanged: boolean;
  FontStylesChanged: TFontStyles);
begin
  AddData('[COLOR="' + ColorToHtml(fLastFG) + '"]');
  if FontStylesChanged <> [] then
  begin
    if fsBold in fLastStyle then
      AddData('[B]');
    if fsItalic in fLastStyle then
      AddData('[I]');
    if fsUnderline in fLastStyle then
      AddData('[U]');
  end;
end;

function TExporterBB.GetFooter: string;
begin
  Result := '[/CODE]';
end;

function TExporterBB.GetHeader: string;
begin
  Result := '[CODE]';
end;

function TExporterBB.GetFormatName: string;
begin
  Result := 'BB';
end;

procedure TExporterBB.FormatNewLine;
begin
  AddNewLine;
end;

function TExporterBB.ColorToHTML(AColor: TColor): string;
begin
  Result := '#' + IntToHex(Red(AColor), 2) + IntToHex(green(AColor), 2) + IntToHex(blue(AColor), 2);
end;

{ TExporterHTML }

function TExporterHTML.GetHeader: string;
begin
  Result := '<div style= "background-color: ' + ColorToHTML(Color) + ';"><pre><code>';
end;

function TExporterHTML.GetFooter: string;
begin
  Result := '</code></pre></div>';
end;


function TExporterHTML.ColorToHTML(AColor: TColor): string;
begin
  Result := '#' + IntToHex(Red(AColor), 2) + IntToHex(green(AColor), 2) + IntToHex(blue(AColor), 2);
end;

end.


