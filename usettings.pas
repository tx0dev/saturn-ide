unit uSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterAny, jsonConf, uMain, SynEdit, uKerboscriptHighlighter;

procedure ReadSettings(Highlighter: TKerboscriptHighlighter; MainForm: TfrmMain);

var
  Settings: TJSONConfig;

implementation

uses Graphics, Forms;

procedure WriteHighlighterSection(Section: string; Attributes: TLazSynCustomTextAttributes);
begin
  Settings.OpenKey('editor/highlighter/' + Section, True);
  Settings.SetValue('background', Attributes.Background);
  Settings.SetValue('foreground', Attributes.Foreground);
  Settings.SetValue('style', longint(Attributes.Style));
  Settings.CloseKey;
end;

function ReadHighlighterSection(Section: string; DefaultValues: TSynHighlighterAttributes): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create('', '');
  Settings.OpenKey('editor/highlighter/' + Section, True);
  Result.Foreground := Settings.GetValue('foreground', DefaultValues.Foreground);
  Result.Background := Settings.GetValue('background', DefaultValues.Background);
  Result.Style := TFontStyles(Settings.GetValue('style', longint(DefaultValues.Style)));
  Settings.CloseKey;
end;

procedure ReadSettings(Highlighter: TKerboscriptHighlighter; MainForm: TfrmMain);
var Used: string;
begin
  //Syntax highlighting
  Used := Settings.GetValue('highlighters/inuse', 'Saturn');
  Highlighter.LoadFromJSON(Settings, Used);

  Settings.OpenKey('editor/highlighter/matchedbracket', True);
  MainForm.seHidden.BracketMatchColor.Foreground :=
    Settings.GetValue('foreground', MainForm.seHidden.BracketMatchColor.Foreground);
  MainForm.seHidden.BracketMatchColor.Background :=
    Settings.GetValue('background', MainForm.seHidden.BracketMatchColor.Background);
  MainForm.seHidden.BracketMatchColor.Style :=
    TFontStyles(Settings.GetValue('style', longint(MainForm.seHidden.BracketMatchColor.Style)));
  Settings.CloseKey;

  MainForm.seHidden.Font.Name := Settings.GetValue('editor/font/name', MainForm.seHidden.Font.Name);
  MainForm.seHidden.Font.Size := Settings.GetValue('editor/font/size', MainForm.seHidden.Font.Size);
  MainForm.seHidden.Font.Quality := TFontQuality(Settings.GetValue('editor/font/quality', ord(MainForm.seHidden.Font.Quality)));
  MainForm.seHidden.ExtraCharSpacing := Settings.GetValue('editor/font/charsep', 0);
  MainForm.seHidden.Color := Settings.GetValue('editor/highlighter/whitespace', MainForm.seHidden.Color);

  if Settings.GetValue('window/maximized', False) or Settings.GetValue('window/ismaximized', False) then
    MainForm.WindowState := wsMaximized
  else
  begin
    MainForm.Left := Settings.GetValue('window/x', (Screen.Width - 800) div 2);
    MainForm.Top := Settings.GetValue('window/y', (Screen.Height - 600) div 2);
    MainForm.Width := Settings.GetValue('window/width', 800);
    MainForm.Height := Settings.GetValue('window/height', 600);
  end;
end;


initialization
  Settings := TJSONConfig.Create(nil);
  Settings.Filename := 'Saturn.cfg';

finalization
  Settings.Flush;
  Settings.Free;

end.
