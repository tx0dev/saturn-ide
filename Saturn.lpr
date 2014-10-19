{
Saturn kOS IDE
 Copyright (C) 2013  Marcelo Limori

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}
program Saturn;

{$mode objfpc}{$H+}

uses
  {$ifdef DEBUG}
  lclproc,
  {$endif}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uSettings, ufSettings, uEditorFile, vinfo,
  uPackConfigurations, uAbout, uHashTable, uPacker, uExporter, uObjects, 
uSearchReplace, uVariableFetcher, uExternalVariables, uContainers, uPackerResults, uKerboscriptHighlighter, uTokens, 
uSyntaxCheck, uMarkup, uMarkupList, uSyntaxCheckingErrors;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSearchReplace, frmSearchReplace);
  Application.Run;
end.

