unit uHashTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

{ pepe }

 { TStringHashTable }

 TStringHashTable = class(TFPStringHashTable)
    private
      KeyList: TStrings;
    public
      procedure Delete(const aKey: string); override;
      procedure Add(const aKey,aItem: string); override;
      function GetKeyList: TStrings;
      constructor Create;

end;

implementation


procedure TStringHashTable.Delete(const aKey: string);
begin
  inherited Delete(aKey);
  KeyList.Delete(KeyList.IndexOf(aKey));
end;

procedure TStringHashTable.Add(const aKey, aItem: string);
begin
  inherited Add(aKey, aItem);
  KeyList.Add(aKey);
end;

constructor TStringHashTable.Create;
begin
  inherited Create;
  KeyList := TStringList.Create;
end;

function TStringHashTable.GetKeyList: TStrings;
begin
  Result := TStringList.Create;
  Result.Assign(KeyList);
end;

end.

