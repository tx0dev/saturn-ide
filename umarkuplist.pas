unit uMarkupList;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uContainers, uTokens;


type

{ TMarkupItem }

 TMarkupItem = record
  StartPoint, EndPoint: TTokenPoint;
  Message: string;
  class operator =(i1, i2: TMarkupItem): boolean;
end;

function MarkupItem(const StartPoint, EndPoint: TTokenPoint; const Message: string): TMarkupItem;


type

{ pepe }

 pepe = class
  private type TMarkupLine = specialize TGenericList<TMarkupItem>;
  private type TMarkupLineList = array of TMarkupLine;
  private
    Items: TMarkupLineList;
    procedure Grow(const UpTo: integer);
  public
    constructor Create;
    procedure Insert(const Item: TMarkupITem; const Index: integer);
    procedure ClearLines;
end;



implementation

function MarkupItem(const StartPoint, EndPoint: TTokenPoint; const Message: string): TMarkupItem;
begin
  Result.StartPoint := StartPoint;
  Result.EndPoint := EndPoint;
  Result.Message := Message;
end;

{ TMarkupItem }

class operator TMarkupItem. = (i1, i2: TMarkupItem): boolean;
begin
  Result := (i1.EndPoint = i2.EndPoint) and (i1.StartPoint = i2.StartPoint) and (i1.Message = i2.Message);
end;

{ pepe }

procedure pepe.Grow(const UpTo: integer);
var Idx: integer;
begin
  SetLength(Items, UpTo + 10);
  for Idx := 0 to UpTo + 9 do
    Items[Idx] := TMarkupLine.Create(3, 3);
end;

constructor pepe.Create;
var Idx: integer;
begin
  SetLength(Items, 100);
  for Idx := 0 to 99 do
    Items[Idx] := TMarkupLine.Create(3, 3);
end;

procedure pepe.Insert(const Item: TMarkupITem; const Index: integer);
begin
  if Length(Items) <= Index then
    Grow(Index);
  Items[Index].Add(Item);
end;

procedure pepe.ClearLines;
var Idx: Integer;
begin
  for Idx := 0 to Length(Items) do
    Items[Idx].Clear;
end;

end.

