unit uMarkup;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
interface

uses
  Classes, SysUtils, SynEditMarkupHighAll, SynEditMiscClasses, uTokens, uContainers;


type

{ TMarkupRecord }

 TMarkupRecord = record
      StartPoint, EndPoint: TTokenPoint;
      Message: string;
      class operator =(i1, i2: TMarkupRecord): boolean;
    end;

type TMarkupList = specialize TGenericList<TMarkupRecord>;

type

{ TMarkup }

 TMarkup = class(TSynEditMarkupHighlightMatches)

 private
     UpdatingLine: integer;
     ErrorMarks: TMarkupList;
   public
     constructor Create(ASynEdit : TSynEditBase);
     procedure ClearMarks;
     procedure AddMark(Start, Finish: TTokenPoint; const Message: string);
     function GetTextForMarkup(const Location: TPoint): string;
     procedure BeginUpdates;
     function FinishedUpdates: boolean;
end;

function MarkupRecord(const StartPoint, EndPoint: TTokenPoint; const Message: string): TMarkupRecord;

implementation

function PointBetween(const Point: TPoint; const P1, P2: TTokenPoint): boolean;
begin
  Result := (Point.Y >= P1.Y) and (Point.Y <= P2.Y) and
            ((Point.X >= P1.X) or (Point.Y > P1.Y)) and
            ((Point.X <= P2.X) or (Point.Y < P2.Y));
end;

function MarkupRecord(const StartPoint, EndPoint: TTokenPoint; const Message: string): TMarkupRecord;
begin
  Result.StartPoint := StartPoint;
  Result.EndPoint := EndPoint;
  Result.Message := Message;
end;

{ TMarkupRecord }

class operator TMarkupRecord. = (i1, i2: TMarkupRecord): boolean;
begin
  Result := i1 = i2;
end;

constructor TMarkup.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  UpdatingLine := -1;
  ErrorMarks := TMarkupList.Create;
end;

procedure TMarkup.ClearMarks;
begin
  Matches.Count := 0;
end;

procedure TMarkup.AddMark(Start, Finish: TTokenPoint; const Message: string);
var StartPt, EndPt: TPoint;
begin
  { TODO : ? }
  //if not Assigned(FMatches) then Exit;
  StartPt := Point(Start.X, Start.Y + 1);
  if Finish.X - Start.X = 0 then
    Finish.X := Finish.X + 1;
  EndPt := Point(Finish.X, Finish.Y + 1);

  Matches.StartPoint[Matches.Count] := StartPt;
  Matches.EndPoint[Matches.Count - 1]:= EndPt;

  ErrorMarks.Add(MarkupRecord(Start, Finish, Message));
end;

function TMarkup.GetTextForMarkup(const Location: TPoint): string;
var Mark: TMarkupRecord;
    Found: boolean;
    m: string;
begin
  Found := False;
  m := '';
  ErrorMarks.PrepareIterator;
  while ErrorMarks.GetNextItem(Mark) and not Found do
    begin
    Found := PointBetween(Location, Mark.StartPoint, Mark.EndPoint);
    if Found then m:= Mark.Message;
    end;
  Result := m;
end;

procedure TMarkup.BeginUpdates;
begin
  ErrorMarks.Clear;

end;

function TMarkup.FinishedUpdates: boolean;
var Idx: integer;
begin
  Result := False;
  if UpdatingLine < 0 then Exit;
  for Idx := UpdatingLine to Lines.Count - 1 do
  begin

  end;
  InvalidateSynLines(0,Lines.Count - 1);
  UpdatingLine := -1;
end;

end.

