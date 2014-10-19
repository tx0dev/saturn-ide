unit uContainers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;


type ENoSuchItem = class(Exception);
     EListIndexOutOfBounds = class(Exception);

type

 { TGenericHashTable }

 generic TGenericHashTable<TKey, TValue> = class
  private
    type
      PItem = ^TItem;
      PKey = ^TKey;
      PValue = ^TValue;
      TItem = record
        Key: PKey;
        Value: PValue;
        Next: PItem;
      end;
    type THashFunc = function(const Key: TKey; TableSize: Longword): Longword;
    type TCompFunc = function(const Key1, Key2: TKey): boolean;

  private
    FItems: array of PItem;
    FTableSize: Longword;
    FHashFunction: THashFunc;
    FCompFunc: TCompFunc;
    IterationIndex: Longword;
    IterationItem: PItem;
    function Get(Key: TKey): TValue;
    procedure Put(Key: TKey; Value: TValue);
    procedure InsertItem(Index: Longword; Key: TKey; Value: TValue);
    function InternalGet(Key: TKey): PItem;
    function CalcIndex(Key: TKey): Longword;
  public
    constructor Create;
    constructor Create(HashFunction: THashFunc; TableSize: Longword);
    destructor Destroy; override;
    function Contains(Key: TKey): boolean;
    procedure Remove(Key: TKey);
    property Items[Key: TKey]: TValue read Get write Put; default;
    procedure PrepareIterator;
    function GetNextKey(out Key: TKey): boolean;
    procedure SetCompFunction(CompFunc: TCompFunc);
        procedure Clear;
    procedure AddFrom(Source: TGenericHashTable);
end;

type

{ TGenericList }

 generic TGenericList<TItem> = class
   private
     FItems: array of TItem;
     FGrowBy: integer;
     FItemCount: integer;
     IteratorIndex: integer;
     function GetItem(Index: integer): TItem;
     procedure SetItem(Index: integer; AValue: TItem);
     procedure CheckIndex(Index: integer);
  public
    constructor Create;
    constructor Create(StartingSize, GrowBy: integer);
    property Items[Index: integer]: TItem read GetItem write SetItem; default;
    function Add(AValue: TItem): integer;
    procedure Remove(Index: integer);
    procedure Clear;
    procedure PrepareIterator;
    function GetNextItem(out Item: TItem): boolean;
    property ItemCount: integer read FItemCount;
    function IndexOf(const {%H-}AValue: TItem): integer;
end;


type

 { TGenericStack }

 generic TGenericStack<TItem> = class
   private
        type TItemList = specialize TGenericList<TItem>;
   private
     Items: TItemList;
   public
     constructor Create;
     procedure Push({%H-}Item: TItem);
     function Pop: TItem;
     function Top: TITem;
     procedure Clear;
     function HasItems: boolean;
 end;

implementation

uses dialogs;

{ TGenericStack }

constructor TGenericStack.Create;
begin
  Items := TItemList.Create(20, 10);
end;

procedure TGenericStack.Push(Item: TItem);
begin
  Items.Add(Item);
end;

function TGenericStack.Pop: TItem;
begin
  Result := Items[Items.ItemCount - 1];
  Items.Remove(Items.ItemCount - 1);
end;

function TGenericStack.Top: TITem;
begin
  Result := Items[Items.ItemCount - 1];
end;

procedure TGenericStack.Clear;
begin
  Items.Clear;
end;

function TGenericStack.HasItems: boolean;
begin
  Result := Items.ItemCount > 0;
end;

{ TGenericList }

function TGenericList.GetItem(Index: integer): TItem;
begin
  CheckIndex(Index);
  Result := FItems[Index];
end;

procedure TGenericList.SetItem(Index: integer; AValue: TItem);
begin
  CheckIndex(Index);
  FItems[Index] := AValue;
end;

procedure TGenericList.CheckIndex(Index: integer);
begin
  if Index >= FItemCount then
      raise EListIndexOutOfBounds.Create('Index ' + IntToStr(Index) + ' outside of bounds [0..' + IntToStr(FItemCount - 1) + ']');
end;

constructor TGenericList.Create;
begin
  Create(1024, 1024);
end;

constructor TGenericList.Create(StartingSize, GrowBy: integer);
begin
  SetLength(FItems, StartingSize);
  FItemCount := 0;
  FGrowBy := GrowBy;
end;


function TGenericList.Add(AValue: TItem): integer;
begin
  if ItemCount = Length(FItems) then
    SetLength(FItems, Length(FItems) + FGrowBy);
  FItems[FItemCount] := AValue;
  Result := FItemCount;
  Inc(FItemCount);
end;

procedure TGenericList.Remove(Index: integer);
var Idx: integer;
begin
  CheckIndex(Index);
  if Index > FItemCount - 2 then
    for Idx := Index to FItemCount - 2 do
      FItems[Idx] := FItems[Idx + 1];
  Dec(FItemCount);
end;

procedure TGenericList.Clear;
begin
  FItemCount := 0;
end;

procedure TGenericList.PrepareIterator;
begin
  IteratorIndex := 0;
end;

function TGenericList.GetNextItem(out Item: TItem): boolean;
begin
  if IteratorIndex >= ItemCount then
    Result := False
  else
  begin
    Item := FItems[IteratorIndex];
    Inc(IteratorIndex);
    Result := True;
  end;
end;

function TGenericList.IndexOf(const AValue: TItem): integer;
begin
  Result := FItemCount - 1;
  while Result >= 0 do
    if FItems[Result] = AValue then
      Exit
    else
      Dec(Result);
end;

{ TGenericHashTable }

function TGenericHashTable.Get(Key: TKey): TValue;
var v: PValue;
    i: PItem;
begin
  i := InternalGet(Key);
  if i <> nil then
    begin
    Result := i^.Value^;
    Move(i^.Value^, Result, sizeof(v));
    end
  else
    raise ENoSuchItem.Create('Key not found.');
end;

procedure TGenericHashTable.Put(Key: TKey; Value: TValue);
begin
  InsertItem(CalcIndex(Key), Key, Value);
end;

procedure TGenericHashTable.InsertItem(Index: Longword; Key: TKey; Value: TValue);
var Item: PItem;
begin
  Item := InternalGet(Key);
  if Item <> nil then
  begin
    Item^.Value^ := Value;
    Move(Value, Item^.Value^, sizeof(Value));
  end
  else
  begin
    New(Item);
    New(Item^.Key);
    Item^.Key^:= Key;
    Move(Key, Item^.Key^, sizeof(Key));
    New(Item^.Value);
    Item^.Value^:= Value;
    Move(Value, Item^.Value^, sizeof(Value));
    Item^.Next := nil;
    if FItems[Index] = nil then
      FItems[Index] := Item
    else
    begin
      Item^.Next := FItems[Index];
      FItems[Index] := Item;
    end;
  end;
end;


function TGenericHashTable.InternalGet(Key: TKey): PItem;
begin
  Result := FItems[CalcIndex(Key)];
  if FCompFunc<> nil then
    while (Result <> nil) and not FCompFunc(Result^.Key^, Key) do
      Result := Result^.Next
  else
    while (Result <> nil) and not (Result^.Key^ = Key) do
      Result := Result^.Next;

{  while (Result <> nil) and (not ((FCompFunc <> nil) and (FCompFunc(Result^.Key^, Key)))
                              or not (Result^.Key^ = Key)) do
    Result := Result^.Next;}
end;

function TGenericHashTable.Contains(Key: TKey): boolean;
begin
  Result := InternalGet(Key) <> nil;
end;

procedure TGenericHashTable.Remove(Key: TKey);
var Aux1, Aux2: PItem;
begin

  if not Contains(Key) then
    raise ENoSuchItem.Create('Key not found.');

  Aux1 := FItems[CalcIndex(Key)];

  if Aux1^.Key^ = Key then
  begin
    Aux2 := Aux1;
    FItems[CalcIndex(Key)] := Aux1^.Next;
    Dispose(Aux2);
  end
  else
  begin

    while (Aux1 <> nil) and (not ((FCompFunc <> nil) and (FCompFunc(Aux1^.Next^.Key^, Key)))
                             or not (Aux1^.Next^.Key^ = Key)) do
      Aux1 := Aux1^.Next;

    Aux2 := Aux1^.Next;
    Aux1^.Next := Aux1^.Next^.Next;
    FreeAndNil(Aux2);
  end;
end;

procedure TGenericHashTable.PrepareIterator;
begin
  IterationIndex := 0;
  while (IterationIndex < FTableSize) and (FItems[IterationIndex] = nil) do
    Inc(IterationIndex);
  if (IterationIndex < FTableSize) then
    IterationItem := FItems[IterationIndex];
end;

function TGenericHashTable.GetNextKey(out Key: TKey): boolean;
begin
  Result := False;
  if IterationItem <> nil then
  begin
    Key := IterationItem^.Key^;
    Result := True;

    if IterationItem^.Next <> nil then
      IterationItem := IterationItem^.Next
    else
    begin
      Inc(IterationIndex);
      while (IterationIndex < FTableSize) and (FItems[IterationIndex] = nil) do
        Inc(IterationIndex);
      if (IterationIndex < FTableSize) then
        IterationItem := FItems[IterationIndex]
      else
        IterationItem := nil;
    end;
  end;
end;

procedure TGenericHashTable.SetCompFunction(CompFunc: TCompFunc);
begin
  FCompFunc := CompFunc;
end;

procedure TGenericHashTable.Clear;
var Key: TKey;
begin
  PrepareIterator;
  while GetNextKey(Key) do
    Remove(Key);
end;

procedure TGenericHashTable.AddFrom(Source: TGenericHashTable);
var Key: TKey;
begin
  Source.PrepareIterator;
  while Source.GetNextKey(Key) do
    Put(Key, Source[Key]);
end;


function TGenericHashTable.CalcIndex(Key: TKey): Longword;
begin
   if FHashFunction <> nil then
    Result := FHashFunction(Key, FTableSize - 1)
  else
    Result := RSHash(Key, FTableSize - 1);
end;

constructor TGenericHashTable.Create;
begin
  Create(nil, 1024);
end;

constructor TGenericHashTable.Create(HashFunction: THashFunc; TableSize: Longword);
var Idx: Longword;
begin
  inherited Create;
  FTableSize := TableSize;
  SetLength(FItems, FTableSize);
  for Idx := 0 to FTableSize - 1 do
    FItems[Idx] := nil;
  FHashFunction := HashFunction;
  FCompFunc := nil;
end;

destructor TGenericHashTable.Destroy;
var Key: TKey;
begin
  PrepareIterator;
  while GetNextKey(Key) do
    Remove(Key);
  inherited Destroy;
end;

end.


