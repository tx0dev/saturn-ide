unit uObjects;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, Dialogs, uTokens, uContainers;


type TObjectType = (otUnknown, otAlt, otAtmosphere, otBodytarget, otBoolean, otConstant, otDirection, otEnumerator, otEta,
                    otFlightControl, otGeocoordinates, otList, otNode, otNumber, otOrbit, otStagevalues, otString, otTimespan,
                    otVector, otVesselsensors, otVesseltarget, otVesselvelocity
                    //otPart, otPartEngine, otUID
                    );

type TIdentifierClass = (icUnknown, icCommand, icKeyword, icFunction, icProgram, icSystemVariable, icUserVariable);

type

{ TObjectData }

 TObjectTypeData = record
  Name: string;
  TypeId: TObjectType;
  class operator = (a, b: TObjectTypeData): Boolean;
end;


type TObjectTypeDataList = specialize TFPGList<TObjectTypeData>;

type

{ TFunctionData }

 TFunctionData = record
  Name: string;
  ReturnType: TObjectType;
  Parameters: TObjectTypeDataList;
  class operator = (a, b: TFunctionData): Boolean;
end;


type
  TObjectsCompletionTable = specialize TFPGMap<TObjectType, TObjectTypeDataList>;

type TVariablesTable = specialize TFPGMap<string, TObjectType>;

type TCommandsTable = specialize TGenericHashTable<String, TCommandType>;

type TKeywordsTable = specialize TGenericHashTable<string, TKeywordType>;

type TFunctionsTable = specialize TFPGMap<string, TFunctionData>;

const IdentChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
const SepChars = [':', ' ', ')'];

function ObjectTypeData(const Name: string; TheType: TObjectType): TObjectTypeData;

function GetSuffixes(Identifiers: TSTringList; EditorVariables: TVariablesTable): TStringList;
function GetIdentifierType(const Identifiers: TStringList; EditorVariables: TVariablesTable; out IdentClass: TIdentifierClass): TObjectType;
function GetTypeName(TypeId: TObjectType): string;

function KeyCompare(const Key1, Key2: string): Integer;

var
  TypeSuffixes: TObjectsCompletionTable;
  KnownSystemVariables: TVariablesTable;
  KnownSystemFunctions: TFunctionsTable;
  Keywords: TKeywordsTable;
  Commands: TCommandsTable;
  KeywordsN: TStringList;


implementation

uses typinfo, Forms, contnrs;

var CharCompTable : array[#0..#255] of Char;

function StrComp(const s1, s2: string): boolean;
var Idx, MaxIdx: integer;
begin
  MaxIdx := Length(s1);
  Result := MaxIdx = Length(s2);
  Idx := 1;
  while Result and (Idx <= MaxIdx) do
    begin
    Result := CharCompTable[s1[Idx]] = CharCompTable[s2[Idx]];
    Inc(Idx);
    end;
end;

function StrHash(const S: string; TableSize: Longword): Longword;
const
  b = 378551;
var
  a: Longword;
  i: Longword;
begin
{$ifopt R+}
  {$define rangec}
  {$Q-}
  {$R-}
{$endif}
 a := 63689;
 Result := 0;
 if length(s)>0 then
   for i := 1 to Length(S) do
   begin
     Result := Result * a + Ord(CharCompTable[S[i]]);
     a := a * b;
   end;
 Result := (Result and $7FFFFFFF) mod TableSize;
{$ifdef rangec}
  {$Q+}
  {$R+}
  {$undef rangec}
{$endif}
end;

{ TFunctionData }

class operator TFunctionData.=(a, b: TFunctionData): Boolean;
begin
  Result := UpperCase(a.Name) = UpperCase(b.Name);
end;

{ TObjectData }

class operator TObjectTypeData.=(a, b: TObjectTypeData): Boolean;
begin
  Result:= UpperCase(a.Name) = UpperCase(b.Name);
end;

function CompareObjectData(const Item1, Item2: TObjectTypeData): Integer;
begin
  Result := CompareStr(Item1.Name, Item2.Name);
{  if Item1.Name > Item2.Name then
    Result := 1
  else if Item1.Name < Item2.Name then
    Result:= -1
  else
    Result := 0;}
end;

function KeyCompare(const Key1, Key2: string): Integer;
begin
  Result := CompareStr(LowerCase(Key1), LowerCase(Key2));
end;

function GetTypeOfSuffix(TypeId: TObjectType; const Suffix: string): TObjectType;
var idx: Integer;
begin
  idx := TypeSuffixes[TypeId].IndexOf(ObjectTypeData(Suffix, otUnknown));
  if idx >= 0 then
    Result:= TypeSuffixes[TypeId].Items[idx].TypeId
  else
    Result := otUnknown;
end;

function ObjectTypeData(const Name: string; TheType: TObjectType): TObjectTypeData;
begin
  Result.Name := Name;
  Result.TypeId := TheType;
end;

function FunctionData(const Name: string; ReturnType: TObjectType): TFunctionData;
begin
  Result.Name := Name;
  Result.ReturnType := ReturnType;
  Result.Parameters := TObjectTypeDataList.Create;
end;

//Get all suffixes for a given identifier
function GetSuffixes(Identifiers: TStringList; EditorVariables: TVariablesTable): TStringList;
var VarType: TObjectType;
    Item: TObjectTypeData;
    Items: TObjectTypeDataList;
    Idx, MaxIdx: Integer;
begin
  try
    Result := TStringList.Create;
    if Identifiers.Count = 0 then
        exit;
    if EditorVariables.IndexOf(Identifiers[0]) >= 0 then
      VarType := EditorVariables[Identifiers[0]]
    else if KnownSystemVariables.IndexOf(Identifiers[0]) >= 0 then
      VarType := KnownSystemVariables[Identifiers[0]]
    else if KnownSystemFunctions.IndexOf(Identifiers[0]) >= 0 then
      VarType := KnownSystemFunctions[Identifiers[0]].ReturnType
    else
      VarType := otUnknown;
    if Identifiers.Count > 1 then
    begin
      MaxIdx := Identifiers.Count - 1;
      for Idx := 1 to MaxIdx do
        begin
        Application.ProcessMessages;
        VarType:= GetTypeOfSuffix(VarType, Identifiers[Idx]);
        end;
    end;
    Items := TypeSuffixes[VarType];
    for Item in Items do
      begin
      Application.ProcessMessages;
      Result.Add(Item.Name);
      end;
  except
    //Identifier not found, nothing to do.
  end;
end;

function GetIdentifierType(const Identifiers: TStringList; EditorVariables: TVariablesTable; out IdentClass: TIdentifierClass): TObjectType;
var
  Idx, MaxIdx: Integer;
begin
  try
    if Identifiers.Count = 0 then
      begin
        Result := otUnknown;
        exit;
      end;
    if EditorVariables.IndexOf(Identifiers[0]) >= 0 then
    begin
      IdentClass := icUserVariable;
      Result := EditorVariables[Identifiers[0]]
    end
    else if KnownSystemFunctions.IndexOf(Identifiers[0]) >= 0 then
    begin
      IdentClass := icFunction;
      Result := KnownSystemFunctions[Identifiers[0]].ReturnType
    end
    else if KnownSystemVariables.IndexOf(Identifiers[0]) >= 0 then
    begin
      IdentClass := icSystemVariable;
      Result := KnownSystemVariables[Identifiers[0]];
    end
    else if Keywords.Contains(Identifiers[0]) then
    begin
      IdentClass := icKeyword;
      Result := otUnknown;
    end
    else if Commands.Contains(Identifiers[0]) then
    begin
      IdentClass := icCommand;
      Result := otUnknown;
    end
    else
      begin
      IdentClass:= icUnknown;
      Result := otUnknown;
      end;
    MaxIdx := Identifiers.Count - 1;
    for Idx := 1 to MaxIdx do
      begin
      Result:= GetTypeOfSuffix(Result, Identifiers[Idx]);
      Application.ProcessMessages;
      end;
  except
    IdentClass := icUnknown;
    Result := otUnknown;
  end;
end;

function GetTypeName(TypeId: TObjectType): string;
begin
  Result:= GetEnumName(TypeInfo(TObjectType), ord(TypeId));
  delete(Result, 1, 2);
  Result[1] := LowerCase(Result[1]);
end;

//Add all types and suffixes to the table
procedure CreateTypes;
var
  Aux: TObjectTypeDataList;
begin
  TypeSuffixes := TObjectsCompletionTable.Create;
  TypeSuffixes.Sorted := True;

  TypeSuffixes.Add(otUnknown, TObjectTypeDataList.Create);
  TypeSuffixes.Add(otNumber, TObjectTypeDataList.Create);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('apoapsis', otNumber));
  Aux.Add(ObjectTypeData('periapsis', otNumber));
  Aux.Add(ObjectTypeData('radar', otNumber));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otAlt, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('body', otBodytarget));
  Aux.Add(ObjectTypeData('exists', otBoolean));
  Aux.Add(ObjectTypeData('hasoxygen', otBoolean));
  Aux.Add(ObjectTypeData('scale', otNumber));
  Aux.Add(ObjectTypeData('sealevelpressure', otNumber));
  Aux.Add(ObjectTypeData('height', otNumber));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otAtmosphere, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('altitude', otNumber));
  Aux.Add(ObjectTypeData('apoapsis', otNumber));
  Aux.Add(ObjectTypeData('atm', otAtmosphere));
  Aux.Add(ObjectTypeData('body', otBodytarget));
  Aux.Add(ObjectTypeData('description', otString));
  Aux.Add(ObjectTypeData('distance', otNumber));
  Aux.Add(ObjectTypeData('mass', otNumber));
  Aux.Add(ObjectTypeData('mu', otNumber));
  Aux.Add(ObjectTypeData('name', otString));
  Aux.Add(ObjectTypeData('periapsis', otNumber));
  Aux.Add(ObjectTypeData('position', otVector));
  Aux.Add(ObjectTypeData('radius', otNumber));
  Aux.Add(ObjectTypeData('velocity', otVector));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otBodytarget, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('g', otNumber));
  Aux.Add(ObjectTypeData('e', otNumber));
  Aux.Add(ObjectTypeData('pi', otNumber));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otConstant, Aux);


  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('pitch', otNumber));
  Aux.Add(ObjectTypeData('roll', otNumber));
  Aux.Add(ObjectTypeData('vector', otVector));
  Aux.Add(ObjectTypeData('yaw', otNumber));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otDirection, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('end', otBoolean));
  Aux.Add(ObjectTypeData('index', otNumber));
  Aux.Add(ObjectTypeData('reset', otUnknown));
  Aux.Add(ObjectTypeData('value', otUnknown));
  TypeSuffixes.Add(otEnumerator, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('apoapsis', otNumber));
  Aux.Add(ObjectTypeData('periapsis', otNumber));
  Aux.Add(ObjectTypeData('transition', otNumber));
  TypeSuffixes.Add(otEta, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('bound', otBoolean));
  Aux.Add(ObjectTypeData('fore', otNumber));
  Aux.Add(ObjectTypeData('mainthrottle', otNumber));
  Aux.Add(ObjectTypeData('neutral', otBoolean));
  Aux.Add(ObjectTypeData('pitch', otNumber));
  Aux.Add(ObjectTypeData('roll', otNumber));
  Aux.Add(ObjectTypeData('rotation', otVector));
  Aux.Add(ObjectTypeData('starboard', otNumber));
  Aux.Add(ObjectTypeData('top', otNumber));
  Aux.Add(ObjectTypeData('translation', otVector));
  Aux.Add(ObjectTypeData('wheelsteer', otNumber));
  Aux.Add(ObjectTypeData('wheelthrottle', otNumber));
  Aux.Add(ObjectTypeData('yaw', otNumber));
  TypeSuffixes.Add(otFlightControl, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('bearing', otNumber));
  Aux.Add(ObjectTypeData('distance', otNumber));
  Aux.Add(ObjectTypeData('heading', otNumber));
  Aux.Add(ObjectTypeData('lat', otNumber));
  Aux.Add(ObjectTypeData('lng', otNumber));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otGeocoordinates, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('add', otUnknown));
  Aux.Add(ObjectTypeData('clear', otUnknown));
  Aux.Add(ObjectTypeData('contains', otBoolean));
  Aux.Add(ObjectTypeData('copy', otUnknown));
  Aux.Add(ObjectTypeData('iterator', otEnumerator));
  Aux.Add(ObjectTypeData('length', otNumber));
  Aux.Add(ObjectTypeData('remove', otUnknown));
  Aux.Sort(@CompareObjectData);
  TypeSuffixes.Add(otList, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('burnvector', otVector));
  Aux.Add(ObjectTypeData('deltav', otNumber));
  Aux.Add(ObjectTypeData('eta', otNumber));
  Aux.Add(ObjectTypeData('normal', otNumber));
  Aux.Add(ObjectTypeData('orbit', otOrbit));
  Aux.Add(ObjectTypeData('prograde', otNumber));
  Aux.Add(ObjectTypeData('radialout', otNumber));
  TypeSuffixes.Add(otNode, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('apoapsis', otNumber));
  Aux.Add(ObjectTypeData('body', otString));
  Aux.Add(ObjectTypeData('eccentricity', otNumber));
  Aux.Add(ObjectTypeData('inclination', otNumber));
  Aux.Add(ObjectTypeData('periapsis', otNumber));
  Aux.Add(ObjectTypeData('period', otNumber));
  Aux.Add(ObjectTypeData('semimajoraxis', otNumber));
  Aux.Add(ObjectTypeData('semiminoraxis', otNumber));
  Aux.Add(ObjectTypeData('transition', otString));
  Aux.Add(ObjectTypeData('patches', otList));
  TypeSuffixes.Add(otOrbit, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('liquidfuel', otNumber));
  Aux.Add(ObjectTypeData('oxidizer', otNumber));
  TypeSuffixes.Add(otStagevalues, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('calendar', otString));
  Aux.Add(ObjectTypeData('clock', otString));
  Aux.Add(ObjectTypeData('day', otNumber));
  Aux.Add(ObjectTypeData('hour', otNumber));
  Aux.Add(ObjectTypeData('minute', otNumber));
  Aux.Add(ObjectTypeData('second', otNumber));
  Aux.Add(ObjectTypeData('seconds', otNumber));
  Aux.Add(ObjectTypeData('year', otNumber));
  TypeSuffixes.Add(otTimespan, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('mag', otNumber));
  Aux.Add(ObjectTypeData('normalized', otNumber));
  Aux.Add(ObjectTypeData('sqrmagnitude', otNumber));
  Aux.Add(ObjectTypeData('vec', otVector));
  Aux.Add(ObjectTypeData('x', otNumber));
  Aux.Add(ObjectTypeData('y', otNumber));
  Aux.Add(ObjectTypeData('z', otNumber));
  TypeSuffixes.Add(otVector, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('acc', otNumber));
  Aux.Add(ObjectTypeData('grav', otNumber));
  Aux.Add(ObjectTypeData('light', otNumber));
  Aux.Add(ObjectTypeData('pres', otNumber));
  Aux.Add(ObjectTypeData('temp', otNumber));
  TypeSuffixes.Add(otVesselsensors, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('airspeed', otNumber));
  Aux.Add(ObjectTypeData('altitude', otNumber));
  Aux.Add(ObjectTypeData('angularmomentum', otDirection));
  Aux.Add(ObjectTypeData('angularvel', otDirection));
  Aux.Add(ObjectTypeData('apoapsis', otNumber));
  Aux.Add(ObjectTypeData('bearing', otNumber));
  Aux.Add(ObjectTypeData('body', otBodytarget));
  Aux.Add(ObjectTypeData('control', otFlightControl));
  Aux.Add(ObjectTypeData('direction', otDirection));
  Aux.Add(ObjectTypeData('distance', otNumber));
  Aux.Add(ObjectTypeData('facing', otDirection));
  Aux.Add(ObjectTypeData('geoposition', otGeocoordinates));
  Aux.Add(ObjectTypeData('heading', otNumber));
  Aux.Add(ObjectTypeData('latitude', otNumber));
  Aux.Add(ObjectTypeData('loaded', otBoolean));
  Aux.Add(ObjectTypeData('longitude', otNumber));
  Aux.Add(ObjectTypeData('mass', otNumber));
  Aux.Add(ObjectTypeData('maxthrust', otNumber));
  Aux.Add(ObjectTypeData('north', otDirection));
  Aux.Add(ObjectTypeData('obt', otOrbit));
  Aux.Add(ObjectTypeData('periapsis', otNumber));
  Aux.Add(ObjectTypeData('prograde', otNumber));
  Aux.Add(ObjectTypeData('retrograde', otNumber));
  Aux.Add(ObjectTypeData('sensors', otVesselsensors));
  Aux.Add(ObjectTypeData('surfacespeed', otNumber));
  Aux.Add(ObjectTypeData('termvelocity', otNumber));
  Aux.Add(ObjectTypeData('up', otDirection));
  Aux.Add(ObjectTypeData('velocity', otVesselvelocity));
  Aux.Add(ObjectTypeData('verticalspeed', otNumber));
  Aux.Add(ObjectTypeData('vesselname', otString));
  TypeSuffixes.Add(otVesseltarget, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('orbit', otNumber));
  Aux.Add(ObjectTypeData('surface', otNumber));
  Aux.Add(ObjectTypeData('surfaceheading', otNumber));
  TypeSuffixes.Add(otVesselvelocity, Aux);


  //Part types

 { Aux := TObjectTypeDataList.Create;
  TypeSuffixes.Add(otUID, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('controlfrom', otUnknown));
  Aux.Add(ObjectTypeData('modules', otList));
  Aux.Add(ObjectTypeData('name', otString));
  Aux.Add(ObjectTypeData('resources', otList));
  Aux.Add(ObjectTypeData('stage', otNumber));
  Aux.Add(ObjectTypeData('targetable', otUnknown)); {TODO: see if it keeps that way}
  Aux.Add(ObjectTypeData('uid', otUID));
  TypeSuffixes.Add(otPart, Aux);

  Aux := TObjectTypeDataList.Create;
  Aux.Add(ObjectTypeData('controlfrom', otUnknown));
  Aux.Add(ObjectTypeData('modules', otList));
  Aux.Add(ObjectTypeData('name', otString));
  Aux.Add(ObjectTypeData('resources', otList));
  Aux.Add(ObjectTypeData('stage', otNumber));
  Aux.Add(ObjectTypeData('targetable', otUnknown)); {TODO: see if it keeps that way}
  Aux.Add(ObjectTypeData('uid', otUID));

  TypeSuffixes.Add(otPartEngine, Aux);
}

end;

//Add all known variables and their types to the table
procedure AddVariables;
begin
  KnownSystemVariables := TVariablesTable.Create;
  KnownSystemVariables.Sorted := True;
  KnownSystemVariables.OnCompare:=@KeyCompare;
  KnownSystemVariables.Add('alt', otAlt);
  KnownSystemVariables.Add('altitude', otNumber);
  KnownSystemVariables.Add('angularmomentum', otDirection);
  KnownSystemVariables.Add('angularvel', otDirection);
  KnownSystemVariables.Add('apoapsis', otNumber);
  KnownSystemVariables.Add('body', otBodytarget);
  KnownSystemVariables.Add('commrange', otNumber);
  KnownSystemVariables.Add('eta', otEta);
  KnownSystemVariables.Add('facing', otDirection);
  KnownSystemVariables.Add('geoposition', otGeocoordinates);
  KnownSystemVariables.Add('heading', otNumber);
  KnownSystemVariables.Add('incommrange', otNumber);
  KnownSystemVariables.Add('latitude', otNumber);
  KnownSystemVariables.Add('loaddistance', otNumber);
  KnownSystemVariables.Add('longitude', otNumber);
  KnownSystemVariables.Add('mass', otNumber);
  KnownSystemVariables.Add('maxthrust', otNumber);
  KnownSystemVariables.Add('missiontime', otNumber);
  KnownSystemVariables.Add('nextnode', otNode);
  KnownSystemVariables.Add('north', otDirection);
  KnownSystemVariables.Add('periapsis', otNumber);
  KnownSystemVariables.Add('prograde', otDirection);
  KnownSystemVariables.Add('retrograde', otDirection);
  KnownSystemVariables.Add('sessiontime', otNumber);
  KnownSystemVariables.Add('ship', otVesseltarget);
  KnownSystemVariables.Add('stage', otStagevalues);
  KnownSystemVariables.Add('status', otString);
  KnownSystemVariables.Add('steering', otNumber);
  KnownSystemVariables.Add('surfacespeed', otNumber);
  KnownSystemVariables.Add('throttle', otNumber);
  KnownSystemVariables.Add('time', otTimespan);
  KnownSystemVariables.Add('up', otDirection);
  KnownSystemVariables.Add('velocity', otVesselvelocity);
  KnownSystemVariables.Add('verticalspeed', otNumber);
  KnownSystemVariables.Add('vesselname', otString);
  KnownSystemVariables.Add('warp', otNumber);
  KnownSystemVariables.Add('wheelsteering', otNumber);
  KnownSystemVariables.Add('wheelthrottle', otNumber);

  KnownSystemVariables.Add('bop', otBodytarget);
  KnownSystemVariables.Add('dres', otBodytarget);
  KnownSystemVariables.Add('duna', otBodytarget);
  KnownSystemVariables.Add('eeloo', otBodytarget);
  KnownSystemVariables.Add('eve', otBodytarget);
  KnownSystemVariables.Add('gilly', otBodytarget);
  KnownSystemVariables.Add('ike', otBodytarget);
  KnownSystemVariables.Add('jool', otBodytarget);
  KnownSystemVariables.Add('kerbin', otBodytarget);
  KnownSystemVariables.Add('laythe', otBodytarget);
  KnownSystemVariables.Add('minmus', otBodytarget);
  KnownSystemVariables.Add('moho', otBodytarget);
  KnownSystemVariables.Add('mun', otBodytarget);
  KnownSystemVariables.Add('pol', otBodytarget);
  KnownSystemVariables.Add('sun', otBodytarget);
  KnownSystemVariables.Add('tylo', otBodytarget);
  KnownSystemVariables.Add('vall', otBodytarget);
end;

//Add all known system function, parameters and return type
procedure AddFunctions;
var Func: TFunctionData;
begin
  KnownSystemFunctions := TFunctionsTable.Create;
  KnownSystemFunctions.Sorted := True;
  KnownSystemFunctions.OnCompare:= @KeyCompare;

  Func := FunctionData('abs', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('abs', Func);

  Func := FunctionData('arccos', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('arccos', Func);

  Func := FunctionData('arcsin', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('arcsin', Func);

  Func := FunctionData('arctan', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('arctan', Func);

  Func := FunctionData('arctan2', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('arctan2', Func);

  Func := FunctionData('ceiling', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('ceiling', Func);

  Func := FunctionData('constant', otConstant);
  KnownSystemFunctions.Add('constant', Func);

  Func := FunctionData('cos', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('cos', Func);

  Func := FunctionData('floor', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('floor', Func);

  Func := FunctionData('list', otList);
  KnownSystemFunctions.Add('list', Func);

  Func := FunctionData('ln', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('ln', Func);

  Func := FunctionData('log10', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('log10', Func);

  Func := FunctionData('max', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('max', Func);

  Func := FunctionData('min', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('min', Func);

  Func := FunctionData('mod', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('mod', Func);

  Func := FunctionData('node', otNode);
  Func.Parameters.Add(ObjectTypeData('Time', otTimespan));
  Func.Parameters.Add(ObjectTypeData('Radial_dV', otNumber));
  Func.Parameters.Add(ObjectTypeData('Normal_dV', otNumber));
  Func.Parameters.Add(ObjectTypeData('Prograde_dV', otNumber));
  KnownSystemFunctions.Add('node', Func);

  Func := FunctionData('q', otDirection);
  Func.Parameters.Add(ObjectTypeData('X', otNumber));
  Func.Parameters.Add(ObjectTypeData('Y', otNumber));
  Func.Parameters.Add(ObjectTypeData('Z', otNumber));
  Func.Parameters.Add(ObjectTypeData('Angle', otNumber));
  KnownSystemFunctions.Add('q', Func);

  Func := FunctionData('r', otDirection);
  Func.Parameters.Add(ObjectTypeData('X', otNumber));
  Func.Parameters.Add(ObjectTypeData('Y', otNumber));
  Func.Parameters.Add(ObjectTypeData('Z', otNumber));
  KnownSystemFunctions.Add('r', Func);

  Func := FunctionData('random', otNumber);
  KnownSystemFunctions.Add('random', Func);

  Func := FunctionData('round', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otDirection));
  KnownSystemFunctions.Add('round', Func);

  Func := FunctionData('sin', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('sin', Func);

  Func := FunctionData('sqrt', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('sqrt', Func);

  Func := FunctionData('tan', otNumber);
  Func.Parameters.Add(ObjectTypeData('Number', otNumber));
  KnownSystemFunctions.Add('tan', Func);

  Func := FunctionData('v', otVector);
  Func.Parameters.Add(ObjectTypeData('X', otNumber));
  Func.Parameters.Add(ObjectTypeData('Y', otNumber));
  Func.Parameters.Add(ObjectTypeData('Z', otNumber));
  KnownSystemFunctions.Add('v', Func);

  Func:= FunctionData('vcrs', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vcrs', Func);

  Func:= FunctionData('vectorcrossproduct', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vectorcrossproduct', Func);

  Func:= FunctionData('vdot', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vdot', Func);

  Func:= FunctionData('vectordotproduct', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vectordotproduct', Func);

  Func:= FunctionData('vxcl', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vxcl', Func);

  Func:= FunctionData('vectorexclude', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vectorexclude', Func);

  Func:= FunctionData('vang', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vang', Func);

  Func:= FunctionData('vectorangle', otVector);
  Func.Parameters.Add(ObjectTypeData('Vector1', otVector));
  Func.Parameters.Add(ObjectTypeData('Vector2', otVector));
  KnownSystemFunctions.Add('vectorangle', Func);

  Func:= FunctionData('vessel', otVesseltarget);
  Func.Parameters.Add(ObjectTypeData('VesselName', otString));
  KnownSystemFunctions.Add('vessel', Func);

end;

procedure AddCommands;
begin
  Commands := TCommandsTable.Create(@StrHash, 1024);
  Commands.SetCompFunction(@StrComp);

  Commands['ADD'] := cAdd;
  Commands['BATCH'] := cBatch;
  Commands['BREAK'] := cBreak;
  Commands['CLEARSCREEN'] := cClearscreen;
  Commands['COPY'] := cCopy;
  Commands['DECLARE'] := cDeclare;
  Commands['DELETE'] := cDelete;
  Commands['DEPLOY'] := cDeploy;
  Commands['EDIT'] := cEdit;
  Commands['FOR'] := cFor;
  Commands['IF'] := cIf;
  Commands['LIST'] := cList;
  Commands['LOCK'] := cLock;
  Commands['LOG'] := cLog;
  Commands['ON'] := cOn;
  Commands['PRINT'] := cPrint;
  Commands['RCS'] := cRCS;
  Commands['REBOOT'] := cReboot;
  Commands['REMOVE'] := cRemove;
  Commands['RENAME'] := cRename;
  Commands['RUN'] := cRun;
  Commands['SET'] := cSet;
  Commands['SHUTDOWN'] := cShutdown;
  Commands['STAGE'] := cStage;
  Commands['SWITCH'] := cSwitch;
  Commands['TOGGLE'] := cToggle;
  Commands['UNLOCK'] := cUnlock;
  Commands['UNSET'] := cUnset;
  Commands['UNTIL'] := cUntil;
  Commands['WAIT'] := cWait;
  Commands['WHEN'] := cWhen;
end;

procedure AddKeywords;
begin
  Keywords := TKeywordsTable.Create(@StrHash, 1024);
  Keywords.SetCompFunction(@StrComp);

  Keywords['ABORT'] := kAbort;

  Keywords['AG1'] := kAG;
  Keywords['AG2'] := kAG;
  Keywords['AG3'] := kAG;
  Keywords['AG4'] := kAG;
  Keywords['AG5'] := kAG;
  Keywords['AG6'] := kAG;
  Keywords['AG7'] := kAG;
  Keywords['AG8'] := kAG;
  Keywords['AG9'] := kAG;
  Keywords['AG10'] := kAG;
  Keywords['ALL'] := kAll;
  Keywords['AND'] := kAnd;
  Keywords['AT'] := kAt;
  Keywords['BODIES'] := kBodies;
  Keywords['BRAKES'] := kBrakes;
  Keywords['BY'] := kBy;
  Keywords['CHUTES'] := kChutes;
  Keywords['ELEMENTS'] := kElements;
  Keywords['ELSE'] := kElse;
  Keywords['ENGINES'] := kEngines;
  Keywords['FALSE'] := kFalse;
  Keywords['FILE'] := kFile;
  Keywords['FILES'] := kFiles;
  Keywords['FROM'] := kFrom;
  Keywords['GEAR'] := kGear;
  Keywords['IN'] := kIn;
  Keywords['LIGHTS'] := kLights;
  Keywords['LIST'] := kList;
  Keywords['NEXTNODE'] := kNextnode;
  Keywords['NOT'] := kNot;
  Keywords['OFF'] := kOff;
  Keywords['OR'] := kOr;
  Keywords['PARAMETER'] := kParameter;
  Keywords['PARTS'] := kParts;
  Keywords['RESOURCES'] := kResources;
  Keywords['SAS'] := kSAS;
  Keywords['TARGETS'] := kTargets;
  Keywords['THEN'] := kThen;
  Keywords['TO'] := kTo;
  Keywords['TRUE'] := kTrue;
  Keywords['VOLUME'] := kVolume;
  Keywords['VOLUMES'] := kVolumes;
end;

var CharIdx, Aux: Char;

initialization
  for CharIdx := #0 to #255 do
  begin
    Aux := UpCase(CharIdx);
    if CharIdx in['a'..'z', 'A'..'Z'] then
      CharCompTable[CharIdx] := Aux
    else
      CharCompTable[CharIdx] := CharIdx;
  end;

  CreateTypes;
  AddVariables;
  AddFunctions;
  AddCommands;
  AddKeywords;


finalization
  TypeSuffixes.Free;
  KnownSystemVariables.Free;
  KnownSystemFunctions.Free;
  Keywords.Free;
  Commands.Free;

end.



