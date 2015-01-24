unit uSSDList;

interface

uses
  SysUtils, Generics.Collections;

type
  TSSDEntry = record
    DeviceName: String;
    IsUSBDevice: Boolean;
  end;

  TSSDList = class(TList<TSSDEntry>)
  public
    function IndexOf(Model, Serial: String): Integer; overload;
    function IndexOf(Entry: TSSDEntry): Integer; overload;
    function IndexOf(DeviceName: String): Integer; overload;
  end;

  TDiffResult = record
    DelList: TSSDList;
    AddList: TSSDList;
  end;

function TraverseDevice
  (IsDiffNeeded: Boolean; IsOnlySupported: Boolean;
   var PrevList: TSSDList): TDiffResult;

implementation

uses uSSDInfo, uDiskFunctions, uSSDSupport;

function TSSDList.IndexOf(Model, Serial: String): Integer;
var
  CurrEntry: Integer;
  CurrSSDInfo: TSSDInfo;
begin
  CurrSSDInfo := TSSDInfo.Create;
  for CurrEntry := 0 to Count - 1 do
  begin
    CurrSSDInfo.SetDeviceName(StrToInt(Self[CurrEntry].DeviceName));
    if (CurrSSDInfo.Model = Model) and
       (CurrSSDInfo.Serial = Serial) then
      break;
  end;
  FreeAndNil(CurrSSDInfo);

  if CurrEntry < Count then
    exit(CurrEntry)
  else
    exit(-1);
end;

function TSSDList.IndexOf(Entry: TSSDEntry): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
    if (self[CurrEntry].DeviceName = Entry.DeviceName) and
       (self[CurrEntry].IsUSBDevice = Entry.IsUSBDevice) then
      break;

  if CurrEntry < Count then
    exit(CurrEntry)
  else
    exit(-1);
end;

function TSSDList.IndexOf(DeviceName: String): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
    if self[CurrEntry].DeviceName = DeviceName then
      break;

  if CurrEntry < Count then
    exit(CurrEntry)
  else
    exit(-1);
end;

function TraverseDevice
  (IsDiffNeeded: Boolean; IsOnlySupported: Boolean;
   var PrevList: TSSDList): TDiffResult;
var
  CurrSSDInfo: TSSDInfo_NST;
  CurrList: TSSDList;
  CurrEntry: TSSDEntry;

  CurrDrv: Integer;
  CurrAvail: Boolean;

  CurrSSDList: TSSDList;
begin
  CurrSSDInfo := TSSDInfo_NST.Create;

  CurrSSDList := GetSSDList;
  CurrList := TSSDList.Create;

  if IsDiffNeeded then
  begin
    result.AddList := TSSDList.Create;
    result.DelList := TSSDList.Create;
  end;

  for CurrDrv := 0 to CurrSSDList.Count - 1 do
  begin
    CurrEntry := CurrSSDList[CurrDrv];

    CurrAvail :=
      PrevList.IndexOf(CurrEntry) > -1;

    CurrSSDInfo.SetDeviceName(StrToInt(CurrEntry.DeviceName));
    if (not IsOnlySupported) or
       (CurrSSDInfo.SupportedDevice <> SUPPORT_NONE) then
      CurrList.Add(CurrEntry);

    if not IsDiffNeeded then
      Continue;

    if (CurrSSDInfo.SupportedDevice <> SUPPORT_NONE) and
       (CurrAvail = false) then
      result.AddList.Add(CurrEntry);
  end;

  if not IsDiffNeeded then
  begin
    FreeAndNil(CurrSSDList);
    PrevList := CurrList;
    exit;
  end;

  for CurrDrv := 0 to PrevList.Count - 1 do
  begin
    CurrEntry := PrevList[CurrDrv];

    CurrAvail :=
      PrevList.IndexOf(CurrEntry) > -1;

    CurrSSDInfo.SetDeviceName(StrToInt(CurrEntry.DeviceName));
    if (not CurrAvail) or
       (CurrSSDInfo.SupportedDevice = SUPPORT_NONE) then
      result.DelList.Add(CurrEntry);
  end;

  FreeAndNil(CurrSSDInfo);
  FreeAndNil(CurrSSDList);
  FreeAndNil(PrevList);

  PrevList := CurrList;
end;
end.
