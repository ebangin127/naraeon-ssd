unit uPhysicalDriveList;

interface

uses
  SysUtils, Generics.Collections;

type
  TPhysicalDriveEntry = record
    DeviceName: String;
    IsUSBDevice: Boolean;
  end;

  TPhysicalDriveList = class(TList<TPhysicalDriveEntry>)
  public
    function IndexOf(Model, Serial: String): Integer; overload;
    function IndexOf(Entry: TPhysicalDriveEntry): Integer; overload;
    function IndexOf(DeviceName: String): Integer; overload;
  end;

  TDiffResult = record
    DelList: TPhysicalDriveList;
    AddList: TPhysicalDriveList;
  end;

function TraverseDevice
  (IsDiffNeeded: Boolean; IsOnlySupported: Boolean;
   var PrevList: TPhysicalDriveList): TDiffResult;

implementation

uses uSSDInfo, uDiskFunctions, uSSDSupport;

function TPhysicalDriveList.IndexOf(Model, Serial: String): Integer;
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

function TPhysicalDriveList.IndexOf(Entry: TPhysicalDriveEntry): Integer;
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

function TPhysicalDriveList.IndexOf(DeviceName: String): Integer;
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
   var PrevList: TPhysicalDriveList): TDiffResult;
var
  CurrSSDInfo: TSSDInfo_NST;
  CurrList: TPhysicalDriveList;
  CurrEntry: TPhysicalDriveEntry;

  CurrDrv: Integer;
  CurrAvail: Boolean;

  CurrSSDList: TPhysicalDriveList;
begin
  CurrSSDInfo := TSSDInfo_NST.Create;

  CurrSSDList := GetSSDList;
  CurrList := TPhysicalDriveList.Create;

  if IsDiffNeeded then
  begin
    result.AddList := TPhysicalDriveList.Create;
    result.DelList := TPhysicalDriveList.Create;
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
