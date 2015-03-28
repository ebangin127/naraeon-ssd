unit uPhysicalDriveList;

interface

uses
  SysUtils, Generics.Collections,
  uPhysicalDrive;

type
  TPhysicalDriveList = class(TList<TPhysicalDrive>)
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer);

    function IndexOf(Model, Serial: String): Integer; overload;
    function IndexOf(Entry: TPhysicalDrive): Integer; overload;
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

uses uSSDInfo, uMixedPhysicalDriveListGetter, uSSDSupport;

function TPhysicalDriveList.IndexOf(Model, Serial: String): Integer;
var
  CurrEntry: Integer;
  CurrSSDInfo: TSSDInfo;
begin
  CurrSSDInfo := TSSDInfo.Create;
  for CurrEntry := 0 to Count - 1 do
  begin
    CurrSSDInfo.SetDeviceName(
      StrToInt(Self[CurrEntry].GetPathOfFileAccessingWithoutPrefix));
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

function TPhysicalDriveList.IndexOf(Entry: TPhysicalDrive): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
    if self[CurrEntry].GetPathOfFileAccessing =
       Entry.GetPathOfFileAccessing then
      break;

  if CurrEntry < Count then
    exit(CurrEntry)
  else
    exit(-1);
end;

procedure TPhysicalDriveList.Delete(Index: Integer);
begin
  Self[Index].Free;
  Self[Index] := nil;
  inherited Delete(Index);
end;

destructor TPhysicalDriveList.Destroy;
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Count - 1 do
    Delete(0);
  inherited;
end;

function TPhysicalDriveList.IndexOf(DeviceName: String): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
    if self[CurrEntry].GetPathOfFileAccessing = DeviceName then
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
  CurrEntry: TPhysicalDrive;

  CurrDrv: Integer;
  CurrAvail: Boolean;

  MixedPhysicalDriveListGetter: TMixedPhysicalDriveListGetter;
  CurrSSDList: TPhysicalDriveList;
begin
  CurrSSDInfo := TSSDInfo_NST.Create;

  MixedPhysicalDriveListGetter := TMixedPhysicalDriveListGetter.Create;
  CurrSSDList := MixedPhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(MixedPhysicalDriveListGetter);
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

    CurrSSDInfo.SetDeviceName(
      StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix));
    if (not IsOnlySupported) or
       (CurrSSDInfo.SupportedDevice <> SUPPORT_NONE) then
      CurrList.Add(
        TPhysicalDrive.Create
          (StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix)));

    if not IsDiffNeeded then
      Continue;

    if (CurrSSDInfo.SupportedDevice <> SUPPORT_NONE) and
       (CurrAvail = false) then
      result.AddList.Add(
        TPhysicalDrive.Create
          (StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix)));
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

    CurrSSDInfo.SetDeviceName(
      StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix));

    if (not CurrAvail) or
       (CurrSSDInfo.SupportedDevice = SUPPORT_NONE) then
      result.DelList.Add(
        TPhysicalDrive.Create
          (StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix)));
  end;

  FreeAndNil(CurrSSDInfo);
  FreeAndNil(CurrSSDList);
  FreeAndNil(PrevList);

  PrevList := CurrList;
end;
end.
