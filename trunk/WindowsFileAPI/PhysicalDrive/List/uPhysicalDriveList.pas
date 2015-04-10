unit uPhysicalDriveList;

interface

uses
  SysUtils, Generics.Collections,
  uPhysicalDrive;

type
  TPhysicalDriveList = class sealed(TList<TPhysicalDrive>)
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

uses uAutoPhysicalDriveListGetter;

function TPhysicalDriveList.IndexOf(Model, Serial: String): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
  begin
    if (Self[CurrEntry].IdentifyDeviceResult.Model = Model) and
       (Self[CurrEntry].IdentifyDeviceResult.Serial = Serial) then
      break;
  end;

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
  CurrList: TPhysicalDriveList;
  CurrEntry: TPhysicalDrive;

  CurrDrv: Integer;
  CurrAvail: Boolean;

  AutoPhysicalDriveListGetter: TAutoPhysicalDriveListGetter;
  CurrSSDList: TPhysicalDriveList;
begin
  AutoPhysicalDriveListGetter := TAutoPhysicalDriveListGetter.Create;
  CurrSSDList := AutoPhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(AutoPhysicalDriveListGetter);
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

    if (not IsOnlySupported) or
       (CurrEntry.SupportStatus.Supported) then
    begin
      CurrList.Add(
        TPhysicalDrive.Create
          (StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix)));
    end;

    if not IsDiffNeeded then
      Continue;

    if (CurrEntry.SupportStatus.Supported) and
       (CurrAvail = false) then
    begin
      result.AddList.Add(
        TPhysicalDrive.Create
          (StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix)));
    end;
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

    if (not CurrAvail) or
       (not CurrEntry.SupportStatus.Supported) then
      result.DelList.Add(
        TPhysicalDrive.Create
          (StrToInt(CurrEntry.GetPathOfFileAccessingWithoutPrefix)));
  end;

  FreeAndNil(CurrSSDList);
  FreeAndNil(PrevList);

  PrevList := CurrList;
end;
end.
