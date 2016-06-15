unit Device.PhysicalDrive.List;

interface

uses
  SysUtils, Generics.Collections,
  Device.PhysicalDrive;

type
  TThreadedPhysicalDriveList = class sealed(TThreadList<IPhysicalDrive>);
  TPhysicalDriveList = class sealed(TList<IPhysicalDrive>)
  public
    destructor Destroy; override;
    function IndexOf(const Model, Serial: String): Integer; overload;
    function IndexOf(Entry: IPhysicalDrive): Integer; overload;
    function IndexOf(const DeviceName: String): Integer; overload;
    function IsExists(Entry: IPhysicalDrive): Boolean;
  end;

implementation

function TPhysicalDriveList.IndexOf(const Model, Serial: String): Integer;
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

function TPhysicalDriveList.IndexOf(Entry: IPhysicalDrive): Integer;
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

destructor TPhysicalDriveList.Destroy;
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Count - 1 do //FI:W528
    Delete(0);
  inherited;
end;

function TPhysicalDriveList.IndexOf(const DeviceName: String): Integer;
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

function TPhysicalDriveList.IsExists(Entry: IPhysicalDrive): Boolean;
begin
  result := IndexOf(Entry) > -1;
end;
end.
