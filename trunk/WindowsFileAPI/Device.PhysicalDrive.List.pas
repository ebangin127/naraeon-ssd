unit Device.PhysicalDrive.List;

interface

uses
  SysUtils, Generics.Collections,
  Device.PhysicalDrive;

type
  TPhysicalDriveList = class sealed(TList<IPhysicalDrive>)
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer);

    function IndexOf(Model, Serial: String): Integer; overload;
    function IndexOf(Entry: TPhysicalDrive): Integer; overload;
    function IndexOf(DeviceName: String): Integer; overload;
    
    function IsExists(Entry: IPhysicalDrive): Boolean;
  end;

implementation

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

function TPhysicalDriveList.IsExists(Entry: IPhysicalDrive): Boolean;
begin
  result := IndexOf(Entry) > -1;
end;
end.
