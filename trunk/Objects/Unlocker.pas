unit Unlocker;

interface

uses
  SysUtils, Generics.Collections,
  Device.PhysicalDrive, Device.PhysicalDrive.List, OS.Handle,
  Getter.PhysicalDrive.PartitionList;

type
  IDriveHandleUnlocker = interface['{8BBD8DB6-A063-4982-8FBA-54051BE87A2A}']

  end;
  TOSFileUnlockList = TList<IOSFileUnlock>;
  TDriveHandleUnlocker = class(TInterfacedObject, IDriveHandleUnlocker)
  public
    constructor Create(const Letter: String;
      const PhysicalDriveList: TPhysicalDriveList;
      const SelectedDrive: IPhysicalDrive);
    destructor Destroy; override;
  private
    UnlockList: TOSFileUnlockList;
    procedure IfThisDriveUnlock(const CurrentDrive: IPhysicalDrive;
      const Letter: string);
  end;

implementation

{ TDriveHandleUnlocker }

destructor TDriveHandleUnlocker.Destroy;
begin
  UnlockList.Free;
  inherited;
end;

procedure TDriveHandleUnlocker.IfThisDriveUnlock(
  const CurrentDrive: IPhysicalDrive; const Letter: string);
var
  PartitionList: TPartitionList;
const
  NotFound = -1;
begin
  PartitionList := CurrentDrive.GetPartitionList;
  try
    if PartitionList.FindEntryByIndex(Letter) > NotFound then
      UnlockList.Add(CurrentDrive.Unlock);
  finally
    PartitionList.Free;
  end;
end;

constructor TDriveHandleUnlocker.Create(const Letter: String;
  const PhysicalDriveList: TPhysicalDriveList;
  const SelectedDrive: IPhysicalDrive);
var
  CurrentDrive: IPhysicalDrive;
begin
  UnlockList := TOSFileUnlockList.Create;
  IfThisDriveUnlock(SelectedDrive, Letter);
  for CurrentDrive in PhysicalDriveList do
    IfThisDriveUnlock(CurrentDrive, Letter);
end;

end.
