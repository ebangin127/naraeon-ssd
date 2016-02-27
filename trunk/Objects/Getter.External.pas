unit Getter.External;

interface

uses
  SysUtils,
  Partition, Getter.PartitionExtent, CommandSet, CommandSet.Factory,
  Device.PhysicalDrive;

type
  TExternalGetter = class
  private
    function GetDriveNumber(const PartitionPathToTrim: String): Cardinal;
    function GetIsExternalWithPhysicalDrive(
      const PhysicalDriveNumber: Cardinal): Boolean;
  public
    function IsExternal(const PartitionPathToTrim: String): Boolean;
  end;

implementation

{ TExternalGetter }

function TExternalGetter.GetIsExternalWithPhysicalDrive(
  const PhysicalDriveNumber: Cardinal): Boolean;
var
  CommandSet: TCommandSet;
begin
  CommandSet := CommandSetFactory.GetSuitableCommandSet(
    TPhysicalDrive.BuildFileAddressByNumber(PhysicalDriveNumber));
  try
    result := CommandSet.IsExternal;
  finally
    FreeAndNil(CommandSet);
  end;
end;

function TExternalGetter.GetDriveNumber(const PartitionPathToTrim: String):
  Cardinal;
var
  Partition: TPartition;
  PartitionExtentList: TPartitionExtentList;
begin
  Partition := TPartition.Create('\\.\' + PartitionPathToTrim);
  try
    PartitionExtentList := Partition.GetPartitionExtentList;
    result := PartitionExtentList[0].DriveNumber;
    FreeAndNil(PartitionExtentList);
  finally
    FreeAndNil(Partition);
  end;
end;

function TExternalGetter.IsExternal(const PartitionPathToTrim: String): Boolean;
var
  DriveNumber: Cardinal;
begin
  DriveNumber := GetDriveNumber(PartitionPathToTrim);
  result := GetIsExternalWithPhysicalDrive(DriveNumber);
end;

end.
