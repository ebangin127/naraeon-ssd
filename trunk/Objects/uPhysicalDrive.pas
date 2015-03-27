unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, uOSFile,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter;

type
  TPhysicalDrive = class
  public
    constructor Create(DriveNumber: Cardinal); 

    function GetDiskSize: TLargeInteger;
    function GetPartitionList: TPartitionList;
    function GetIsDriveAvailable: Boolean;

  private
    PhysicalDrivePath: String;
  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(DriveNumber: Cardinal);
begin
  PhysicalDrivePath :=
    ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber);
end;

function TPhysicalDrive.GetDiskSize: TLargeInteger;
var
  DiskGeometryGetter: TDiskGeometryGetter;
begin
  DiskGeometryGetter := TDiskGeometryGetter.Create(PhysicalDrivePath);
  result := DiskGeometryGetter.GetDiskSizeInByte;
  FreeAndNil(DiskGeometryGetter);
end;

function TPhysicalDrive.GetIsDriveAvailable: Boolean;
var
  DriveAvailabilityGetter: TDriveAvailabilityGetter;
begin
  DriveAvailabilityGetter := TDriveAvailabilityGetter.Create(PhysicalDrivePath);
  result := DriveAvailabilityGetter.GetAvailability;
  FreeAndNil(DriveAvailabilityGetter);
end;

function TPhysicalDrive.GetPartitionList: TPartitionList;
var
  PartitionListGetter: TPartitionListGetter;
begin
  PartitionListGetter := TPartitionListGetter.Create(PhysicalDrivePath);
  result := PartitionListGetter.GetPartitionList;
  FreeAndNil(PartitionListGetter);
end;

end.
