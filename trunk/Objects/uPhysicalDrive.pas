unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, uOSFile,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter;

type
  TPhysicalDrive = class
  public
    constructor Create(DriveNumber: Cardinal); overload;
    constructor Create(DrivePath: String); overload;
    destructor Destroy; override;

    function GetDiskSize: TLargeInteger;
    function GetPartitionList: TPartitionList;
    function GetIsDriveAvailable: Boolean;

  private
    DiskGeometryGetter: TDiskGeometryGetter;
    PartitionListGetter: TPartitionListGetter;
    DriveAvailabilityGetter: TDriveAvailabilityGetter;
  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(DriveNumber: Cardinal);
var
  PhysicalDrivePath: String;
begin
  PhysicalDrivePath :=
    ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber);
  Create(PhysicalDrivePath);
end;

constructor TPhysicalDrive.Create(DrivePath: String);
begin
  DiskGeometryGetter := TDiskGeometryGetter.Create(DrivePath);
  PartitionListGetter := TPartitionListGetter.Create(DrivePath);
  DriveAvailabilityGetter := TDriveAvailabilityGetter.Create(DrivePath);
end;

destructor TPhysicalDrive.Destroy;
begin
  FreeAndNil(DiskGeometryGetter);
  FreeAndNil(PartitionListGetter);
  FreeAndNil(DriveAvailabilityGetter);
  inherited;
end;

function TPhysicalDrive.GetDiskSize: TLargeInteger;
begin
  result := DiskGeometryGetter.GetDiskSizeInByte;
end;

function TPhysicalDrive.GetIsDriveAvailable: Boolean;
begin
  result := DriveAvailabilityGetter.GetAvailability;
end;

function TPhysicalDrive.GetPartitionList: TPartitionList;
begin
  result := PartitionListGetter.GetPartitionList;
end;

end.
