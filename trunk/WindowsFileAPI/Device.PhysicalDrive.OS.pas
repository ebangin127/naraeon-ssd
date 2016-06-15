unit Device.PhysicalDrive.OS;

interface

uses
  Windows, SysUtils,
  OSFile, Getter.PhysicalDrive.DiskGeometry, Getter.PhysicalDrive.PartitionList,
  Getter.PhysicalDrive.DriveAvailability, Getter.PhysicalDrive.NCQAvailability;

type
  TOSPhysicalDrive = class(TOSFile)
  private
    NCQAvailabilityReadWrite: TNCQAvailability;
    DiskSizeInByteReadWrite: Int64;
    procedure RequestNCQAvailability;
    procedure RequestDiskSizeInByte;
    function GetNCQAvailabilityOrRequestAndReturn: TNCQAvailability;
    function GetDiskSizeInByte: TLargeInteger;
    function GetIsDriveAvailable: Boolean;
  public
    property DiskSizeInByte: TLargeInteger
      read GetDiskSizeInByte;
    property IsDriveAvailable: Boolean
      read GetIsDriveAvailable;
    property NCQAvailability: TNCQAvailability
      read GetNCQAvailabilityOrRequestAndReturn;
    function GetPartitionList: TPartitionList;
  end;

implementation

{ TOSPhysicalDrive }

function TOSPhysicalDrive.GetDiskSizeInByte: TLargeInteger;
begin
  if DiskSizeInByteReadWrite = 0 then
    RequestDiskSizeInByte;
  result := DiskSizeInByteReadWrite;
end;

procedure TOSPhysicalDrive.RequestNCQAvailability;
var
  NCQAvailabilityGetter: TNCQAvailabilityGetter;
begin
  NCQAvailabilityGetter := TNCQAvailabilityGetter.Create
    (GetPathOfFileAccessing);
  NCQAvailabilityReadWrite := NCQAvailabilityGetter.GetNCQStatus;
  FreeAndNil(NCQAvailabilityGetter);
end;

function TOSPhysicalDrive.GetNCQAvailabilityOrRequestAndReturn:
  TNCQAvailability;
begin
  if NCQAvailabilityReadWrite = TNCQAvailability.Unknown then
    RequestNCQAvailability;
  result := NCQAvailabilityReadWrite;
end;

function TOSPhysicalDrive.GetIsDriveAvailable: Boolean;
var
  DriveAvailabilityGetter: TDriveAvailabilityGetter;
begin
  DriveAvailabilityGetter :=
    TDriveAvailabilityGetter.Create(GetPathOfFileAccessing);
  try
    result := DriveAvailabilityGetter.GetAvailability;
  finally
    FreeAndNil(DriveAvailabilityGetter);
  end;
end;

function TOSPhysicalDrive.GetPartitionList: TPartitionList;
var
  PartitionList: TPartitionListGetter;
begin
  PartitionList := TPartitionListGetter.Create(GetPathOfFileAccessing);
  result := PartitionList.GetPartitionList;
  FreeAndNil(PartitionList);
end;

procedure TOSPhysicalDrive.RequestDiskSizeInByte;
var
  DiskGeometryGetter: TDiskGeometryGetter;
begin
  DiskGeometryGetter := TDiskGeometryGetter.Create(GetPathOfFileAccessing);
  try
    DiskSizeInByteReadWrite := DiskGeometryGetter.GetDiskSizeInByte;
  finally
    FreeAndNil(DiskGeometryGetter);
  end;
end;

end.

