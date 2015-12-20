unit uOSPhysicalDrive;

interface

uses
  Windows, SysUtils,
  uOSFile, uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter,
  uNCQAvailabilityGetter;

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
  PartitionListGetter: TPartitionListGetter;
begin
  PartitionListGetter := TPartitionListGetter.Create(GetPathOfFileAccessing);
  result := PartitionListGetter.GetPartitionList;
  FreeAndNil(PartitionListGetter);
end;

procedure TOSPhysicalDrive.RequestDiskSizeInByte;
var
  DiskGeometryGetter: TDiskGeometryGetter;
begin
  try
    DiskGeometryGetter := TDiskGeometryGetter.Create(GetPathOfFileAccessing);
    DiskSizeInByteReadWrite := DiskGeometryGetter.GetDiskSizeInByte;
  finally
    FreeAndNil(DiskGeometryGetter);
  end;
end;

end.

