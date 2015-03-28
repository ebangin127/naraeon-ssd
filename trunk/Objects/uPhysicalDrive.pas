unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, uOSFile, uCommandSet,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter,
  uBufferInterpreter;

type
  TPhysicalDrive = class(TOSFile)
  private
    Model: String;
    Firmware: String;
    Serial: String;
    UserSizeInKB: UInt64;
    SATASpeed: TSATASpeed;

    DriveAvailabilityGetter: TDriveAvailabilityGetter;
    function TryToGetIsDriveAvailable: Boolean;

    function GetDiskSizeInByte: TLargeInteger;
    function GetPartitionList: TPartitionList;
    function GetIsDriveAvailable: Boolean;

  public
    property Model: String read FModel;
    property Firmware: String read FFirmware;
    property Serial: String read FSerial;
    property UserSizeInKB: UInt64 read FUserSizeInKB;
    property SATASpeed: TSATASpeed read FSATASpeed;

    constructor Create(DriveNumber: Cardinal); reintroduce; overload;

  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(DriveNumber: Cardinal);
begin
  inherited
    Create(ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber));
end;

function TPhysicalDrive.GetDiskSizeInByte: TLargeInteger;
var
  DiskGeometryGetter: TDiskGeometryGetter;
begin
  DiskGeometryGetter := TDiskGeometryGetter.Create(GetPathOfFileAccessing);
  result := DiskGeometryGetter.GetDiskSizeInByte;
  FreeAndNil(DiskGeometryGetter);
end;

function TPhysicalDrive.TryToGetIsDriveAvailable: Boolean;
begin
  try
    DriveAvailabilityGetter :=
      TDriveAvailabilityGetter.Create(GetPathOfFileAccessing);
    result := DriveAvailabilityGetter.GetAvailability;
  except
    result := false;
  end;
end;

function TPhysicalDrive.GetIsDriveAvailable: Boolean;
begin
  try
    result := TryToGetIsDriveAvailable;
  finally
    FreeAndNil(DriveAvailabilityGetter);
  end;
end;

function TPhysicalDrive.GetPartitionList: TPartitionList;
var
  PartitionListGetter: TPartitionListGetter;
begin
  PartitionListGetter := TPartitionListGetter.Create(GetPathOfFileAccessing);
  result := PartitionListGetter.GetPartitionList;
  FreeAndNil(PartitionListGetter);
end;

end.
