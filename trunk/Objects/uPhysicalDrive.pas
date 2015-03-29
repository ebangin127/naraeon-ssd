unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, uOSFile, uCommandSet,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter,
  uBufferInterpreter, uSMARTValueList, uMixedCommandSet;

type
  TPhysicalDrive = class(TOSFile)
  private
    IdentifyDeviceResultReadWrite: TIdentifyDeviceResult;
    DriveAvailabilityGetter: TDriveAvailabilityGetter;
    MixedCommandSet: TMixedCommandSet;
    SMARTValueList: TSMARTValueList;

    procedure RequestIdentifyDevice;
    procedure RequestSMARTReadData;

    function GetIdentifyDeviceResultOrReadyAndReturn: TIdentifyDeviceResult;

  public
    property IdentifyDeviceResult: TIdentifyDeviceResult
      read GetIdentifyDeviceResultOrReadyAndReturn;

    function GetDiskSizeInByte: TLargeInteger;
    function GetPartitionList: TPartitionList;
    function GetIsDriveAvailable: Boolean;
    function TryToGetIsDriveAvailable: Boolean;
    function GetSMARTValueEntryByID(ID: Byte): TSMARTValueEntry;

    constructor Create(DriveNumber: Cardinal); reintroduce; overload;
    destructor Destroy; override;

  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(DriveNumber: Cardinal);
begin
  inherited
    Create(ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber));
  MixedCommandSet := TMixedCommandSet.Create(GetPathOfFileAccessing);
end;

destructor TPhysicalDrive.Destroy;
begin
  if SMARTValueList <> nil then
    FreeAndNil(SMARTValueList);
  FreeAndNil(MixedCommandSet);
  inherited;
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

function TPhysicalDrive.GetIdentifyDeviceResultOrReadyAndReturn:
  TIdentifyDeviceResult;
begin
  if IdentifyDeviceResultReadWrite.Model = '' then
    RequestIdentifyDevice;
  result := IdentifyDeviceResultReadWrite;
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

function TPhysicalDrive.GetSMARTValueEntryByID(ID: Byte): TSMARTValueEntry;
begin
  if SMARTValueList = nil then
    RequestSMARTReadData;
  result := SMARTValueList[SMARTValueList.IndexByID(ID)];
end;

procedure TPhysicalDrive.RequestIdentifyDevice;
begin
  IdentifyDeviceResultReadWrite := MixedCommandSet.IdentifyDevice;
end;

procedure TPhysicalDrive.RequestSMARTReadData;
begin
  SMARTValueList := MixedCommandSet.SMARTReadData;
end;

end.
