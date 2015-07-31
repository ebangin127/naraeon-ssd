unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, uOSFile, uCommandSet,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter,
  uBufferInterpreter, uSMARTValueList, uAutoCommandSet, uPartitionTrimmer,
  uNSTSupport, uNCQAvailabilityGetter;

type
  TPhysicalDrive = class(TOSFile)
  private
    IdentifyDeviceResultReadWrite: TIdentifyDeviceResult;
    SupportStatusReadWrite: TSupportStatus;
    SMARTInterpretedReadWrite: TSMARTInterpreted;
    DriveAvailabilityGetter: TDriveAvailabilityGetter;
    SMARTValueListReadWrite: TSMARTValueList;
    NCQAvailabilityReadWrite: TNCQAvailability;

    AutoCommandSet: TAutoCommandSet;
    PartitionTrimmer: TPartitionTrimmer;

    procedure RequestIdentifyDevice;
    procedure RequestSMARTReadData;
    procedure RequestSMARTInterpreted;
    procedure RequestSupportStatus;
    procedure RequestNCQAvailability;

    function GetIdentifyDeviceResultOrRequestAndReturn: TIdentifyDeviceResult;
    function GetSupportStatusOrRequestAndReturn: TSupportStatus;
    function GetSMARTInterpretedOrRequestAndReturn: TSMARTInterpreted;
    function GetSMARTValueListOrRequestAndReturn: TSMARTValueList;
    function GetNCQAvailabilityOrRequestAndReturn: TNCQAvailability;

    function GetDiskSizeInByte: TLargeInteger;
    function GetIsDriveAvailable: Boolean;
    function TryToGetIsDriveAvailable: Boolean;
    procedure TryToCreateAndSetPartitionTrimmer;

  public
    property IdentifyDeviceResult: TIdentifyDeviceResult
      read GetIdentifyDeviceResultOrRequestAndReturn;
    property SupportStatus: TSupportStatus
      read GetSupportStatusOrRequestAndReturn;
    property SMARTInterpreted: TSMARTInterpreted
      read GetSMARTInterpretedOrRequestAndReturn;
    property DiskSizeInByte: TLargeInteger
      read GetDiskSizeInByte;
    property IsDriveAvailable: Boolean
      read GetIsDriveAvailable;
    property NCQAvailability: TNCQAvailability
      read GetNCQAvailabilityOrRequestAndReturn;
      
    function GetPartitionList: TPartitionList;
    procedure ClearSMARTCache;
    procedure ClearIdentifyDeviceResultCache;
    procedure ClearCache;

    constructor Create(FileToGetAccess: String); reintroduce; overload;
    constructor Create(DriveNumber: Cardinal); reintroduce; overload;
    destructor Destroy; override;

  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess);
  AutoCommandSet := TAutoCommandSet.Create(FileToGetAccess);
end;

constructor TPhysicalDrive.Create(DriveNumber: Cardinal);
var
  PathToAccess: String;
begin
  PathToAccess :=
    ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber);
  Create(PathToAccess);
end;

destructor TPhysicalDrive.Destroy;
begin
  if AutoCommandSet <> nil then
    FreeAndNil(AutoCommandSet);
  if PartitionTrimmer <> nil then
    FreeAndNil(PartitionTrimmer);
  if SMARTValueListReadWrite <> nil then
    FreeAndNil(SMARTValueListReadWrite);
  inherited;
end;

procedure TPhysicalDrive.ClearCache;
begin
  ClearIdentifyDeviceResultCache;
  ClearSMARTCache;
end;

procedure TPhysicalDrive.ClearSMARTCache;
begin
  ZeroMemory(@SMARTInterpretedReadWrite, SizeOf(SMARTInterpretedReadWrite));
  if SMARTValueListReadWrite <> nil then
    FreeAndNil(SMARTValueListReadWrite);
end;

procedure TPhysicalDrive.ClearIdentifyDeviceResultCache;
begin
  ZeroMemory(@IdentifyDeviceResultReadWrite,
    SizeOf(IdentifyDeviceResultReadWrite));
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

function TPhysicalDrive.GetIdentifyDeviceResultOrRequestAndReturn:
  TIdentifyDeviceResult;
begin
  if IdentifyDeviceResultReadWrite.Model = '' then
    RequestIdentifyDevice;
  result := IdentifyDeviceResultReadWrite;
end;

function TPhysicalDrive.GetSupportStatusOrRequestAndReturn:
  TSupportStatus;
begin
  if SupportStatusReadWrite.Supported = false then
    RequestSupportStatus;
  result := SupportStatusReadWrite;
end;

function TPhysicalDrive.GetSMARTInterpretedOrRequestAndReturn:
  TSMARTInterpreted;
begin
  if SMARTInterpretedReadWrite.UsedHour = 0 then
    RequestSMARTInterpreted;
  result := SMARTInterpretedReadWrite;
end;

function TPhysicalDrive.GetSMARTValueListOrRequestAndReturn:
  TSMARTValueList;
begin
  if SMARTValueListReadWrite = nil then
    RequestSMARTReadData;
  result := SMARTValueListReadWrite;
end;

function TPhysicalDrive.GetNCQAvailabilityOrRequestAndReturn: TNCQAvailability;
begin
  if NCQAvailabilityReadWrite = TNCQAvailability.Unknown then
    RequestNCQAvailability;
  result := NCQAvailabilityReadWrite;
end;


procedure TPhysicalDrive.RequestNCQAvailability;

var
  NCQAvailabilityGetter: TNCQAvailabilityGetter;
begin
  NCQAvailabilityGetter := TNCQAvailabilityGetter.Create
    (GetPathOfFileAccessing);
  NCQAvailabilityReadWrite := NCQAvailabilityGetter.GetNCQStatus;
  FreeAndNil(NCQAvailabilityGetter);
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

procedure TPhysicalDrive.RequestIdentifyDevice;
begin
  IdentifyDeviceResultReadWrite := AutoCommandSet.IdentifyDevice;
end;

procedure TPhysicalDrive.RequestSMARTReadData;

begin
  SMARTValueListReadWrite := AutoCommandSet.SMARTReadData;
end;

procedure TPhysicalDrive.TryToCreateAndSetPartitionTrimmer;
begin
  try
    PartitionTrimmer :=
      TPartitionTrimmer.Create(
        IdentifyDeviceResult.Model,
        IdentifyDeviceResult.Firmware);
  except
    FreeAndNil(PartitionTrimmer);
  end;
end;

procedure TPhysicalDrive.RequestSupportStatus;
begin
  if PartitionTrimmer = nil then
    TryToCreateAndSetPartitionTrimmer;
  if PartitionTrimmer <> nil then
    SupportStatusReadWrite := PartitionTrimmer.GetSupportStatus
  else
    SupportStatusReadWrite.Supported := false;
end;

procedure TPhysicalDrive.RequestSMARTInterpreted;
begin
  if PartitionTrimmer = nil then
    TryToCreateAndSetPartitionTrimmer;
  if PartitionTrimmer <> nil then
    SMARTInterpretedReadWrite := PartitionTrimmer.GetSMARTInterpreted(
      GetSMARTValueListOrRequestAndReturn);
end;


end.

