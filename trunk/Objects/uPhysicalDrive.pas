unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, uOSFile, uCommandSet,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter,
  uBufferInterpreter, uSMARTValueList, uAutoCommandSet, uAutoNSTSupport,
  uNSTSupport;

type
  TPhysicalDrive = class(TOSFile)
  private
    IdentifyDeviceResultReadWrite: TIdentifyDeviceResult;
    SupportStatusReadWrite: TSupportStatus;
    SMARTInterpretedReadWrite: TSMARTInterpreted;
    DriveAvailabilityGetter: TDriveAvailabilityGetter;
    
    SMARTValueListReadWrite: TSMARTValueList;
    AutoNSTSupportReadWrite: TAutoNSTSupport;
    AutoCommandSet: TAutoCommandSet;
    AutoNSTSupport: TAutoNSTSupport;

    procedure RequestIdentifyDevice;
    procedure RequestSMARTReadData;
    procedure RequestSMARTInterpreted;
    procedure RequestSupportStatus;

    function GetIdentifyDeviceResultOrRequestAndReturn: TIdentifyDeviceResult;
    function GetSupportStatusOrRequestAndReturn: TSupportStatus;
    function GetSMARTInterpretedOrRequestAndReturn: TSMARTInterpreted;
    function GetSMARTValueListOrRequestAndReturn: TSMARTValueList;

    function GetDiskSizeInByte: TLargeInteger;
    function GetIsDriveAvailable: Boolean;
    function TryToGetIsDriveAvailable: Boolean;

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
      
    function GetPartitionList: TPartitionList;
    procedure ClearSMARTCache;
    procedure ClearIdentifyDeviceResultCache;
    procedure ClearCache;

    constructor Create(DriveNumber: Cardinal); reintroduce; overload;
    destructor Destroy; override;

  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(DriveNumber: Cardinal);
begin
  inherited
    Create(ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber));
  AutoCommandSet := TAutoCommandSet.Create(GetPathOfFileAccessing);
end;

destructor TPhysicalDrive.Destroy;
begin
  if AutoCommandSet <> nil then
    FreeAndNil(AutoCommandSet);
  if AutoNSTSupportReadWrite <> nil then
    FreeAndNil(AutoNSTSupportReadWrite);
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
  if SupportStatus.Supported = false then
    RequestSupportStatus;
  result := SupportStatusReadWrite;
end;

function TPhysicalDrive.GetSMARTInterpretedOrRequestAndReturn:
  TSMARTInterpreted;
begin
  if SMARTInterpreted.UsedHour = 0 then
    RequestSMARTInterpreted;
  result := SMARTInterpreted;
end;

function TPhysicalDrive.GetSMARTValueListOrRequestAndReturn:
  TSMARTValueList;
begin
  if SMARTValueListReadWrite <> nil then
    RequestSMARTReadData;
  result := SMARTValueListReadWrite;
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

procedure TPhysicalDrive.RequestSupportStatus;
begin
  if AutoNSTSupport = nil then
    AutoNSTSupport := 
      TAutoNSTSupport.Create(
        IdentifyDeviceResult.Model,
        IdentifyDeviceResult.Firmware);
  SupportStatusReadWrite := AutoNSTSupport.GetSupportStatus;
end;

procedure TPhysicalDrive.RequestSMARTInterpreted;
begin
  if AutoNSTSupport = nil then
    AutoNSTSupport := 
      TAutoNSTSupport.Create(
        IdentifyDeviceResult.Model,
        IdentifyDeviceResult.Firmware);
  SMARTInterpretedReadWrite := AutoNSTSupport.GetSMARTInterpreted(
    GetSMARTValueListOrRequestAndReturn);
end;

end.
