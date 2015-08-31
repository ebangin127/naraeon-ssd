unit uPhysicalDrive;

interface

uses
  Windows, SysUtils, 
  uOSFile, uNSTSupport, uNSTSupportFactory,
  uDiskGeometryGetter, uPartitionListGetter, uDriveAvailabilityGetter,
  uBufferInterpreter, uSMARTValueList, uCommandSet, 
  uNCQAvailabilityGetter, uCommandSetFactory;

type
  TPhysicalDrive = class(TOSFile)
  private
    IdentifyDeviceResultReadWrite: TIdentifyDeviceResult;
    SupportStatusReadWrite: TSupportStatus;
    SMARTInterpretedReadWrite: TSMARTInterpreted;
    DriveAvailabilityGetter: TDriveAvailabilityGetter;
    SMARTValueListReadWrite: TSMARTValueList;
    NCQAvailabilityReadWrite: TNCQAvailability;

    CommandSet: TCommandSet;
    NSTSupport: TNSTSupport;

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
    procedure TryToCreateAndSetNSTSupport;

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

    constructor Create(FileToGetAccess: String); override;
    class function BuildFileAddressByNumber(DriveNumber: Cardinal): String;
    destructor Destroy; override;

  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess);
  CommandSet := CommandSetFactory.GetSuitableCommandSet(FileToGetAccess);
end;

class function TPhysicalDrive.BuildFileAddressByNumber(
  DriveNumber: Cardinal): String;
begin
  result :=
    ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber);
end;

destructor TPhysicalDrive.Destroy;
begin
  if CommandSet <> nil then
    FreeAndNil(CommandSet);
  if NSTSupport <> nil then
    FreeAndNil(NSTSupport);
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
  IdentifyDeviceResultReadWrite := CommandSet.IdentifyDevice;
end;

procedure TPhysicalDrive.RequestSMARTReadData;

begin
  SMARTValueListReadWrite := CommandSet.SMARTReadData;
end;

procedure TPhysicalDrive.TryToCreateAndSetNSTSupport;
begin
  try
    NSTSupport :=
      NSTSupportFactory.GetSuitableNSTSupport(
        IdentifyDeviceResult.Model,
        IdentifyDeviceResult.Firmware);
  except
    FreeAndNil(NSTSupport);
  end;
end;

procedure TPhysicalDrive.RequestSupportStatus;
begin
  if NSTSupport = nil then
    TryToCreateAndSetNSTSupport;
  if NSTSupport = nil then
    SupportStatusReadWrite.Supported := false
  else
    SupportStatusReadWrite := NSTSupport.GetSupportStatus;
end;

procedure TPhysicalDrive.RequestSMARTInterpreted;
begin
  if NSTSupport = nil then
    TryToCreateAndSetNSTSupport;
  if NSTSupport <> nil then
    SMARTInterpretedReadWrite := NSTSupport.GetSMARTInterpreted(
      GetSMARTValueListOrRequestAndReturn);
end;


end.

