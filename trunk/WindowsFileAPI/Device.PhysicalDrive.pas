unit Device.PhysicalDrive;

interface

uses
  Windows, SysUtils, Device.PhysicalDrive.Bus, Device.PhysicalDrive.OS, OSFile,
  OSFile.Interfaced, Support, Support.Factory,
  Getter.PhysicalDrive.PartitionList,
  BufferInterpreter, Getter.PhysicalDrive.NCQAvailability, OS.Handle;

type
  IPhysicalDrive = interface['{6D76DB52-42D7-4E84-AFAB-61594C7456F9}']
    function GetSupportStatusOrRequestAndReturn: TSupportStatus;
    function GetDiskSizeInByte: TLargeInteger;
    function GetIsDriveAvailable: Boolean;
    function GetIdentifyDeviceResult: TIdentifyDeviceResult;
    function GetSMARTInterpretedOrRequestAndReturn: TSMARTInterpreted;
    function GetNCQAvailability: TNCQAvailability;
    function GetPathOfFileAccessing: String;
    function GetPathOfFileAccessingWithoutPrefix: String;
    property IdentifyDeviceResult: TIdentifyDeviceResult
      read GetIdentifyDeviceResult;
    property SMARTInterpreted: TSMARTInterpreted
      read GetSMARTInterpretedOrRequestAndReturn;
    property SupportStatus: TSupportStatus
      read GetSupportStatusOrRequestAndReturn;
    property DiskSizeInByte: TLargeInteger
      read GetDiskSizeInByte;
    property IsDriveAvailable: Boolean
      read GetIsDriveAvailable;
    property NCQAvailability: TNCQAvailability
      read GetNCQAvailability;
    function GetPartitionList: TPartitionList;
    function Unlock: IOSFileUnlock;
  end;

  TPhysicalDrive = class(TInterfacedOSFile, IPhysicalDrive)
  private
    SupportStatusReadWrite: TSupportStatus;
    SMARTInterpretedReadWrite: TSMARTInterpreted;
    OSPhysicalDrive: TOSPhysicalDrive;
    BusPhysicalDrive: TBusPhysicalDrive;
    NSTSupport: TNSTSupport;
    procedure RequestSMARTInterpreted;
    procedure RequestSupportStatus;
    function GetSupportStatusOrRequestAndReturn: TSupportStatus;
    function GetSMARTInterpretedOrRequestAndReturn: TSMARTInterpreted;
    function GetDiskSizeInByte: TLargeInteger;
    function GetIsDriveAvailable: Boolean;
    procedure TryToCreateAndSetNSTSupport;
    function GetIdentifyDeviceResult: TIdentifyDeviceResult;
    function GetNCQAvailability: TNCQAvailability;
    procedure TryToCreateNSTSupportByFactory(
      const NSTSupportFactory: TNSTSupportFactory);
  public
    property IdentifyDeviceResult: TIdentifyDeviceResult
      read GetIdentifyDeviceResult;
    property SMARTInterpreted: TSMARTInterpreted
      read GetSMARTInterpretedOrRequestAndReturn;
    property SupportStatus: TSupportStatus
      read GetSupportStatusOrRequestAndReturn;
    property DiskSizeInByte: TLargeInteger
      read GetDiskSizeInByte;
    property IsDriveAvailable: Boolean
      read GetIsDriveAvailable;
    property NCQAvailability: TNCQAvailability
      read GetNCQAvailability;
    function GetPartitionList: TPartitionList;
    function Unlock: IOSFileUnlock;
    constructor Create(const FileToGetAccess: String); override;
    class function BuildFileAddressByNumber(const DriveNumber: Cardinal): String;
    destructor Destroy; override;
  end;

implementation

{ TPhysicalDrive }

constructor TPhysicalDrive.Create(const FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess);
  BusPhysicalDrive := TBusPhysicalDrive.Create(FileToGetAccess);
  OSPhysicalDrive := TOSPhysicalDrive.Create(FileToGetAccess);
end;

class function TPhysicalDrive.BuildFileAddressByNumber(
  const DriveNumber: Cardinal): String;
begin
  result :=
    ThisComputerPrefix + PhysicalDrivePrefix + UIntToStr(DriveNumber);
end;

destructor TPhysicalDrive.Destroy;
begin
  if OSPhysicalDrive <> nil then
    FreeAndNil(OSPhysicalDrive);
  if BusPhysicalDrive <> nil then
    FreeAndNil(BusPhysicalDrive);
  if NSTSupport <> nil then
    FreeAndNil(NSTSupport);
  inherited;
end;

function TPhysicalDrive.GetDiskSizeInByte: TLargeInteger;
begin
  result := OSPhysicalDrive.DiskSizeInByte;
end;

function TPhysicalDrive.GetNCQAvailability: TNCQAvailability;
begin
  result := OSPhysicalDrive.NCQAvailability;
end;

function TPhysicalDrive.GetIsDriveAvailable: Boolean;
begin
  result := OSPhysicalDrive.IsDriveAvailable;
end;

function TPhysicalDrive.GetPartitionList: TPartitionList;
begin
  result := OSPhysicalDrive.GetPartitionList;
end;

function TPhysicalDrive.GetIdentifyDeviceResult: TIdentifyDeviceResult;
begin
  result := BusPhysicalDrive.IdentifyDeviceResult;
end;

function TPhysicalDrive.GetSupportStatusOrRequestAndReturn:
  TSupportStatus;
begin
  if SupportStatusReadWrite.Supported = NotSupported then
    RequestSupportStatus;
  result := SupportStatusReadWrite;
end;

function TPhysicalDrive.Unlock: IOSFileUnlock;
begin
  result := BusPhysicalDrive.Unlock;
end;

function TPhysicalDrive.GetSMARTInterpretedOrRequestAndReturn:
  TSMARTInterpreted;
begin
  if SMARTInterpretedReadWrite.UsedHour = 0 then
    RequestSMARTInterpreted;
  result := SMARTInterpretedReadWrite;
end;

procedure TPhysicalDrive.TryToCreateAndSetNSTSupport;
var
  NSTSupportFactory: TNSTSupportFactory;
begin
  NSTSupportFactory := TNSTSupportFactory.Create;
  try
    TryToCreateNSTSupportByFactory(NSTSupportFactory);
  finally
    FreeAndNil(NSTSupportFactory);
  end;
end;

procedure TPhysicalDrive.TryToCreateNSTSupportByFactory(
  const NSTSupportFactory: TNSTSupportFactory);
begin
  try
    NSTSupport :=
      NSTSupportFactory.GetSuitableNSTSupport(
        IdentifyDeviceResult,
        BusPhysicalDrive.SMARTValueList);
  except
    FreeAndNil(NSTSupport);
  end;
end;

procedure TPhysicalDrive.RequestSupportStatus;
begin
  if NSTSupport = nil then
    TryToCreateAndSetNSTSupport;
  if NSTSupport = nil then
    SupportStatusReadWrite.Supported := NotSupported
  else
    SupportStatusReadWrite := NSTSupport.GetSupportStatus;
end;

procedure TPhysicalDrive.RequestSMARTInterpreted;
begin
  if NSTSupport = nil then
    TryToCreateAndSetNSTSupport;
  if NSTSupport <> nil then
    SMARTInterpretedReadWrite := NSTSupport.GetSMARTInterpreted(
      BusPhysicalDrive.SMARTValueList);
end;

end.

