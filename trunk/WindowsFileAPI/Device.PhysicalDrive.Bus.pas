unit Device.PhysicalDrive.Bus;

interface

uses
  Windows, SysUtils,
  OSFile, Device.SMART.List, BufferInterpreter, CommandSet,
  CommandSet.Factory, OS.Handle;

type
  TBusPhysicalDrive = class(TOSFile)
  private
    IdentifyDeviceResultReadWrite: TIdentifyDeviceResult;
    SMARTValueListReadWrite: TSMARTValueList;
    CommandSet: TCommandSet;
    procedure RequestIdentifyDevice;
    procedure RequestSMARTReadData;
    function GetIdentifyDeviceResultOrRequestAndReturn: TIdentifyDeviceResult;
    function GetSMARTValueListOrRequestAndReturn: TSMARTValueList;
  public
    property IdentifyDeviceResult: TIdentifyDeviceResult
      read GetIdentifyDeviceResultOrRequestAndReturn;
    property SMARTValueList: TSMARTValueList
      read GetSMARTValueListOrRequestAndReturn;
    function Unlock: IOSFileUnlock;
    constructor Create(const FileToGetAccess: String); override;
    destructor Destroy; override;

  end;

implementation

{ TBusPhysicalDrive }

constructor TBusPhysicalDrive.Create(const FileToGetAccess: String);
var
  CommandSetWithIdentifyDevice: TCommandSetWithIdentifyDevice;
begin
  inherited Create(FileToGetAccess);
  CommandSetWithIdentifyDevice :=
    CommandSetFactory.GetSuitableCommandSetWithIdentifyDevice(FileToGetAccess);
  CommandSet := CommandSetWithIdentifyDevice.CommandSet;
  IdentifyDeviceResultReadWrite := CommandSetWithIdentifyDevice.IdentifyDevice;
end;

destructor TBusPhysicalDrive.Destroy;
begin
  if CommandSet <> nil then
    FreeAndNil(CommandSet);
  if SMARTValueListReadWrite <> nil then
    FreeAndNil(SMARTValueListReadWrite);
  inherited;
end;

function TBusPhysicalDrive.GetIdentifyDeviceResultOrRequestAndReturn:
  TIdentifyDeviceResult;
begin
  if IdentifyDeviceResultReadWrite.Model = '' then
    RequestIdentifyDevice;
  result := IdentifyDeviceResultReadWrite;
end;

function TBusPhysicalDrive.GetSMARTValueListOrRequestAndReturn:
  TSMARTValueList;
begin
  if SMARTValueListReadWrite = nil then
    RequestSMARTReadData;
  result := SMARTValueListReadWrite;
end;

function TBusPhysicalDrive.Unlock: IOSFileUnlock;
begin
  result := CommandSet.Unlock;
end;

procedure TBusPhysicalDrive.RequestIdentifyDevice;
begin
  IdentifyDeviceResultReadWrite := CommandSet.IdentifyDevice;
end;

procedure TBusPhysicalDrive.RequestSMARTReadData;
begin
  SMARTValueListReadWrite := CommandSet.SMARTReadData;
end;


end.

