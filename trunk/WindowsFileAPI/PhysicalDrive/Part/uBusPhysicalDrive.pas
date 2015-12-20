unit uBusPhysicalDrive;

interface

uses
  Windows, SysUtils,
  uOSFile, uSMARTValueList, uBufferInterpreter, uCommandSet, uCommandSetFactory;

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

    constructor Create(FileToGetAccess: String); override;
    destructor Destroy; override;

  end;

implementation

{ TBusPhysicalDrive }

constructor TBusPhysicalDrive.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess);
  CommandSet := CommandSetFactory.GetSuitableCommandSet(FileToGetAccess);
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

procedure TBusPhysicalDrive.RequestIdentifyDevice;
begin
  IdentifyDeviceResultReadWrite := CommandSet.IdentifyDevice;
end;

procedure TBusPhysicalDrive.RequestSMARTReadData;
begin
  SMARTValueListReadWrite := CommandSet.SMARTReadData;
end;


end.

