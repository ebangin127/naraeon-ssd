unit uAutoCommandSet;

interface

uses
  Windows, SysUtils, Dialogs,
  uIoControlFile, uOSFile, uCommandSet,
  uBufferInterpreter, uSMARTValueList,
  uATACommandSet, uLegacyATACommandSet, uSATCommandSet;

type
  TMetaCommandSet = class of TCommandSet;
  TAutoCommandSet = class(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
    destructor Destroy; override;

  private
    CommandSet: TCommandSet;
    function TestCommandSetCompatibilityAndReturnIdentifyDevice
      (TCommandSetToTry: TMetaCommandSet;
       LastResult: TIdentifyDeviceResult): TIdentifyDeviceResult;
    function DefaultIdentifyDevice: TIdentifyDeviceResult;
    procedure IfCommandSetNilRaiseException;
  end;

implementation

{ TAutoCommandSet }

function TAutoCommandSet.TestCommandSetCompatibilityAndReturnIdentifyDevice
  (TCommandSetToTry: TMetaCommandSet;
   LastResult: TIdentifyDeviceResult): TIdentifyDeviceResult;
var
  CommandSetToTry: TCommandSet;
begin
  if LastResult.Model <> '' then
  begin
    result := LastResult;
    exit;
  end;
  
  CommandSetToTry := TCommandSetToTry.Create(GetPathOfFileAccessing);
  
  try
    result := CommandSetToTry.IdentifyDevice;
    result.IsDataSetManagementSupported :=
      CommandSetToTry.IsDataSetManagementSupported;
  except
    result.Model := '';
    result.IsDataSetManagementSupported := false;
  end;

  if result.Model = '' then
    FreeAndNil(CommandSetToTry)
  else
    CommandSet := CommandSetToTry;
end;

function TAutoCommandSet.DefaultIdentifyDevice: TIdentifyDeviceResult;
begin
  result := TestCommandSetCompatibilityAndReturnIdentifyDevice
    (TATACommandSet, result);

  result := TestCommandSetCompatibilityAndReturnIdentifyDevice
    (TLegacyATACommandSet, result);

  result := TestCommandSetCompatibilityAndReturnIdentifyDevice
    (TSATCommandSet, result);
end;

destructor TAutoCommandSet.Destroy;
begin
  if CommandSet <> nil then
    FreeAndNil(CommandSet);
  inherited;
end;

function TAutoCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  if CommandSet = nil then
    exit(DefaultIdentifyDevice);
  result := CommandSet.IdentifyDevice;
  result.IsDataSetManagementSupported :=
    CommandSet.IsDataSetManagementSupported;
end;

procedure TAutoCommandSet.IfCommandSetNilRaiseException;
begin
  if CommandSet = nil then
    raise EArgumentNilException.Create('Argument Nil: CommandSet is not set');
end;

function TAutoCommandSet.DataSetManagement(StartLBA,
  LBACount: Int64): Cardinal;
begin
  IfCommandSetNilRaiseException;
  result := CommandSet.DataSetManagement(StartLBA, LBACount);
end;

function TAutoCommandSet.SMARTReadData: TSMARTValueList;
begin
  IfCommandSetNilRaiseException;
  result := CommandSet.SMARTReadData;
end;

function TAutoCommandSet.IsDataSetManagementSupported: Boolean;
begin
  IfCommandSetNilRaiseException;
  result := CommandSet.IsDataSetManagementSupported;
end;

end.
