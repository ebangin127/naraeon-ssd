unit uAutoCommandSet;

interface

uses
  Windows, SysUtils,
  uIoControlFile, uOSFile, uCommandSet,
  uBufferInterpreter, uSMARTValueList,
  uATACommandSet, uSATCommandSet;

type
  TAutoCommandSet = class(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;

    function IsDataSetManagementSupported: Boolean; override;

  private
    CommandSet: TCommandSet;
    function TestCommandSetCompatibilityAndReturnIdentifyDevice
      (CommandSetToTry: TCommandSet;
       LastResult: TIdentifyDeviceResult): TIdentifyDeviceResult;
    function DefaultIdentifyDevice: TIdentifyDeviceResult;
    procedure IfCommandSetNilRaiseException;
  end;

implementation

{ TAutoCommandSet }

function TAutoCommandSet.TestCommandSetCompatibilityAndReturnIdentifyDevice
  (CommandSetToTry: TCommandSet;
   LastResult: TIdentifyDeviceResult): TIdentifyDeviceResult;
begin
  if LastResult.Model <> '' then
  begin
    FreeAndNil(CommandSetToTry);
    result := LastResult;
  end;

  result := CommandSetToTry.IdentifyDevice;
  result.IsDataSetManagementSupported :=
    CommandSetToTry.IsDataSetManagementSupported;

  if result.Model = '' then
    FreeAndNil(CommandSetToTry)
  else
    CommandSet := CommandSetToTry;
end;

function TAutoCommandSet.DefaultIdentifyDevice: TIdentifyDeviceResult;
begin
  result :=
    TestCommandSetCompatibilityAndReturnIdentifyDevice
      (TATACommandSet.Create(GetPathOfFileAccessing),
       result);
  result :=
    TestCommandSetCompatibilityAndReturnIdentifyDevice
      (TSATCommandSet.Create(GetPathOfFileAccessing),
       result);
end;

function TAutoCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  if CommandSet = nil then
    result := DefaultIdentifyDevice;
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
