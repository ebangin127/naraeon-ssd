unit uMixedCommandSet;

interface

uses
  Windows, SysUtils,
  uIoControlFile, uOSFile, uCommandSet,
  uBufferInterpreter, uSMARTValueList,
  uATACommandSet, uSATCommandSet;

type
  TMixedCommandSet = class(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;

    function IsDataSetManagementSupported: Boolean; override;
  private
    CommandSet: TCommandSet;
    function TryIdentifyDevice
      (CommandSetToTry: TCommandSet;
       LastResult: TIdentifyDeviceResult): TIdentifyDeviceResult;
    function DefaultIdentifyDevice: TIdentifyDeviceResult;
    procedure IfCommandSetNilRaiseException;
  end;

implementation

{ TMixedCommandSet }

function TMixedCommandSet.TryIdentifyDevice
  (CommandSetToTry: TCommandSet;
   LastResult: TIdentifyDeviceResult): TIdentifyDeviceResult;
begin
  if result.Model <> '' then
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

function TMixedCommandSet.DefaultIdentifyDevice: TIdentifyDeviceResult;
begin
  result :=
    TryIdentifyDevice
      (TATACommandSet.Create(GetPathOfFileAccessing),
       result);
  result :=
    TryIdentifyDevice
      (TSATCommandSet.Create(GetPathOfFileAccessing),
       result);
end;

function TMixedCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  if CommandSet = nil then
    result := DefaultIdentifyDevice;
  result := CommandSet.IdentifyDevice;
  result.IsDataSetManagementSupported :=
    CommandSet.IsDataSetManagementSupported;
end;

procedure TMixedCommandSet.IfCommandSetNilRaiseException;
begin
  if CommandSet = nil then
    raise EArgumentNilException.Create('Argument Nil: CommandSet is not set');
end;

function TMixedCommandSet.DataSetManagement(StartLBA,
  LBACount: Int64): Cardinal;
begin
  IfCommandSetNilRaiseException;
  result := CommandSet.DataSetManagement(StartLBA, LBACount);
end;

function TMixedCommandSet.SMARTReadData: TSMARTValueList;
begin
  IfCommandSetNilRaiseException;
  result := CommandSet.SMARTReadData;
end;

function TMixedCommandSet.IsDataSetManagementSupported: Boolean;
begin
  IfCommandSetNilRaiseException;
  result := CommandSet.IsDataSetManagementSupported;
end;

end.
