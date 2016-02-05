unit Mock.CommandSets;

interface

uses
  Windows, SysUtils, Dialogs,
  OSFile.IoControl, CommandSet, BufferInterpreter, Device.SMART.List;

type
  TIntelNVMeCommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TSamsungNVMeCommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TATACommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TLegacyATACommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TSATCommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TNVMeWithoutDriverCommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TCommandOrder = (
    CommandOrderOfNVMeIntel,
    CommandOrderOfNVMeSamsung,
    CommandOrderOfATA,
    CommandOrderOfATALegacy,
    CommandOrderOfSAT,
    CommandOrderOfNVMeWithoutDriver,
    CommandOrderFinished);
  EWrongOrderException = class(Exception);  

function GetCurrentCommandSet: TCommandOrder;

implementation

var
  CurrentCommandSet: TCommandOrder;

function GetCurrentCommandSet: TCommandOrder;
begin
  result := CurrentCommandSet;
end;

function TIntelNVMeCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  result.Model := '';
  if CurrentCommandSet = TCommandOrder.CommandOrderOfNVMeIntel then
    result.Model := 'Right!'
  else
    exit;
  Inc(CurrentCommandSet);
end;

function TIntelNVMeCommandSet.SMARTReadData: TSMARTValueList;
begin
end;

function TIntelNVMeCommandSet.IsDataSetManagementSupported: Boolean;
begin
end;

function TIntelNVMeCommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
end;

function TSamsungNVMeCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  result.Model := '';
  if CurrentCommandSet = TCommandOrder.CommandOrderOfNVMeSamsung then
    result.Model := 'Right!'
  else
    exit;
  Inc(CurrentCommandSet);
end;

function TSamsungNVMeCommandSet.SMARTReadData: TSMARTValueList;
begin
end;

function TSamsungNVMeCommandSet.IsDataSetManagementSupported: Boolean;
begin
end;

function TSamsungNVMeCommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
end;

function TATACommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  result.Model := '';
  if CurrentCommandSet = TCommandOrder.CommandOrderOfATA then
    result.Model := 'Right!'
  else
    exit;
  Inc(CurrentCommandSet);
end;

function TATACommandSet.SMARTReadData: TSMARTValueList;
begin
end;

function TATACommandSet.IsDataSetManagementSupported: Boolean;
begin
end;

function TATACommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
end;

function TLegacyATACommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  result.Model := '';
  if CurrentCommandSet = TCommandOrder.CommandOrderOfATALegacy then
    result.Model := 'Right!'
  else
    exit;
  Inc(CurrentCommandSet);
end;

function TLegacyATACommandSet.SMARTReadData: TSMARTValueList;
begin
end;

function TLegacyATACommandSet.IsDataSetManagementSupported: Boolean;
begin
end;

function TLegacyATACommandSet.DataSetManagement(
  StartLBA, LBACount: Int64): Cardinal;
begin
end;

function TSATCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  result.Model := '';
  if CurrentCommandSet = TCommandOrder.CommandOrderOfSAT then
    result.Model := 'Right!'
  else
    exit;
  Inc(CurrentCommandSet);
end;

function TSATCommandSet.SMARTReadData: TSMARTValueList;
begin
end;

function TSATCommandSet.IsDataSetManagementSupported: Boolean;
begin
end;

function TSATCommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
end;

function TNVMeWithoutDriverCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  result.Model := '';
  if CurrentCommandSet = TCommandOrder.CommandOrderOfNVMeWithoutDriver then
    result.Model := 'Right!'
  else
    exit;
  Inc(CurrentCommandSet);
end;

function TNVMeWithoutDriverCommandSet.SMARTReadData: TSMARTValueList;
begin
end;

function TNVMeWithoutDriverCommandSet.IsDataSetManagementSupported: Boolean;
begin
end;

function TNVMeWithoutDriverCommandSet.DataSetManagement(
  StartLBA, LBACount: Int64): Cardinal;
begin
end;

initialization
  CurrentCommandSet := CommandOrderOfNVMeIntel;
finalization
end.
