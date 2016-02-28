unit Mock.CommandSets;

interface

uses
  Windows, SysUtils, Dialogs,
  Mock.OSFile.IoControl, CommandSet, BufferInterpreter, Device.SMART.List;

type
  TMockCommandSet = class abstract(TCommandSet)
  public
    function IsExternal: Boolean; override;
  end;
  TIntelNVMeCommandSet = class sealed(TMockCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TSamsungNVMeCommandSet = class sealed(TMockCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TATACommandSet = class sealed(TMockCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TLegacyATACommandSet = class sealed(TMockCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TSATCommandSet = class sealed(TMockCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
  end;
  TNVMeWithoutDriverCommandSet = class sealed(TMockCommandSet)
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
  result := nil;
end;

function TIntelNVMeCommandSet.IsDataSetManagementSupported: Boolean;
begin
  result := false;
end;

function TIntelNVMeCommandSet.DataSetManagement(StartLBA, LBACount: Int64):
  Cardinal;
begin
  result := 1;
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
  result := nil;
end;

function TSamsungNVMeCommandSet.IsDataSetManagementSupported: Boolean;
begin
  result := false;
end;

function TSamsungNVMeCommandSet.DataSetManagement(StartLBA, LBACount: Int64):
  Cardinal;
begin
  result := 1;
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
  result := nil;
end;

function TATACommandSet.IsDataSetManagementSupported: Boolean;
begin
  result := false;
end;

function TATACommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
  result := 1;
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
  result := nil;
end;

function TLegacyATACommandSet.IsDataSetManagementSupported: Boolean;
begin
  result := false;
end;

function TLegacyATACommandSet.DataSetManagement(
  StartLBA, LBACount: Int64): Cardinal;
begin
  result := 1;
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
  result := nil;
end;

function TSATCommandSet.IsDataSetManagementSupported: Boolean;
begin
  result := false;
end;

function TSATCommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
  result := 1;
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
  result := nil;
end;

function TNVMeWithoutDriverCommandSet.IsDataSetManagementSupported: Boolean;
begin
  result := false;
end;

function TNVMeWithoutDriverCommandSet.DataSetManagement(
  StartLBA, LBACount: Int64): Cardinal;
begin
  result := 1;
end;

{ TMockCommandSet }

function TMockCommandSet.IsExternal: Boolean;
begin
  result := false;
end;

initialization
  CurrentCommandSet := CommandOrderOfNVMeIntel;
finalization
end.
