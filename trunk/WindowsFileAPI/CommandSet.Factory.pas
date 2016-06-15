unit CommandSet.Factory;

interface

uses
  Windows, SysUtils, Dialogs,
  CommandSet, BufferInterpreter,
  {$IfDef UNITTEST}
  Mock.CommandSets;
  {$Else}
  CommandSet.ATA, CommandSet.ATA.Legacy, CommandSet.SAT,
  CommandSet.NVMe.Intel, CommandSet.NVMe.Samsung, CommandSet.NVMe.OS,
  CommandSet.NVMe.WithoutDriver;
  {$EndIf}

type
  TCommandSetWithIdentifyDevice = record
    CommandSet: TCommandSet;
    IdentifyDevice: TIdentifyDeviceResult;
  end;
  TMetaCommandSet = class of TCommandSet;
  ENoCommandSetException = class(EArgumentNilException);
  ENoNVMeDriverException = class(EArgumentOutOfRangeException);
  TCommandSetFactory = class
  public
    function GetSuitableCommandSet(const FileToGetAccess: String):
      TCommandSet;
    function GetSuitableCommandSetWithIdentifyDevice(
      const FileToGetAccess: String): TCommandSetWithIdentifyDevice;
    class function Create: TCommandSetFactory;
  private
    function TryCommandSetsAndGetRightSet(
      const FileToGetAccess: String): TCommandSetWithIdentifyDevice;
    function TestCommandSetCompatibility(
      const FileToGetAccess: String;
      const TCommandSetToTry: TMetaCommandSet;
      const LastResult: TCommandSetWithIdentifyDevice):
      TCommandSetWithIdentifyDevice;
    function IsNVMe(const FileToGetAccess: string): Boolean;
  end;

var
  CommandSetFactory: TCommandSetFactory;

implementation

{ TCommandSetFactory }

class function TCommandSetFactory.Create: TCommandSetFactory;
begin
  if CommandSetFactory = nil then
    result := inherited Create as self
  else
    result := CommandSetFactory;
end;

function TCommandSetFactory.GetSuitableCommandSet(
  const FileToGetAccess: String): TCommandSet;
begin
  result :=
    GetSuitableCommandSetWithIdentifyDevice(FileToGetAccess).CommandSet;
end;

function TCommandSetFactory.GetSuitableCommandSetWithIdentifyDevice(
  const FileToGetAccess: String): TCommandSetWithIdentifyDevice;
begin
  result := TryCommandSetsAndGetRightSet(FileToGetAccess);
  if result.CommandSet = nil then
    raise ENoCommandSetException.Create('Argument Nil: CommandSet is not set');
end;

function TCommandSetFactory.TryCommandSetsAndGetRightSet(
  const FileToGetAccess: String): TCommandSetWithIdentifyDevice;
var
  ResultOfIsNVMe: Boolean;
begin
  result.CommandSet := nil;
  ResultOfIsNVMe := IsNVMe(FileToGetAccess);
  if ResultOfIsNVMe then
  begin
    result := TestCommandSetCompatibility(
      FileToGetAccess, TIntelNVMeCommandSet, result);
    result := TestCommandSetCompatibility(
      FileToGetAccess, TSamsungNVMeCommandSet, result);
  end;
  result := TestCommandSetCompatibility(
    FileToGetAccess, TOSNVMeCommandSet, result);
  result := TestCommandSetCompatibility(
    FileToGetAccess, TATACommandSet, result);
  result := TestCommandSetCompatibility(
    FileToGetAccess, TLegacyATACommandSet, result);
  result := TestCommandSetCompatibility(
    FileToGetAccess, TSATCommandSet, result);
  if (result.CommandSet = nil) and ResultOfIsNVMe then
    raise ENoNVMeDriverException.Create('No NVMe Driver with: ' +
      FileToGetAccess);
end;

function TCommandSetFactory.TestCommandSetCompatibility(
  const FileToGetAccess: String;
  const TCommandSetToTry: TMetaCommandSet;
  const LastResult: TCommandSetWithIdentifyDevice):
  TCommandSetWithIdentifyDevice;
begin
  if LastResult.CommandSet <> nil then
    exit(LastResult);

  result.CommandSet := TCommandSetToTry.Create(FileToGetAccess);
  
  try
    result.IdentifyDevice := result.CommandSet.IdentifyDevice;
  except
    result.IdentifyDevice.Model := '';
  end;

  if result.IdentifyDevice.Model = '' then
    FreeAndNil(result);
end;

function TCommandSetFactory.IsNVMe(const FileToGetAccess: string):
  Boolean;
var
  Dummy: TCommandSetWithIdentifyDevice;
  TestResult: TCommandSet;
begin
  FillChar(Dummy, SizeOf(Dummy), #0);
  TestResult := TestCommandSetCompatibility(
    FileToGetAccess, TNVMeWithoutDriverCommandSet, Dummy).CommandSet;
  result := TestResult <> nil;
  if result then
    FreeAndNil(TestResult);
end;

initialization
  CommandSetFactory := TCommandSetFactory.Create;
finalization
  CommandSetFactory.Free;
end.
