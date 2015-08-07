unit uCommandSetFactory;

interface

uses
  Windows, SysUtils, Dialogs,
  uCommandSet, uATACommandSet, uLegacyATACommandSet, uSATCommandSet;

type
  TMetaCommandSet = class of TCommandSet;
  TCommandSetFactory = class
  public
    class function GetSuitableCommandSet(FileToGetAccess: String):
      TCommandSet;

  private
    class var FileToGetAccess: String;
    class function TryCommandSetsAndGetRightSet: TCommandSet;
    class function TestCommandSetCompatibility(
      TCommandSetToTry: TMetaCommandSet; LastResult: TCommandSet): TCommandSet;
  end;

implementation

{ TCommandSetFactory }

class function TCommandSetFactory.GetSuitableCommandSet(
  FileToGetAccess: String): TCommandSet;
begin
  self.FileToGetAccess := FileToGetAccess;
  result := TryCommandSetsAndGetRightSet;
  if result = nil then
    raise EArgumentNilException.Create('Argument Nil: CommandSet is not set');
end;

class function TAutoCommandSet.TryCommandSetsAndGetRightSet: TCommandSet;
begin
  result := nil;
  result := TestCommandSetCompatibility(TATACommandSet, result);
  result := TestCommandSetCompatibility(TLegacyATACommandSet, result);
  result := TestCommandSetCompatibility(TSATCommandSet, result);
end;

class function TAutoCommandSet.TestCommandSetCompatibility
  (TCommandSetToTry: TMetaCommandSet; LastResult: TCommandSet): TCommandSet;
var
  CommandSetToTry: TCommandSet;
  IdentifyDeviceResult: TIdentifyDeviceResult;
begin
  if LastResult <> nil then
    exit(LastResult);
  
  result := TCommandSetToTry.Create(FileToGetAccess);
  
  try
    IdentifyDeviceResult := CommandSetToTry.IdentifyDevice;
  except
    IdentifyDeviceResult.Model := '';
  end;

  if IdentifyDeviceResult.Model = '' then
    FreeAndNil(result);
end;

end.
