unit Registry.Helper;

interface

uses
  Registry, Windows, Classes, Dialogs, SysUtils,
  Registry.Helper.Internal, OS.Version.Helper;

type
  TNSTRegistry = class
  public
    function GetRegInt(const Path: TRegistryPath):
      Integer;
    function GetRegStr(const Path: TRegistryPath):
      String;
    function GetKeyList(const Path: TRegistryPath;
      PreparedList: TStringList): TStringList;
    function GetValueList(const Path: TRegistryPath;
      PreparedList: TStringList): TStringList;
    function SetRegInt(const Path: TRegistryPath; NewValue: Integer):
      Boolean;
    function SetRegStr(const Path: TRegistryPath; const NewValue: String):
      Boolean;
    function LegacyPathToNew(const Root: String; const PathUnderHKEY: String;
      const ValueName: String): TRegistryPath;
    class function Create: TNSTRegistry;
  private
    procedure SetPath(PathToSet: TRegistryPath);
    procedure OpenRegistryWithRight(Right: Cardinal);
    procedure CloseRegistry;
    function GetBitSpecificRight: Cardinal;
    function GetTRegistryWithRight(Right: Cardinal): TRegistry;
    var
      Registry: TRegistry;
      Path: TRegistryPath;
  end;

var
  NSTRegistry: TNSTRegistry;

implementation

class function TNSTRegistry.Create: TNSTRegistry;
begin
  if NSTRegistry = nil then
    result := inherited Create as self
  else
    result := NSTRegistry;
end;

function TNSTRegistry.GetBitSpecificRight: Cardinal;
const
  KEY_WOW64_64KEY = $0100;
begin
  if Is64Bit then
    exit(KEY_WOW64_64KEY);
  exit(0);
end;

function TNSTRegistry.GetTRegistryWithRight(Right: Cardinal):
  TRegistry;
begin
  result := TRegistry.Create(Right or GetBitSpecificRight);
end;

procedure TNSTRegistry.OpenRegistryWithRight(Right: Cardinal);
begin
  Registry := GetTRegistryWithRight(Right);
  if Right = KEY_READ then
    Registry.OpenKeyReadOnlyWithRootAndPath(Path.Root, Path.PathUnderHKEY)
  else
    Registry.OpenKeyWithRootAndPath(Path.Root, Path.PathUnderHKEY);
end;

function TNSTRegistry.LegacyPathToNew(const Root: String; const PathUnderHKEY,
  ValueName: String): TRegistryPath;
begin
  if Root = 'CR' then result.Root := ClassesRoot
  else if Root = 'CU' then result.Root := CurrentUser
  else if Root = 'LM' then result.Root := LocalMachine
  else if Root = 'U' then result.Root := Users
  else if Root = 'PD' then result.Root := PerformanceData
  else if Root = 'CC' then result.Root := CurrentConfig
  else if Root = 'DD' then result.Root := DynData;

  result.PathUnderHKEY := PathUnderHKEY;
  result.ValueName := ValueName;
end;

procedure TNSTRegistry.CloseRegistry;
begin
  Registry.CloseKey;
  FreeAndNil(Registry);
end;

procedure TNSTRegistry.SetPath(PathToSet: TRegistryPath);
begin
  Path := PathToSet;
end;

function TNSTRegistry.GetRegInt(const Path: TRegistryPath): Integer;
begin
  SetPath(Path);
  try
    OpenRegistryWithRight(KEY_READ);
    result := Registry.ReadInteger(Path.ValueName);
  except
    on E: EOSError do
      if E.ErrorCode = ERROR_BADKEY then
        result := -1
      else
        result := 0;
    on E: ERegistryException do
      result := -1;
    else
      result := 0;
  end;
  CloseRegistry;
end;

function TNSTRegistry.GetRegStr(const Path: TRegistryPath): String;
begin
  SetPath(Path);
  try
    OpenRegistryWithRight(KEY_READ);
    result := Registry.ReadString(Path.ValueName);
  except
    result := '';
  end;
  CloseRegistry;
end;

function TNSTRegistry.SetRegInt(const Path: TRegistryPath;
  NewValue: Integer): Boolean;
begin
  SetPath(Path);
  result := true;
  try
    OpenRegistryWithRight(KEY_READ or KEY_WRITE);
    Registry.WriteInteger(Path.ValueName, NewValue);
  except
    result := false;
  end;
  CloseRegistry;
end;

function TNSTRegistry.SetRegStr(const Path: TRegistryPath;
  const NewValue: String): Boolean;
begin
  SetPath(Path);
  result := true;
  try
    OpenRegistryWithRight(KEY_READ or KEY_WRITE);
    Registry.WriteString(Path.ValueName, NewValue);
  except
    result := false;
  end;
  CloseRegistry;
end;

function TNSTRegistry.GetKeyList(const Path: TRegistryPath;
  PreparedList: TStringList): TStringList;
begin
  SetPath(Path);
  try
    OpenRegistryWithRight(KEY_READ);
    result := PreparedList;
    if result = nil then
      result := TStringList.Create;
    Registry.GetKeyNames(result);
  except
    result := TStringList.Create;
  end;
  CloseRegistry;
end;

function TNSTRegistry.GetValueList(const Path: TRegistryPath;
  PreparedList: TStringList): TStringList;
begin
  SetPath(Path);
  try
    OpenRegistryWithRight(KEY_READ);
    result := PreparedList;
    if result = nil then
      result := TStringList.Create;
    Registry.GetKeyNames(result);
  except
    result := TStringList.Create;
  end;
  CloseRegistry;
end;

initialization
  NSTRegistry := TNSTRegistry.Create;
finalization
  NSTRegistry.Free;
end.
