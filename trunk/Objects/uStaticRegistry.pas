unit uStaticRegistry;

interface

uses
  Registry, Windows, Classes, Dialogs, SysUtils,
  uRegistryHelper, uIs64Bit;

type
  TStaticRegistry = class
  public
    class function GetRegInt(const Path: TRegistryPath):
      Integer;
    class function GetRegStr(const Path: TRegistryPath):
      String;
    class function GetKeyList(const Path: TRegistryPath;
      PreparedList: TStringList): TStringList;
    class function GetValueList(const Path: TRegistryPath;
      PreparedList: TStringList): TStringList;
    class function SetRegInt(const Path: TRegistryPath; NewValue: Integer):
      Boolean;
    class function SetRegStr(const Path: TRegistryPath; NewValue: String):
      Boolean;
    class function LegacyPathToNew(Root: String; PathUnderHKEY: String;
      ValueName: String): TRegistryPath;
  private
    class var Registry: TRegistry;
    class var Path: TRegistryPath;

    class procedure SetPath(PathToSet: TRegistryPath);
    class procedure OpenRegistryWithRight(Right: Cardinal);
    class procedure CloseRegistry;
    class function GetBitSpecificRight: Cardinal;
    class function GetTRegistryWithRight(Right: Cardinal): TRegistry;
  end;

implementation

class function TStaticRegistry.GetBitSpecificRight: Cardinal;
const
  KEY_WOW64_64KEY = $0100;
begin
  if Is64Bit then
    exit(KEY_WOW64_64KEY);
  exit(0);
end;

class function TStaticRegistry.GetTRegistryWithRight(Right: Cardinal):
  TRegistry;
begin
  result := TRegistry.Create(Right or GetBitSpecificRight);
end;

class procedure TStaticRegistry.OpenRegistryWithRight(Right: Cardinal);
begin
  Registry := GetTRegistryWithRight(Right);
  if Right = KEY_READ then
    Registry.OpenKeyReadOnlyWithRootAndPath(Path.Root, Path.PathUnderHKEY)
  else
    Registry.OpenKeyWithRootAndPath(Path.Root, Path.PathUnderHKEY);
end;

class function TStaticRegistry.LegacyPathToNew(Root: String; PathUnderHKEY,
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

class procedure TStaticRegistry.CloseRegistry;
begin
  Registry.CloseKey;
  FreeAndNil(Registry);
end;

class procedure TStaticRegistry.SetPath(PathToSet: TRegistryPath);
begin
  Path := PathToSet;
end;

class function TStaticRegistry.GetRegInt(const Path: TRegistryPath): Integer;
begin
  SetPath(Path);
  OpenRegistryWithRight(KEY_READ);
  try
    result := Registry.ReadInteger(Path.ValueName);
  except
    result := 0;
  end;
  CloseRegistry;
end;

class function TStaticRegistry.GetRegStr(const Path: TRegistryPath): String;
begin
  SetPath(Path);
  OpenRegistryWithRight(KEY_READ);
  try
    result := Registry.ReadString(Path.ValueName);
  except
    result := '';
  end;
  CloseRegistry;
end;

class function TStaticRegistry.SetRegInt(const Path: TRegistryPath;
  NewValue: Integer): Boolean;
begin
  SetPath(Path);
  OpenRegistryWithRight(KEY_READ or KEY_WRITE);
  result := true;
  try
    Registry.WriteInteger(Path.ValueName, NewValue);
  except
    result := false;
  end;
  CloseRegistry;
end;

class function TStaticRegistry.SetRegStr(const Path: TRegistryPath;
  NewValue: String): Boolean;
begin
  SetPath(Path);
  OpenRegistryWithRight(KEY_READ or KEY_WRITE);
  result := true;
  try
    Registry.WriteString(Path.ValueName, NewValue);
  except
    result := false;
  end;
  CloseRegistry;
end;

class function TStaticRegistry.GetKeyList(const Path: TRegistryPath;
  PreparedList: TStringList): TStringList;
begin
  SetPath(Path);
  OpenRegistryWithRight(KEY_READ);
  result := PreparedList;
  if result = nil then
    result := TStringList.Create;
  Registry.GetKeyNames(result);
  CloseRegistry;
end;

class function TStaticRegistry.GetValueList(const Path: TRegistryPath;
  PreparedList: TStringList): TStringList;
begin
  SetPath(Path);
  OpenRegistryWithRight(KEY_READ);
  result := PreparedList;
  if result = nil then
    result := TStringList.Create;
  Registry.GetKeyNames(result);
  CloseRegistry;
end;

end.
