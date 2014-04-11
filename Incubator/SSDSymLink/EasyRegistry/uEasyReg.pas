unit uEasyReg;

interface

uses Registry, Windows, SysUtils;

function GetRegInt(Const Root, Path, ValueName: String): Integer;
function GetRegStr(Const Root, Path, ValueName: String): String;
function SetRegInt(Const Root, Path, ValueName: String; NewValue: Integer): Boolean;
function SetRegStr(Const Root, Path, ValueName, NewValue: String): Boolean;

implementation

function GetRegInt(Const Root, Path, ValueName: String): Integer;
var
  TempRegistry: TRegistry;
begin
  result := 0;
  if (Length(Root) > 0) and (Length(Path) > 0) and (Length(ValueName) > 0) then
  begin
    TempRegistry := TRegistry.Create;
    if Root = 'CR' then TempRegistry.RootKey := HKEY_CLASSES_ROOT
    else if Root = 'CU' then TempRegistry.RootKey := HKEY_CURRENT_USER
    else if Root = 'LM' then TempRegistry.RootKey := HKEY_LOCAL_MACHINE
    else if Root = 'U' then TempRegistry.RootKey := HKEY_USERS
    else if Root = 'PD' then TempRegistry.RootKey := HKEY_PERFORMANCE_DATA
    else if Root = 'CC' then TempRegistry.RootKey := HKEY_CURRENT_CONFIG
    else if Root = 'DD' then TempRegistry.RootKey := HKEY_DYN_DATA;
    TempRegistry.OpenKey(Path, False);
    result := TempRegistry.ReadInteger(ValueName);
    TempRegistry.CloseKey;
    FreeAndNil(TempRegistry);
  end;
end;

function GetRegStr(Const Root, Path, ValueName: String): String;
var
  TempRegistry: TRegistry;
begin
  result := '';
  if (Length(Root) > 0) and (Length(Path) > 0) and (Length(ValueName) > 0) then
  begin
    TempRegistry := TRegistry.Create;
    if Root = 'CR' then TempRegistry.RootKey := HKEY_CLASSES_ROOT
    else if Root = 'CU' then TempRegistry.RootKey := HKEY_CURRENT_USER
    else if Root = 'LM' then TempRegistry.RootKey := HKEY_LOCAL_MACHINE
    else if Root = 'U' then TempRegistry.RootKey := HKEY_USERS
    else if Root = 'PD' then TempRegistry.RootKey := HKEY_PERFORMANCE_DATA
    else if Root = 'CC' then TempRegistry.RootKey := HKEY_CURRENT_CONFIG
    else if Root = 'DD' then TempRegistry.RootKey := HKEY_DYN_DATA;
    TempRegistry.OpenKey(Path, False);
    result := TempRegistry.ReadString(ValueName);
    TempRegistry.CloseKey;
    FreeAndNil(TempRegistry);
  end;
end;

function SetRegInt(Const Root, Path, ValueName: String; NewValue: Integer): Boolean;
var
  TempRegistry: TRegistry;
begin
  result := false;
  if (Length(Root) > 0) and (Length(Path) > 0) and (Length(ValueName) > 0) then
  begin
    TempRegistry := TRegistry.Create;
    if Root = 'CR' then TempRegistry.RootKey := HKEY_CLASSES_ROOT
    else if Root = 'CU' then TempRegistry.RootKey := HKEY_CURRENT_USER
    else if Root = 'LM' then TempRegistry.RootKey := HKEY_LOCAL_MACHINE
    else if Root = 'U' then TempRegistry.RootKey := HKEY_USERS
    else if Root = 'PD' then TempRegistry.RootKey := HKEY_PERFORMANCE_DATA
    else if Root = 'CC' then TempRegistry.RootKey := HKEY_CURRENT_CONFIG
    else if Root = 'DD' then TempRegistry.RootKey := HKEY_DYN_DATA;
    TempRegistry.OpenKey(Path, True);
    TempRegistry.WriteInteger(ValueName, NewValue);
    if TempRegistry.ReadInteger(ValueName) = NewValue then result := true;
    TempRegistry.CloseKey;
    FreeAndNil(TempRegistry);
  end;
end;

function SetRegStr(Const Root, Path, ValueName, NewValue: String): Boolean;
var
  TempRegistry: TRegistry;
begin
  result := false;
  if (Length(Root) > 0) and (Length(Path) > 0) and (Length(ValueName) > 0) then
  begin
    TempRegistry := TRegistry.Create;
    if Root = 'CR' then TempRegistry.RootKey := HKEY_CLASSES_ROOT
    else if Root = 'CU' then TempRegistry.RootKey := HKEY_CURRENT_USER
    else if Root = 'LM' then TempRegistry.RootKey := HKEY_LOCAL_MACHINE
    else if Root = 'U' then TempRegistry.RootKey := HKEY_USERS
    else if Root = 'PD' then TempRegistry.RootKey := HKEY_PERFORMANCE_DATA
    else if Root = 'CC' then TempRegistry.RootKey := HKEY_CURRENT_CONFIG
    else if Root = 'DD' then TempRegistry.RootKey := HKEY_DYN_DATA;
    TempRegistry.OpenKey(Path, True);
    TempRegistry.WriteString(ValueName, NewValue);
    if TempRegistry.ReadString(ValueName) = NewValue then result := true;
    TempRegistry.CloseKey;
    FreeAndNil(TempRegistry);
  end;
end;
end.
