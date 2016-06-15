unit Registry.Helper.Internal;

interface

uses
  Registry, Windows, Classes, Dialogs, SysUtils;

type
  TRegistryRootKey = (
    ClassesRoot,
    CurrentUser,
    LocalMachine,
    Users,
    PerformanceData,
    CurrentConfig,
    DynData);
  TRegistryPath = record
    Root: TRegistryRootKey;
    PathUnderHKEY: String;
    ValueName: String;
  end;
  TRegistryHelper = class helper for TRegistry
  public
    procedure SetRootKey(RegistryRootKey: TRegistryRootKey);
    procedure OpenKeyWithRootAndPath
      (Root: TRegistryRootKey; const Path: String);
    procedure OpenKeyReadOnlyWithRootAndPath
      (Root: TRegistryRootKey; const Path: String);
  private
    const
      TOSRegistryRootKey: Array[TRegistryRootKey] of HKEY =
        (HKEY_CLASSES_ROOT,
         HKEY_CURRENT_USER,
         HKEY_LOCAL_MACHINE,
         HKEY_USERS,
         HKEY_PERFORMANCE_DATA,
         HKEY_CURRENT_CONFIG,
         HKEY_DYN_DATA);
  end;
  
implementation

procedure TRegistryHelper.SetRootKey(RegistryRootKey: TRegistryRootKey);
begin
  RootKey := TOSRegistryRootKey[RegistryRootKey];
end;

procedure RaiseOSError(const ErrorCode: Cardinal; const Path: String);
var
  E: EOSError;
begin
  E := EOSError.Create(
    'Registry Access Denied: (' + IntToStr(ERROR_BADKEY) + ') ' + Path);
  E.ErrorCode := ErrorCode;
  raise E;
end;

procedure TRegistryHelper.OpenKeyWithRootAndPath
  (Root: TRegistryRootKey; const Path: String);
begin 
  SetRootKey(Root);
  if not KeyExists(Path) then
    RaiseOSError(ERROR_BADKEY, Path);
  if not OpenKey(Path, false) then
    RaiseOSError(GetLastError, Path);
end;

procedure TRegistryHelper.OpenKeyReadOnlyWithRootAndPath
  (Root: TRegistryRootKey; const Path: String);
begin
  SetRootKey(Root);
  if not KeyExists(Path) then
    RaiseOSError(ERROR_BADKEY, Path);
  if not OpenKeyReadOnly(Path) then
    RaiseOSError(GetLastError, Path);
end;
end.
