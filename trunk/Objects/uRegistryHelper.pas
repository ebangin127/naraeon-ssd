unit uRegistryHelper;

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
    DynData
    );

  TRegistryPath = record
    Root: TRegistryRootKey;
    PathUnderHKEY: String;
    ValueName: String;
  end;

  TRegistryHelper = class helper for TRegistry
  public
    procedure SetRootKey(RegistryRootKey: TRegistryRootKey);
    procedure OpenKeyWithRootAndPath
      (Root: TRegistryRootKey; Path: String);
    procedure OpenKeyReadOnlyWithRootAndPath
      (Root: TRegistryRootKey; Path: String);
  private
    const
      TOSRegistryRootKey: Array[TRegistryRootKey] of HKEY =
        (HKEY_CLASSES_ROOT,
         HKEY_CURRENT_USER,
         HKEY_LOCAL_MACHINE,
         HKEY_USERS,
         HKEY_PERFORMANCE_DATA,
         HKEY_CURRENT_CONFIG,
         HKEY_DYN_DATA
         );
  end;
  
implementation

procedure TRegistryHelper.SetRootKey(RegistryRootKey: TRegistryRootKey);
begin
  RootKey := TOSRegistryRootKey[RegistryRootKey];
end;

procedure TRegistryHelper.OpenKeyWithRootAndPath
  (Root: TRegistryRootKey; Path: String);
begin 
  SetRootKey(Root);
  if not OpenKey(Path, false) then
    raise EOSError.Create(
      'Registry Access Denied: (' + IntToStr(GetLastError) + ') ' + Path);
end;

procedure TRegistryHelper.OpenKeyReadOnlyWithRootAndPath
  (Root: TRegistryRootKey; Path: String);
begin
  SetRootKey(Root);
  if not OpenKeyReadOnly(Path) then
    raise EOSError.Create(
      'Registry Access Denied: (' + IntToStr(GetLastError) + ') ' + Path);
end;
end.
