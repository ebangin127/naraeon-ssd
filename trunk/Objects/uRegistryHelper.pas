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
    
  TRegistryHelper = class helper for TRegistry
  public
    procedure SetRootKey(RegistryRootKey: TRegistryRootKey);
    function OpenKeyWithRootAndPath
      (Root: TRegistryRootKey; Path: String): Boolean;
    function OpenKeyWithReadOnlyWithRootAndPath
      (Root: TRegistryRootKey; Path: String): Boolean;
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

function TRegistryHelper.OpenKeyWithRootAndPath
  (Root: TRegistryRootKey; Path: String): Boolean;
begin 
  SetRootKey(Root);
  result := OpenKeyReadOnly(Path);
end;

function TRegistryHelper.OpenKeyWithReadOnlyWithRootAndPath
  (Root: TRegistryRootKey; Path: String): Boolean;
begin
  SetRootKey(Root);
  result := OpenKeyReadOnly(Path);
end;
end.
