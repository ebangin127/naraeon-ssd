unit OS.WindowsVersion;

interface

uses
  Windows,
  OS.VersionHelper;

function Is64Bit: Boolean;
function IsBelowVista: Boolean;
function IsBelowWindows8: Boolean;
function GetWindowsVersionString: String;
function GetWindowsArchitectureString: String;

implementation

uses
  Registry.Helper, Registry.Helper.Internal;

function Is64Bit: Boolean;
type
  TIsWow64Process = function(
    AHandle: THandle; var AIsWow64: BOOL): BOOL; stdcall;
var
  vKernel32Handle: DWORD;
  vIsWow64Process: TIsWow64Process;
  vIsWow64: BOOL;
begin
  Result := False;

  vKernel32Handle := LoadLibrary('kernel32.dll');
  if (vKernel32Handle = 0) then Exit;
  try
    @vIsWow64Process := GetProcAddress(vKernel32Handle, 'IsWow64Process');
    if not Assigned(vIsWow64Process) then Exit;
    vIsWow64 := False;
    if (vIsWow64Process(GetCurrentProcess, vIsWow64)) then
      Result := vIsWow64;
  finally
    FreeLibrary(vKernel32Handle);
  end;
end;

function IsBelowVista: Boolean;
begin
  result := VersionHelper.MajorVersion = 10;
end;

function IsBelowWindows8: Boolean;
begin
  result :=
    (VersionHelper.MajorVersion < 6) or
    ((VersionHelper.MajorVersion = 6) and (VersionHelper.MinorVersion = 1));
end;

function GetWindowsVersionString: String;
const
  Path: TRegistryPath = (
    Root: TRegistryRootKey.LocalMachine;
    PathUnderHKEY: 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\';
    ValueName: 'ProductName');
  PathToBuildLab: TRegistryPath = (
    Root: TRegistryRootKey.LocalMachine;
    PathUnderHKEY: 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\';
    ValueName: 'BuildLab');
  PathToBuildLabEx: TRegistryPath = (
    Root: TRegistryRootKey.LocalMachine;
    PathUnderHKEY: 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\';
    ValueName: 'BuildLabEx');
var
  BuildLab: String;
begin
  result := NSTRegistry.GetRegStr(Path);
  BuildLab := NSTRegistry.GetRegStr(PathToBuildLabEx);
  if BuildLab = '' then
    BuildLab := NSTRegistry.GetRegStr(PathToBuildLab);
  result := result + ' ' + BuildLab;
end;

function GetWindowsArchitectureString: String;
const
  Path: TRegistryPath = (
    Root: TRegistryRootKey.LocalMachine;
    PathUnderHKEY: 'SYSTEM\CurrentControlSet\Control\Session Manager' +
      '\Environment';
    ValueName: 'PROCESSOR_ARCHITECTURE');
begin
  result := NSTRegistry.GetRegStr(Path);
end;
end.
