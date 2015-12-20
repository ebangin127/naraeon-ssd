unit uWindowsVersion;

interface

uses
  Windows,
  uVersionHelper;

function Is64Bit: Boolean;
function IsBelowWindows8: Boolean;

implementation

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

function IsBelowWindows8: Boolean;
begin
  result :=
    (VersionHelper.MajorVersion < 6) or
    ((VersionHelper.MajorVersion = 6) and (VersionHelper.MinorVersion = 1));
end;
end.
