unit Getter.OS.Version;

interface

uses
  Windows,
  Version;

type
  TWindowsVersionGetter = class
  private
    FMajorVersion: WORD;
    FMinorVersion: WORD;
    FServicePackMajor: WORD;
    function IsWindowsVersionEquals(Major, Minor, ServicePack: WORD): Boolean;
    function GuessServicePackMajorVersion: Boolean;
    function GuessMinorVersion: Boolean;
    procedure GetVersionByBruteforce;
    procedure Initializer;
    function GetWindowsVersionAsTVersion: TVersion;
  public
    class function Create: TWindowsVersionGetter;
    property Version: TVersion read GetWindowsVersionAsTVersion;
  end;

var
  VersionHelper: TWindowsVersionGetter;

implementation

class function TWindowsVersionGetter.Create: TWindowsVersionGetter;
begin
  if VersionHelper = nil then
  begin
    result := inherited Create as self;
    result.Initializer;
  end
  else
    result := VersionHelper;
end;

procedure TWindowsVersionGetter.GetVersionByBruteforce;
const
  MajorVersionMin = 5;
  MajorVersionMax = 20;
var
  CurrentVersion: WORD;
begin
  for CurrentVersion := MajorVersionMin to MajorVersionMax do
  begin
    FMajorVersion := CurrentVersion;
    if GuessMinorVersion then
      break;
  end;
end;

function TWindowsVersionGetter.GetWindowsVersionAsTVersion: TVersion;
begin
  result.FMajorVer := FMajorVersion;
  result.FMinorVer := FMinorVersion;
  result.FBuildVer := FServicePackMajor;
end;

function TWindowsVersionGetter.GuessMinorVersion: Boolean;
const
  MinorVersionMax = 20;
var
  CurrentVersion: WORD;
begin
  result := false;
  for CurrentVersion := 0 to MinorVersionMax do
  begin
    FMinorVersion := CurrentVersion;
    if GuessServicePackMajorVersion then
    begin
      result := true;
      break;
    end;
  end;
end;

function TWindowsVersionGetter.GuessServicePackMajorVersion: Boolean;
const
  ServicePackMajorMax = 20;
var
  CurrentVersion: WORD;
begin
  result := false;
  for CurrentVersion := 0 to ServicePackMajorMax do
  begin
    FServicePackMajor := CurrentVersion;
    if IsWindowsVersionEquals(
        FMajorVersion, FMinorVersion, FServicePackMajor) then
    begin
      result := true;
      break;
    end;
  end;
end;

procedure TWindowsVersionGetter.Initializer;
begin
  GetVersionByBruteforce;
end;

function TWindowsVersionGetter.IsWindowsVersionEquals(
  Major, Minor, ServicePack: WORD): Boolean;
var
  osvi: OSVERSIONINFOEXW;
  dwlConditionMask: DWORDLONG;
begin
  dwlConditionMask :=
    VerSetConditionMask(0, VER_MAJORVERSION, VER_EQUAL);
  dwlConditionMask :=
    VerSetConditionMask(dwlConditionMask, VER_MINORVERSION, VER_EQUAL);
  dwlConditionMask :=
    VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMAJOR, VER_EQUAL);

  osvi.dwMajorVersion := Major;
  osvi.dwMinorVersion := Minor;
  osvi.wServicePackMajor := ServicePack;

  result :=
    VerifyVersionInfo(
      &osvi,
      VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,
      dwlConditionMask);
end;

initialization
  VersionHelper := TWindowsVersionGetter.Create;
finalization
  VersionHelper.Free;
end.
