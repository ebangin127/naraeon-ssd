unit OS.VersionHelper;

interface

uses
  Windows;

type
  TVersionHelper = class
  private
    FMajorVersion: WORD;
    FMinorVersion: WORD;
    FServicePackMajor: WORD;

    function IsWindowsVersionEquals(Major, Minor, ServicePack: WORD): Boolean;

    function GuessServicePackMajorVersion: Boolean;
    function GuessMinorVersion: Boolean;
    procedure GetVersionByBruteforce;
    procedure Initializer;
  public
    class function Create: TVersionHelper;

    property MajorVersion: WORD read FMajorVersion;
    property MinorVersion: WORD read FMinorVersion;
    property ServicePackMajor: WORD read FServicePackMajor;
  end;

var
  VersionHelper: TVersionHelper;

implementation

class function TVersionHelper.Create: TVersionHelper;
begin
  if VersionHelper = nil then
  begin
    result := inherited Create as self;
    result.Initializer;
  end
  else
    result := VersionHelper;
end;

procedure TVersionHelper.GetVersionByBruteforce;
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

function TVersionHelper.GuessMinorVersion: Boolean;
const
  MinorVersionMax = 10;
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

function TVersionHelper.GuessServicePackMajorVersion: Boolean;
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

procedure TVersionHelper.Initializer;
begin
  GetVersionByBruteforce;
end;

function TVersionHelper.IsWindowsVersionEquals(
  Major, Minor, ServicePack: WORD): Boolean;
var
  osvi: OSVERSIONINFOEXW;
  dwlConditionMask: DWORDLONG;
begin
  dwlConditionMask := VerSetConditionMask(
    VerSetConditionMask(
      VerSetConditionMask(0, VER_MAJORVERSION, VER_EQUAL),
      VER_MINORVERSION, VER_EQUAL),
    VER_SERVICEPACKMAJOR, VER_EQUAL);

  osvi.dwMajorVersion := Major;
  osvi.dwMinorVersion := Minor;
  osvi.wServicePackMajor := ServicePack;

  result :=
    VerifyVersionInfoW(
      &osvi,
      VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,
      dwlConditionMask);
end;

initialization
  VersionHelper := TVersionHelper.Create;
finalization
  VersionHelper.Free;
end.
