unit Version;

interface

uses SysUtils;

type
  TVersion = record
    FMajorVer: Integer;
    FMinorVer: Integer;
    FBuildVer: Integer;

    class operator Equal(a: TVersion; b: TVersion): Boolean;
    class operator NotEqual(a: TVersion; b: TVersion): Boolean;
    class operator GreaterThan(a: TVersion; b: TVersion): Boolean;
    class operator GreaterThanOrEqual(a: TVersion; b: TVersion): Boolean;
    class operator LessThan(a: TVersion; b: TVersion): Boolean;
    class operator LessThanOrEqual(a: TVersion; b: TVersion): Boolean;
  end;

function VersionStringToTVersion(const Input: String): TVersion;

implementation

function VersionStringToTVersion(const Input: String): TVersion;
var
  CurrentChar, CurrStat: Integer;
  MajorVer, MinorVer, BuildVer: String;
begin
  CurrStat := 0;
  for CurrentChar := 1 to Length(Input) do
  begin
    if (Input[CurrentChar] <> '.') and (CurrStat = 0) then
      MajorVer := MajorVer + Input[CurrentChar]
    else if (Input[CurrentChar] <> '.') and (CurrStat = 1) then
      MinorVer := MinorVer + Input[CurrentChar]
    else if (Input[CurrentChar] <> '.') and (CurrStat = 2) then
      BuildVer := BuildVer + Input[CurrentChar]
    else if Input[CurrentChar] = '.' then
    begin
      CurrStat := CurrStat + 1;
      if CurrStat = 3 then
        break;
    end;
  end;

  try
    result.FMajorVer := StrToInt(MajorVer);
    result.FMinorVer := StrToInt(MinorVer);
    result.FBuildVer := StrToInt(BuildVer);
  except
    result.FMajorVer := 0;
    result.FMinorVer := 0;
    result.FBuildVer := 0;
  end;
end;

class operator TVersion.NotEqual(a: TVersion; b: TVersion): Boolean;
begin
  result := (a.FMajorVer <> b.FMajorVer) or
            (a.FMinorVer <> b.FMinorVer) or
            (a.FBuildVer <> b.FBuildVer);
end;

class operator TVersion.Equal(a, b: TVersion): Boolean;
begin
  result := (a.FMajorVer = b.FMajorVer) and
            (a.FMinorVer = b.FMinorVer) and
            (a.FBuildVer = b.FBuildVer);
end;

class operator TVersion.GreaterThan(a: TVersion; b: TVersion): Boolean;
begin
  result := (a.FMajorVer > b.FMajorVer) or
            ((a.FMajorVer = b.FMajorVer) and
             (a.FMinorVer > b.FMinorVer)) or
            ((a.FMajorVer = b.FMajorVer) and
             (a.FMinorVer = b.FMinorVer) and
             (a.FBuildVer > b.FBuildVer));
end;

class operator TVersion.GreaterThanOrEqual(a: TVersion; b: TVersion): Boolean;
begin
  result := (a > b) or (a = b);
end;

class operator TVersion.LessThan(a: TVersion; b: TVersion): Boolean;
begin
  result := (a.FMajorVer < b.FMajorVer) or
            ((a.FMajorVer = b.FMajorVer) and
             (a.FMinorVer < b.FMinorVer)) or
            ((a.FMajorVer = b.FMajorVer) and
             (a.FMinorVer = b.FMinorVer) and
             (a.FBuildVer < b.FBuildVer));
end;

class operator TVersion.LessThanOrEqual(a: TVersion; b: TVersion): Boolean;
begin
  result := (a < b) or (a = b);
end;

end.
