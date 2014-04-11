unit uSSDVersion;

interface

uses SysUtils, Dialogs, uLanguageSettings;

const
  NEW_VERSION = 2;
  OLD_VERSION = 1;
  NOT_MINE = 0;

const
  LastVA8 = 5;
  LastVB8 = 5;
  LastVD8 = 5;
  LastVE8 = 2;
  LastVF8 = 2;

  Last64M3 = 1.06;
  Last128M3 = 1.07;
  Last256M3 = 1.07;
  Last512M3 = 1.06;

  Last128M3P = 1.06;
  Last256M3P = 1.06;
  Last512M3P = 1.06;

  Last128M5P = 1.06;
  Last256M5P = 1.06;
  Last512M5P = 1.06;

  Last128M5M = 1.04;
  Last256M5M = 1.04;
  Last512M5M = 1.04;

  Last64M5S = 1.05;
  Last128M5S = 1.05;
  Last256M5S = 1.05;

  LastNinja = 1.01;

  LastSS = 03;

function IsLiteONNewVer(Model, Revision: String): Byte;
function IsPlextorNewVer(Model, Revision: String): Byte;
function NewFirmCaption(Model, Revision: String): String;
function NewFirmSub(Model, Revision: String): String;

implementation


function IsLiteONNewVer(Model, Revision: String): Byte;
begin
  if  (Pos('LITEONIT', Model) > 0) and
      ((Pos('S100', Model) > 0)
        or (Pos('M3S', Model) > 0)
        or (Pos('E200', Model) > 0)) then
  begin
    result := NEW_VERSION;
    if ((Copy(Revision, 1, 3) = 'VE8') and (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVE8)) or
          ((Copy(Revision, 1, 3) = 'VF8') and (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVF8)) or
          ((Copy(Revision, 1, 3) = 'VA8') and (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVA8)) or
          ((Copy(Revision, 1, 3) = 'VB8') and (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVB8)) or
          ((Copy(Revision, 1, 3) = 'VD8') and (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVD8)) then
      result := OLD_VERSION;
  end
  else
    result := NOT_MINE;
end;

function IsPlextorNewVer(Model, Revision: String): Byte;
begin
  if  (Pos('Ninja', Model) > 0) or
      ((Pos('PLEXTOR', Model) > 0) and
       ((Pos('M3', Model) > 0)
        or (Pos('M5', Model) > 0))) then
  begin
    result := NEW_VERSION;
    if ((Pos('Ninja', Model) > 0) and
          (StrToFloat(Revision) < LastNinja)) or
        ((Pos('64M3', Model) > 0) and
          (StrToFloat(Revision) < Last64M3)) or

        ((Pos('128M3P', Model) > 0) and
          (StrToFloat(Revision) < Last128M3P)) or
        ((Pos('256M3P', Model) > 0) and
          (StrToFloat(Revision) < Last256M3P)) or
        ((Pos('512M3P', Model) > 0) and
          (StrToFloat(Revision) < Last512M3P)) or

        ((Pos('128M5P', Model) > 0) and
          (StrToFloat(Revision) < Last128M5P)) or
        ((Pos('256M5P', Model) > 0) and
          (StrToFloat(Revision) < Last256M5P)) or
        ((Pos('512M5P', Model) > 0) and
          (StrToFloat(Revision) < Last512M5P)) or

        ((Pos('128M5M', Model) > 0) and
          (StrToFloat(Revision) < Last128M5M)) or
        ((Pos('256M5M', Model) > 0) and
          (StrToFloat(Revision) < Last256M5M)) or
        ((Pos('512M5M', Model) > 0) and
          (StrToFloat(Revision) < Last512M5M)) or

        ((Pos('64M5S', Model) > 0) and
          (StrToFloat(Revision) < Last64M5S)) or
        ((Pos('128M5S', Model) > 0) and
          (StrToFloat(Revision) < Last128M5S)) or
        ((Pos('256M5S', Model) > 0) and
          (StrToFloat(Revision) < Last256M5S)) or

        ((Pos('128M3', Model) > 0) and
         ((Model[Pos('128M3', Model) + 5] <>'P') and
         (Model[Pos('128M3', Model) + 5] <>'S')) and
          (StrToFloat(Revision) < Last128M3)) or
        ((Pos('256M3', Model) > 0) and
         ((Model[Pos('256M3', Model) + 5] <>'P') and
         (Model[Pos('256M3', Model) + 5] <>'S')) and
          (StrToFloat(Revision) < Last256M3)) or
        ((Pos('512M3', Model) > 0) and
         ((Model[Pos('512M3', Model) + 5] <>'P') and
         (Model[Pos('512M3', Model) + 5] <>'S')) and
          (StrToFloat(Revision) < Last512M3)) then
      result := OLD_VERSION;
  end
  else
    result := NOT_MINE;
end;

function NewFirmSub(Model, Revision: String): String;
begin
  Result := Copy(Revision, 1, 3);

  if Copy(Revision, 1, 3) = 'VA8' then Result := Result + IntToStr(LastVA8)
  else if Copy(Revision, 1, 3) = 'VB8' then Result := Result + IntToStr(LastVB8)
  else if Copy(Revision, 1, 3) = 'VD8' then Result := Result + IntToStr(LastVD8)
  else if Copy(Revision, 1, 3) = 'VE8' then Result := Result + IntToStr(LastVE8)
  else if Copy(Revision, 1, 3) = 'VF8' then Result := Result + IntToStr(LastVF8)

  else if Pos('64M3', Model) > 0 then
    Result := FloatToStr(Last64M3)
  else if Pos('128M3P', Model) > 0 then
    Result := FloatToStr(Last128M3P)
  else if Pos('256M3P', Model) > 0 then
    Result := FloatToStr(Last256M3P)
  else if Pos('512M3P', Model) > 0 then
    Result := FloatToStr(Last512M3P)

  else if Pos('64M5S', Model) > 0 then
    Result := FloatToStr(Last64M5S)
  else if Pos('128M5S', Model) > 0 then
    Result := FloatToStr(Last128M5S)
  else if Pos('256M5S', Model) > 0 then
    Result := FloatToStr(Last256M5S)

  else if Pos('128M5P', Model) > 0 then
    Result := FloatToStr(Last128M5P)
  else if Pos('256M5P', Model) > 0 then
    Result := FloatToStr(Last256M5P)
  else if Pos('512M5P', Model) > 0 then
    Result := FloatToStr(Last512M5P)

  else if Pos('128M5M', Model) > 0 then
    Result := FloatToStr(Last128M5M)
  else if Pos('256M5M', Model) > 0 then
    Result := FloatToStr(Last256M5M)
  else if Pos('512M5M', Model) > 0 then
    Result := FloatToStr(Last512M5M)

  else if Pos('Ninja', Model) > 0 then
    Result := FloatToStr(LastNinja)

  else if (Pos('128M3', Model) > 0) and
    ((Model[Pos('128M3', Model) + 5] <>'P') and
    (Model[Pos('128M3', Model) + 5] <>'S')) then
      Result := FloatToStr(Last128M3)
  else if (Pos('256M3', Model) > 0) and
    ((Model[Pos('256M3', Model) + 5] <>'P') and
    (Model[Pos('256M3', Model) + 5] <>'S')) then
      Result := FloatToStr(Last256M3)
  else if (Pos('512M3', Model) > 0) and
    ((Model[Pos('512M3', Model) + 5] <>'P') and
    (Model[Pos('512M3', Model) + 5] <>'S')) then
      Result := FloatToStr(Last512M3)
  else if Pos('SAMSUNG', Model) > 0  then
    if LastSS < 10 then Result := Copy(Revision, 1, 3) + '0' + IntToStr(LastSS) + Copy(Revision, 6, 3)
  else Result := Copy(Revision, 1, 3) + IntToStr(LastSS) + Copy(Revision, 6, 3);
end;

function NewFirmCaption(Model, Revision: String): String;
begin
  Result := CapNewFirm[CurrLang] + NewFirmSub(Model, Revision);
end;
end.
