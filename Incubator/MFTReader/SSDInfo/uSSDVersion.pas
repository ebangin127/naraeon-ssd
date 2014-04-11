unit uSSDVersion;

interface

uses SysUtils, Dialogs;

const
  NEW_VERSION = 2;
  OLD_VERSION = 1;
  NOT_MINE = 0;

const
  LastVA8 = 3;
  LastVB8 = 3;
  LastVD8 = 3;
  LastVE8 = 2;
  LastVF8 = 2;

  Last64M3 = 1.04;
  Last128M3 = 1.05;
  Last256M3 = 1.05;
  Last512M3 = 1.04;

  Last128M3P = 1.04;
  Last256M3P = 1.04;
  Last512M3P = 1.04;

  Last128M5P = 1.02;
  Last256M5P = 1.02;
  Last512M5P = 1.02;

  Last64M5S = 1.02;
  Last128M5S = 1.02;
  Last256M5S = 1.02;

  LastNinja = 1.01;

  LastVerAgi3 = 2.25;

  LastDS = '5.0.2';
  LastCruMic = '040H';

  LastSS = 03;

function IsLiteONNewVer(Model, Revision: String): Byte;
function IsPlextorNewVer(Model, Revision: String): Byte;
function IsSamsungNewVer(Model, Revision: String): Byte;
function IsOCZNewVer(Model, Revision: String): Byte;
function IsMachExtremeNewVer(Model, Revision: String): Byte;
function IsCrucialMicronNewVer(Model, Revision: String): Byte;
function NewFirmCaption(Model, Revision: String): String;

implementation


function IsLiteONNewVer(Model, Revision: String): Byte;
begin
  if Pos('LITEONIT', Model) > 0 then
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
  if (Pos('PLEXTOR', Model) > 0) or (Pos('Ninja', Model) > 0) then
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
    result := NOT_MINE;;
end;

function IsSamsungNewVer(Model, Revision: String): Byte;
begin
  if (Pos('SAMSUNG', Model) > 0) and (StrToInt(Copy(Revision, 4, 2)) >= LastSS) then
    result := NEW_VERSION
  else if (Pos('SAMSUNG', Model) > 0) and (StrToInt(Copy(Revision, 4, 2)) < LastSS) then
    result := OLD_VERSION
  else
    result := NOT_MINE;
end;


function IsOCZNewVer(Model, Revision: String): Byte;
begin
  if ((Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3')) and (StrToFloat(Revision) >= LastVerAgi3) then
    result := NEW_VERSION
  else if ((Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3')) and (StrToFloat(Revision) < LastVerAgi3) then
    result := OLD_VERSION
  else
    result := NOT_MINE;
end;

function IsMachExtremeNewVer(Model, Revision: String): Byte;
begin
  if (Pos('MX', Model) > 0) and (Pos('DSF', Model) > 0) and (StrToInt(Revision[1] + Revision[3] + Revision[5])
                                                          >= StrToInt(LastDS[1] + LastDS[3] + LastDS[5])) then
    result := NEW_VERSION
  else if (Pos('MX', Model) > 0) and (Pos('DSF', Model) > 0) and (StrToInt(Revision[1] + Revision[3] + Revision[5])
                                                                < StrToInt(LastDS[1] + LastDS[3] + LastDS[5])) then
    result := OLD_VERSION
  else
    result := NOT_MINE;
end;

function IsCrucialMicronNewVer(Model, Revision: String): Byte;
begin
  if (((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) or ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)))
       and (Revision = LastCruMic) then
    result := NEW_VERSION
  else if (((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) or ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)))
        and (Revision <> LastCruMic) then
    result := OLD_VERSION
  else
    result := NOT_MINE;
end;

function NewFirmCaption(Model, Revision: String): String;
begin
  Result := Copy(Revision, 1, 3);

  if (Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3') then Result := '새 펌웨어 버전 : ' + FloatToStr(LastVerAgi3)

  else if (Pos('MX', Model) > 0) and (Pos('DS', Model) > 0) then Result := '새 펌웨어 버전 : ' + LastDS

  else if Copy(Revision, 1, 3) = 'VA8' then Result := '새 펌웨어 버전 : ' + Result + IntToStr(LastVA8)
  else if Copy(Revision, 1, 3) = 'VB8' then Result := '새 펌웨어 버전 : ' + Result + IntToStr(LastVB8)
  else if Copy(Revision, 1, 3) = 'VD8' then Result := '새 펌웨어 버전 : ' + Result + IntToStr(LastVD8)
  else if Copy(Revision, 1, 3) = 'VE8' then Result := '새 펌웨어 버전 : ' + Result + IntToStr(LastVE8)
  else if Copy(Revision, 1, 3) = 'VF8' then Result := '새 펌웨어 버전 : ' + Result + IntToStr(LastVF8)

  else if Pos('64M3', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last64M3)
  else if Pos('128M3P', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last128M3P)
  else if Pos('256M3P', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last256M3P)
  else if Pos('512M3P', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last512M3P)

  else if Pos('64M5S', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last64M5S)
  else if Pos('128M5S', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last128M5S)
  else if Pos('256M5S', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last256M5S)

  else if Pos('128M5P', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last128M5P)
  else if Pos('256M5P', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last256M5P)
  else if Pos('512M5P', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(Last512M5P)

  else if Pos('Ninja', Model) > 0 then
    Result := '새 펌웨어 버전 : ' + FloatToStr(LastNinja)

  else if (Pos('128M3', Model) > 0) and
    ((Model[Pos('128M3', Model) + 5] <>'P') and
    (Model[Pos('128M3', Model) + 5] <>'S')) then
      Result := '새 펌웨어 버전 : ' + FloatToStr(Last128M3)
  else if (Pos('256M3', Model) > 0) and
    ((Model[Pos('256M3', Model) + 5] <>'P') and
    (Model[Pos('256M3', Model) + 5] <>'S')) then
      Result := '새 펌웨어 버전 : ' + FloatToStr(Last256M3)
  else if (Pos('512M3', Model) > 0) and
    ((Model[Pos('512M3', Model) + 5] <>'P') and
    (Model[Pos('512M3', Model) + 5] <>'S')) then
      Result := '새 펌웨어 버전 : ' + FloatToStr(Last512M3)
  else if Pos('SAMSUNG', Model) > 0  then
    if LastSS < 10 then Result := '새 펌웨어 버전 : ' + Copy(Revision, 1, 3) + '0' + IntToStr(LastSS) + Copy(Revision, 6, 3)
  else Result := '새 펌웨어 버전 : ' + Copy(Revision, 1, 3) + IntToStr(LastSS) + Copy(Revision, 6, 3);
end;
end.
