unit uSSDSupport;

interface

uses SysUtils, Dialogs, uLanguageSettings, uDiskFunctions, uSMARTFunctions;

type
  TFirmVersion = (NOT_MINE, OLD_VERSION, NEW_VERSION);
  TSupportStatus = (SUPPORT_NONE, SUPPORT_SEMI, SUPPORT_FULL);
  THostSupportStatus = (HSUPPORT_NONE, HSUPPORT_COUNT, HSUPPORT_FULL);

  THostWrite = record
    IsHostWrite: Boolean;
    HostWrites: UInt64;
  end;

  TRepSector = record
    RepSectorAlert: Boolean;
    ReplacedSectors: UInt64;
  end;

  TEraseError = record
    EraseErrorAlert: Boolean;
    EraseError: UInt64;
  end;

function IsNewVersion(Model, Revision: String): TFirmVersion;
function GetSupportStatus(Model, Revision: String): TSupportStatus;

function GetWriteSupportLevel(Model, Revision: String): THostSupportStatus;
function IsS10085Affected(Model, Revision: String): Boolean;

function GetHostWrites(Model, Revision: String; SMARTData: SENDCMDOUTPARAMS;
          S10085: Boolean): THostWrite;
function GetEraseError(Model, Revision: String; SMARTData: SENDCMDOUTPARAMS)
          :TEraseError;
function GetRepSector(Model, Revision: String; SMARTData: SENDCMDOUTPARAMS)
          :TRepSector;

function NewFirmCaption(Model, Revision: String): String;
function NewFirmSub(Model, Revision: String): String;

implementation

// 내부용 클래스
type
  TNewVer = class
    class function IsLiteONNewVer(Model, Revision: String): TFirmVersion;
    class function IsPlextorNewVer(Model, Revision: String): TFirmVersion;
    class function IsCrucialNewVer(Model, Revision: String): TFirmVersion;
  end;

  TSupportedSSD = class
    class function IsToshibaSupported(Model, Revision: String): TSupportStatus;
    class function IsSandiskSupported(Model, Revision: String): TSupportStatus;
    class function IsSeagateSupported(Model, Revision: String): TSupportStatus;
    class function IsCrucialSupported(Model, Revision: String): TSupportStatus;

    class function IsFullySupported(Model, Revision: String): TSupportStatus;
    class function IsSemiSupported(Model, Revision: String): TSupportStatus;
  end;

//내부용 상수
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

  Last128M5P = 1.07;
  Last256M5P = 1.07;
  Last512M5P = 1.07;

  Last64M5S = 1.05;
  Last128M5S = 1.05;
  Last256M5S = 1.05;

  LastNinja = 1.01;

  LastM500 = 5;

  RepSectorThreshold = 50;
  RepSectorThreshold_PLEXTOR = 25;

  EraseErrorThreshold = 10;


function IsNewVersion(Model, Revision: String): TFirmVersion;
var
  CurrTestingVer: TFirmVersion;
begin
  CurrTestingVer := TNewVer.IsPlextorNewVer(Model, Revision);
  if CurrTestingVer <> NOT_MINE then
    exit(CurrTestingVer);

  CurrTestingVer := TNewVer.IsLiteONNewVer(Model, Revision);
  if CurrTestingVer <> NOT_MINE then
    exit(CurrTestingVer);

  CurrTestingVer := TNewVer.IsCrucialNewVer(Model, Revision);
  result := CurrTestingVer;
end;

function GetSupportStatus(Model, Revision: String): TSupportStatus;
begin
  result :=
    TSupportStatus(
      Integer(TSupportedSSD.IsFullySupported(Model, Revision)) or
      Integer(TSupportedSSD.IsSemiSupported(Model, Revision))
    );
end;

class function TSupportedSSD.IsFullySupported(Model, Revision: String):
                TSupportStatus;
begin      
  result := SUPPORT_NONE;
  if (TNewVer.IsPlextorNewVer(Model, Revision) <> NOT_MINE) or
     (TNewVer.IsLiteONNewVer(Model, Revision) <> NOT_MINE) or
     (TSupportedSSD.IsCrucialSupported(Model, Revision) <> SUPPORT_NONE) or
     (TSupportedSSD.IsToshibaSupported(Model, Revision) <> SUPPORT_NONE) or
     (TSupportedSSD.IsSandiskSupported(Model, Revision) <> SUPPORT_NONE) or
     (TSupportedSSD.IsSeagateSupported(Model, Revision) <> SUPPORT_NONE) or
     (Pos('MXSSD', UpperCase(Model)) > 0) then
    result := SUPPORT_FULL;
end;

class function TSupportedSSD.IsSemiSupported(Model, Revision: String):
                TSupportStatus;
begin            
  result := SUPPORT_NONE;
  if ((Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3') or
      (Model = 'OCZ-VERTEX3 MI')) or
      ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
      ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) or
      ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
      (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
      ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
      ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
      ((Pos('SAMSUNG', UpperCase(Model)) > 0) and
       (Pos('SSD', UpperCase(Model)) > 0)) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
       (Pos('THNSNS', UpperCase(Model)) > 0)) then
    result := SUPPORT_SEMI;
end;

function GetWriteSupportLevel(Model, Revision: String): THostSupportStatus;
begin       
  Model := UpperCase(Model);
  if (
      //S100 Under 83
      (Pos('LITEONIT', Model) > 0) and
      (Pos('S100', Model) > 0) and
      (StrToInt(Copy(Revision, 3, 2)) < 83)
     ) or
     (
      //MachXtreme Myles
      (Pos('MXSSD', Model) > 0) and
      (Pos('MMY', Model) > 0)
     ) or
     (TSupportedSSD.IsToshibaSupported(Model, Revision) <> SUPPORT_NONE) then
  begin
    result := HSUPPORT_NONE;
  end
  else if (
            (Pos('C400', Model) > 0) and 
            (Pos('MT', Model) > 0)
          ) or
          (
            (Pos('M4', Model) > 0) and 
            (Pos('CT', Model) > 0)
          ) then
  begin
    result := HSUPPORT_COUNT;
  end
  else
  begin
    result := HSUPPORT_FULL;
  end;   
end;

function IsS10085Affected(Model, Revision: String): Boolean;
begin
  result := 
   ((Pos('S100', Model) > 0) and
    (Pos('85', Revision) > 0)) or
   ((TNewVer.IsPlextorNewVer(Model, Revision) = NEW_VERSION) and
    (Pos('M3', Model) > 0));
end;

function GetHostWrites(Model, Revision: String; SMARTData: SENDCMDOUTPARAMS;
          S10085: Boolean): THostWrite;
var
  Position: String;
begin
  with result do
  begin

    // LBA 단위
    if (TNewVer.IsCrucialNewVer(Model, Revision) <> NOT_MINE) or
       ((Pos('SAMSUNG', UpperCase(Model)) > 0) and
        (Pos('SSD', UpperCase(Model)) > 0)) then
    begin
      if TNewVer.IsCrucialNewVer(Model, Revision) <> NOT_MINE then
        Position := 'F6'
      else
        Position := 'F1';

      HostWrites :=
        round(ExtractSMART(SMARTData, Position) / 1024 / 2048 * 10 * 1.56);
      IsHostWrite := true;
    end

    // 32MB 단위
    else if (Pos('MXSSD', Model) > 0) and (Pos('JT', Model) > 0) then
    begin
      HostWrites := round(ExtractSMART(SMARTData, 'F1') / 2);
      IsHostWrite := true;
    end

    // 1GB 표준단위
    else if (Pos('MXSSD', Model) > 0) or ((Pos('OCZ', Model) > 0) and
            (Pos('VERTEX3', Model) > 0)) or
            ((Pos('OCZ', Model) > 0) and (Pos('AGILITY3', Model) > 0)) or
            ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
            (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
            ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
            ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
            ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
             (Pos('THNSNS', UpperCase(Model)) > 0)) or
            ((Pos('SANDISK', UpperCase(Model)) > 0) and
             (Pos('SD6SB1', UpperCase(Model)) > 0)) or
            ((Pos('ST', Model) > 0) and (Pos('HM000', Model) > 0)) then
    begin
      HostWrites := ExtractSMART(SMARTData, 'F1') * 16;
      IsHostWrite := true;
    end

    // 128MB 단위
    else if (Pos('Ninja-', Model) > 0) or
            (Pos('M5P', Model) > 0) or
            (S10085) then
      HostWrites := (ExtractSMART(SMARTData, 177) * 2)

    // 64MB 단위
    else
      HostWrites := ExtractSMART(SMARTData, 177);
  end;
end;

function GetEraseError(Model, Revision: String; SMARTData: SENDCMDOUTPARAMS)
          :TEraseError;
begin
  if ((Pos('SAMSUNG', UpperCase(Model)) > 0) and
      (Pos('SSD', UpperCase(Model)) > 0)) then
    result.EraseError := ExtractSMART(SMARTData, 'B6')

  else if ((Pos('MX', Model) > 0) and (Pos('MMY', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
           (Pos('THNSNF', UpperCase(Model)) > 0)) then
    result.EraseError := ExtractSMART(SMARTData, 1)

  else if (TNewVer.IsCrucialNewVer(Model, Revision) <> NOT_MINE) or
          (Pos('MXSSD', Model) > 0) or
          ((Pos('OCZ', Model) > 0) and
           ((Pos('VERTEX3', Model) > 0) or
            (Pos('AGILITY3', Model) > 0))) or
          ((Model = 'SSD 128GB') or
           (Model = 'SSD 64GB')) or
          (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
          ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
          ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
            (Pos('THNSNS', UpperCase(Model)) > 0)) or
          ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
          ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) then
    result.EraseError := ExtractSMART(SMARTData, 'AC')

  else
    result.EraseError := ExtractSMART(SMARTData, 182);

  result.EraseErrorAlert := result.EraseError >= EraseErrorThreshold;
end;

function GetRepSector(Model, Revision: String; SMARTData: SENDCMDOUTPARAMS)
          :TRepSector;
begin
  result.ReplacedSectors := ExtractSMART(SMARTData, 5);

  if (Pos('LITEONIT', UpperCase(Model)) > 0) or
     (Pos('PLEXTOR', UpperCase(Model)) > 0) or
     (Pos('NINJA-', UpperCase(Model)) > 0) then
    result.RepSectorAlert :=
      result.ReplacedSectors >= RepSectorThreshold_PLEXTOR
  else
    result.RepSectorAlert :=
      result.ReplacedSectors >= RepSectorThreshold;
end;

function NewFirmSub(Model, Revision: String): String;
begin
  Result := Copy(Revision, 1, 3);

  if Copy(Revision, 1, 3) = 'VA8' then
    Result := Result + IntToStr(LastVA8)
  else if Copy(Revision, 1, 3) = 'VB8' then
    Result := Result + IntToStr(LastVB8)
  else if Copy(Revision, 1, 3) = 'VD8' then
    Result := Result + IntToStr(LastVD8)
  else if Copy(Revision, 1, 3) = 'VE8' then
    Result := Result + IntToStr(LastVE8)
  else if Copy(Revision, 1, 3) = 'VF8' then
    Result := Result + IntToStr(LastVF8)

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

  else if (Pos('Crucial', Model) > 0) and
          (Pos('M500', Model) > 0) then
    Result := 'MU' + Format('%.2d', [LastM500]);
end;

function NewFirmCaption(Model, Revision: String): String;
begin
  Result := CapNewFirm[CurrLang] + NewFirmSub(Model, Revision);
end;

{ TNewVerClass }

class function TNewVer.IsLiteONNewVer(Model, Revision: String): TFirmVersion;
begin
  if  (Pos('LITEONIT', Model) > 0) and
      ((Pos('S100', Model) > 0)
        or (Pos('M3S', Model) > 0)
        or (Pos('E200', Model) > 0)) then
  begin
    result := NEW_VERSION;
    if ((Copy(Revision, 1, 3) = 'VE8') and
        (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVE8)) or
       ((Copy(Revision, 1, 3) = 'VF8') and
        (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVF8)) or
       ((Copy(Revision, 1, 3) = 'VA8') and
        (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVA8)) or
       ((Copy(Revision, 1, 3) = 'VB8') and
        (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVB8)) or
       ((Copy(Revision, 1, 3) = 'VD8') and
        (StrToInt(Copy(Revision, 4, Length(Revision) - 3)) < LastVD8)) then
      result := OLD_VERSION;
  end
  else
    result := NOT_MINE;
end;

class function TNewVer.IsPlextorNewVer(Model, Revision: String): TFirmVersion;
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

class function TNewVer.IsCrucialNewVer(Model, Revision: String): TFirmVersion;
begin
  Model := UpperCase(Model);

  result := NOT_MINE;
  //M550&MX100 펌웨어 나오면 주석 풀 것
  if (Pos('CRUCIAL', Model) > 0) and
     ((Pos('M500', Model) > 0){ or
      (Pos('M550', Model) > 0) or
      (Pos('MX100', Model) > 0)}) then
  begin
    result := NEW_VERSION;
    if (Pos('M500', Model) > 0) and
       (StrToInt(Copy(Revision, 3, 2)) < LastM500) then
      result := OLD_VERSION;
  end;
end;

{ TSupportedClass }

class function TSupportedSSD.IsSandiskSupported(Model,
  Revision: String): TSupportStatus;
begin
  Model := UpperCase(Model);

  result := SUPPORT_NONE;
  if (Pos('SANDISK', Model) > 0) and
     (Pos('SD6SB1', Model) > 0) then
    result := SUPPORT_FULL;
end;

class function TSupportedSSD.IsSeagateSupported(Model,
  Revision: String): TSupportStatus;
begin
  Model := UpperCase(Model);

  result := SUPPORT_NONE;
  if (Pos('ST', Model) > 0) and
     (Pos('HM000', Model) > 0) then
    result := SUPPORT_FULL;
end;

class function TSupportedSSD.IsToshibaSupported(Model,
  Revision: String): TSupportStatus;
begin
  Model := UpperCase(Model);

  result := SUPPORT_NONE;
  if (Pos('TOSHIBA', Model) > 0) and
     ((Pos('THNSNF', Model) > 0) or
      (Pos('THNSNH', Model) > 0) or
      (Pos('THNSNJ', Model) > 0)) then
    result := SUPPORT_FULL;
end;

class function TSupportedSSD.IsCrucialSupported(Model,
  Revision: String): TSupportStatus;
begin
  Model := UpperCase(Model);

  result := SUPPORT_NONE;
  if (Pos('CRUCIAL', Model) > 0) and
     ((Pos('M500', Model) > 0) or
      (Pos('M550', Model) > 0) or
      (Pos('MX100', Model) > 0)) then
    result := SUPPORT_FULL;
end;

end.
