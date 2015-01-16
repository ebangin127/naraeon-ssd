unit uSSDSupport;

interface

uses
  SysUtils, Dialogs, Windows, WinInet, uGetFirm,
  uLanguageSettings, uDiskFunctions, uSMARTFunctions;

type
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
  TSupportedSSD = class
    class function IsLiteONSupported(Model, Revision: String): TSupportStatus;
    class function IsPlextorSupported(Model, Revision: String): TSupportStatus;
    class function IsToshibaSupported(Model, Revision: String): TSupportStatus;
    class function IsSandiskSupported(Model, Revision: String): TSupportStatus;
    class function IsSeagateSupported(Model, Revision: String): TSupportStatus;
    class function IsCrucialSupported(Model, Revision: String): TSupportStatus;

    class function IsFullySupported(Model, Revision: String): TSupportStatus;
    class function IsSemiSupported(Model, Revision: String): TSupportStatus;
  end;

//내부용 상수
const
  RepSectorThreshold = 50;
  RepSectorThreshold_PLEXTOR = 25;

  EraseErrorThreshold = 10;


function IsNewVersion(Model, Revision: String): TFirmVersion;
var
  ifConnected: DWORD;
  GetFirm: TGetFirm;
begin
  result := NOT_MINE;

  InternetGetConnectedState(@ifConnected, 0);
  if (ifConnected = INTERNET_CONNECTION_OFFLINE) or
      (ifConnected = 0) then
    exit;

  GetFirm := TGetFirm.Create(Model, Revision);
  result := GetFirm.GetVersion.CurrVersion;
  FreeAndNil(GetFirm);
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
  if (TSupportedSSD.IsPlextorSupported(Model, Revision) <> SUPPORT_NONE) or
     (TSupportedSSD.IsLiteONSupported(Model, Revision) <> SUPPORT_NONE) or
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
   ((IsNewVersion(Model, Revision) = NEW_VERSION) and
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
    if (TSupportedSSD.IsCrucialSupported(Model, Revision) <> SUPPORT_NONE) or
       ((Pos('SAMSUNG', UpperCase(Model)) > 0) and
        (Pos('SSD', UpperCase(Model)) > 0)) then
    begin
      if TSupportedSSD.IsCrucialSupported(Model, Revision) <> SUPPORT_NONE then
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

  else if (TSupportedSSD.IsCrucialSupported(Model, Revision) <> SUPPORT_NONE) or
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
var
  ifConnected: DWORD;
  GetFirm: TGetFirm;
begin
  result := '';

  InternetGetConnectedState(@ifConnected, 0);
  if (ifConnected = INTERNET_CONNECTION_OFFLINE) or
      (ifConnected = 0) then
    exit;

  GetFirm := TGetFirm.Create(Model, Revision);
  result := GetFirm.GetVersion.LatestVersion;
  FreeAndNil(GetFirm);
end;

function NewFirmCaption(Model, Revision: String): String;
begin
  Result := CapNewFirm[CurrLang] + NewFirmSub(Model, Revision);
end;

{ TSupportedClass }
class function TSupportedSSD.IsLiteONSupported(Model,
  Revision: String): TSupportStatus;
begin
  Model := UpperCase(Model);

  result := SUPPORT_NONE;
  if  (Pos('LITEONIT', Model) > 0) and
      ((Pos('S100', Model) > 0)
        or (Pos('M3S', Model) > 0)
        or (Pos('E200', Model) > 0)) then
    result := SUPPORT_FULL;
end;

class function TSupportedSSD.IsPlextorSupported(Model,
  Revision: String): TSupportStatus;
begin
  Model := UpperCase(Model);

  result := SUPPORT_NONE;
  if  (Pos('Ninja', Model) > 0) or
      ((Pos('PLEXTOR', Model) > 0) and
       ((Pos('M3', Model) > 0)
        or (Pos('M5', Model) > 0))) then
    result := SUPPORT_FULL;
end;

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
