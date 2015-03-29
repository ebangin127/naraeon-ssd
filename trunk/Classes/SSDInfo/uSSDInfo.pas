unit uSSDInfo;

interface

uses
  uPhysicalDrive, uGetFirm;

type
  TSSDSupportStatus = record
    SupportHostWrite: THostSupportStatus;
    SupportFirmUp: Boolean;
  end;

  TSSDInfo_NST = class(TSSDInfo)
    //표시될 정보들
    /// <remarks>
    ///   <para>S10085 False: 기본 64MB 단위</para>
    ///   <para>S10085 True: 기본 128MB 단위</para>
    /// </remarks>
    HostWriteInLiteONUnit: UInt64;
    EraseError: UInt64;
    ReplacedSectors: UInt64;
    RepSectorAlert: Boolean;
    EraseErrorAlert: Boolean;
    /// <remarks>호스트 쓰기(T) / 낸드 쓰기(F)</remarks>
    IsHostWrite: Boolean; // 호스트 쓰기/낸드 쓰기

    //지원 수준
    SupportedDevice: TSupportStatus;
    SSDSupport: TSSDSupportStatus;

    //128MB 용량 단위 적용 여부
    S10085: Boolean;

    //창조자와 파괴자
    constructor Create;
    procedure SetDeviceName(DeviceNum: Integer); reintroduce;
    procedure CollectAllSMARTData; reintroduce;
  end;

var
  SimulationMode: Boolean = false;

const
  SimulationModel = 'PLEXTOR PX-128M3P';
  SimulationFirmware = '1.03';

  CurrentVersion = '4.8.2';

implementation

constructor TSSDInfo_NST.Create;
begin
  inherited Create;
  S10085 := false;
end;

procedure TSSDInfo_NST.SetDeviceName(DeviceNum: Integer);
begin
  inherited SetDeviceName(DeviceNum);

  SSDSupport.SupportHostWrite := HSUPPORT_NONE;
  SSDSupport.SupportFirmUp := false;

  SupportedDevice := GetSupportStatus(Model, Firmware);
  if SupportedDevice = SUPPORT_NONE then
  begin
    exit;
  end;

  SSDSupport.SupportHostWrite := GetWriteSupportLevel(Model, Firmware);

  if IsNewVersion(Model, Firmware) <> NOT_MINE then
    SSDSupport.SupportFirmUp := true;

  S10085 := IsS10085Affected(Model, Firmware);
end;

procedure TSSDInfo_NST.CollectAllSmartData;
var
  HWResult: THostWrite;
  RSResult: TRepSector;
  EEResult: TEraseError;
begin
  inherited CollectAllSMARTData;

  IsHostWrite := false;

  //HostWrites
  if SSDSupport.SupportHostWrite = HSUPPORT_FULL then
  begin
    HWResult := GetHostWriteInLiteONUnit(Model, Firmware, SMARTData, S10085);
    IsHostWrite := HWResult.IsHostWrite;
    HostWriteInLiteONUnit := HWResult.HostWriteInLiteONUnit;
  end;

  //EraseError
  EEResult := GetEraseError(Model, Firmware, SMARTData);
  EraseError := EEResult.EraseError;
  EraseErrorAlert := EEResult.EraseErrorAlert;

  //RepSectorAlert
  RSResult := GetRepSector(Model, Firmware, SMARTData);
  ReplacedSectors := RSResult.ReplacedSectors;
  RepSectorAlert := RSResult.RepSectorAlert;
end;

end.
