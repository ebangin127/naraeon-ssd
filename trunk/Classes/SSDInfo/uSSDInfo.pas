unit uSSDInfo;

interface

uses Windows, Classes, Math, Dialogs, SysUtils,
      uATALowOps, uDiskFunctions, uSSDSupport, uSMARTFunctions, uStrFunctions,
      uGetFirm;

type
  TSATASpeed =
    (SPEED_UNKNOWN, SPEED_SATA150, SPEED_SATA300, SPEED_SATA600, SPEED_USB);

  TStorInterface =
    (MODEL_NULL, MODEL_ATA, MODEL_SCSI, MODEL_DETERMINE);

  TSSDInfo = class
    //기본 정보들
    /// <remarks>모델명</remarks>
    Model: String;
    /// <remarks>펌웨어 버전</remarks>
    Firmware: String;
    /// <remarks>시리얼 번호</remarks>
    Serial: String;
    /// <remarks>물리 주소 (ex: \\.\PhysicalDrive0)</remarks>
    DeviceName: String;
    UserSize: UInt64;

    //SATA 정보
    /// <remarks>
    ///   <para>SATA 속도 (0~4)</para>
    ///   <para>Unknown / SATA 1.5Gbps / SATA 3Gbps / SATA 6Gbps / USB</para>
    /// </remarks>
    SATASpeed: TSATASpeed;
    /// <remarks>
    ///   <para>NCQ 지원 여부 (0~2)</para>
    ///   <para>Unknown / 지원 / 미지원</para>
    /// </remarks>
    NCQSupport: Byte;

    //연결 수단
    /// <remarks>
    ///   <para>ATA인가 SCSI인가 (0~3)</para>
    ///   <para>Null / ATA / SCSI / 자동 감지</para>
    /// </remarks>
    ATAorSCSI: TStorInterface;
    USBMode: Boolean;

    //내부에서만 사용되는 정보들
    SMARTData: SENDCMDOUTPARAMS;
    LBASize: Integer;

    procedure SetDeviceName(DeviceNum: Integer); virtual;
    procedure LLBufferToInfo(Buffer: TLLBuffer);
    procedure CollectAllSMARTData; virtual;

    //1. 창조자와 파괴자
    constructor Create;
  end;

const
  ModelStart = 27;
  ModelEnd = 46;
  FirmStart = 23;
  FirmEnd = 26;
  SerialStart = 10;
  SerialEnd = 19;
  UserSizeStart = 100;
  UserSizeEnd = 103;
  LBAStart = 118;
  LBAEnd = 119;
  HPABit = 82;

  SataNegStart = 77;

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
    HostWrites: UInt64;
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

  CurrentVersion = '4.8.1';

implementation

constructor TSSDInfo.Create;
begin
  Model := '';
  Firmware := '';
  Serial := '';
  DeviceName := '';
  ATAorSCSI := MODEL_NULL;
  USBMode := false;
end;

procedure TSSDInfo.SetDeviceName(DeviceNum: Integer);
var
  DeviceHandle: THandle;
begin
  DeviceName := '\\.\PhysicalDrive' + IntToStr(DeviceNum);

  DeviceHandle := TATALowOps.CreateHandle(DeviceNum);

  ATAorSCSI := MODEL_ATA;
  LLBufferToInfo(TATALowOps.GetInfoATADirect(DeviceHandle));

  if Trim(Model) = '' then
    LLBufferToInfo(TATALowOps.GetInfoATA(DeviceHandle));

  if Trim(Model) = '' then
  begin
    ATAorSCSI := MODEL_SCSI;
    LLBufferToInfo(TATALowOps.GetInfoSCSI(DeviceHandle));
  end;

  NCQSupport := TATALowOps.GetNCQStatus(DeviceHandle);

  if SimulationMode then
  begin
    Model := SimulationModel;
    Firmware := SimulationFirmware;
  end;

  CloseHandle(DeviceHandle);
end;

procedure TSSDInfo.LLBufferToInfo(Buffer: TLLBuffer);
var
  CurrBuf: Integer;
  SATASpeedInNum: Integer;
begin
  Model := '';
  for CurrBuf := ModelStart to ModelEnd do
    Model := Model + Chr(Buffer[CurrBuf * 2 + 1]) +
                     Chr(Buffer[CurrBuf * 2]);
  Model := Trim(Model);

  Firmware := '';
  for CurrBuf := FirmStart to FirmEnd do
    Firmware := Firmware + Chr(Buffer[CurrBuf * 2 + 1]) +
                           Chr(Buffer[CurrBuf * 2]);
  Firmware := Trim(Firmware);

  Serial := '';
  for CurrBuf := SerialStart to SerialEnd do
    Serial := Serial + Chr(Buffer[CurrBuf * 2 + 1]) +
                       Chr(Buffer[CurrBuf * 2]);
  Serial := Trim(Serial);

  SATASpeedInNum := Buffer[SataNegStart * 2 + 1] +
                    Buffer[SataNegStart * 2];

  SATASpeedInNum := SATASpeedInNum shr 1 and 3;

  SATASpeed := TSATASpeed(SATASpeedInNum);

  LBASize := 512;

  UserSize := 0;
  for CurrBuf := UserSizeStart to UserSizeEnd do
  begin
    UserSize := UserSize + Buffer[CurrBuf * 2] shl
                (((CurrBuf - UserSizeStart) * 2) * 8);
    UserSize := UserSize + Buffer[CurrBuf * 2 + 1]  shl
                ((((CurrBuf - UserSizeStart) * 2) + 1) * 8);
  end;
end;

procedure TSSDInfo.CollectAllSMARTData;
var
  DeviceNum: Integer;
  DeviceHandle: THandle;
begin
  DeviceNum := StrToInt(ExtractDeviceNum(DeviceName));
  DeviceHandle := TATALowOps.CreateHandle(DeviceNum);

  if ATAorSCSI = MODEL_ATA then
    TransSMART(TATALowOps.GetSMARTATA(DeviceHandle), SMARTData);
  if IsValidSMART(SMARTData) = false then
    TransSMART(TATALowOps.GetSMARTATADirect(DeviceHandle), SMARTData);
  if (ATAorSCSI = MODEL_SCSI) or (IsValidSMART(SMARTData) = false) then
    SMARTData := TATALowOps.GetSMARTSCSI(DeviceHandle);

  CloseHandle(DeviceHandle);
end;

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
    HWResult := GetHostWrites(Model, Firmware, SMARTData, S10085);
    IsHostWrite := HWResult.IsHostWrite;
    HostWrites := HWResult.HostWrites;
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
