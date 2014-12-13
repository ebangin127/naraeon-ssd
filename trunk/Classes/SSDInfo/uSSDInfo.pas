unit uSSDInfo;

interface

uses Windows, Classes, Math, Dialogs, SysUtils,
      uATALowOps, uDiskFunctions, uSSDSupport, uSMARTFunctions, uStrFunctions;

const
  NullModel = 0;
  ATAModel = 1;
  SCSIModel = 2;
  DetermineModel = 3;

type
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
    SATASpeed: Byte;
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
    ATAorSCSI: Byte;
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
    SupportHostWrite: Integer;
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
    /// <remarks>호스트 쓰기(T) / 낸드 쓰기(F)</remarks>
    IsHostWrite: Boolean; // 호스트 쓰기/낸드 쓰기

    //지원 수준
    SupportedDevice: Byte;
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
  SimulationModel = 'SanDisk SD26SB1M128G1022I';
  SimulationFirmware = 'X230';

  CurrentVersion = '4.7.0';

implementation

constructor TSSDInfo.Create;
begin
  Model := '';
  Firmware := '';
  Serial := '';
  DeviceName := '';
  ATAorSCSI := NullModel;
  USBMode := false;
end;

procedure TSSDInfo.SetDeviceName(DeviceNum: Integer);
var
  DeviceHandle: THandle;
begin
  DeviceName := '\\.\PhysicalDrive' + IntToStr(DeviceNum);
  Model := '';
  Firmware := '';
  Serial := '';

  DeviceHandle := TATALowOps.CreateHandle(DeviceNum);

  LLBufferToInfo(TATALowOps.GetInfoATA(DeviceHandle));
  if Trim(Model) = '' then
  begin
    LLBufferToInfo(TATALowOps.GetInfoSCSI(DeviceHandle));
    ATAorSCSI := SCSIModel;
  end
  else
  begin
    ATAorSCSI := ATAModel;
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
begin
  for CurrBuf := ModelStart to ModelEnd do
    Model := Model + Chr(Buffer[CurrBuf * 2 + 1]) +
                     Chr(Buffer[CurrBuf * 2]);
  Model := Trim(Model);

  for CurrBuf := FirmStart to FirmEnd do
    Firmware := Firmware + Chr(Buffer[CurrBuf * 2 + 1]) +
                           Chr(Buffer[CurrBuf * 2]);
  Firmware := Trim(Firmware);

  for CurrBuf := SerialStart to SerialEnd do
    Serial := Serial + Chr(Buffer[CurrBuf * 2 + 1]) +
                       Chr(Buffer[CurrBuf * 2]);
  Serial := Trim(Serial);

  SATASpeed := Buffer[SataNegStart * 2 + 1] +
               Buffer[SataNegStart * 2];

  SATASpeed := SATASpeed shr 1 and 3;

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

  if ATAorSCSI = ATAModel then
    SMARTData := TATALowOps.GetSMARTATA(DeviceHandle, DeviceNum);
  if (ATAorSCSI = SCSIModel) or (isValidSMART(SMARTData) = false) then
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

  if IsFullySupported(Model, Firmware) <> NOT_MINE then
    SupportedDevice := SUPPORT_FULL
  else if IsSemiSupported(Model, Firmware) <> NOT_MINE then
    SupportedDevice := SUPPORT_SEMI
  else
  begin
    SupportedDevice := SUPPORT_NONE;
    exit;
  end;

  SSDSupport.SupportHostWrite := GetWriteSupportLevel(Model, Firmware);

  if (IsPlextorNewVer(Model, Firmware) <> NOT_MINE) or
      (IsLiteONNewVer(Model, Firmware) <> NOT_MINE) or
      (IsCrucialNewVer(Model, Firmware) <> NOT_MINE) then
  begin
    SSDSupport.SupportFirmUp := true;
  end;

  S10085 := IsS10085Affected(Model, Firmware);
end;

procedure TSSDInfo_NST.CollectAllSmartData;
var
  HWResult: THostWrite;
  RSResult: TRepSector;
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
  EraseError := GetEraseError(Model, Firmware, SMARTData);

  //RepSectorAlert
  RSResult := GetRepSector(Model, Firmware, SMARTData);
  ReplacedSectors := RSResult.ReplacedSectors;
  RepSectorAlert := RSResult.RepSectorAlert;
end;

end.
