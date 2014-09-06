unit uSSDInfo;

interface

uses Windows, Classes, Math, Dialogs, SysUtils,
      uATALowOps, uDiskFunctions, uSSDVersion, uSMARTFunctions, uStrFunctions;

const
  NullModel = 0;
  ATAModel = 1;
  SCSIModel = 2;
  DetermineModel = 3;

  SUPPORT_FULL = 0;
  SUPPORT_SEMI = 1;
  SUPPORT_NONE = 2;

  HSUPPORT_NONE = 0;
  HSUPPORT_COUNT = 1;
  HSUPPORT_FULL = 2;

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

  EraseErrorThreshold = 10;
  RepSectorThreshold = 50;
  RepSectorThreshold_PLEXTOR = 25;

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
  SimulationModel = 'SanDisk SD6SB1M128G1022I';
  SimulationFirmware = 'X231600';

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

  NCQSupport := TATALowOps.
                  GetNCQStatus(DeviceHandle);

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

  if ((Pos('LITEONIT', UpperCase(Model)) > 0) and
      (Pos('S100', UpperCase(Model)) > 0) and
      (StrToInt(Copy(Firmware, 3, 2)) < 83)) or
     (((Pos('MXSSD', UpperCase(Model)) > 0) and
       (Pos('MMY', UpperCase(Model)) > 0))) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
      ((Pos('THNSNF', UpperCase(Model)) > 0) or
       (Pos('THNSNH', UpperCase(Model)) > 0) or
       (Pos('THNSNJ', UpperCase(Model)) > 0))) then
  begin
    SSDSupport.SupportHostWrite := HSUPPORT_NONE;
  end
  else if (((Pos('C400', UpperCase(Model)) > 0) and (Pos('MT', UpperCase(Model)) > 0)) or
          ((Pos('M4', UpperCase(Model)) > 0) and (Pos('CT', UpperCase(Model)) > 0))) then
  begin
    SSDSupport.SupportHostWrite := HSUPPORT_COUNT;
  end
  else
  begin
    SSDSupport.SupportHostWrite := HSUPPORT_FULL;
  end;

  if (IsPlextorNewVer(Model, Firmware) <> NOT_MINE) or
      (IsLiteONNewVer(Model, Firmware) <> NOT_MINE) or
      (IsCrucialNewVer(Model, Firmware) <> NOT_MINE) then
  begin
    SSDSupport.SupportFirmUp := true;
  end;

  if ((Pos('S100', Model) > 0) and (Pos('85', Firmware) > 0)) or
      ((IsPlextorNewVer(Model, Firmware) = NEW_VERSION) and
       (Pos('M3', UpperCase(Model)) > 0)) then S10085 := true
  else S10085 := false;

  if (IsPlextorNewVer(Model, Firmware) <> NOT_MINE) or
      (IsLiteONNewVer(Model, Firmware) <> NOT_MINE) or
      (Pos('MXSSD', UpperCase(Model)) > 0) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
       ((Pos('THNSNF', UpperCase(Model)) > 0) or
        (Pos('THNSNH', UpperCase(Model)) > 0) or
        (Pos('THNSNJ', UpperCase(Model)) > 0))) or
      ((Pos('SANDISK', UpperCase(Model)) > 0) and
       (Pos('SD6SB1', UpperCase(Model)) > 0)) or
      ((Pos('ST', UpperCase(Model)) > 0) and
       (Pos('HM000', UpperCase(Model)) > 0)) or
      ((Pos('CRUCIAL', UpperCase(Model)) > 0) and
       ((Pos('M500', UpperCase(Model)) > 0) or
        (Pos('M550', UpperCase(Model)) > 0) or
        (Pos('MX100', UpperCase(Model)) > 0)))
      then
    SupportedDevice := SUPPORT_FULL
  else if
      ((Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3') or
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
    SupportedDevice := SUPPORT_SEMI
  else
    SupportedDevice := SUPPORT_NONE;
end;

procedure TSSDInfo_NST.CollectAllSmartData;
begin
  inherited CollectAllSMARTData;

  IsHostWrite := false;
  if  ((Pos('SAMSUNG', UpperCase(Model)) > 0) and
        (Pos('SSD', UpperCase(Model)) > 0)) or
      ((Pos('CRUCIAL', UpperCase(Model)) > 0) and
       ((Pos('M500', UpperCase(Model)) > 0) or
        (Pos('M550', UpperCase(Model)) > 0) or
        (Pos('MX100', UpperCase(Model)) > 0))) then
  begin
    HostWrites :=
      round(ExtractSMART(SMARTData, 'F1') / 1024 / 2048 * 10 * 1.56);
    IsHostWrite := true;
  end
  else if (Pos('MXSSD', Model) > 0) and (Pos('MMY', Model) > 0) then
    HostWrites := 0
  else if (Pos('MXSSD', Model) > 0) and (Pos('JT', Model) > 0) then
  begin
    HostWrites := round(ExtractSMART(SMARTData, 'F1') / 2);
    IsHostWrite := true;
  end
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
    HostWrites := ExtractSMART(SMARTData, 'F1') * 16;  // 1GB 표준단위
    IsHostWrite := true;
  end
  else if (Pos('Ninja-', Model) > 0) or (Pos('M5P', Model) > 0)
            or (Pos('M5M', Model) > 0) or
          (S10085) then
    HostWrites := (ExtractSMART(SMARTData, 177) * 2)
  else if (Pos('M4', Model) > 0) and (Pos('CT', Model) > 0) then
    HostWrites := 0
  else
    HostWrites := ExtractSMART(SMARTData, 177);

  if ((Pos('SAMSUNG', UpperCase(Model)) > 0) and
      (Pos('SSD', UpperCase(Model)) > 0)) then
    EraseError := ExtractSMART(SMARTData, 'B6')
  else if ((Pos('MX', Model) > 0) and (Pos('MMY', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
           (Pos('THNSNF', UpperCase(Model)) > 0)) then
    EraseError := ExtractSMART(SMARTData, 1)
  else if (Pos('MXSSD', Model) > 0) or ((Pos('OCZ', Model) > 0) and
          (Pos('VERTEX3', Model) > 0)) or
          ((Pos('OCZ', Model) > 0) and
          (Pos('AGILITY3', Model) > 0)) or ((Model = 'SSD 128GB')
                                            or (Model = 'SSD 64GB')) or
          (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
          ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
          ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
            (Pos('THNSNS', UpperCase(Model)) > 0)) then
    EraseError := ExtractSMART(SMARTData, 'AC')
  else if ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
          ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) or
      ((Pos('CRUCIAL', UpperCase(Model)) > 0) and
       ((Pos('M500', UpperCase(Model)) > 0) or
        (Pos('M550', UpperCase(Model)) > 0) or
        (Pos('MX100', UpperCase(Model)) > 0))) then
    EraseError := ExtractSMART(SMARTData, 'AC')
  else
    EraseError := ExtractSMART(SMARTData, 182);
  
  ReplacedSectors := ExtractSMART(SMARTData, 5);
  if (Pos('LITEONIT', UpperCase(Model)) > 0) or
      (Pos('PLEXTOR', UpperCase(Model)) > 0) or
      (Pos('NINJA-', UpperCase(Model)) > 0) then
    RepSectorAlert := ReplacedSectors >= RepSectorThreshold_PLEXTOR
  else RepSectorAlert := ReplacedSectors >= RepSectorThreshold;
end;

end.
