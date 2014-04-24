unit uSSDInfo;

interface

uses Windows, Classes, Math, Dialogs, SysUtils,
      uDiskFunctions, uSSDVersion, uSMARTFunctions, uStrFunctions;

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
    Model: String;
    Firmware: String;
    Serial: String;
    DeviceName: String;
    UserSize: UInt64;

    //SATA 정보
    SATASpeed: Byte;
    NCQSupport: Byte;

    //연결 수단
    ATAorSCSI: Byte;
    USBMode: Boolean;

    //내부에서만 사용되는 정보들
    SMARTData: SENDCMDOUTPARAMS;
    LBASize: Integer;

    procedure SetDeviceName(Harddisk: string); virtual;
    procedure CollectAllSmartData; virtual;
    //1. 창조자와 파괴자
    constructor Create;
    protected
      procedure GetInfoATA;
      procedure GetInfoSCSI;

      function GetSmartDataATA: SENDCMDOUTPARAMS;
      function GetSmartDataSCSI: SENDCMDOUTPARAMS;
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
    HostWrites: UInt64;
    EraseError: UInt64;
    ReplacedSectors: UInt64;
    RepSectorAlert: Boolean;

    //지원 수준
    SupportedDevice: Byte;
    SSDSupport: TSSDSupportStatus;

    //128MB 용량 단위 적용 여부
    S10085: Boolean;

    //1. 창조자와 파괴자
    constructor Create;
    procedure SetDeviceName(Harddisk: string); reintroduce;
    procedure CollectAllSmartData; reintroduce;
  end;

var
  SimulationMode: Boolean = false;

const
  SimulationModel = 'SanDisk SD6SB1M128G1022I';
  SimulationFirmware = 'X231600';

  CurrentVersion = '4.6.0';

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

procedure TSSDInfo.SetDeviceName(Harddisk: string);
begin
  DeviceName := '\\.\' + Harddisk;
  Model := '';
  Firmware := '';
  Serial := '';

  GetInfoATA;
  if Trim(Model) = '' then
  begin
    GetInfoSCSI;
    ATAorSCSI := SCSIModel;
  end
  else
  begin
    ATAorSCSI := ATAModel;
  end;

  NCQSupport := GetNCQStatus(DeviceName);

  if SimulationMode then
  begin
    Model := SimulationModel;
    Firmware := SimulationFirmware;
  end;
end;

procedure TSSDInfo.CollectAllSmartData;
begin
  if ATAorSCSI = ATAModel then SMARTData := GetSmartDataATA;
  if (ATAorSCSI = SCSIModel) or (isValidSMART(SMARTData) = false) then SMARTData := GetSmartDataSCSI;
end;

procedure TSSDInfo.GetInfoATA;
var
  ICBuffer: ATA_PTH_BUFFER;
  hdrive: THandle;
  bResult: Boolean;
  BytesRead: Cardinal;
  CurrBuf: Integer;
begin
  FillChar(ICBuffer, SizeOf(ICBuffer), #0);

  hdrive := CreateFile(PChar(DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_DATA_IN;
    ICBuffer.PTH.DataTransferLength := 512;
    ICBuffer.PTH.TimeOutValue := 2;
    ICBuffer.PTH.DataBufferOffset := PChar(@ICBuffer.Buffer) - PChar(@ICBuffer.PTH) + 20;

    ICBuffer.PTH.CurrentTaskFile[6] := $EC;

    bResult := DeviceIOControl(hdrive, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
    if bResult and (GetLastError = 0) then
    begin
      for CurrBuf := ModelStart to ModelEnd do
        Model := Model + Chr(ICBuffer.Buffer[CurrBuf * 2 + 1]) +
                         Chr(ICBuffer.Buffer[CurrBuf * 2]);
      Model := Trim(Model);

      for CurrBuf := FirmStart to FirmEnd do
        Firmware := Firmware + Chr(ICBuffer.Buffer[CurrBuf * 2 + 1]) +
                               Chr(ICBuffer.Buffer[CurrBuf * 2]);
      Firmware := Trim(Firmware);

      for CurrBuf := SerialStart to SerialEnd do
        Serial := Serial + Chr(ICBuffer.Buffer[CurrBuf * 2 + 1]) +
                           Chr(ICBuffer.Buffer[CurrBuf * 2]);
      Serial := Trim(Serial);

      SATASpeed := ICBuffer.Buffer[SataNegStart * 2 + 1] +
                    ICBuffer.Buffer[SataNegStart * 2];

      SATASpeed := SATASpeed shr 1 and 3;

      LBASize := ReadSector(DeviceName, 0, 4096);

      UserSize := 0;
      for CurrBuf := UserSizeStart to UserSizeEnd do
      begin
        UserSize := UserSize + ICBuffer.Buffer[CurrBuf * 2] shl (((CurrBuf - UserSizeStart) * 2) * 8);
        UserSize := UserSize + ICBuffer.Buffer[CurrBuf * 2 + 1]  shl ((((CurrBuf - UserSizeStart) * 2) + 1) * 8);
      end;
    end;
    CloseHandle(hdrive);
  end;
end;

procedure TSSDInfo.GetInfoSCSI;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  Status: Longbool;
  ICBuffer: SCSI_PTH_BUFFER;
  CurrBuf: Integer;
begin
  fillchar(ICBuffer, SizeOf(ICBuffer), #0);
	ICBuffer.spt.Length     := sizeof(SCSI_PASS_THROUGH);
  ICBuffer.spt.TargetId   := 1;
  ICBuffer.spt.CdbLength  := 12;
	ICBuffer.spt.SenseInfoLength := 24;
	ICBuffer.spt.DataIn  := 1;
	ICBuffer.spt.DataTransferLength := 512;
	ICBuffer.spt.TimeOutValue := 2;
	ICBuffer.spt.DataBufferOffset := pansichar(@ICBuffer.Buffer)-pansichar(@ICBuffer);
	ICBuffer.spt.SenseInfoOffset  := pansichar(@ICBuffer.SenseBuf)-pansichar(@ICBuffer);
  ICBuffer.spt.Cdb[0] := $A1;
  ICBuffer.spt.Cdb[1] := $8;
  ICBuffer.spt.Cdb[2] := $E;
  ICBuffer.spt.Cdb[4] := $1;
	ICBuffer.spt.Cdb[9] := $EC;

  hdrive := CreateFile(PChar(DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, IOCTL_SCSI_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), dwBytesReturned, nil);
    if status and (GetLastError = 0) and (ICBuffer.SenseBuf[0] = 0) then
    begin
      for CurrBuf := ModelStart to ModelEnd do
        Model := Model + Chr(ICBuffer.Buffer[CurrBuf * 2 + 1]) +
                         Chr(ICBuffer.Buffer[CurrBuf * 2]);
      Model := Trim(Model);

      for CurrBuf := FirmStart to FirmEnd do
        Firmware := Firmware + Chr(ICBuffer.Buffer[CurrBuf * 2 + 1]) +
                               Chr(ICBuffer.Buffer[CurrBuf * 2]);
      Firmware := Trim(Firmware);

      for CurrBuf := SerialStart to SerialEnd do
        Serial := Serial + Chr(ICBuffer.Buffer[CurrBuf * 2 + 1]) +
                           Chr(ICBuffer.Buffer[CurrBuf * 2]);
      Serial := Trim(Serial);

      for CurrBuf := LBAStart to LBAEnd do
        LBASize := LBASize + ICBuffer.Buffer[CurrBuf * 2 + 1] +
                             ICBuffer.Buffer[CurrBuf * 2];

        SATASpeed := ICBuffer.Buffer[SataNegStart * 2 + 1] +
                      ICBuffer.Buffer[SataNegStart * 2];

        SATASpeed := SATASpeed shr 1 and 3;

      LBASize := 0;
      UserSize := 0;
      for CurrBuf := UserSizeStart to UserSizeEnd do
      begin
        UserSize := UserSize + ICBuffer.Buffer[CurrBuf * 2] shl (((CurrBuf - UserSizeStart) * 2) * 8);
        UserSize := UserSize + ICBuffer.Buffer[CurrBuf * 2 + 1]  shl ((((CurrBuf - UserSizeStart) * 2) + 1) * 8);
      end;
    end;
    CloseHandle(hdrive);
  end;
end;

function TSSDInfo.GetSmartDataATA: SENDCMDOUTPARAMS;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  opar: SENDCMDOUTPARAMS;
  opar2: SENDCMDOUTPARAMS;
  Status: Longbool;
  ipar2: SENDCMDINPARAMS;
begin
  ipar2.cBufferSize := 512;
  ipar2.bDriveNumber := StrToInt(ExtractDrvNum(DeviceName));
  ipar2.irDriveRegs.bFeaturesReg := SMART_READ_ATTRIBUTE_VALUES;
  ipar2.irDriveRegs.bSectorCountReg := 1;
  ipar2.irDriveRegs.bSectorNumberReg := 1;
  ipar2.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
  ipar2.irDriveRegs.bCylHighReg := SMART_CYL_HI;
  ipar2.irDriveRegs.bDriveHeadReg := ((StrToInt(ExtractDrvNum(DeviceName)) and 1) shl 4) or $a0;
  ipar2.irDriveRegs.bCommandReg := SMART_CMD;

  fillchar(opar, SizeOf(opar), #0);

  hdrive := CreateFile(PChar(DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, SMART_RCV_DRIVE_DATA, @ipar2, SizeOf(SENDCMDINPARAMS), @opar, SizeOf(SENDCMDOUTPARAMS), dwBytesReturned, nil);
    if (status = false) or (getLastError <> 0) then
      result := opar2;
    CloseHandle(hdrive);
  end;
  Result := opar;
end;

function TSSDInfo.GetSmartDataSCSI: SENDCMDOUTPARAMS;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  opar: SENDCMDOUTPARAMS;
  opar2: SENDCMDOUTPARAMS;
  Status: Longbool;
  ipar: SCSI_PTH_BUFFER;
  CurrBuf: Integer;
begin
  fillchar(ipar, SizeOf(ipar), #0);
  fillchar(opar, SizeOf(opar), #0);
  fillchar(opar2, SizeOf(opar2), #0);
	ipar.spt.Length     := sizeof(SCSI_PASS_THROUGH);
  ipar.spt.TargetId   := 1;
  ipar.spt.CdbLength  := 12;
	ipar.spt.SenseInfoLength := 24;
	ipar.spt.DataIn  := 1;
	ipar.spt.DataTransferLength := 512;
	ipar.spt.TimeOutValue := 2;
	ipar.spt.DataBufferOffset := pansichar(@ipar.Buffer)-pansichar(@ipar);
	ipar.spt.SenseInfoOffset  := pansichar(@ipar.SenseBuf)-pansichar(@ipar);
  ipar.spt.Cdb[0] := $A1;
  ipar.spt.Cdb[1] := $8;
  ipar.spt.Cdb[2] := $E;
	ipar.spt.Cdb[3] := $D0;
  ipar.spt.Cdb[4] := $1;
  ipar.spt.Cdb[5] := $0;
  ipar.spt.Cdb[6] := $4F;
  ipar.spt.Cdb[7] := $C2;
  ipar.spt.Cdb[8] := $0;
	ipar.spt.Cdb[9] := $B0;

  fillchar(opar, SizeOf(opar), #0);

  hdrive := CreateFile(PChar(DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, IOCTL_SCSI_PASS_THROUGH, @ipar, SizeOf(ipar), @ipar, SizeOf(ipar), dwBytesReturned, nil);
    if status = false then
      result := opar2;
    CloseHandle(hdrive);
  end;

  for CurrBuf := 0 to 511 do
    opar.bBuffer[CurrBuf] := ipar.Buffer[CurrBuf];
  Result := opar;
end;

constructor TSSDInfo_NST.Create;
begin
  inherited Create;
  S10085 := false;
end;

procedure TSSDInfo_NST.SetDeviceName(Harddisk: string);
begin
  inherited SetDeviceName(Harddisk);

  SSDSupport.SupportHostWrite := HSUPPORT_NONE;
  SSDSupport.SupportFirmUp := false;

  if ((Pos('LITEONIT', UpperCase(Model)) > 0) and (Pos('S100', UpperCase(Model)) > 0) and (StrToInt(Copy(Firmware, 3, 2)) < 83)) or
     (((Pos('MXSSD', UpperCase(Model)) > 0) and (Pos('MMY', UpperCase(Model)) > 0))) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
      ((Pos('THNSNF', UpperCase(Model)) > 0) or (Pos('THNSNH', UpperCase(Model)) > 0) or (Pos('THNSNJ', UpperCase(Model)) > 0))) then
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
      (IsLiteONNewVer(Model, Firmware) <> NOT_MINE) then
  begin
    SSDSupport.SupportFirmUp := true;
  end;

  if ((Pos('S100', Model) > 0) and (Pos('85', Firmware) > 0)) or
      ((IsPlextorNewVer(Model, Firmware) = NEW_VERSION) and (Pos('M3', UpperCase(Model)) > 0)) then S10085 := true
  else S10085 := false;

  if (IsPlextorNewVer(Model, Firmware) <> NOT_MINE) or
      (IsLiteONNewVer(Model, Firmware) <> NOT_MINE) or
      (Pos('MXSSD', UpperCase(Model)) > 0) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and
       ((Pos('THNSNF', UpperCase(Model)) > 0) or (Pos('THNSNH', UpperCase(Model)) > 0) or (Pos('THNSNJ', UpperCase(Model)) > 0))) or
      ((Pos('SANDISK', UpperCase(Model)) > 0) and (Pos('SD6SB1', UpperCase(Model)) > 0)) or
      ((Pos('ST', UpperCase(Model)) > 0) and (Pos('HM000', UpperCase(Model)) > 0)) then
    SupportedDevice := SUPPORT_FULL
  else if
      ((Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3') or (Model = 'OCZ-VERTEX3 MI')) or
      ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
      ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) or
      ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
      (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
      ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
      ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
      ((Pos('SAMSUNG', UpperCase(Model)) > 0) and (Pos('SSD', UpperCase(Model)) > 0)) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and (Pos('THNSNS', UpperCase(Model)) > 0)) then
    SupportedDevice := SUPPORT_SEMI
  else
    SupportedDevice := SUPPORT_NONE;
end;

procedure TSSDInfo_NST.CollectAllSmartData;
begin
  inherited CollectAllSmartData;

  if  ((Pos('SAMSUNG', UpperCase(Model)) > 0) and (Pos('SSD', UpperCase(Model)) > 0)) then HostWrites := round(ExtractSMART(SMARTData, 'F1') / 1024 / 2048 * 10 * 1.56)
  else if (Pos('MXSSD', Model) > 0) and (Pos('MMY', Model) > 0) then HostWrites := 0
  else if (Pos('MXSSD', Model) > 0) and (Pos('JT', Model) > 0) then HostWrites := round(ExtractSMART(SMARTData, 'F1') / 2)
  else if (Pos('MXSSD', Model) > 0) or ((Pos('OCZ', Model) > 0) and (Pos('VERTEX3', Model) > 0)) or
          ((Pos('OCZ', Model) > 0) and (Pos('AGILITY3', Model) > 0)) or ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
          (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
          ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
          ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and (Pos('THNSNS', UpperCase(Model)) > 0)) or
          ((Pos('SANDISK', UpperCase(Model)) > 0) and (Pos('SD6SB1', UpperCase(Model)) > 0)) or
          ((Pos('ST', Model) > 0) and (Pos('HM000', Model) > 0)) then HostWrites := ExtractSMART(SMARTData, 'F1') * 16  // 1GB 표준단위
  else if (Pos('Ninja-', Model) > 0) or (Pos('M5P', Model) > 0) or (Pos('M5M', Model) > 0) or
          (S10085) then HostWrites := (ExtractSMART(SMARTData, 177) * 2)
  else if (Pos('M4', Model) > 0) and (Pos('CT', Model) > 0) then HostWrites := 0
  else HostWrites := ExtractSMART(SMARTData, 177);

  if ((Pos('SAMSUNG', UpperCase(Model)) > 0) and (Pos('SSD', UpperCase(Model)) > 0)) then EraseError := ExtractSMART(SMARTData, 'B6')
  else if ((Pos('MX', Model) > 0) and (Pos('MMY', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and (Pos('THNSNF', UpperCase(Model)) > 0)) then EraseError := ExtractSMART(SMARTData, 1)
  else if (Pos('MXSSD', Model) > 0) or ((Pos('OCZ', Model) > 0) and (Pos('VERTEX3', Model) > 0)) or
          ((Pos('OCZ', Model) > 0) and (Pos('AGILITY3', Model) > 0)) or ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
          (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
          ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
          ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and (Pos('THNSNS', UpperCase(Model)) > 0)) then EraseError := ExtractSMART(SMARTData, 'AC')
  else if ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
          ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) then EraseError := ExtractSMART(SMARTData, 'AC')
  else EraseError := ExtractSMART(SMARTData, 182);
  
  ReplacedSectors := ExtractSMART(SMARTData, 5);
  if (Pos('LITEONIT', UpperCase(Model)) > 0) or
      (Pos('PLEXTOR', UpperCase(Model)) > 0) or
      (Pos('NINJA-', UpperCase(Model)) > 0) then RepSectorAlert := ReplacedSectors >= RepSectorThreshold_PLEXTOR
  else RepSectorAlert := ReplacedSectors >= RepSectorThreshold;
end;

end.
