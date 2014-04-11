unit uSSDInfo;

interface

uses Windows, uDiskFunctions, Classes, Math, Dialogs, SysUtils;

const
  NullModel = 0;
  ATAModel = 1;
  SCSIModel = 2;
  DetermineModel = 3;

  SUPPORT_FULL = 0;
  SUPPORT_SEMI = 1;
  SUPPORT_NONE = 2;

type
  TSSDInfo = class
    Model: String;
    Firmware: String;
    Serial: String;
    DeviceName: String;
    ATAorSCSI: Byte;
    USBMode: Boolean;
    UsedByService: Boolean;
    SupportedDevice: Byte;
    HostWrites: UInt64;
    EraseError: UInt64;
    UserSize: UInt64;
    ReplacedSectors: UInt64;
    SMARTData: SENDCMDOUTPARAMS;
    procedure SetDeviceName(Harddisk: string);
    procedure CollectAllSmartData;
    //1. 창조자와 파괴자
    constructor Create;
    protected
      procedure GetInfoATA;
      procedure GetInfoSCSI;
  end;

function ExtractSMART(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: Integer): UInt64; overload;
function ExtractSMART(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: String): UInt64; overload;
function ExtractSMARTPercent(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: Integer): UInt64; overload;
function ExtractSMARTPercent(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: String): UInt64; overload;

const
  ModelStart = 27;
  ModelEnd = 46;
  FirmStart = 23;
  FirmEnd = 26;
  SerialStart = 10;
  SerialEnd = 19;
  UserSizeStart = 100;
  UserSizeEnd = 103;
  HPABit = 82;

const
  DebugMode = false;
  CurrentVersion = '3.7.1';

implementation

constructor TSSDInfo.Create;
begin
  Model := '';
  Firmware := '';
  Serial := '';
  DeviceName := '';
  ATAorSCSI := NullModel;
  UsedByService := false;
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
    if Trim(Model) <> '' then
      ATAorSCSI := SCSIModel;
  end
  else
    ATAorSCSI := ATAModel;

  if (Pos('LITEONIT', UpperCase(Model)) > 0) or
      (Pos('PLEXTOR', UpperCase(Model)) > 0) or
      (Pos('NINJA-', UpperCase(Model)) > 0) or
      (Pos('MXSSD', UpperCase(Model)) > 0) or
      ((Pos('TOSHIBA', UpperCase(Model)) > 0) and (Pos('THNSNF', UpperCase(Model)) > 0)) then
    SupportedDevice := SUPPORT_FULL
  else if
      ((Model = 'OCZ-VERTEX3') or (Model = 'OCZ-AGILITY3') or (Model = 'OCZ-VERTEX3 MI')) or
      ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
      ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) or
      ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
      (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
      ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
      ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) or
      ((Pos('SAMSUNG', UpperCase(Model)) > 0) and (Pos('SSD', UpperCase(Model)) > 0)) then
    SupportedDevice := SUPPORT_SEMI
  else
    SupportedDevice := SUPPORT_NONE;

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

procedure TSSDInfo.CollectAllSmartData;
begin
  if ATAorSCSI = ATAModel then SMARTData := GetSmartDataATA(DeviceName)
  else if ATAorSCSI = SCSIModel then SMARTData := GetSmartDataSCSI(DeviceName);

  if  ((Pos('SAMSUNG', UpperCase(Model)) > 0) and (Pos('SSD', UpperCase(Model)) > 0)) then HostWrites := round(ExtractSMART(SMARTData, 'F1') / 1024 / 2048 * 10 * 1.56)
  else if (Pos('MXSSD', Model) > 0) and (Pos('MMY', Model) > 0) then HostWrites := 0
  else if (Pos('MXSSD', Model) > 0) and (Pos('JT', Model) > 0) then HostWrites := round(ExtractSMART(SMARTData, 'F1') / 2)
  else if (Pos('MXSSD', Model) > 0) or ((Pos('OCZ', Model) > 0) and (Pos('VERTEX3', Model) > 0)) or
          ((Pos('OCZ', Model) > 0) and (Pos('AGILITY3', Model) > 0)) or ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
          (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
          ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
          ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) then HostWrites := ExtractSMART(SMARTData, 'F1') * 16
  else if (Pos('Ninja-', Model) > 0) or (Pos('M5P', Model) > 0) or (Pos('M5M', Model) > 0) then HostWrites := (ExtractSMART(SMARTData, 177) * 2)
  else if (Pos('M4', Model) > 0) and (Pos('CT', Model) > 0) then HostWrites := 0
  else HostWrites := ExtractSMART(SMARTData, 177);

  if ((Pos('SAMSUNG', UpperCase(Model)) > 0) and (Pos('SSD', UpperCase(Model)) > 0)) then EraseError := ExtractSMART(SMARTData, 'B6')
  else if ((Pos('MX', Model) > 0) and (Pos('MMY', Model) > 0)) or
          ((Pos('TOSHIBA', UpperCase(Model)) > 0) and (Pos('THNSNF', UpperCase(Model)) > 0)) then EraseError := ExtractSMART(SMARTData, 1)
  else if (Pos('MXSSD', Model) > 0) or ((Pos('OCZ', Model) > 0) and (Pos('VERTEX3', Model) > 0)) or
          ((Pos('OCZ', Model) > 0) and (Pos('AGILITY3', Model) > 0)) or ((Model = 'SSD 128GB') or (Model = 'SSD 64GB')) or
          (Pos('SHYSF', Model) > 0) or (Pos('Patriot Pyro', Model) > 0) or
          ((Pos('SuperSSpeed', Model) > 0) and (Pos('Hyper', Model) > 0)) or
          ((Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0)) then EraseError := ExtractSMART(SMARTData, 'AC')
  else if ((Pos('C400', Model) > 0) and (Pos('MT', Model) > 0)) or
          ((Pos('M4', Model) > 0) and (Pos('CT', Model) > 0)) then EraseError := ExtractSMART(SMARTData, 'AC')
  else EraseError := ExtractSMART(SMARTData, 182);
  
  ReplacedSectors := ExtractSMART(SMARTData, 5);
end;

function ExtractSMART(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: Integer): UInt64; overload;
var
  CurrInfo: Integer;
  SCSIConst: Integer;
begin
  FillChar(result, SizeOf(result), #0);
  for CurrInfo := 0 to floor(Length(SMARTData.bBuffer) / 12) do
  begin
    if SMARTData.cBufferSize = 0 then SCSIConst := -6
      else SCSIConst := 0;
    if SMARTData.bBuffer[8 + (CurrInfo * 12) + SCSIConst] = WantedInfo then
    begin
      result := SMARTData.bBuffer[1 + ((CurrInfo + 1) * 12) + SCSIConst] + (SMARTData.bBuffer[2 + ((CurrInfo + 1) * 12) + SCSIConst] * 256) +
                        (SMARTData.bBuffer[3 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256) + (SMARTData.bBuffer[4 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256) +
                        (SMARTData.bBuffer[5 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256 * 256) + (SMARTData.bBuffer[6 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256 * 256 * 256)
                        + (SMARTData.bBuffer[7 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256 * 256 * 256 * 256);
      break;
    end;
  end;
end;

function ExtractSMARTPercent(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: Integer): UInt64; overload;
var
  CurrInfo: Integer;
  SCSIConst: Integer;
begin
  result := 0;
  for CurrInfo := 0 to floor(Length(SMARTData.bBuffer) / 12) do
  begin
    if SMARTData.cBufferSize = 0 then SCSIConst := -6
      else SCSIConst := 0;
    if SMARTData.bBuffer[8 + (CurrInfo * 12) + SCSIConst] = WantedInfo then
    begin
      Result := SMARTData.bBuffer[11 + ((CurrInfo + 1) * 12) + SCSIConst];
      break;
    end;
  end;
end;

function ExtractSMART(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: String): UInt64; overload;
var
  CurrInfo: Integer;
  SCSIConst: Integer;
  WantedInfoInt: Integer;
begin
  FillChar(result, SizeOf(result), #0);
  WantedInfoInt := StrToInt('$' + WantedInfo);
  for CurrInfo := 0 to floor(Length(SMARTData.bBuffer) / 12) do
  begin
    if SMARTData.cBufferSize = 0 then SCSIConst := -6
      else SCSIConst := 0;
    if SMARTData.bBuffer[8 + (CurrInfo * 12) + SCSIConst] = WantedInfoInt then
    begin
      result := SMARTData.bBuffer[1 + ((CurrInfo + 1) * 12) + SCSIConst] + (SMARTData.bBuffer[2 + ((CurrInfo + 1) * 12) + SCSIConst] * 256) +
                        (SMARTData.bBuffer[3 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256) + (SMARTData.bBuffer[4 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256) +
                        (SMARTData.bBuffer[5 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256 * 256) + (SMARTData.bBuffer[6 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256 * 256 * 256)
                        + (SMARTData.bBuffer[7 + ((CurrInfo + 1) * 12) + SCSIConst] * 256 * 256 * 256 * 256 * 256 * 256);
      break;
    end;
  end;
end;

function ExtractSMARTPercent(const SMARTData: SENDCMDOUTPARAMS; WantedInfo: String): UInt64; overload;
var
  CurrInfo: Integer;
  SCSIConst: Integer;
  WantedInfoInt: Integer;
begin
  result := 0;
  WantedInfoInt := StrToInt('$' + WantedInfo);
  for CurrInfo := 0 to floor(Length(SMARTData.bBuffer) / 12) do
  begin
    if SMARTData.cBufferSize = 0 then SCSIConst := -6
      else SCSIConst := 0;
    if SMARTData.bBuffer[8 + (CurrInfo * 12) + SCSIConst] = WantedInfoInt then
    begin
      Result := SMARTData.bBuffer[-1 + ((CurrInfo + 1) * 12) + SCSIConst];
      break;
    end;
  end;
end;

end.
