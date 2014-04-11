unit uEasySMART;

interface

uses Windows, SysUtils, Dialogs, Math, Classes, uAlert;

type
  PARTITION_INFORMATION = RECORD
    StartingOffset: TLargeInteger;
    PartitionLength: TLargeInteger;
    HiddenSectors: DWORD;
    PartitionNumber: DWORD;
    PartitionType: BYTE;
    BootIndicator: WordBool;
    RecognizedPartition: WordBool;
    RewritePartition: WordBool;
  end;

  DISK_EXTENT = RECORD
    DiskNumber: DWORD;
    StartingOffset: TLargeInteger;
    ExtentLength: TLargeInteger;
  end;

  DRIVE_LAYOUT_INFORMATION = Record
    PartitionCount: DWORD;
    Signature: DWORD;
    Partitioninfo: Array[0..50] of PARTITION_INFORMATION;
  end;


  DISK_GEOMETRY = Record
    Cylinders: TLargeInteger;
    MediaType: Byte;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;

  DISK_GEOMETRY_EX = Record
    Geometry: DISK_GEOMETRY;
    DiskSize: TLargeInteger;
    Data: Array[0..1] of UChar;
  end;

  VOLUME_DISK_EXTENTS = Record
    NumberOfDiskExtents: DWORD;
    Extents: Array[0..50] of DISK_EXTENT;
  end;

  TDRIVERSTATUS = Record
    bDriverError: UChar;
    bIDEError: UChar;
    bReserved: Array[0..1] of UCHAR;
    dwReserved: Array[0..1] of UCHAR;
  End;

  SENDCMDOUTPARAMS  = Record
    cBufferSize: DWORD;
    DriverStatus: TDRIVERSTATUS;
    bBuffer: Array[0..1023] of UCHAR;
  end;

  IDEREGS  = Record
    bFeaturesReg: UCHAR;
    bSectorCountReg: UCHAR;
    bSectorNumberReg: UCHAR;
    bCylLowReg: UCHAR;
    bCylHighReg: UCHAR;
    bDriveHeadReg: UCHAR;
    bCommandReg: UCHAR;
    bReserved: UCHAR;
  end;

  SENDCMDINPARAMS  = Record
    cBufferSize: ULONG;
    irDriveRegs: IDEREGS;
    bDriveNumber: UCHAR;
    bReserved: Array[0..3] of UCHAR;
    dwReserved: Array[0..4] of ULONG;
    bBuffer: Array[0..1] of UCHAR;
  end;

  TDriveLetters  = Record
    Letters: String;
    StartOffset: Array[0..99] of TLargeInteger;
  end;

  PSTARTING_LCN_INPUT_BUFFER = ^STARTING_LCN_INPUT_BUFFER;
  {$EXTERNALSYM PSTARTING_LCN_INPUT_BUFFER}
  STARTING_LCN_INPUT_BUFFER = record
    StartingLcn: LARGE_INTEGER;
  end;

  PVOLUME_BITMAP_BUFFER = ^VOLUME_BITMAP_BUFFER;
  {$EXTERNALSYM PVOLUME_BITMAP_BUFFER}
  VOLUME_BITMAP_BUFFER = record
    StartingLcn: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
    Buffer: array [0..0] of BYTE;
  end;

function ExtractSMART(const WantedInfo: Integer; DiskNumber: String): Integer; overload;
function ExtractSMART(const WantedInfo: String; DiskNumber: String): Integer; overload;
function GetPartitionList(DiskNumber: String): TDriveLetters;
function GetDiskSize(const DiskNumber: String): TLargeInteger;
function GetPartitionStructure(const DriveLetter: String): PVOLUME_BITMAP_BUFFER;
procedure GetUSBDrives(USBDrives: TStrings);
procedure GetRamDrives(RamDrives: TStrings);

var
  AllDrives: String;

implementation

uses uMain, HddInfo;

function GetSmartData(const Harddisk: String): SENDCMDOUTPARAMS;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  opar: SENDCMDOUTPARAMS;
  opar2: SENDCMDOUTPARAMS;
  Status: Longbool;
  TempInfo: THDDInfo;
  ipar: array[0..31] of Byte;
begin
  ipar[0] := 0;
  ipar[1] := $02;
  ipar[2] := 0;
  ipar[3] := 0;
  ipar[4] := $d0;
  ipar[5] := $01;
  ipar[6] := $01;
  ipar[7] := $4f;
  ipar[8] := $c2;
  ipar[9] := $a0;
  ipar[10] := $b0;
  ipar[11] := 0;
  ipar[12] := 0;
  ipar[13] := 0;
  ipar[14] := 0;
  ipar[15] := 0;
  ipar[16] := $8c;
  ipar[17] := $fd;
  ipar[18] := $14;
  ipar[19] := 0;
  ipar[20] := 0;
  ipar[21] := $02;
  ipar[22] := 0;
  ipar[23] := 0;
  ipar[24] := $03;
  ipar[25] := 0;
  ipar[26] := 0;
  ipar[27] := 0;
  ipar[28] := $03;
  ipar[29] := 0;
  ipar[30] := 0;
  ipar[31] := 0;

  hdrive := CreateFile(PChar(Harddisk), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  If hdrive <> -1 Then
  begin
    Status := DeviceIoControl(hdrive, SMART_RCV_DRIVE_DATA, @ipar, SizeOf(ipar), @opar, SizeOf(opar), dwBytesReturned, nil);
    if (status = false) and (fAlert = nil) and (getLastError <> 21) then
    begin
      TempInfo := THDDInfo.Create(nil);
      TempInfo.Method := TGetInfoMethod(gimByName);
      TempInfo.DeviceName := Copy(Harddisk, 5, Length(Harddisk) - 4);
      TempInfo.CanonicalScsiAddressing := false;
      AlertCreate(nil, 'Failed Getting SMART : '+inttostr(getLastError()) + Chr(13) + Chr(10) +
                            'Drive : ' + Harddisk + Chr(13) + Chr(10) +
                            'Model : ' + TempInfo.Model + Chr(13) + Chr(10) +
                            'Firmware : ' + TempInfo.Revision);
      FreeAndNil(TempInfo);
    end
    else if (status = false) and (getLastError = 21) then
      result := opar2;
    CloseHandle(hdrive);
  end
  else
  begin
      AlertCreate(nil, 'Failed Getting SMART : '+inttostr(getLastError()) + Chr(13) + Chr(10) +
                            'Drive : ' + Harddisk + Chr(13) + Chr(10) +
                            'And There is NO DRIVE');
  end;
  Result := opar;
end;

function ExtractSMART(const WantedInfo: Integer; DiskNumber: String): Integer; overload;
var
  CurrInfo: Integer;
  Data: SENDCMDOUTPARAMS;
begin
  result := -1;
  Data := GetSmartData('\\.\PhysicalDrive' + DiskNumber);
  for CurrInfo := 0 to floor(Length(Data.bBuffer) / 12) do
  begin
    if Data.bBuffer[8 + (CurrInfo * 12)] = WantedInfo then
    begin
      Result := Data.bBuffer[1 + ((CurrInfo + 1) * 12)] + (Data.bBuffer[2 + ((CurrInfo + 1) * 12)] * 256);
      break;
    end;
  end;
end;

function ExtractSMART(const WantedInfo: String; DiskNumber: String): Integer; overload;
var
  CurrInfo: Integer;
  WantedInfoInt: Integer;
  Data: SENDCMDOUTPARAMS;
begin
  result := -1;
  Data := GetSmartData('\\.\PhysicalDrive' + DiskNumber);
  WantedInfoInt := StrToInt('$' + WantedInfo);
  for CurrInfo := 0 to floor(Length(Data.bBuffer) / 12) do
  begin
    if Data.bBuffer[8 + (CurrInfo * 12)] = WantedInfoInt then
    begin
      Result := Data.bBuffer[1 + ((CurrInfo + 1) * 12)] + (Data.bBuffer[2 + ((CurrInfo + 1) * 12)] * 256) +
                (Data.bBuffer[3 + ((CurrInfo + 1) * 12)] * 256 * 256) + (Data.bBuffer[4 + ((CurrInfo + 1) * 12)] * 256 * 256 * 256);
      break;
    end;
  end;
end;

procedure GetUSBDrives(USBDrives: TStrings);
var
  CurrDrv, DriveCount: Integer;
  Drives: Array[0..255] of char;
  DrvName: String;
begin
  USBDrives.Clear;
  FillChar(Drives, 256, #0 );
  DriveCount := GetLogicalDriveStrings(256, Drives);
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    if Drives[CurrDrv] = #0  then
    begin
      if GetDriveType(PChar(DrvName)) = DRIVE_REMOVABLE then
        USBDrives.Add(DrvName);
      DrvName := '';
    end
    else
      DrvName := DrvName + Drives[CurrDrv];
  end;
end;


function GetFixedDrivesFunction: String;
var
  CurrDrv, DriveCount: Integer;
  Drives: Array[0..255] of char;
  DrvName: String;
begin
  FillChar(Drives, 256, #0 );
  DriveCount := GetLogicalDriveStrings(256, Drives);
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    if Drives[CurrDrv] = #0  then
    begin
      if GetDriveType(PChar(DrvName + ':\')) = DRIVE_FIXED then
        result := result + DrvName;
      DrvName := '';
    end
    else if (Drives[CurrDrv] <> ':') and (Drives[CurrDrv] <> '\') then
      DrvName := DrvName + Drives[CurrDrv];
  end;
end;

function GetDiskGeometry(const DiskNumber: String): DISK_GEOMETRY_EX;
Var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  TempInfo: THddInfo;
begin
  hDevice := CreateFile(PChar('\\.\PhysicalDrive' + DiskNumber), GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  If hDevice <> -1 Then
  begin
    Status := DeviceIoControl(hDevice, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
              nil, 0, @result, Sizeof(DISK_GEOMETRY_EX), RetBytes, nil);
    if (status = false) and (fAlert = nil) and (getLastError <> 21) then
    begin
      TempInfo := THDDInfo.Create(nil);
      TempInfo.Method := TGetInfoMethod(gimByName);
      TempInfo.DeviceName := 'PhysicalDrive' + DiskNumber;
      TempInfo.CanonicalScsiAddressing := false;
      AlertCreate(nil, 'Failed Getting Geometry : '+inttostr(getLastError) + Chr(13) + Chr(10) +
                            'Drive : \\.\PhysicalDrive' + DiskNumber + Chr(13) + Chr(10) +
                            'Model : ' + TempInfo.Model + Chr(13) + Chr(10) +
                            'Firmware : ' + TempInfo.Revision);
      FreeAndNil(TempInfo);
    end
    else if (status = false) and (getLastError = 21) then
      result.DiskSize := 0;
    CloseHandle(hDevice);
  end
  else
  begin
      AlertCreate(nil, 'Failed Getting Geometry : '+inttostr(getLastError) + Chr(13) + Chr(10) +
                            'Drive : \\.\PhysicalDrive' + DiskNumber + Chr(13) + Chr(10) +
                            'And There is NO DRIVE');
  end;
end;

function GetDiskSize(const DiskNumber: String): TLargeInteger;
Var
  Geometry: DISK_GEOMETRY_EX;
begin
  Geometry := GetDiskGeometry(DiskNumber);
  //실제로 수는 다르므로 확인작업
  result := Geometry.DiskSize;
end;

function GetMotherDrive(const DriveLetter: String): VOLUME_DISK_EXTENTS;
var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  VolumeName: Array[0..MAX_PATH] of Char;
  i: Integer;
begin
  for i := 0 to MAX_PATH do
    VolumeName[i] := #0;
  QueryDosDeviceW(PChar(DriveLetter), VolumeName, MAX_PATH);
  hDevice := CreateFile(PChar('\\.\' + DriveLetter), GENERIC_READ,
              FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  If hDevice <> -1 Then
  begin
    Status := DeviceIoControl (hDevice, IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
              nil, 0, @result, Sizeof(VOLUME_DISK_EXTENTS), RetBytes, nil);
    if (status = false) then
    begin
      result.NumberOfDiskExtents := 0;
      {AlertCreate(nil, 'Failed Getting Mother Drive : ' + inttostr(getLastError()) + Chr(13) + Chr(10) +
                          'DriveName : \\.\' + DriveLetter + Chr(13) + Chr(10) +
                          'DriveType : ' + IntToStr(GetDriveType(PChar(DriveLetter + '\'))));}
    end;
    CloseHandle(hDevice);
  end
  else
  begin
    result.NumberOfDiskExtents := 0;
      {AlertCreate(nil, 'Failed Getting Mother Drive : ' + inttostr(getLastError()) + Chr(13) + Chr(10) +
                          'DriveName : \\.\' + DriveLetter + Chr(13) + Chr(10) +
                          'DriveType : ' + IntToStr(GetDriveType(PChar(DriveLetter + '\'))) + Chr(13) + Chr(10) +
                          'And There is NO DRIVE');}
  end;
end;


function GetPartitionStructure(const DriveLetter: String): PVOLUME_BITMAP_BUFFER;
const
    VOLUME_BITMAP_BYTES = 512; //bytes to retrieve
    VOLUME_BITMAP_SIZE = 2*SizeOf(LARGE_INTEGER)+VOLUME_BITMAP_BYTES;
var
    hDevice: THandle;
    bResult: Boolean;
    StartingBuffer: STARTING_LCN_INPUT_BUFFER;
    nError: DWORD;
    BytesRead: Cardinal;
begin
    hDevice := CreateFile(
          PChar('\\.\' + DriveLetter),  // drive to open
          GENERIC_READ,                // no access to the drive
          FILE_SHARE_READ or FILE_SHARE_WRITE, // share mode
          nil,            // default security attributes
          OPEN_EXISTING,    // disposition
          0,                // file attributes
          0);            // do not copy file attributes}

    GetMem(result, VOLUME_BITMAP_SIZE);
    StartingBuffer.StartingLcn.QuadPart := 12801024;

    bResult := DeviceIOControl(
              hDevice ,
              FSCTL_GET_VOLUME_BITMAP,
              @StartingBuffer,
              SizeOf(StartingBuffer),
              result,
              VOLUME_BITMAP_SIZE,
              BytesRead,
              nil);
end;

procedure GetRamDrives(RamDrives: TStrings);
var
  CurrDrv, DriveCount: Integer;
  Drives: Array[0..255] of char;
  DrvName: String;
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  VolumeName: Array[0..MAX_PATH] of Char;
  MotherDriveName: String;
  TempHDDInfo: THDDInfo;
  i: Integer;
begin
  RamDrives.Clear;
  FillChar(Drives, 256, #0 );
  DriveCount := GetLogicalDriveStrings(256, Drives);
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    if Drives[CurrDrv] = #0  then
    begin
      if GetDriveType(PChar(DrvName)) = DRIVE_FIXED then
      begin
        for i := 0 to MAX_PATH do
          VolumeName[i] := #0;
        QueryDosDeviceW(PChar(DrvName), VolumeName, MAX_PATH);
        hDevice := CreateFile(PChar('\\.\' + Copy(DrvName, 1, 2)), GENERIC_READ,
                    FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
        If hDevice = -1 Then
        begin
          RamDrives.Add(DrvName);
        end
        else
        begin
          TempHDDInfo := THDDInfo.Create(nil);
          TempHDDInfo.CanonicalScsiAddressing := false;
          TempHDDInfo.Method := TGetInfoMethod(gimByName);
          TempHDDInfo.DeviceName := 'PhysicalDrive' + IntToStr(GetMotherDrive(Copy(DrvName, 1, 2)).Extents[0].DiskNumber);
          if Length(TempHDDInfo.Serial) <= 5 then
            RamDrives.Add(DrvName);
          FreeAndNil(TempHDDInfo);
        end;
      end;
      DrvName := '';
    end
    else
      DrvName := DrvName + Drives[CurrDrv];
  end;
end;

function GetPartitionList(DiskNumber: String): TDriveLetters;
var
  CurrDrv, CurrExtents: Integer;
  CurrDrvInfo: VOLUME_DISK_EXTENTS;
  CurrPartition, DiskNumberInt: Integer;
begin
  AllDrives := GetFixedDrivesFunction;
  CurrPartition := 0;
  DiskNumberInt := StrToInt(DiskNumber);
  result.Letters := '';
  for CurrDrv := 1 to Length(AllDrives) do
  begin
    CurrDrvInfo := GetMotherDrive(AllDrives[CurrDrv] + ':');
    for CurrExtents := 0 to (CurrDrvInfo.NumberOfDiskExtents - 1) do
    begin
      if CurrDrvInfo.Extents[CurrExtents].DiskNumber = DiskNumberInt then
      begin
        result.Letters := result.Letters + AllDrives[CurrDrv];
        result.StartOffset[CurrPartition] :=
          CurrDrvInfo.Extents[CurrExtents].StartingOffset;
        Inc(CurrPartition);
      end;
    end;
  end;
end;
end.
