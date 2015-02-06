unit uTrimThread;

interface

uses
  Classes, SysUtils, uDiskFunctions, Math, Dialogs, Windows,
  uATALowOps, uLanguageSettings, uPartitionFunctions, uTrimList;

type
  TTrimStage = (TRIMSTAGE_NONE, TRIMSTAGE_INPROGRESS,
    TRIMSTAGE_END, TRIMSTAGE_ERROR);
  TPartInfo = record
    StartPadding: Int64;
    LBAPerCluster: Cardinal;
  end;
  
  TTrimThread = class(TThread)
  public
    class var IsSemiAuto: Boolean; //반자동 트림 상황인가
    class var TrimStage: TTrimStage; //현재 트림 스테이지
    
    function ApplyPartList(PartListToTrim: TTrimList): Boolean;
  protected
    AllBlocks: _LARGE_INTEGER;
    Progress: Cardinal;
    LBASize: Cardinal;
    
    PartCount, CompletedPartition: Integer;
    NeedTrimPartition: Array of String;
    NeedTrimLBASize: Array of Integer;
    
    procedure ChangeProgressbar;
    procedure ChangeStage;
    procedure Execute; override;
    procedure DoTrim(const DriveLetter: String);
    procedure EndTrim;
  end;

implementation

uses uMain;

function TTrimThread.ApplyPartList(PartListToTrim: TTrimList): Boolean;
const
  LBA_SIZE = 512;
var
  CurrDrive: Integer;
begin
  result := PartListToTrim <> nil;

  PartCount := PartListToTrim.Count;
  if (not result) or (PartCount = 0) then
  begin
    if PartListToTrim <> nil then
      FreeAndNil(PartListToTrim);
    exit;
  end;
  
  SetLength(NeedTrimPartition, PartCount);
  SetLength(NeedTrimLBASize, PartCount);
  
  for CurrDrive := 0 to PartListToTrim.Count - 1 do
  begin
    NeedTrimPartition[CurrDrive] := PartListToTrim[CurrDrive];
    NeedTrimLBASize[CurrDrive] := LBA_SIZE;
  end;
  
  FreeAndNil(PartListToTrim);
end;

procedure TTrimThread.Execute;
var
  CurrDrive: Integer;
begin
  PartCount := Length(NeedTrimPartition);
  if PartCount <= 0 then
  begin
    TrimStage := TRIMSTAGE_ERROR;
    exit;
  end;
  
  TrimStage := TRIMSTAGE_INPROGRESS;
  CompletedPartition := 0;
  for CurrDrive := 0 to Length(NeedTrimPartition) - 1 do
  begin
    if (not IsSemiAuto) then
      Synchronize(ChangeStage);
    LBASize := NeedTrimLBASize[CurrDrive];
    DoTrim(NeedTrimPartition[CurrDrive]);
    CompletedPartition := CurrDrive + 1;
    if (not IsSemiAuto) then
      Synchronize(ChangeStage);
  end;
  
  if (not IsSemiAuto) then
    Synchronize(EndTrim);
    
  TrimStage := TRIMSTAGE_END;
end;

procedure TTrimThread.DoTrim(const DriveLetter: String);
const
  VOLUME_BITMAP_BYTES = 4096;
  VOLUME_BITMAP_SIZE = 2*SizeOf(LARGE_INTEGER)+VOLUME_BITMAP_BYTES;
var
  DriveHandle, hPhyDevice: THandle;
  StartingBuffer: STARTING_LCN_INPUT_BUFFER;
  BytesRead: Cardinal;
  CurrPart: Int64;
  CurrByte, CurrBit: Integer;
  Status: Byte;
  StartLBA: Int64;
  LBACount: Int64;
  SetupPoint: Int64;
  TempResult: VOLUME_BITMAP_BUFFER;
  CurrBitBool: Integer;
  error: Integer;
  LBAPerSector: Cardinal;
  LastPart, LastBit, BitCount: Integer;
  NTFSInfo: NTFS_INFO;
  Nouse: Array[0..2] of Cardinal;
  GottenLBAPerSector: Cardinal;
  FATLength: Integer;
  CurrTrimCount, CurrTrimLBAs: Int64;
  SleepTime_LBACount: Integer;
begin
  DriveHandle := CreateFile(
              PChar('\\.\' + DriveLetter),
              GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE,
              nil,
              OPEN_EXISTING,
              0,
              0);

  Status := 0;
  StartLBA := GetMotherDrive(DriveLetter).Extents[0].StartingOffset div LBASize;
  SetupPoint := 0;
  LBACount := 0;
  CurrPart := 0;
  NTFSInfo := GetNTFSVolumeData(DriveLetter);
  StartingBuffer.StartingLcn.QuadPart := 0;

  DeviceIOControl(
              DriveHandle,
              FSCTL_GET_VOLUME_BITMAP,
              @StartingBuffer,
              SizeOf(StartingBuffer),
              @TempResult,
              VOLUME_BITMAP_SIZE,
              BytesRead,
              nil);
  error := GetLastError;

  if NTFSInfo.ErrorCode = 0 then
  begin
    LBAPerSector := (Cardinal(NTFSInfo.SectorPerCluster) * 512) div LBASize;
  end
  else
  begin
    GetDiskFreeSpace(PChar(DriveLetter + '\'), Nouse[0],
                     GottenLBAPerSector, Nouse[1], Nouse[2]);
    FATLength := (GetPartitionLength(DriveLetter) div GottenLBAPerSector) -
                 (GottenLBAPerSector * TempResult.BitmapSize.QuadPart);
    StartLBA := StartLBA + FATLength;
    LBAPerSector := (GottenLBAPerSector shl 9) div LBASize;
  end;

  AllBlocks := TempResult.BitmapSize;
  Progress := round(((TempResult.StartingLcn.QuadPart / AllBlocks.QuadPart) +
                     CompletedPartition) / PartCount * 100);
  if (not IsSemiAuto) then
    Synchronize(ChangeProgressbar);

  CurrTrimCount := 0;
  CurrTrimLBAs := 0;

  hPhyDevice :=
    TATALowOps.CreateHandle(GetMotherDrive(DriveLetter).Extents[0].DiskNumber);
  while (error = 234) or (error = 87) or (error = 0) do
  begin
    if TempResult.BitmapSize.QuadPart >= 32768 then
    begin
      LastPart := 4095;
      LastBit := 7;
    end
    else
    begin
      LastPart := ceil(TempResult.BitmapSize.QuadPart / 8) - 1;
      LastBit := (TempResult.BitmapSize.QuadPart and 7) - 1;
    end;
    for CurrByte := 0 to LastPart do
    begin
      if CurrByte = LastPart then BitCount := LastBit
      else BitCount := 7;
      for CurrBit := 0 to BitCount do
      begin
        CurrBitBool := TempResult.Buffer[CurrByte] and 1;
        TempResult.Buffer[CurrByte] := TempResult.Buffer[CurrByte] shr 1;

        if (CurrBitBool = 0) and (Status = 0) then
        begin
          SetupPoint := StartLBA + (((CurrByte shl 3) +
                                      TempResult.StartingLcn.QuadPart + CurrBit)
                                      * LBAPerSector);
          LBACount := LBAPerSector;
          Status := 1;
        end
        else if ((CurrBitBool = 1) and (Status = 1)) or
                ((CurrBitBool = 0) and (Status = 1) and
                 ((LBACount > 65500) or
                  ((CurrByte = LastPart) and (CurrBit = LastBit)))) then
        begin
          if (CurrBitBool = 0) and (Status = 1) then
            LBACount := LBACount + LBAPerSector;
          CurrTrimCount := CurrTrimCount + 1;
          CurrTrimLBAs := CurrTrimLBAs + LBACount;
          TATALowOps.TrimCommand(hPhyDevice,
            SetupPoint, LBACount);
          SetupPoint := 0;
          LBACount := 0;
          Status := 0;
        end
        else if (CurrBitBool = 0) and (Status = 1) then
          LBACount := LBACount + LBAPerSector;
      end;
    end;
    if (error <> 87) and (error <> 0) then
    begin
      if (CurrTrimCount > 10) or (CurrTrimLBAs > 200000) then
      begin
        SleepTime_LBACount := CurrTrimLBAs shr 16;
        if CurrTrimCount > SleepTime_LBACount then
          Sleep(CurrTrimCount shr 2)
        else
          Sleep(SleepTime_LBACount);
        CurrTrimCount := 0;
        CurrTrimLBAs := 0;
      end;
      CurrPart := CurrPart + (Length(TempResult.Buffer) shl 3);
      StartingBuffer.StartingLcn.QuadPart := CurrPart;
      SetupPoint := 0;
      LBACount := 0;
      Status := 0;
      DeviceIOControl(
                  DriveHandle,
                  FSCTL_GET_VOLUME_BITMAP,
                  @StartingBuffer,
                  SizeOf(StartingBuffer),
                  @TempResult,
                  VOLUME_BITMAP_SIZE,
                  BytesRead,
                  nil);
      error := GetLastError;
      Progress := round(((StartingBuffer.StartingLcn.QuadPart /
                          AllBlocks.QuadPart) + CompletedPartition) /
                          PartCount * 100);
      if (not IsSemiAuto) then
        Synchronize(ChangeProgressbar);
    end
    else
      break;
  end;
  CloseHandle(hPhyDevice);
  CloseHandle(DriveHandle);
end;

procedure TTrimThread.ChangeProgressbar;
begin
  fMain.pDownload.Position := Progress;
end;

procedure TTrimThread.ChangeStage;
begin
  fMain.pDownload.Position := Progress;
  if CompletedPartition < PartCount then
    fMain.lProgress.Caption := CapProg1[CurrLang] +
                               NeedTrimPartition[CompletedPartition] +
                               ' ' + CapProg2[CurrLang] +
                               ' (' + IntToStr(CompletedPartition + 1) + '/' +
                               IntToStr(PartCount) + ')';
end;

procedure TTrimThread.EndTrim;
begin
  fMain.pDownload.Height := fMain.pDownload.Height - 10;
  fMain.pDownload.Top := fMain.pDownload.Top - 5;
  fMain.pDownload.Position := 0;
  fMain.gTrim.Visible := true;
  fMain.HideProgress;
end;
end.
