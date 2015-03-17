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
    class var IsSemiAuto: Boolean;
    class var TrimStage: TTrimStage;

    destructor Destroy; override;

    function ApplyPartList(PartListToTrim: TTrimList): Boolean;
  private
    PartToTrim: TTrimList;
    AllClusters: _LARGE_INTEGER;
    Progress: Cardinal;
    procedure ApplyTrimProgressToUI;
    procedure ApplyTrimStageToUI;
    function CheckPartitionInputValidity: Boolean;
    procedure SetTrimError;
    procedure SetTrimInProgress;
    procedure SetTrimIsEnd;
    procedure InitializeTrim;
    procedure TrimPartitions;
    procedure ApplyEndStateOfTrim;
    procedure SetThisPartitionAsCompleted;
    procedure CleanupTrim;
  protected
    procedure Execute; override;
    procedure TrimPartition(const DriveLetter: String);
    function GetPartInfo(DriveLetter: String;
      PartSize: Int64): TPartInfo;

    //Main과의 Sync함수
    procedure ApplyProgress;
    procedure ApplyStage;
    procedure EndTrim;
  end;

  EException = class(Exception)

  end;

implementation

uses uMain;

const
  LBASize = 512;

destructor TTrimThread.Destroy;
begin
  if PartToTrim <> nil then
    FreeAndNil(PartToTrim);

  inherited Destroy;
end;

function TTrimThread.ApplyPartList(PartListToTrim: TTrimList): Boolean;
begin
  result := PartListToTrim <> nil;

  if not result then
    exit;

  PartToTrim := PartListToTrim;
end;

procedure TTrimThread.ApplyTrimStageToUI;
begin
  if not IsSemiAuto then
    Synchronize(ApplyStage);
end;

procedure TTrimThread.ApplyTrimProgressToUI;
begin
  if not IsSemiAuto then
    Synchronize(ApplyProgress);
end;

procedure TTrimThread.ApplyEndStateOfTrim;
begin
  if not IsSemiAuto then
    Synchronize(EndTrim);
end;

function TTrimThread.CheckPartitionInputValidity: Boolean;
begin
  raise EArgumentNilException;
end;

procedure TTrimThread.SetTrimError;
begin
  TrimStage := TRIMSTAGE_ERROR;
end;

procedure TTrimThread.SetTrimInProgress;
begin
  TrimStage := TRIMSTAGE_INPROGRESS;
end;

procedure TTrimThread.SetTrimIsEnd;
begin
  TrimStage := TRIMSTAGE_END;
end;

procedure TTrimThread.InitializeTrim;
begin
  CheckPartitionInputValidity;
  PartToTrim.PointerToFirst;
  SetTrimInProgress;
end;

procedure TTrimThread.CleanupTrim;
begin
  SetTrimIsEnd;
  ApplyEndStateOfTrim;
end;

procedure TTrimThread.SetThisPartitionAsCompleted;
begin
  PartToTrim.MarkAsCompleted;
  ApplyTrimStageToUI;
end;

procedure TTrimThread.TrimPartitions;
var
  CurrDrive: Integer;
begin
  for CurrDrive := 0 to PartToTrim.Count - 1 do
  begin
    TrimPartition(PartToTrim.GetNextPartition.PartitionName);
    SetThisPartitionAsCompleted;
  end;
end;

procedure TTrimThread.Execute;
begin
  try
    InitializeTrim;
    TrimPartitions;
    CleanupTrim;
  except
    SetTrimError;
  end;
end;

function TTrimThread.GetPartInfo(DriveLetter: String;
  PartSize: Int64): TPartInfo;
var
  NTFSInfo: NTFS_INFO;
  Nouse: Array[0..2] of Cardinal;
  GottenLBAPerCluster: Cardinal;
  FATLength: UInt64;
begin
  result.StartPadding := 0;

  //NTFS라고 가정
  NTFSInfo := GetNTFSVolumeData(DriveLetter);

  if NTFSInfo.ErrorCode = 0 then
  begin
    result.LBAPerCluster := Cardinal(NTFSInfo.SectorPerCluster);
    exit;
  end;

  //FAT종류인 경우
  GetDiskFreeSpace(PChar(DriveLetter + '\'), GottenLBAPerCluster,
    Nouse[0], Nouse[1], Nouse[2]);

  {GetPartitionLength의 결과값은 Bytes로 나오므로 그대로 사용
   PartSize는 클러스터단위므로 클러스터당 LBA & LBA 사이즈 곱한다
   마지막으로 StartLBA이므로 LBASize로 나눠준다}

  FATLength :=
    (GetPartitionLength(DriveLetter) -
      (PartSize * GottenLBAPerCluster * LBASize))
    div LBASize;

  result.StartPadding := FATLength;
  result.LBAPerCluster := GottenLBAPerCluster;
end;

procedure TTrimThread.TrimPartition(const DriveLetter: String);
const
  BITS_PER_BYTE = 8;
  VOLUME_BITMAP_BYTES = SizeOf(VOLUME_BITMAP_BUFFER);
  VOLUME_BITMAP_BITS = VOLUME_BITMAP_BYTES * BITS_PER_BYTE;

  BIT_UNUSED = 0;
  BIT_USED = 1;

  TRIM_LIMIT_LBA = 65500;
  LBACOUNT_REST_THRESHOLD: Integer = TRIM_LIMIT_LBA shl 4;
var
  //핸들
  PartHandle, DeviceHandle: THandle;

  //파티션 정보
  PartInfo: TPartInfo;
  StartLBA: Int64;
  LBAPerCluster: Cardinal;

  //비트맵 요청 관련
  StartingBuffer: STARTING_LCN_INPUT_BUFFER;
  BitmapBuffer: VOLUME_BITMAP_BUFFER;
  BytesRead: Cardinal;
  error: Integer;

  //현재 위치
  CurrPart: Int64;
  CurrByte, CurrBit: Integer;
  IsCurrClusterUnused: Boolean;
  CurrPosByte: Integer;

  //트림 정보
  IsUnusedSpaceFound: Boolean;
  SetupPoint, LBACount: Int64;

  //쉬어주는 부분(샌드포스)
  CurrTrimLBAs: Int64;

  LastPart, LastBit, BitCount: Integer;
begin
  PartHandle :=
    CreateFile(
      PChar('\\.\' + DriveLetter),
      GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil, OPEN_EXISTING, 0, 0);
  error := GetLastError;
  if error <> ERROR_SUCCESS then
    exit;

  //초기 설정 반영
  StartLBA :=
    GetMotherDrive(DriveLetter)
      .Extents[0]
      .StartingOffset
    div LBASize;
  SetupPoint := 0;
  LBACount := 0;
  CurrPart := 0;
  IsUnusedSpaceFound := false;
  CurrTrimLBAs := 0;

  //파티션 정보 받기보다 무조건 위에 있어야 함
  StartingBuffer.StartingLcn.QuadPart := 0;
  DeviceIoControl(
    PartHandle,
    FSCTL_GET_VOLUME_BITMAP,
    @StartingBuffer, SizeOf(STARTING_LCN_INPUT_BUFFER),
    @BitmapBuffer, VOLUME_BITMAP_BYTES,
    BytesRead, nil);
  error := GetLastError;

  //파티션 정보 받기
  PartInfo :=
    GetPartInfo(DriveLetter, BitmapBuffer.BitmapSize.QuadPart);
  StartLBA := StartLBA + PartInfo.StartPadding;
  LBAPerCluster := PartInfo.LBAPerCluster;

  //시작 지점 진행률 계산 및 반영
  AllClusters := BitmapBuffer.BitmapSize;
  Progress :=
    round(
      ((CurrPart / AllClusters.QuadPart) + PartToTrim.CompletedPartition)
      / PartToTrim.Count * 100);
  if not IsSemiAuto then
    Synchronize(ApplyProgress);

  //디바이스에 대한 핸들 열기
  DeviceHandle :=
    TATALowOps.CreateHandle(
      GetMotherDrive(DriveLetter).Extents[0].DiskNumber);

  LastPart := VOLUME_BITMAP_BYTES - 1;
  LastBit := BITS_PER_BYTE - 1;
  while
    (error = ERROR_MORE_DATA) or
    (error = ERROR_INVALID_PARAMETER) or
    (error = ERROR_SUCCESS) do
  begin
    //0부터 시작하므로 1씩 뺀다
    if BitmapBuffer.BitmapSize.QuadPart < VOLUME_BITMAP_BITS then
    begin
      LastPart :=
        ceil(BitmapBuffer.BitmapSize.QuadPart / BITS_PER_BYTE)
        - 1;
      LastBit :=
        (BitmapBuffer.BitmapSize.QuadPart and
         (BITS_PER_BYTE - 1)) - 1;
    end;

    //LastPart / LastBit은 이미 1씩 빠져있다는 점에 주의
    BitCount := BITS_PER_BYTE - 1;
    for CurrByte := 0 to LastPart do
    begin
      //마지막이면 LastBit에 따르고 아니면 Full로 적용
      if CurrByte = LastPart then
        BitCount := LastBit;

      CurrPosByte := BitmapBuffer.Buffer[CurrByte];
      for CurrBit := 0 to BitCount do
      begin
        //안 쓰는 공간인지 Bit단위로 체크
        IsCurrClusterUnused :=
          (CurrPosByte and 1) = BIT_UNUSED;
        CurrPosByte := CurrPosByte shr 1;

        //안 쓰는 공간이면 다음과 같이 적용
        if IsCurrClusterUnused then
        begin
          if IsUnusedSpaceFound then
          begin
            LBACount := LBACount + LBAPerCluster
          end
          else
          begin
            SetupPoint :=
              StartLBA +
              ((BitmapBuffer.StartingLcn.QuadPart +
                  //비트맵 받아오는 단위 위치 (CurrPart와는 다르니 주의!)
                (CurrByte shl 3) +
                  // 현재 비트맵 내의 바이트 위치
                CurrBit)
                  //현재 비트
               * LBAPerCluster); //섹터당 LBA
            LBACount := LBAPerCluster;
            IsUnusedSpaceFound := true;
          end;
        end;

        {트림 해야되는 조건:
         1. 트림 길이 재는 도중
         2. 사용된 공간 발견
         3. 비사용 공간임에도
            3-1. LBA Count가 Limit을 넘었거나
            3-2. 마지막 위치일경우}
        if IsUnusedSpaceFound and //1
          ((not IsCurrClusterUnused) or //2
           ((IsCurrClusterUnused) and //3
            ((LBACount > TRIM_LIMIT_LBA) or //3-1
             ((CurrByte = LastPart) and (CurrBit = LastBit))))) //3-2
          then
        begin
          //쉬어주는 부분 계산
          CurrTrimLBAs := CurrTrimLBAs + LBACount;

          TATALowOps.TrimCommand(DeviceHandle,
            SetupPoint, LBACount);

          //트림 완료했으므로 초기화
          IsUnusedSpaceFound := false;
          SetupPoint := 0;
          LBACount := 0;
        end;
      end;
    end;

    //다음이 있는 경우
    if (error <> ERROR_INVALID_PARAMETER) and
       (error <> ERROR_SUCCESS) then
    begin
      //쉬어줘야 하면 쉬어줌
      if CurrTrimLBAs > LBACOUNT_REST_THRESHOLD then
      begin
        //진행률 계산 및 반영
        Progress :=
          round(
            ((CurrPart / AllClusters.QuadPart) +
             (PartToTrim.CompletedPartition + 1))
            / PartToTrim.Count * 100);

        if not IsSemiAuto then
          Synchronize(ApplyProgress);

        CurrTrimLBAs := 0;
      end;

      //트림 관련한 것들 초기화
      SetupPoint := 0;
      LBACount := 0;
      IsUnusedSpaceFound := false;


      //현재 위치 재계산 및 비트맵 받아옴
      CurrPart := CurrPart + (Length(BitmapBuffer.Buffer) shl 3);
      StartingBuffer.StartingLcn.QuadPart := CurrPart;
      DeviceIoControl(
        PartHandle,
        FSCTL_GET_VOLUME_BITMAP,
        @StartingBuffer, SizeOf(STARTING_LCN_INPUT_BUFFER),
        @BitmapBuffer, VOLUME_BITMAP_BYTES,
        BytesRead, nil);
      error := GetLastError;
    end
    else
      break;
  end;

  CloseHandle(DeviceHandle);
  CloseHandle(PartHandle);
end;

procedure TTrimThread.ApplyProgress;
begin
  fMain.pDownload.Position := Progress;
end;

procedure TTrimThread.ApplyStage;
begin
  fMain.pDownload.Position := Progress;

  if PartToTrim.CompletedPartition < 0 then
    exit;

  if PartToTrim.CompletedPartition < PartToTrim.Count then
    fMain.lProgress.Caption :=
      CapProg1[CurrLang] +
      PartToTrim[PartToTrim.CompletedPartition] + ' ' +
      CapProg2[CurrLang] + ' (' +
      IntToStr(PartToTrim.CompletedPartition + 1) + '/' +
      IntToStr(PartToTrim.Count) + ')';
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
