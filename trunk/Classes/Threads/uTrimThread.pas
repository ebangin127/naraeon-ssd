unit uTrimThread;

interface

uses
  Classes, SysUtils, uDiskFunctions, Math, 
  Generics.Collections, Dialogs, Windows,
  uATALowOps, uLanguageSettings, uPartitionFunctions;

type
  TTrimStage = (TRIMSTAGE_NONE, TRIMSTAGE_INPROGRESS,
    TRIMSTAGE_END, TRIMSTAGE_ERROR);
  TTrimListNextStat = (NEXTSTAT_SUCCESS, NEXTSTAT_NOTCMPL);
  TTrimListNext = record
    Status: TTrimListNextStat;
    NextPartition: String;
  end;
  TPartInfo = record
    StartPadding: Int64;
    LBAPerCluster: Cardinal;
  end;
  
  TTrimList = class(TList<String>)
  public
    property CurrPartition: Integer
      read FCurrPartition;
    property CompletedPartition: Integer
      read FCompletedPartition;
  
    procedure PointerToFirst;
    function GetNextPartition: TTrimListNext;
    procedure MarkAsCompleted;
    function GetPartInfo(DriveLetter: String; PartSize: Int64): TPartInfo;
  private
    FCurrPartition: Integer;
    FCompletedPartition: Integer;
  end;
  
  TTrimThread = class(TThread)
  public
    class var IsSemiAuto: Boolean; //반자동 트림 상황인가
    class var TrimStage: TTrimStage; //현재 트림 스테이지
    
    destructor Destroy; override;
    
    function ApplyPartList(PartListToTrim: TTrimList): Boolean;
  private
    PartToTrim: TTrimList;
    AllClusters: _LARGE_INTEGER;
    Progress: Cardinal;
  protected
    procedure Execute; override;
    procedure TrimPartition(const DriveLetter: String);
    
    //Main과의 Sync함수
    procedure ChangeProgressbar;
    procedure ChangeStage;
    procedure EndTrim;
  end;

implementation

uses uMain;

const
  LBASize = 512;
  
procedure TTrimList.PointerToFirst;
begin
  FCurrPartition := -1;
  FCompletedPartition := -1;
end;

function TTrimList.GetNextPartition: TTrimListNext;
begin
  if FCurrPartition < FCompletedPartition then
  begin
    result.Status := NEXTSTAT_NOTCMPL;
    exit;
  end;

  FCurrPartition := FCurrPartition + 1;
  result.Status := NEXTSTAT_SUCCESS;
  result.NextPartition := self[CurrPartition];
end;

procedure TTrimList.MarkAsCompleted;
begin
  FCompletedPartition := FCurrPartition;
end;
  
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

procedure TTrimThread.Execute;
var
  CurrDrive: Integer;
  PartCount: Integer;
begin
  if PartToTrim = nil then
  begin
    TrimStage := TRIMSTAGE_ERROR;
    exit;
  end;
  
  PartToTrim.PointerToFirst;

  TrimStage := TRIMSTAGE_INPROGRESS;
  PartCount := PartToTrim.Count;
  
  if not IsSemiAuto then
    Synchronize(ChangeStage);
  
  for CurrDrive := 0 to PartCount - 1 do
  begin
    TrimPartition(PartToTrim.GetNextPartition.NextPartition);
    PartToTrim.MarkAsComplete;
    
    if not IsSemiAuto then
      Synchronize(ChangeStage);
  end;
  
  if not IsSemiAuto then
    Synchronize(EndTrim);
    
  TrimStage := TRIMSTAGE_END;
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
  StartingBuffer.StartingLcn.QuadPart := 0;
  
  if NTFSInfo.ErrorCode = 0 then
  begin
    result.LBAPerCluster := Cardinal(NTFSInfo.SectorPerCluster);
    exit;
  end;
  
  //FAT종류인 경우
  GetDiskFreeSpace(PChar(DriveLetter + '\'), GottenLBAPerSector,
    Nouse[0], Nouse[1], Nouse[2]);
                   
  {GetPartitionLength의 결과값은 Bytes로 나오므로 그대로 사용
   PartSize는 클러스터단위므로 클러스터당 LBA & LBA 사이즈 곱한다
   마지막으로 StartLBA이므로 LBASize로 나눠준다}
  
  FATLength :=
    (GetPartitionLength(DriveLetter) -
      (TempResult.BitmapSize.QuadPart * GottenLBAPerSector * LBASize))
    div LBASize;
  
  result.StartPadding := FATLength;
  result.LBAPerCluster := GottenLBAPerCluster;
end;

procedure TTrimThread.TrimPartition(const DriveLetter: String);
const
  BITS_PER_BYTE = 8;
  VOLUME_BITMAP_SIZE = SizeOf(VOLUME_BITMAP_BUFFER);
  VOLUME_BITMAP_BITS = VOLUME_BITMAP_BYTES * BITS_PER_BYTE;
  
  BIT_UNUSED = 0;
  BIT_USED = 1;
  
  TRIM_LIMIT_LBA = 65500;
  LBACOUNT_REST_THRESHOLD = TRIM_LIMIT_LBA shl 1;
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
  IsCurrClusterUsed: Boolean;
  
  //트림 정보
  IsUnusedSpaceFound: Boolean;
  SetupPoint, LBACount: Int64;
  
  //쉬어주는 부분(샌드포스)
  CurrTrimLBAs: Int64;
  SleepTime_LBACount: Integer;
  
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
  IsUnusedSpaceFound := 0;
  CurrTrimLBAs := 0;

  //파티션 정보 받기보다 무조건 위에 있어야 함
  StartingBuffer.StartingLcn.QuadPart := 0;
  DeviceIoControl(
    PartHandle,
    FSCTL_GET_VOLUME_BITMAP,
    @StartingBuffer, SizeOf(STARTING_LCN_INPUT_BUFFER),
    @BitmapBuffer, VOLUME_BITMAP_SIZE,
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
      ((CurrPart / AllClusters.QuadPart) + CompletedPartition)
      / PartCount * 100);
  if MainLoaded then
    Synchronize(ApplyProgress);

  //디바이스에 대한 핸들 열기
  DeviceHandle :=
    TATALowOps.CreateHandle(
      GetMotherDrive(DriveLetter).Extents[0].DiskNumber);
        
  while
    (error = ERROR_MORE_DATA) or
    (error = ERROR_INVALID_PARAMETER) or
    (error = ERROR_SUCCESS) do
  begin
    //0부터 시작하므로 1씩 뺀다
    if BitmapBuffer.BitmapSize.QuadPart >= VOLUME_BITMAP_BITS then
    begin
      LastPart := VOLUME_BITMAP_BYTES - 1;
      LastBit := BITS_PER_BYTE - 1;
    end
    else
    begin
      LastPart :=
        ceil(BitmapBuffer.BitmapSize.QuadPart / BITS_PER_BYTE)
        - 1;
      LastBit :=
        (BitmapBuffer.BitmapSize.QuadPart and 
         (BITS_PER_BYTE - 1)) - 1;
    end;
    
    //LastPart / LastBit은 이미 1씩 빠져있다는 점에 주의
    for CurrByte := 0 to LastPart do
    begin
      //마지막이면 LastBit에 따르고 아니면 Full로 적용
      if CurrByte = LastPart then
        BitCount := LastBit
      else
        BitCount := BITS_PER_BYTE - 1;
      
      for CurrBit := 0 to BitCount do
      begin
        //안 쓰는 공간인지 Bit단위로 체크
        IsCurrClusterUsed :=
          (BitmapBuffer.Buffer[CurrByte] and 1) = BIT_UNUSED;
        BitmapBuffer.Buffer[CurrByte] :=
          BitmapBuffer.Buffer[CurrByte] shr 1;
          
        //안 쓰는 공간이면 다음과 같이 적용
        if not IsCurrClusterUsed then
        begin
          if IsUnusedSpaceFound then
          begin
            Assert(BitmapBuffer.StartingLcn.QuadPart = CurrPart,
              IntToStr(BitmapBuffer.StartingLcn.QuadPart)
              + ' <> ' +
              IntToStr(CurrPart));
            SetupPoint :=
              StartLBA + 
              ((CurrPart +
                  //비트맵 받아오는 단위 위치
                (CurrByte shl 3) +
                  // 현재 비트맵 내의 바이트 위치
                CurrBit)
                  //현재 비트
               * LBAPerCluster); //섹터당 LBA
            LBACount := LBAPerCluster;
            IsUnusedSpaceFound := true;
            Continue;
          end
          else
            LBACount := LBACount + LBAPerSector;
        end;
          
        {트림 해야되는 조건:
         1. 트림 길이 재는 도중
         2. 사용된 공간 발견
         3. 비사용 공간임에도
            3-1. LBA Count가 Limit을 넘었거나
            3-2. 마지막 위치일경우}
        if IsUnusedSpaceFound and //1
          ((IsCurrClusterUsed) or //2
           ((not IsCurrClusterUsed) and //3
            ((LBACount > TRIM_LIMIT_LBA) or //3-1
             ((CurrByte = LastPart) and (CurrBit = LastBit)))) //3-2
          then
        begin
          //쉬어주는 부분 계산
          CurrTrimLBAs := CurrTrimLBAs + LBACount;
          
          TATALowOps.TrimCommand(DeviceHandle,
            SetupPoint, LBACount);
          
          //트림 완료했으므로 초기화
          SetupPoint := 0;
          LBACount := 0;
          IsUnusedSpaceFound := false;
          
          Continue;
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
        SleepTime_LBACount := CurrTrimLBAs shr 16;
        Sleep(SleepTime_LBACount);
        CurrTrimLBAs := 0;
      end;
      
      //트림 관련한 것들 초기화
      SetupPoint := 0;
      LBACount := 0;
      IsUnusedSpaceFound := false;
      
      
      //현재 위치 재계산 및 비트맵 받아옴
      CurrPart := CurrPart + (Length(TempResult.Buffer) shl 3);
      StartingBuffer.StartingLcn.QuadPart := CurrPart;
      DeviceIoControl(
        PartHandle,
        FSCTL_GET_VOLUME_BITMAP,
        @StartingBuffer, SizeOf(STARTING_LCN_INPUT_BUFFER),
        @BitmapBuffer, VOLUME_BITMAP_SIZE,
        BytesRead, nil);
      error := GetLastError;
      
      //진행률 계산 및 반영
      Progress := 
        round(
          ((CurrPart / AllClusters.QuadPart) + CompletedPartition)
          / PartCount * 100);
      if MainLoaded then
        Synchronize(ChangeProgressbar);
    end
    else
      break;
  end;
  
  CloseHandle(DeviceHandle);
  CloseHandle(PartHandle);
end;

procedure TTrimThread.ChangeProgressbar;
begin
  fMain.pDownload.Position := Progress;
end;

procedure TTrimThread.ChangeStage;
begin
  fMain.pDownload.Position := Progress;
  if CompletedPartition < PartCount then
    fMain.lProgress.Caption := 
      CapProg1[CurrLang] +
      NeedTrimPartition[CompletedPartition] + ' ' +
      CapProg2[CurrLang] + ' (' +
      IntToStr(CompletedPartition + 1) + '/' +
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
