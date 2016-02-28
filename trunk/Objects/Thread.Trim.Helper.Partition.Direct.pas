unit Thread.Trim.Helper.Partition.Direct;

interface

uses
  SysUtils, Classes, Windows, Math,
  OSFile, Thread.Trim.Helper.Partition, Getter.OS.Version, OS.Version.Helper,
  OS.Partition.Lock, Getter.Filesystem.Name
  {$IfNDef UNITTEST}, ThreadToView.Trim,{$EndIf}
  {$IfDef UNITTEST}, Mock.Getter.TrimBasics.Factory,
  {$Else}Getter.TrimBasics, Getter.TrimBasics.Factory,{$EndIf}
  {$IfDef UNITTEST}Mock.DeviceTrimmer,
  {$Else}Thread.Trim.Helper.Device,{$EndIf}
  {$IfDef UNITTEST}Mock.Getter.VolumeBitmap
  {$Else}Getter.VolumeBitmap{$EndIf};

type
  TDirectPartitionTrimmer = class(TPartitionTrimmer)
  private
    type
      TCurrentPoint = record
        PartitionStartLBA: UInt64;
        OffsetInCluster: LARGE_INTEGER;
        CardinalLength, BitLengthOfLastCardinal: Integer;
        BitLengthOfCurrentCardinal: Integer;
        CurrentCardinalInBuffer, CurrentBitInCardinal: Integer;
      end;
      TPendingTrimOperation = record
        IsUnusedSpaceFound: Boolean;
        StartLBA: UInt64;
        LengthInLBA: UInt64;
      end;
    const
      BitsPerByte = 8;
      BitsPerCardinal = 32;
      VolumeBitmapBufferSizeInBit = SizeOf(TBitmapBuffer) * BitsPerByte;
      BufferSizeInCluster = BitmapSizePerBuffer * BitsPerCardinal;
  private
    TrimSynchronization: TTrimSynchronization;
    VolumeBitmapGetter: TVolumeBitmapGetter;
    VolumeBitmapBufferWithErrorCode: TVolumeBitmapBufferWithErrorCode;
    CurrentPartitionTrimProgress: TCurrentPartitionTrimProgress;
    TrimBasicsToInitialize: TTrimBasicsToInitialize;
    DeviceTrimmer: TDeviceTrimmer;
    CurrentPoint: TCurrentPoint;
    {$IfNDef UNITTEST}
    TrimThreadToView: TTrimThreadToView;
    {$EndIf}
    VolumeSizeInCluster: LARGE_INTEGER;
    LastTick: Cardinal;
    PartitionLock: TPartitionLock;
    procedure TryToTrimPartition(
      const TrimSynchronizationToApply: TTrimSynchronization);
    procedure ProcessTrim;
    procedure TrimNextPartOfPartition;
    procedure TrimCurrentCardinal;
    procedure TrimCurrentBit(const IsCurrentClusterUnused: Boolean);
    procedure InitializeTrim;
    procedure InitializeTrimBasicsGetter;
    procedure InitializeVolumeBitmap;
    procedure InitializeModel;
    procedure InitializeDeviceTrimmer;
    procedure InitializeBitCardinalLength;
    procedure InitializeStartLBA;
    function IsMoreTrimNeeded: Boolean;
    function FoundUsedSpaceOrLastPart(const IsCurrentClusterUnused: Boolean):
      Boolean;
    function IsTrimNeeded(const IsCurrentClusterUnused: Boolean): Boolean;
    function IsLastPartOfBuffer: Boolean;
    function IsMorePartLeftSetVolumeBitmapBuffer: Boolean;
    function GetCurrentPositionInLBA: UInt64;
    procedure SetVolumeSizeInCluster;
    procedure SetNextPartPosition;
    procedure SetBaseProgress;
    procedure IfLastPartSetCardinalBitLength;
    function GetBitLengthOfLastCardinal(const BitmapSizeInBit: LARGE_INTEGER):
      Integer;
    procedure CalculateProgress;
    procedure ProcessAndClearPendingTrim;
    procedure IfNeedToRestApplyToUI;
    procedure IncreaseOrSetTrimPosition;
    procedure SetVolumeBitmapBuffer(const StartingLCN: LARGE_INTEGER);
    procedure IfLastCardinalInBufferSetBitLength;
    procedure FreeClassesForTrim;
    procedure LockPartition;
    procedure UnlockPartition;
    {$IfDef UNITTEST}
    {$Hints Off}
    {$EndIf}
    procedure InitializeLock;
    procedure FinalizeLock;
    procedure WaitAndRetryLock;
    {$IfDef UNITTEST}
    {$Hints On}
    {$EndIf}
  public
    procedure TrimPartition(
      const TrimSynchronizationToApply: TTrimSynchronization); override;
  end;

implementation

{ TPartitionTrimmer }

procedure TDirectPartitionTrimmer.SetVolumeBitmapBuffer
  (const StartingLCN: LARGE_INTEGER);
begin
  VolumeBitmapBufferWithErrorCode :=
    VolumeBitmapGetter.GetVolumeBitmap(StartingLCN);
end;

procedure TDirectPartitionTrimmer.SetBaseProgress;
const
  ToPercent = 100;
begin
  if CurrentPartitionTrimProgress.BaseProgress > 0 then
    exit;

  if TrimSynchronization.Progress.PartitionCount > 0 then
    CurrentPartitionTrimProgress.ProgressPerPartition :=
      round(1 / TrimSynchronization.Progress.PartitionCount * ToPercent)
  else
    CurrentPartitionTrimProgress.ProgressPerPartition := 0;

  CurrentPartitionTrimProgress.BaseProgress :=
    round(CurrentPartitionTrimProgress.ProgressPerPartition *
      (TrimSynchronization.Progress.CurrentPartition - 1));
end;

procedure TDirectPartitionTrimmer.CalculateProgress;
begin
  SetBaseProgress;
  CurrentPartitionTrimProgress.Progress :=
    CurrentPartitionTrimProgress.BaseProgress +
    round(CurrentPartitionTrimProgress.ProgressPerPartition *
      (VolumeBitmapBufferWithErrorCode.PositionSize.StartingLCN.QuadPart /
       VolumeSizeInCluster.QuadPart));
end;

procedure TDirectPartitionTrimmer.SetNextPartPosition;
begin
  CurrentPoint.OffsetInCluster.QuadPart :=
    VolumeBitmapBufferWithErrorCode.PositionSize.StartingLCN.QuadPart +
    BufferSizeInCluster;
end;

function TDirectPartitionTrimmer.IsMorePartLeftSetVolumeBitmapBuffer: Boolean;
begin
  result := VolumeBitmapBufferWithErrorCode.LastError = ERROR_MORE_DATA;
  if result then
  begin
    SetNextPartPosition;
    SetVolumeBitmapBuffer(CurrentPoint.OffsetInCluster);
  end;
end;

function TDirectPartitionTrimmer.GetBitLengthOfLastCardinal
  (const BitmapSizeInBit: LARGE_INTEGER): Integer;
begin
  result := BitmapSizeInBit.QuadPart and (BitsPerCardinal - 1);
end;

procedure TDirectPartitionTrimmer.IfLastPartSetCardinalBitLength;
begin
  if VolumeBitmapBufferWithErrorCode.PositionSize.BitmapSize.QuadPart <
     VolumeBitmapBufferSizeInBit then
  begin
    CurrentPoint.CardinalLength :=
      ceil(VolumeBitmapBufferWithErrorCode.PositionSize.BitmapSize.QuadPart /
        BitsPerCardinal);
    CurrentPoint.BitLengthOfLastCardinal :=
      GetBitLengthOfLastCardinal(
        VolumeBitmapBufferWithErrorCode.PositionSize.BitmapSize);
  end;
end;

function TDirectPartitionTrimmer.GetCurrentPositionInLBA: UInt64;
begin
  result := VolumeBitmapBufferWithErrorCode.PositionSize.StartingLCN.QuadPart;
  result := result + CurrentPoint.CurrentCardinalInBuffer shl 5;
  result := result + CurrentPoint.CurrentBitInCardinal;
  result := result * TrimBasicsToInitialize.LBAPerCluster;
end;

procedure TDirectPartitionTrimmer.IncreaseOrSetTrimPosition;
begin
  if DeviceTrimmer.IsUnusedSpaceFound then
    DeviceTrimmer.IncreaseLength(TrimBasicsToInitialize.LBAPerCluster)
  else
    DeviceTrimmer.SetStartPoint(
      CurrentPoint.PartitionStartLBA + GetCurrentPositionInLBA,
      TrimBasicsToInitialize.LBAPerCluster);
end;

function TDirectPartitionTrimmer.IsTrimNeeded(const IsCurrentClusterUnused: Boolean):
  Boolean;
begin
  result :=
    DeviceTrimmer.IsUnusedSpaceFound and
    FoundUsedSpaceOrLastPart(IsCurrentClusterUnused);
end;

function TDirectPartitionTrimmer.IsLastPartOfBuffer: Boolean;
begin
  result :=
    (CurrentPoint.CurrentCardinalInBuffer = CurrentPoint.CardinalLength - 1) and
    (CurrentPoint.CurrentBitInCardinal =
     CurrentPoint.BitLengthOfCurrentCardinal - 1);
end;

function TDirectPartitionTrimmer.FoundUsedSpaceOrLastPart(
  const IsCurrentClusterUnused: Boolean): Boolean;
begin
  result :=
    (not IsCurrentClusterUnused) or
    DeviceTrimmer.IsLBACountOverLimit or
    IsLastPartOfBuffer;
end;

procedure TDirectPartitionTrimmer.IfNeedToRestApplyToUI;
const
  Threshold = 1000;
  Overflow = 0;
var
  CurrentTick: DWORD;
  Difference: Int64;
begin
  CurrentTick := GetTickCount;
  Difference := Int64(LastTick) - Int64(CurrentTick);
  if (Difference < Overflow) or (Difference > Threshold) then
  begin
    LastTick := CurrentTick;
    CalculateProgress;
    {$IfNDef UNITTEST}
    TrimThreadToView.ApplyProgressToUI(CurrentPartitionTrimProgress.Progress);
    {$EndIf}
  end;
end;

procedure TDirectPartitionTrimmer.ProcessAndClearPendingTrim;
begin
  if not IsBelowWindows8(VersionHelper.Version) then
    LockPartition;
  DeviceTrimmer.Flush;
  if not IsBelowWindows8(VersionHelper.Version) then
    UnlockPartition;
  IfNeedToRestApplyToUI;
end;

procedure TDirectPartitionTrimmer.TrimCurrentBit(
  const IsCurrentClusterUnused: Boolean);
begin
  if IsCurrentClusterUnused then
    IncreaseOrSetTrimPosition;
  if IsTrimNeeded(IsCurrentClusterUnused) then
    ProcessAndClearPendingTrim;
end;

procedure TDirectPartitionTrimmer.IfLastCardinalInBufferSetBitLength;
begin
  if CurrentPoint.CurrentCardinalInBuffer = CurrentPoint.CardinalLength - 1 then
    CurrentPoint.BitLengthOfCurrentCardinal :=
      CurrentPoint.BitLengthOfLastCardinal;
end;

procedure TDirectPartitionTrimmer.TrimCurrentCardinal;
var
  CurrentCardinal: Cardinal;
  CurrentBitInCardinal: Integer;
begin
  IfLastCardinalInBufferSetBitLength;

  CurrentCardinal :=
    VolumeBitmapBufferWithErrorCode.Buffer[
      CurrentPoint.CurrentCardinalInBuffer];
  for CurrentBitInCardinal :=
    0 to CurrentPoint.BitLengthOfCurrentCardinal - 1 do
  begin
    CurrentPoint.CurrentBitInCardinal := CurrentBitInCardinal;
    TrimCurrentBit((CurrentCardinal and 1) = 0);
    CurrentCardinal := CurrentCardinal shr 1;
  end;
end;

procedure TDirectPartitionTrimmer.TrimNextPartOfPartition;
var
  CurrentCardinalInBuffer: Integer;
begin
  IfLastPartSetCardinalBitLength;

  CurrentPoint.BitLengthOfCurrentCardinal := BitsPerCardinal;
  for CurrentCardinalInBuffer := 0 to CurrentPoint.CardinalLength - 1 do
  begin
    CurrentPoint.CurrentCardinalInBuffer := CurrentCardinalInBuffer;
    TrimCurrentCardinal;
  end;

  if not IsMorePartLeftSetVolumeBitmapBuffer then
    VolumeBitmapBufferWithErrorCode.LastError := ERROR_NO_MORE_ITEMS;
end;

function TDirectPartitionTrimmer.IsMoreTrimNeeded: Boolean;
begin
  result :=
    (VolumeBitmapBufferWithErrorCode.LastError = ERROR_MORE_DATA) or
    (VolumeBitmapBufferWithErrorCode.LastError = ERROR_INVALID_PARAMETER) or
    (VolumeBitmapBufferWithErrorCode.LastError = ERROR_SUCCESS);
end;

procedure TDirectPartitionTrimmer.ProcessTrim;
begin
  while IsMoreTrimNeeded do
    TrimNextPartOfPartition;
end;

procedure TDirectPartitionTrimmer.InitializeBitCardinalLength;
begin
  CurrentPoint.CardinalLength := Length(VolumeBitmapBufferWithErrorCode.Buffer);
  CurrentPoint.BitLengthOfLastCardinal := BitsPerCardinal;
end;

procedure TDirectPartitionTrimmer.InitializeStartLBA;
begin
  CurrentPoint.PartitionStartLBA :=
    TrimBasicsToInitialize.StartLBA +
    TrimBasicsToInitialize.PaddingLBA;
end;

procedure TDirectPartitionTrimmer.InitializeDeviceTrimmer;
begin
  DeviceTrimmer := TDeviceTrimmer.Create(GetPathOfFileAccessing);
end;

procedure TDirectPartitionTrimmer.SetVolumeSizeInCluster;
begin
  VolumeSizeInCluster :=
    VolumeBitmapBufferWithErrorCode.PositionSize.BitmapSize;
end;

procedure TDirectPartitionTrimmer.InitializeTrimBasicsGetter;
var
  TrimBasicsGetter: TTrimBasicsGetter;
begin
  TrimBasicsGetter := TrimBasicsGetterFactory.GetSuitableTrimBasicsGetter(
    GetPathOfFileAccessing);
  if not TrimBasicsGetter.IsPartitionMyResponsibility then
    raise EUnknownPartition.Create
      ('Unknown Partiton: Use with known partition');
  TrimBasicsToInitialize := TrimBasicsGetter.GetTrimBasicsToInitialize;
  FreeAndNil(TrimBasicsGetter);
  InitializeStartLBA;
end;

procedure TDirectPartitionTrimmer.InitializeVolumeBitmap;
begin
  VolumeBitmapGetter := TVolumeBitmapGetter.Create(GetPathOfFileAccessing);
  SetVolumeBitmapBuffer(CurrentPoint.OffsetInCluster);
  SetVolumeSizeInCluster;
  InitializeBitCardinalLength;
end;

procedure TDirectPartitionTrimmer.InitializeModel;
begin
  CalculateProgress;
  {$IfNDef UNITTEST}
  TrimThreadToView := TTrimThreadToView.Create(TrimSynchronization);
  TrimThreadToView.ApplyNextDriveStartToUI(
    CurrentPartitionTrimProgress.Progress);
  {$EndIf}
end;

procedure TDirectPartitionTrimmer.InitializeLock;
begin
  PartitionLock := TPartitionLock.Create(GetPathOfFileAccessing);
end;

procedure TDirectPartitionTrimmer.LockPartition;
begin
  {$IfNDef UNITTEST}
  FreeAndNil(VolumeBitmapGetter);
  InitializeLock;
  try
    PartitionLock.Lock;
  except
    on Exception: EOSError do
      if Exception.ErrorCode = ERROR_ACCESS_DENIED then
        WaitAndRetryLock
      else raise;
    else raise;
  end;
  {$EndIf}
end;

procedure TDirectPartitionTrimmer.UnlockPartition;
begin
  {$IfNDef UNITTEST}
  PartitionLock.Unlock;
  FinalizeLock;
  VolumeBitmapGetter := TVolumeBitmapGetter.Create(GetPathOfFileAccessing);
  {$EndIf}
end;

procedure TDirectPartitionTrimmer.InitializeTrim;
begin
  CurrentPoint.OffsetInCluster.QuadPart := 0;
  InitializeTrimBasicsGetter;
  InitializeVolumeBitmap;
  InitializeDeviceTrimmer;
  InitializeModel;
end;

procedure TDirectPartitionTrimmer.TryToTrimPartition(
  const TrimSynchronizationToApply: TTrimSynchronization);
begin
  TrimSynchronization := TrimSynchronizationToApply;
  InitializeTrim;
  ProcessTrim;
end;

procedure TDirectPartitionTrimmer.FinalizeLock;
begin
  FreeAndNil(PartitionLock);
end;

procedure TDirectPartitionTrimmer.FreeClassesForTrim;
begin
  FreeAndNil(VolumeBitmapGetter);
  {$IfNDef UNITTEST}
  FreeAndNil(TrimThreadToView);
  {$EndIf}
  FreeAndNil(DeviceTrimmer);
end;

procedure TDirectPartitionTrimmer.TrimPartition(
  const TrimSynchronizationToApply: TTrimSynchronization);
begin
  try
    TryToTrimPartition(TrimSynchronizationToApply);
  finally
    FreeClassesForTrim;
  end;
end;

procedure TDirectPartitionTrimmer.WaitAndRetryLock;
begin
  FinalizeLock;
  Sleep(500);
  LockPartition;
end;

end.
