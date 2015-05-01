unit uPartitionTrimmer;

interface

uses
  SysUtils, Classes, Windows, Math,
  uOSFile, uTrimBasicsGetter, uAutoTrimBasicsGetter, uPartition,
  uPartitionExtentGetter, uAutoCommandSet, uVolumeBitmapGetter,
  uTrimThreadToModel;

type
  TPartitionTrimmer = class(TOSFile)
  private
    type
      TCurrentPartitionTrimProgress = record
        Progress: Integer;
        BaseProgress: Integer;
        ProgressPerPartition: Integer;
      end;

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
    AutoTrimBasicsGetter: TAutoTrimBasicsGetter;
    VolumeBitmapGetter: TVolumeBitmapGetter;
    VolumeBitmapBufferWithErrorCode: TVolumeBitmapBufferWithErrorCode;
    CurrentPartitionTrimProgress: TCurrentPartitionTrimProgress;
    TrimBasicsToInitialize: TTrimBasicsToInitialize;
    PendingTrimOperation: TPendingTrimOperation;
    CurrentPoint: TCurrentPoint;
    AutoCommandSet: TAutoCommandSet;
    TrimThreadToModel: TTrimThreadToModel;
    VolumeSizeInCluster: LARGE_INTEGER;
    LastTick: Cardinal;
    procedure InitializeTrim;
    procedure ProcessTrim;
    function IsMoreTrimNeeded: Boolean;
    procedure TrimNextPartOfPartition;
    procedure TrimCurrentCardinal;
    procedure TrimCurrentBit(IsCurrentClusterUnused: Boolean);
    procedure SetNextPartPosition;
    function IsMorePartLeftSetVolumeBitmapBuffer: Boolean;
    procedure IfLastPartSetCardinalBitLength;
    function GetBitLengthOfLastCardinal(BitmapSizeInBit: LARGE_INTEGER): Integer;
    procedure InitializeBitCardinalLength;
    procedure InitializeStartLBA;
    procedure CalculateProgress;
    procedure SetBaseProgress;
    procedure IncreaseOrSetTrimPosition;
    procedure SetThisPositionAsStart;
    function GetCurrentPositionInLBA: UInt64;
    function IsTrimNeeded(IsCurrentClusterUnused: Boolean): Boolean;
    function FoundUsedSpaceOrLastPart(IsCurrentClusterUnused: Boolean): Boolean;
    function IsLastPartOfBuffer: Boolean;
    function IsLBACountOverLimit: Boolean;
    procedure ProcessAndClearPendingTrim;
    procedure ClearPendingTrim;
    procedure ProcessPendingTrim;
    procedure InitializeCommandSet;
    procedure FreeClassesForTrim;
    procedure IfNeedToRestApplyToUI;
    procedure TryToTrimPartition(
      TrimSynchronizationToApply: TTrimSynchronization);
    procedure SetVolumeSizeInCluster;
    procedure InitializeTrimBasicsGetter;
    procedure InitializeVolumeBitmap;
    procedure InitializeModel;
    function GetMotherDrivePath: String;
    procedure SetVolumeBitmapBuffer(StartingLCN: LARGE_INTEGER);
    procedure IfLastCardinalInBufferSetBitLength;

  public
    destructor Destroy; override;
    procedure TrimPartition(TrimSynchronizationToApply: TTrimSynchronization);
    function GetPathOfFileAccessing: String; override;
  end;

implementation

{ TPartitionTrimmer }

destructor TPartitionTrimmer.Destroy;
begin
  if AutoTrimBasicsGetter <> nil then
    FreeAndNil(AutoTrimBasicsGetter);
  inherited;
end;

procedure TPartitionTrimmer.SetVolumeBitmapBuffer
  (StartingLCN: LARGE_INTEGER);
begin
  VolumeBitmapBufferWithErrorCode :=
    VolumeBitmapGetter.GetVolumeBitmap(StartingLCN);
end;

function TPartitionTrimmer.GetMotherDrivePath: String;
var
  PartitionExtentGetter: TPartitionExtentGetter;
  PartitionExtentList: TPartitionExtentList;
begin
  PartitionExtentGetter :=
    TPartitionExtentGetter.Create(GetPathOfFileAccessing);
  PartitionExtentList := PartitionExtentGetter.GetPartitionExtentList;
  result :=
    ThisComputerPrefix +
    PhysicalDrivePrefix +
    IntToStr(PartitionExtentList[0].DriveNumber);
  FreeAndNil(PartitionExtentList);
  FreeAndNil(PartitionExtentGetter);
end;

procedure TPartitionTrimmer.InitializeCommandSet;
begin
  AutoCommandSet := TAutoCommandSet.Create(GetMotherDrivePath);
  AutoCommandSet.IdentifyDevice;
end;

procedure TPartitionTrimmer.SetVolumeSizeInCluster;
begin
  VolumeSizeInCluster :=
    VolumeBitmapBufferWithErrorCode.PositionSize.BitmapSize;
end;

procedure TPartitionTrimmer.InitializeTrimBasicsGetter;
begin
  AutoTrimBasicsGetter := TAutoTrimBasicsGetter.Create(GetPathOfFileAccessing);
  if not AutoTrimBasicsGetter.IsPartitionMyResponsibility then
    raise EUnknownPartition.Create
      ('Unknown Partiton: Use with known partition');
  TrimBasicsToInitialize := AutoTrimBasicsGetter.GetTrimBasicsToInitialize;
  InitializeStartLBA;
end;

procedure TPartitionTrimmer.InitializeVolumeBitmap;
begin
  VolumeBitmapGetter := TVolumeBitmapGetter.Create(GetPathOfFileAccessing);
  SetVolumeBitmapBuffer(CurrentPoint.OffsetInCluster);
  SetVolumeSizeInCluster;
  InitializeBitCardinalLength;
end;

procedure TPartitionTrimmer.InitializeModel;
begin
  CalculateProgress;
  TrimThreadToModel := TTrimThreadToModel.Create(TrimSynchronization);
  TrimThreadToModel.ApplyNextDriveStartToUI(
    CurrentPartitionTrimProgress.Progress);
end;

procedure TPartitionTrimmer.InitializeTrim;
begin
  CurrentPoint.OffsetInCluster.QuadPart := 0;
  InitializeTrimBasicsGetter;
  InitializeVolumeBitmap;
  InitializeCommandSet;
  InitializeModel;
end;

procedure TPartitionTrimmer.SetBaseProgress;
const
  ToPercent = 100;
begin
  if CurrentPartitionTrimProgress.BaseProgress > 0 then
    exit;

  CurrentPartitionTrimProgress.ProgressPerPartition :=
    round(1 / TrimSynchronization.Progress.PartitionCount * ToPercent);

  CurrentPartitionTrimProgress.BaseProgress :=
    round(CurrentPartitionTrimProgress.ProgressPerPartition *
      (TrimSynchronization.Progress.CurrentPartition - 1));
end;

procedure TPartitionTrimmer.CalculateProgress;
begin
  SetBaseProgress;
  CurrentPartitionTrimProgress.Progress :=
    CurrentPartitionTrimProgress.BaseProgress +
    round(CurrentPartitionTrimProgress.ProgressPerPartition *
      (VolumeBitmapBufferWithErrorCode.PositionSize.StartingLCN.QuadPart /
       VolumeSizeInCluster.QuadPart));
end;

procedure TPartitionTrimmer.SetNextPartPosition;
begin
  CurrentPoint.OffsetInCluster.QuadPart :=
    VolumeBitmapBufferWithErrorCode.PositionSize.StartingLCN.QuadPart +
    BufferSizeInCluster;
end;

function TPartitionTrimmer.IsMorePartLeftSetVolumeBitmapBuffer: Boolean;
begin
  result := VolumeBitmapBufferWithErrorCode.LastError = ERROR_MORE_DATA;
  if result then
  begin
    SetNextPartPosition;
    SetVolumeBitmapBuffer(CurrentPoint.OffsetInCluster);
  end;
end;

function TPartitionTrimmer.GetBitLengthOfLastCardinal
  (BitmapSizeInBit: LARGE_INTEGER): Integer;
begin
  result := BitmapSizeInBit.QuadPart and (BitsPerCardinal - 1);
end;

procedure TPartitionTrimmer.IfLastPartSetCardinalBitLength;
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

function TPartitionTrimmer.GetCurrentPositionInLBA: UInt64;
begin
  result := VolumeBitmapBufferWithErrorCode.PositionSize.StartingLCN.QuadPart;
  result := result + CurrentPoint.CurrentCardinalInBuffer shl 5;
  result := result + CurrentPoint.CurrentBitInCardinal;
  result := result * TrimBasicsToInitialize.LBAPerCluster;
end;

function TPartitionTrimmer.GetPathOfFileAccessing: String;
begin
  result := inherited GetPathOfFileAccessing;
  result := ThisComputerPrefix + result;
end;

procedure TPartitionTrimmer.SetThisPositionAsStart;
begin
  PendingTrimOperation.IsUnusedSpaceFound := true;
  PendingTrimOperation.StartLBA :=
    CurrentPoint.PartitionStartLBA +
    GetCurrentPositionInLBA;
  PendingTrimOperation.LengthInLBA :=
    TrimBasicsToInitialize.LBAPerCluster;
end;

procedure TPartitionTrimmer.IncreaseOrSetTrimPosition;
begin
  if PendingTrimOperation.IsUnusedSpaceFound then
    PendingTrimOperation.LengthInLBA :=
      PendingTrimOperation.LengthInLBA +
      TrimBasicsToInitialize.LBAPerCluster
  else
    SetThisPositionAsStart;
end;

function TPartitionTrimmer.IsLBACountOverLimit: Boolean;
const
  LimitLengthInLBA = 65500;
begin
  result := PendingTrimOperation.LengthInLBA > LimitLengthInLBA;
end;

function TPartitionTrimmer.IsLastPartOfBuffer: Boolean;
begin
  result :=
    (CurrentPoint.CurrentCardinalInBuffer = CurrentPoint.CardinalLength) and
    (CurrentPoint.CurrentBitInCardinal =
     CurrentPoint.BitLengthOfCurrentCardinal);
end;

function TPartitionTrimmer.FoundUsedSpaceOrLastPart(
  IsCurrentClusterUnused: Boolean): Boolean;
begin
  result :=
    (not IsCurrentClusterUnused) or
    IsLBACountOverLimit or
    IsLastPartOfBuffer;
end;

function TPartitionTrimmer.IsTrimNeeded(IsCurrentClusterUnused: Boolean):
  Boolean;
begin
  result :=
    PendingTrimOperation.IsUnusedSpaceFound and
    FoundUsedSpaceOrLastPart(IsCurrentClusterUnused);
end;

procedure TPartitionTrimmer.ProcessPendingTrim;
begin
  AutoCommandSet.DataSetManagement(
    PendingTrimOperation.StartLBA,
    PendingTrimOperation.LengthInLBA);
end;

procedure TPartitionTrimmer.ClearPendingTrim;
begin
  ZeroMemory(@PendingTrimOperation, SizeOf(PendingTrimOperation));
end;

procedure TPartitionTrimmer.IfNeedToRestApplyToUI;
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
    TrimThreadToModel.ApplyProgressToUI(CurrentPartitionTrimProgress.Progress);
  end;
end;

procedure TPartitionTrimmer.ProcessAndClearPendingTrim;
begin
  ProcessPendingTrim;
  ClearPendingTrim;
  IfNeedToRestApplyToUI;
end;

procedure TPartitionTrimmer.TrimCurrentBit(IsCurrentClusterUnused: Boolean);
begin
  if IsCurrentClusterUnused then
    IncreaseOrSetTrimPosition;
  if IsTrimNeeded(IsCurrentClusterUnused) then
    ProcessAndClearPendingTrim;
end;

procedure TPartitionTrimmer.IfLastCardinalInBufferSetBitLength;
begin
  if CurrentPoint.CurrentCardinalInBuffer = CurrentPoint.CardinalLength then
    CurrentPoint.BitLengthOfCurrentCardinal :=
      CurrentPoint.BitLengthOfLastCardinal;
end;

procedure TPartitionTrimmer.TrimCurrentCardinal;
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

procedure TPartitionTrimmer.TrimNextPartOfPartition;
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

function TPartitionTrimmer.IsMoreTrimNeeded: Boolean;
begin
  result :=
    (VolumeBitmapBufferWithErrorCode.LastError = ERROR_MORE_DATA) or
    (VolumeBitmapBufferWithErrorCode.LastError = ERROR_INVALID_PARAMETER) or
    (VolumeBitmapBufferWithErrorCode.LastError = ERROR_SUCCESS);
end;

procedure TPartitionTrimmer.InitializeBitCardinalLength;
begin
  CurrentPoint.CardinalLength := Length(VolumeBitmapBufferWithErrorCode.Buffer);
  CurrentPoint.BitLengthOfLastCardinal := BitsPerCardinal;
end;
 
procedure TPartitionTrimmer.InitializeStartLBA;
begin
  CurrentPoint.PartitionStartLBA :=
    TrimBasicsToInitialize.StartLBA +
    TrimBasicsToInitialize.PaddingLBA;
end;

procedure TPartitionTrimmer.ProcessTrim;
begin
  while IsMoreTrimNeeded do
    TrimNextPartOfPartition;
end;

procedure TPartitionTrimmer.FreeClassesForTrim;
begin
  if AutoTrimBasicsGetter <> nil then
    FreeAndNil(AutoTrimBasicsGetter);
  if VolumeBitmapGetter <> nil then
    FreeAndNil(VolumeBitmapGetter);
  if AutoCommandSet <> nil then
    FreeAndNil(AutoCommandSet);
  if TrimThreadToModel <> nil then
    FreeAndNil(TrimThreadToModel);
end;

procedure TPartitionTrimmer.TryToTrimPartition(
  TrimSynchronizationToApply: TTrimSynchronization);
begin
  TrimSynchronization := TrimSynchronizationToApply;
  InitializeTrim;
  ProcessTrim;
end;

procedure TPartitionTrimmer.TrimPartition(
  TrimSynchronizationToApply: TTrimSynchronization);
begin
  try
    TryToTrimPartition(TrimSynchronizationToApply);
  finally
    FreeClassesForTrim;
  end;
end;

end.
