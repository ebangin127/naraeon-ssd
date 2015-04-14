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
        ByteLength, BitLengthOfLastByte: Integer;
        BitLengthOfCurrentByte: Integer;
        CurrentByteInBuffer, CurrentBitInByte: Integer;
      end;

      TPendingTrimOperation = record
        IsUnusedSpaceFound: Boolean;
        StartLBA: UInt64;
        LengthInLBA: UInt64;
      end;
  private
    TrimSynchronization: TTrimSynchronization;
    AutoTrimBasicsGetter: TAutoTrimBasicsGetter;
    VolumeBitmapGetter: TVolumeBitmapGetter;
    VolumeBitmapBuffer: TVolumeBitmapBuffer;
    CurrentPartitionTrimProgress: TCurrentPartitionTrimProgress;
    TrimBasicsToInitialize: TTrimBasicsToInitialize;
    PendingTrimOperation: TPendingTrimOperation;
    CurrentPoint: TCurrentPoint;
    AutoCommandSet: TAutoCommandSet;
    TrimThreadToModel: TTrimThreadToModel;
    LastError: Integer;
    LastTick: Cardinal;
    procedure InitializeTrim;
    procedure ProcessTrim;
    function IsMoreTrimNeeded: Boolean;
    procedure TrimNextPartOfPartition;
    procedure SetVolumeBitmapAndSetLastError(StartingLCN: LARGE_INTEGER);
    procedure SetNextPartPosition;
    function IsMorePartLeftSetVolumeBitmapBuffer: Boolean;
    procedure IfLastPartSetByteBitLength;
    function GetBitLengthOfLastByte(BitmapSizeInBit: LARGE_INTEGER): Integer;
    procedure InitializeBitByteLength;
    procedure InitializeStartLBA;
    procedure CalculateProgress;
    procedure SetBaseProgress;
    procedure TrimCurrentByte;
    procedure TrimCurrentBit(IsCurrentClusterUnused: Boolean);
    procedure IncreaseOrSetTrimPosition;
    procedure SetThisPositionAsStart;
    function GetCurrentPositionInLBA: UInt64;
    function IsTrimNeeded(IsCurrentClusterUnused: Boolean): Boolean;
    function FoundUsedSpaceOrLastPart(IsCurrentClusterUnused: Boolean): Boolean;
    function IsLastPartOfByte: Boolean;
    function IsLBACountOverLimit: Boolean;
    procedure ProcessAndClearPendingTrim;
    procedure ClearPendingTrim;
    procedure ProcessPendingTrim;
    procedure InitializeCommandSet;
    procedure FreeClassesForTrim;
    procedure IfNeedToRestApplyToUI;

    const
      BitsPerByte = 8;
      VolumeBitmapBufferSizeInBit =
        SizeOf(TBitmapBuffer) * BitsPerByte;

  public
    destructor Destroy; override;
    procedure TrimPartition(TrimSynchronizationToApply: TTrimSynchronization);
  end;

implementation

{ TPartitionTrimmer }

destructor TPartitionTrimmer.Destroy;
begin
  if AutoTrimBasicsGetter <> nil then
    FreeAndNil(AutoTrimBasicsGetter);
  inherited;
end;

procedure TPartitionTrimmer.SetVolumeBitmapAndSetLastError
  (StartingLCN: LARGE_INTEGER);
begin
  LastError := ERROR_SUCCESS;
  try
    VolumeBitmapBuffer := VolumeBitmapGetter.GetVolumeBitmap(StartingLCN);
  except
    on E: EOSError do
      LastError := E.ErrorCode;
  end;
end;

procedure TPartitionTrimmer.InitializeCommandSet;
var
  PartitionExtentGetter: TPartitionExtentGetter;
  PartitionExtentList: TPartitionExtentList;
  PhysicalDriveName: String;
begin
  PartitionExtentGetter :=
    TPartitionExtentGetter.Create(GetPathOfFileAccessing);
  PartitionExtentList := PartitionExtentGetter.GetPartitionExtentList;
  PhysicalDriveName :=
    ThisComputerPrefix +
    PhysicalDrivePrefix +
    IntToStr(PartitionExtentList[0].DriveNumber);
  AutoCommandSet := TAutoCommandSet.Create(PhysicalDriveName);
  FreeAndNil(PartitionExtentGetter);
  FreeAndNil(PartitionExtentList);
end;

procedure TPartitionTrimmer.InitializeTrim;
begin
  VolumeBitmapGetter := TVolumeBitmapGetter.Create(GetPathOfFileAccessing);
  TrimThreadToModel := TTrimThreadToModel.Create(TrimSynchronization);
  AutoTrimBasicsGetter := TAutoTrimBasicsGetter.Create(GetPathOfFileAccessing);
  if not AutoTrimBasicsGetter.IsPartitionMyResponsibility then
    raise EUnknownPartition.Create
      ('Unknown Partiton: Use with known partition');

  CurrentPoint.OffsetInCluster.QuadPart := 0;
  TrimBasicsToInitialize := AutoTrimBasicsGetter.GetTrimBasicsToInitialize;
  SetVolumeBitmapAndSetLastError(CurrentPoint.OffsetInCluster);
  InitializeBitByteLength;
  InitializeStartLBA;
  InitializeCommandSet;
  CalculateProgress;
  TrimThreadToModel.ApplyNextDriveStartToUI(
    CurrentPartitionTrimProgress.Progress);
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
      TrimSynchronization.Progress.CurrentPartition);
end;

procedure TPartitionTrimmer.CalculateProgress;
begin
  SetBaseProgress;
  CurrentPartitionTrimProgress.Progress :=
    CurrentPartitionTrimProgress.BaseProgress +
    round(CurrentPartitionTrimProgress.ProgressPerPartition *
      (VolumeBitmapBuffer.BitmapSize.QuadPart /
       VolumeBitmapBuffer.StartingLCN.QuadPart));
end;

procedure TPartitionTrimmer.SetNextPartPosition;
begin
  CurrentPoint.OffsetInCluster.QuadPart :=
    VolumeBitmapBuffer.StartingLCN.QuadPart +
    (Length(VolumeBitmapBuffer.Buffer) shl 3);
end;

function TPartitionTrimmer.IsMorePartLeftSetVolumeBitmapBuffer: Boolean;
begin
  result := LastError = ERROR_MORE_DATA;
  if result then
  begin
    SetNextPartPosition;
    SetVolumeBitmapAndSetLastError(CurrentPoint.OffsetInCluster);
  end;
end;

function TPartitionTrimmer.GetBitLengthOfLastByte
  (BitmapSizeInBit: LARGE_INTEGER): Integer;
begin
  result := BitmapSizeInBit.QuadPart and (BitsPerByte - 1);
end;

procedure TPartitionTrimmer.IfLastPartSetByteBitLength;
begin
  if VolumeBitmapBuffer.BitmapSize.QuadPart < VolumeBitmapBufferSizeInBit then
  begin
    CurrentPoint.ByteLength :=
      ceil(VolumeBitmapBuffer.BitmapSize.QuadPart / BitsPerByte);
    CurrentPoint.BitLengthOfLastByte :=
      GetBitLengthOfLastByte(VolumeBitmapBuffer.BitmapSize);
  end;
end;

function TPartitionTrimmer.GetCurrentPositionInLBA: UInt64;
begin
  result := VolumeBitmapBuffer.StartingLCN.QuadPart;
  result := result + CurrentPoint.CurrentByteInBuffer shl 3;
  result := result + CurrentPoint.CurrentBitInByte;
  result := result * TrimBasicsToInitialize.LBAPerCluster;
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

function TPartitionTrimmer.IsLastPartOfByte: Boolean;
begin
  result :=
    (CurrentPoint.CurrentByteInBuffer = CurrentPoint.ByteLength) and
    (CurrentPoint.CurrentBitInByte = CurrentPoint.BitLengthOfCurrentByte);
end;

function TPartitionTrimmer.FoundUsedSpaceOrLastPart(
  IsCurrentClusterUnused: Boolean): Boolean;
begin
  result :=
    (not IsCurrentClusterUnused) or
    IsLBACountOverLimit or
    IsLastPartOfByte;
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
  CurrentTick: Int64;
begin
  CurrentTick := GetTickCount;
  CurrentTick := LastTick - CurrentTick;
  if (CurrentTick < Overflow) or (CurrentTick > Threshold) then
  begin
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

procedure TPartitionTrimmer.TrimCurrentByte;
var
  CurrentByte: Integer;
  CurrentBitInByte: Integer;
begin
  if CurrentPoint.CurrentByteInBuffer = CurrentPoint.ByteLength then
    CurrentPoint.BitLengthOfCurrentByte := CurrentPoint.BitLengthOfLastByte;

  CurrentByte := VolumeBitmapBuffer.Buffer[CurrentPoint.CurrentByteInBuffer];
  for CurrentBitInByte := 0 to CurrentPoint.BitLengthOfCurrentByte - 1 do
  begin
    CurrentPoint.CurrentBitInByte := CurrentBitInByte;
    TrimCurrentBit((CurrentByte and 1) = 0);
    CurrentByte := CurrentByte shr 1;
  end;
end;

procedure TPartitionTrimmer.TrimNextPartOfPartition;
var
  CurrentByteInBuffer: Integer;
begin
  IfLastPartSetByteBitLength;

  CurrentPoint.BitLengthOfCurrentByte := BitsPerByte;
  for CurrentByteInBuffer := 0 to CurrentPoint.ByteLength - 1 do
  begin
    CurrentPoint.CurrentByteInBuffer := CurrentByteInBuffer;
    TrimCurrentByte;
  end;

  if not IsMorePartLeftSetVolumeBitmapBuffer then
    LastError := ERROR_NO_MORE_ITEMS;
end;

function TPartitionTrimmer.IsMoreTrimNeeded: Boolean;
begin
  result :=
    (LastError = ERROR_MORE_DATA) or
    (LastError = ERROR_INVALID_PARAMETER) or
    (LastError = ERROR_SUCCESS);
end;

procedure TPartitionTrimmer.InitializeBitByteLength;
begin
  CurrentPoint.ByteLength := SizeOf(VolumeBitmapBuffer.Buffer);
  CurrentPoint.BitLengthOfLastByte := BitsPerByte;
end;

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

procedure TPartitionTrimmer.TrimPartition(
  TrimSynchronizationToApply: TTrimSynchronization);
begin
  try
    TrimSynchronization := TrimSynchronizationToApply;
    InitializeTrim;
    ProcessTrim;
  finally
    FreeClassesForTrim;
  end;
end;

end.
