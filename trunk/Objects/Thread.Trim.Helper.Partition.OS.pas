unit Thread.Trim.Helper.Partition.OS;

interface

uses
  SysUtils, Character,
  Thread.Trim.Helper.Partition, OS.ProcessOpener
  {$IfNDef UNITTEST}, OS.EnvironmentVariable, ThreadToView.Trim
  {$Else}, Mock.Getter.TrimBasics.Factory{$EndIf};

type
  TOSPartitionTrimmer = class abstract(TPartitionTrimmer)
  private
    {$IfNDef UNITTEST}
    TrimThreadToView: TTrimThreadToView;
    {$EndIf}
    CurrentProgress: Integer;
    ProcessBufferFunction: TProcessBuffer;
    TrimSynchronization: TTrimSynchronization;
    CurrentPartitionTrimProgress: TCurrentPartitionTrimProgress;
    procedure InitializeTrim;
    procedure ProcessTrim;
    procedure CalculateProgress(const ProgressString: String);
    procedure TryToTrimPartition(
      const TrimSynchronizationToApply: TTrimSynchronization);
    procedure FreeClassesForTrim;
    procedure SetBaseProgress;
    procedure ApplyProgress(const ProgressInString: String);
    function FindPercentPosition(const OptimizerOutput: String): Integer;
    function GetProgressInString(const OptimizerOutput: String;
      const PercentPosition: Integer): String;
  {$IfDef UNITTEST}
  public
    property Progress: Integer read CurrentProgress;
  {$EndIf}
    procedure ParseProgress(const OptimizerOutput: String);
    procedure SetBaseProgressToStart;
  public
    procedure TrimPartition(
      const TrimSynchronizationToApply: TTrimSynchronization);
      override;
  end;

implementation

{ TOSPartitionTrimmer }

procedure TOSPartitionTrimmer.TryToTrimPartition(
  const TrimSynchronizationToApply: TTrimSynchronization);
begin
  TrimSynchronization := TrimSynchronizationToApply;
  InitializeTrim;
  ProcessTrim;
end;

procedure TOSPartitionTrimmer.TrimPartition(
  const TrimSynchronizationToApply: TTrimSynchronization);
begin
  try
    TryToTrimPartition(TrimSynchronizationToApply);
  finally
    FreeClassesForTrim;
  end;
end;

procedure TOSPartitionTrimmer.SetBaseProgressToStart;
begin
  SetBaseProgress;
  CalculateProgress('0%');
end;

procedure TOSPartitionTrimmer.ApplyProgress(const ProgressInString: String);
begin
  if ProgressInString <> '' then
    CurrentProgress := StrToInt(ProgressInString)
  else
    CurrentProgress := 0;
end;

procedure TOSPartitionTrimmer.FreeClassesForTrim;
begin
  {$IfNDef UNITTEST}
  FreeAndNil(TrimThreadToView);
  {$EndIf}
end;

procedure TOSPartitionTrimmer.ProcessTrim;
begin
  {$IfNDef UNITTEST}
  ProcessOpener.OpenProcWithProcessFunction(
    EnvironmentVariable.WinDrive,
    EnvironmentVariable.WinDir + '\Sysnative\defrag.exe /O /U ' +
      GetPathOfFileAccessingWithoutPrefix,
    ProcessBufferFunction);
  {$EndIf}
end;

function TOSPartitionTrimmer.FindPercentPosition(const OptimizerOutput: String):
  Integer;
const
  PercentString = '%';
var
  CurrentPosition: Integer;
begin
  result := 0;
  for CurrentPosition := Length(OptimizerOutput) downto 1 do
    if OptimizerOutput[CurrentPosition] = PercentString then
      exit(CurrentPosition);
end;

function TOSPartitionTrimmer.GetProgressInString(const OptimizerOutput: String;
  const PercentPosition: Integer): String;
const
  PercentString = '%';
var
  CurrentPosition: Integer;
begin
  result := '';
  for CurrentPosition := PercentPosition - 1 downto 1 do
    if OptimizerOutput[CurrentPosition].IsDigit then
      result := OptimizerOutput[CurrentPosition] + result
    else
      exit;
end;

procedure TOSPartitionTrimmer.ParseProgress(const OptimizerOutput: String);
var
  PercentPosition: Integer;
  ProgressInString: String;
begin
  PercentPosition := FindPercentPosition(OptimizerOutput);
  ProgressInString := GetProgressInString(OptimizerOutput, PercentPosition);
  ApplyProgress(ProgressInString);
end;

procedure TOSPartitionTrimmer.CalculateProgress(const ProgressString: String);
begin
  SetBaseProgress;
  ParseProgress(ProgressString);
  CurrentPartitionTrimProgress.Progress :=
    CurrentPartitionTrimProgress.BaseProgress +
    round(CurrentPartitionTrimProgress.ProgressPerPartition *
      CurrentProgress / 100);
end;

procedure TOSPartitionTrimmer.SetBaseProgress;
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

procedure TOSPartitionTrimmer.InitializeTrim;
begin
  SetBaseProgressToStart;
  {$IfNDef UNITTEST}
  TrimThreadToView := TTrimThreadToView.Create(TrimSynchronization);
  TrimThreadToView.ApplyNextDriveStartToUI(
    CurrentPartitionTrimProgress.Progress);
  {$EndIf}
  ProcessBufferFunction :=
    procedure (const CurrentBuffer: string; var CurrentResult: AnsiString)
    begin
      CalculateProgress(CurrentBuffer);
      {$IfNDef UNITTEST}
      TrimThreadToView.ApplyProgressToUI(CurrentPartitionTrimProgress.Progress);
      {$EndIf}
    end;
end;

end.
