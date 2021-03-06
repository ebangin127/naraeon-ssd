unit Thread.Trim.Helper.List;

interface

uses
  SysUtils, Classes,
  TrimList, Thread.Trim.Helper.Partition, Thread.Trim.Helper.Partition.Direct,
  Thread.Trim.Helper.Partition.OS, ThreadToView.Trim, Component.ProgressSection,
  OS.Version.Helper, Getter.OS.Version, Getter.External;

type
  TTrimStage = (Initial, InProgress, Finished, Error);

  TListTrimmer = class
  private
    IsUIInteractionNeededReadWrite: Boolean;
    ProgressReadWrite: Integer;
    TrimStageReadWrite: TTrimStage;
    TrimThreadToView: TTrimThreadToView;
    PartitionsToTrim: TTrimList;
    ThreadToSynchronize: TThread;
    ProgressSynchronizer: TProgressSection;
    function IsFreeFromSynchronizeIssue: Boolean;
    procedure CheckSynchronizeIssue;
    procedure CheckIssueAndTrimAppliedPartitions;
    procedure TrimAppliedPartitions;
    procedure CheckNilPartitionListIssue;
    procedure TrimPartition(const PartitionPathToTrim: String);
    procedure CheckEntryAndTrimPartition(PartitionToTrim: TTrimListEntry);
    function GetTrimSynchronization: TTrimSynchronization;
    procedure IfNeedUICreateModelController;
    procedure IfNeedUIFreeModelController;
    procedure IfPartitionListExistsFreeAndNil;
    procedure TrimAppliedPartitionsWithProgressSection;
    function IsExternal(const PartitionPathToTrim: String): Boolean;
    function NeedDirectTrim(const PartitionPathToTrim: String): Boolean;
  public
    property IsUIInteractionNeeded:
      Boolean read IsUIInteractionNeededReadWrite;
    property Progress: Integer read ProgressReadWrite;
    property TrimStage: TTrimStage read TrimStageReadWrite;
    constructor Create(IsUIInteractionNeeded: Boolean);
    function SetPartitionList(PartitionsToSet: TTrimList): Boolean;
    procedure TrimAppliedPartitionsWithUI(ThreadToSynchronize: TThread);
    procedure TrimAppliedPartitionsWithoutUI;
  end;

implementation

{ TTrimmer }

function TListTrimmer.SetPartitionList(PartitionsToSet: TTrimList): Boolean;
begin
  result := PartitionsToSet <> nil;

  if not result then
    exit;

  PartitionsToTrim := PartitionsToSet;
end;

function TListTrimmer.IsFreeFromSynchronizeIssue:
  Boolean;
begin
  result :=
    (not IsUIInteractionNeeded) or
    (IsUIInteractionNeeded and (ThreadToSynchronize <> nil));
end;

procedure TListTrimmer.TrimAppliedPartitionsWithoutUI;
begin
  TrimAppliedPartitionsWithUI(nil);
end;

procedure TListTrimmer.CheckSynchronizeIssue;
begin
  if not IsFreeFromSynchronizeIssue then
    raise EInvalidOperation.Create
      ('Invalid Operation: Call with proper thread to synchronize');
end;

procedure TListTrimmer.CheckNilPartitionListIssue;
begin
  if PartitionsToTrim = nil then
    raise EArgumentNilException.Create
      ('Null Argument: Call with proper partition list to trim');
end;

procedure TListTrimmer.IfPartitionListExistsFreeAndNil;
begin
  if PartitionsToTrim <> nil then
    FreeAndNil(PartitionsToTrim);
end;


procedure TListTrimmer.TrimAppliedPartitionsWithProgressSection;
begin
  try
    ProgressSynchronizer.ShowProgress;
    CheckIssueAndTrimAppliedPartitions;
    IfPartitionListExistsFreeAndNil;
  except
    TrimStageReadWrite := TTrimStage.Error;
  end;
end;

procedure TListTrimmer.TrimAppliedPartitionsWithUI
  (ThreadToSynchronize: TThread);
begin
  self.ThreadToSynchronize := ThreadToSynchronize;
  ProgressSynchronizer := TProgressSection.Create(ThreadToSynchronize);
  try
    TrimAppliedPartitionsWithProgressSection;
  finally
    FreeAndNil(ProgressSynchronizer);
  end;
end;

procedure TListTrimmer.CheckIssueAndTrimAppliedPartitions;
begin
  CheckSynchronizeIssue;
  CheckNilPartitionListIssue;
  TrimStageReadWrite := TTrimStage.InProgress;
  TrimAppliedPartitions;
  TrimStageReadWrite := TTrimStage.Finished;
end;

procedure TListTrimmer.IfNeedUICreateModelController;
begin
  if ThreadToSynchronize <> nil then
    TrimThreadToView := TTrimThreadToView.Create(GetTrimSynchronization);
end;

procedure TListTrimmer.IfNeedUIFreeModelController;
begin
  if TrimThreadToView <> nil then
  begin
    TrimThreadToView.ApplyOriginalUI;
    ProgressSynchronizer.HideProgress;
    FreeAndNil(TrimThreadToView);
  end;
end;

procedure TListTrimmer.TrimAppliedPartitions;
var
  CurrentPartition: Integer;
begin
  IfNeedUICreateModelController;
  for CurrentPartition := 0 to PartitionsToTrim.Count - 1 do //FI:W528
    CheckEntryAndTrimPartition(PartitionsToTrim.GetNextPartition);
  IfNeedUIFreeModelController;
end;

constructor TListTrimmer.Create(IsUIInteractionNeeded: Boolean);
begin
  IsUIInteractionNeededReadWrite := IsUIInteractionNeeded;
end;

procedure TListTrimmer.CheckEntryAndTrimPartition(
  PartitionToTrim: TTrimListEntry);
begin
  if PartitionToTrim.Status =
     TNextPartitionStatus.CompleteLastPartitionFirst then
    exit;

  TrimPartition(PartitionToTrim.PartitionPath);
  PartitionsToTrim.CompleteCurrentPartition;
end;

function TListTrimmer.GetTrimSynchronization: TTrimSynchronization;
begin
  result.IsUIInteractionNeeded := IsUIInteractionNeededReadWrite;
  result.ThreadToSynchronize := ThreadToSynchronize;
  result.Progress.CurrentPartition := PartitionsToTrim.CurrentPartition;
  result.Progress.PartitionCount := PartitionsToTrim.Count;
end;

function TListTrimmer.IsExternal(const PartitionPathToTrim: String): Boolean;
var
  ExternalGetter: TExternalGetter;
begin
  ExternalGetter := TExternalGetter.Create;
  try
    result := ExternalGetter.IsExternal(PartitionPathToTrim);
  finally
    FreeAndNil(ExternalGetter);
  end;
end;

function TListTrimmer.NeedDirectTrim(const PartitionPathToTrim: String):
  Boolean;
begin
  result :=
    IsBelowWindows8(VersionHelper.Version) or IsExternal(PartitionPathToTrim);
end;

procedure TListTrimmer.TrimPartition(const PartitionPathToTrim: String);
var
  PartitionTrimmer: TPartitionTrimmer;
  TrimSynchronization: TTrimSynchronization;
begin
  if NeedDirectTrim(PartitionPathToTrim) then
    PartitionTrimmer := TDirectPartitionTrimmer.Create(PartitionPathToTrim)
  else
    PartitionTrimmer := TOSPartitionTrimmer.Create(PartitionPathToTrim);
  TrimSynchronization := GetTrimSynchronization;
  PartitionTrimmer.TrimPartition(TrimSynchronization);
  FreeAndNil(PartitionTrimmer);
end;

end.
