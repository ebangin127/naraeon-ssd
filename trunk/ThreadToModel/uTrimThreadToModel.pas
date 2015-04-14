unit uTrimThreadToModel;

interface

uses
  SysUtils, Classes, Windows,
  uLanguageSettings;

type
  TTrimProgress = record
    CurrentPartition: Integer;
    PartitionCount: Integer;
  end;

  TTrimSynchronization = record
    IsUIInteractionNeeded: Boolean;
    ThreadToSynchronize: TThread;
    Progress: TTrimProgress;
  end;

  TTrimThreadToModel = class
  public
    constructor Create(TrimSynchronizationToApply: TTrimSynchronization);
    procedure ApplyOriginalUI(ProgressToApply: Integer);
    procedure ApplyProgressToUI(ProgressToApply: Integer);
    procedure ApplyStageToUI(ProgressToApply: Integer);
  private
    Progress: Integer;
    TrimSynchronization: TTrimSynchronization;
    procedure SynchronizedApplyOriginalUI;
    procedure SynchronizedApplyProgressToUI;
    procedure SynchronizedApplyStageToUI;
    function IsTrimInProgress: Boolean;
    procedure SynchronizedApplyProgressToLabel;
  end;

implementation

uses
  uMain;

constructor TTrimThreadToModel.Create(
  TrimSynchronizationToApply: TTrimSynchronization);
begin
  TrimSynchronization := TrimSynchronizationToApply;
end;

procedure TTrimThreadToModel.ApplyProgressToUI(ProgressToApply: Integer);
begin
  if TrimSynchronization.IsUIInteractionNeeded then
  begin
    Progress := ProgressToApply;
    TThread.Synchronize(
      TrimSynchronization.ThreadToSynchronize,
      SynchronizedApplyProgressToUI);
  end;
end;

procedure TTrimThreadToModel.ApplyStageToUI(ProgressToApply: Integer);
begin
  if TrimSynchronization.IsUIInteractionNeeded then
  begin
    Progress := ProgressToApply;
    TThread.Synchronize(
      TrimSynchronization.ThreadToSynchronize,
      SynchronizedApplyStageToUI);
  end;
end;

procedure TTrimThreadToModel.ApplyOriginalUI(ProgressToApply: Integer);
begin
  if TrimSynchronization.IsUIInteractionNeeded then
  begin
    Progress := ProgressToApply;
    TThread.Synchronize(
      TrimSynchronization.ThreadToSynchronize,
      SynchronizedApplyOriginalUI);
  end;
end;

procedure TTrimThreadToModel.SynchronizedApplyOriginalUI;
begin
  fMain.pDownload.Position := Progress;
end;

function TTrimThreadToModel.IsTrimInProgress: Boolean;
begin
  result := TrimSynchronization.Progress.CurrentPartition <
    TrimSynchronization.Progress.PartitionCount;
end;

procedure TTrimThreadToModel.SynchronizedApplyProgressToLabel;
begin
  fMain.lProgress.Caption :=
    CapProg1[CurrLang] +
    IntToStr(TrimSynchronization.Progress.CurrentPartition + 1) + '/' +
    IntToStr(TrimSynchronization.Progress.PartitionCount) + ')';
end;

procedure TTrimThreadToModel.SynchronizedApplyProgressToUI;
begin
  SynchronizedApplyOriginalUI;

  if TrimSynchronization.Progress.CurrentPartition < 0 then
    exit;

  if IsTrimInProgress then
    SynchronizedApplyProgressToLabel;
end;

procedure TTrimThreadToModel.SynchronizedApplyStageToUI;
begin
  fMain.pDownload.Height := fMain.pDownload.Height - 10;
  fMain.pDownload.Top := fMain.pDownload.Top - 5;
  fMain.pDownload.Position := 0;
  fMain.gTrim.Visible := true;
  fMain.HideProgress;
end;

end.
