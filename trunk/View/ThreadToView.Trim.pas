unit ThreadToView.Trim;

interface

uses
  SysUtils, Classes, Windows,
  Global.LanguageString;

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

  TTrimThreadToView = class
  public
    constructor Create(TrimSynchronizationToApply: TTrimSynchronization);
    procedure ApplyOriginalUI;
    procedure ApplyProgressToUI(ProgressToApply: Integer);
    procedure ApplyNextDriveStartToUI(ProgressToApply: Integer);
  private
    Progress: Integer;
    TrimSynchronization: TTrimSynchronization;
    procedure SynchronizedApplyProgressToUI;
    procedure SynchronizedApplyOriginalUI;
    procedure SynchronizedApplyNextDriveStartToUI;
    function IsTrimInProgress: Boolean;
    procedure SynchronizedApplyProgressToLabel;
  end;

implementation

uses
  Form.Main;

constructor TTrimThreadToView.Create(
  TrimSynchronizationToApply: TTrimSynchronization);
begin
  TrimSynchronization := TrimSynchronizationToApply;
end;

procedure TTrimThreadToView.ApplyProgressToUI(ProgressToApply: Integer);
begin
  if TrimSynchronization.IsUIInteractionNeeded then
  begin
    Progress := ProgressToApply;
    TThread.Queue(
      TrimSynchronization.ThreadToSynchronize,
      SynchronizedApplyProgressToUI);
  end;
end;

procedure TTrimThreadToView.ApplyNextDriveStartToUI(ProgressToApply: Integer);
begin
  if TrimSynchronization.IsUIInteractionNeeded then
  begin
    Progress := ProgressToApply;
    TThread.Queue(
      TrimSynchronization.ThreadToSynchronize,
      SynchronizedApplyNextDriveStartToUI);
  end;
end;

procedure TTrimThreadToView.ApplyOriginalUI;
begin
  if TrimSynchronization.IsUIInteractionNeeded then
  begin
    TThread.Synchronize(
      TrimSynchronization.ThreadToSynchronize,
      SynchronizedApplyOriginalUI);
  end;
end;

procedure TTrimThreadToView.SynchronizedApplyProgressToUI;
begin
  fMain.pDownload.Position := Progress;
end;

function TTrimThreadToView.IsTrimInProgress: Boolean;
begin
  result := TrimSynchronization.Progress.CurrentPartition <=
    TrimSynchronization.Progress.PartitionCount;
end;

procedure TTrimThreadToView.SynchronizedApplyProgressToLabel;
begin
  fMain.lProgress.Caption :=
    CapProg1[CurrLang] +
    IntToStr(TrimSynchronization.Progress.CurrentPartition) + ' / ' +
    IntToStr(TrimSynchronization.Progress.PartitionCount);
end;

procedure TTrimThreadToView.SynchronizedApplyOriginalUI;
begin
  fMain.pDownload.Position := 0;
  fMain.gTrim.Visible := true;
end;

procedure TTrimThreadToView.SynchronizedApplyNextDriveStartToUI;
begin
  SynchronizedApplyProgressToUI;

  if TrimSynchronization.Progress.CurrentPartition < 0 then
    exit;

  if IsTrimInProgress then
    SynchronizedApplyProgressToLabel;
end;

end.
