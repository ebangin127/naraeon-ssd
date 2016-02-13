unit Component.ProgressSection;

interface

uses
  Classes,
  Component.ButtonGroup;

type
  TProgressToApply = record
    ProgressValue: Integer;
    ProgressCaption: String;
    SpeedCaption: String;
  end;

  TProgressSection = class
  private
    ThreadToSynchronize: TThread;
    ProgressToApply: TProgressToApply;
    procedure SetEnabledPropertyTo(Enabled: Boolean;
      ButtonGroupEntry: TButtonGroupEntry);
    procedure SynchronizedApplyProgress;
    procedure SynchronizedHideProgress;
    procedure SynchronizedShowProgress;
  public
    constructor Create(ThreadToSynchronize: TThread);
    procedure HideProgress;
    procedure ShowProgress;
    procedure ChangeProgress(ProgressToApply: TProgressToApply);
  end;

implementation

uses
  Form.Main;

procedure TProgressSection.SetEnabledPropertyTo(Enabled: Boolean;
  ButtonGroupEntry: TButtonGroupEntry);
begin
  ButtonGroupEntry.ImageButton.Enabled := Enabled;
  ButtonGroupEntry.LabelButton.Enabled := Enabled;
end;
  
procedure TProgressSection.SynchronizedShowProgress;
var
  CurrentButtonGroupEntry: TButtonGroupEntry;
begin
  fMain.DisableSSDLabel;

  for CurrentButtonGroupEntry in fMain.GetButtonGroup do
    SetEnabledPropertyTo(false, CurrentButtonGroupEntry);
  fMain.iHelp.Enabled := false;
  fMain.lHelp.Enabled := false;

  fMain.gDownload.Visible := true;
  fMain.OpenButtonGroup;
end;

procedure TProgressSection.HideProgress;
begin
  if ThreadToSynchronize <> nil then
    TThread.Synchronize(ThreadToSynchronize, SynchronizedHideProgress)
  else
    SynchronizedHideProgress;
end;

procedure TProgressSection.ShowProgress;
begin
  if ThreadToSynchronize <> nil then
    TThread.Synchronize(ThreadToSynchronize, SynchronizedShowProgress)
  else
    SynchronizedShowProgress;
end;

procedure TProgressSection.ChangeProgress(ProgressToApply: TProgressToApply);
begin
  self.ProgressToApply := ProgressToApply;
  if ThreadToSynchronize <> nil then
    TThread.Queue(ThreadToSynchronize, SynchronizedApplyProgress)
  else
    SynchronizedApplyProgress;
end;

constructor TProgressSection.Create(ThreadToSynchronize: TThread);
begin
  self.ThreadToSynchronize := ThreadToSynchronize;
end;

procedure TProgressSection.SynchronizedApplyProgress;
begin
  fMain.pDownload.Position := ProgressToApply.ProgressValue;
  fMain.lProgress.Caption := ProgressToApply.ProgressCaption;
  fMain.lSpeed.Caption := ProgressToApply.SpeedCaption;
end;

procedure TProgressSection.SynchronizedHideProgress;
var
  CurrentButtonGroupEntry: TButtonGroupEntry;
begin
  fMain.EnableSSDLabel;
  for CurrentButtonGroupEntry in fMain.GetButtonGroup do
    SetEnabledPropertyTo(true, CurrentButtonGroupEntry);
  fMain.iHelp.Enabled := true;
  fMain.lHelp.Enabled := true;
  fMain.gDownload.Visible := false;
end;
end.
