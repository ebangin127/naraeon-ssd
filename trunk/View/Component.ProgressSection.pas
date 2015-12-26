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
  CurrentSSDLabel: Integer;
  CurrentButtonGroupEntry: Integer;
begin
  for CurrentSSDLabel := 0 to fMain.SSDLabel.Count - 1 do
    fMain.SSDLabel[CurrentSSDLabel].Enabled := false;

  for CurrentButtonGroupEntry := 0 to fMain.ButtonGroup.Count - 1 do
    SetEnabledPropertyTo(false, fMain.ButtonGroup[CurrentButtonGroupEntry]);
  fMain.iHelp.Enabled := false;
  fMain.lHelp.Enabled := false;

  fMain.gDownload.Visible := true;
  fMain.ButtonGroup.Open;
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
    TThread.Synchronize(ThreadToSynchronize, SynchronizedApplyProgress)
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
  CurrentSSDLabel: Integer;
  CurrentButtonGroupEntry: Integer;
begin
  for CurrentSSDLabel := 0 to fMain.SSDLabel.Count - 1 do
    fMain.SSDLabel[CurrentSSDLabel].Enabled := true;

  for CurrentButtonGroupEntry := 0 to fMain.ButtonGroup.Count - 1 do
    SetEnabledPropertyTo(true, fMain.ButtonGroup[CurrentButtonGroupEntry]);
  fMain.iHelp.Enabled := true;
  fMain.lHelp.Enabled := true;
    
  fMain.gDownload.Visible := false;
end;
end.
