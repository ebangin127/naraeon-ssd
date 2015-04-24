unit uProgressSection;

interface

uses
  uButtonGroup;

type
  TProgressSection = class
  private
    procedure SetEnabledPropertyTo(Enabled: Boolean;
      ButtonGroupEntry: TButtonGroupEntry);
      
  public
    procedure HideProgress;
    procedure ShowProgress;
    procedure ChangeProgress(ProgressValue: Integer; ProgressString,
      SpeedString: String);
  end;

implementation

uses
  uMain;

procedure TProgressSection.SetEnabledPropertyTo(Enabled: Boolean;
  ButtonGroupEntry: TButtonGroupEntry);
begin
  ButtonGroupEntry.ImageButton.Enabled := Enabled;
  ButtonGroupEntry.LabelButton.Enabled := Enabled;
end;
  
procedure TProgressSection.ShowProgress;
var
  CurrentSSDLabel: Integer;
  CurrentButtonGroupEntry: Integer;
begin
  for CurrentSSDLabel := 0 to fMain.SSDLabel.Count - 1 do
    fMain.SSDLabel[CurrentSSDLabel].Enabled := false;

  for CurrentButtonGroupEntry := 0 to fMain.ButtonGroup.Count - 1 do
    SetEnabledPropertyTo(false, fMain.ButtonGroup[CurrentButtonGroupEntry]);
    
  fMain.gDownload.Visible := true;
  fMain.ButtonGroup.Open;
end;

procedure TProgressSection.ChangeProgress(ProgressValue: Integer;
  ProgressString, SpeedString: String);
begin
  fMain.pDownload.Position := ProgressValue;
  fMain.lProgress.Caption := ProgressString;
  fMain.lSpeed.Caption := SpeedString;
end;

procedure TProgressSection.HideProgress;
var
  CurrentSSDLabel: Integer;
  CurrentButtonGroupEntry: Integer;
begin
  for CurrentSSDLabel := 0 to fMain.SSDLabel.Count - 1 do
    fMain.SSDLabel[CurrentSSDLabel].Enabled := true;

  for CurrentButtonGroupEntry := 0 to fMain.ButtonGroup.Count - 1 do
    SetEnabledPropertyTo(true, fMain.ButtonGroup[CurrentButtonGroupEntry]);
    
  fMain.gDownload.Visible := false;
end;
end.
