unit uMainformReplacedSectorApplier;

interface

uses
  uPhysicalDrive, uListChangeGetter;

type
  TMainformReplacedSectorApplier = class
  private
    ReplacedSectorLog: TNSTLog;
    ReplacedSectors: UInt64;
  public
    procedure ApplyMainformReplacedSector;
  end;

implementation

uses uMain;

procedure TMainformReplacedSectorApplier.SetReplacedSectors;
begin
  ReplacedSectors := fMain.PhysicalDrive.SMARTInterpreted.ReplacedSectors;
  fMain.lSectors.Caption := CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
    CapCount[CurrLang];
end;

procedure TMainformReplacedSectorApplier.CreateReplacedSectorLog;
begin
  ReplacedSectorLog := TNSTLog.Create(
    TPathManager.AppPath, PhysicalDrive.IdentifyDeviceResult.Serial +
    'RSLog', UIntToStr(ReplacedSectors), true);
end;

procedure TMainformReplacedSectorApplier.FreeReplacedSectorLog;
begin
  if ReplacedSectorLog <> nil then
    FreeAndNil(ReplacedSectorLog);
end;

function TMainformReplacedSectorApplier.IsTotalWriteNotSupported: Boolean;
begin
  result :=
    fMain.PhysicalDrive.SupportStatus.TotalWriteType =
    TTotalWriteType.WriteNotSupported;
end;

function TMainformReplacedSectorApplier.GetAvailableAverageType: Integer;
var
  CurrentAverageType: Integer;
begin
  result := -1;
  for CurrentAverageType := AvgMax downto 0 do
    if Length(WriteLog.Average[IntToAvg[CurrentAverageType]]) > 0 then
      exit(CurrentAverageType);
end;

procedure TMainformReplacedSectorApplier.ApplyUsageByLog(WriteLog: TNSTLog;
  AvailableAverageType: Integer);
begin
  if Length(ReplacedSectorLog.Average[IntToAvg[AvailableAverageType]]) > 0 then
    fMain.l1Month.Caption :=
      CapAvg[AvailableAverageType][CurrLang] +
      ReplacedSectorLog.Average[IntToAvg[AvailableAverageType]] +
      CapCount[CurrLang] +
      CapDay[CurrLang];
end;

procedure TMainformReplacedSectorApplier.SetUsageLabelAsUnknown;
begin
  fMain.l1Month.Caption :=
    CapAvg[0][CurrLang]+ CapCount[CurrLang] + CapDay[CurrLang];
end;

procedure TMainformReplacedSectorApplier.ApplyTodayUsageByLog;
begin
  fMain.lTodayUsage.Caption := CapToday[CurrLang] + ReplacedSectorLog.TodayUsage +
    CapCount[CurrLang];
end;

procedure TMainformReplacedSectorApplier.ApplyUsageByLog;
var
  AvailableAverageType: Integer;
begin
  AvailableAverageType := GetAvailableAverageType;
  if AvailableAverageType >= 0 then
    SetUsageLabelByLogAndAvailableType(ReplacedSectorLog, AvailableAverageType)
  else
    SetUsageLabelAsUnknown;
  ApplyTodayUsageByLog;
end;

procedure TMainformReplacedSectorApplier.ApplyReplacedSectorsAsTotalWrite;
begin
  SetAnalyticsLabelAsLifeAnalysis;
  SetHostWriteLabelAsReplacedSectors;
  ApplyUsageByLog;
end;

procedure TMainformReplacedSectorApplier.SetHostWriteLabelAsReplacedSectors;
begin
  fMain.lHost.Caption :=
    CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
    CapCount[CurrLang];
begin;

procedure TMainformReplacedSectorApplier.SetAnalyticsLabelAsLifeAnalysis;
begin
  fMain.lAnalytics.Caption := BtLifeAnaly[CurrLang];
  fMain.lAnaly.Caption := CapLifeAnaly[CurrLang];
begin;

procedure TMainformReplacedSectorApplier.RecoverAnalyticsLabel;
begin
  fMain.lAnalytics.Caption := BtAnaly[CurrLang];
  fMain.lAnaly.Caption := CapAnaly[CurrLang];
begin;

procedure TMainformReplacedSectorApplier.ApplyMainformReplacedSector;
begin
  SetReplacedSectors;
  CreateReplacedSectorLog;
  if IsTotalWriteNotSupported then
    ApplyReplacedSectorsAsTotalWrite
  else
    RecoverAnalyticsLabel;
  FreeReplacedSectorLog;
  
end;

end.