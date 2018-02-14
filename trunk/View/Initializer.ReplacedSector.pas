unit Initializer.ReplacedSector;

interface

uses
  SysUtils,
  OS.EnvironmentVariable, Global.LanguageString, Device.PhysicalDrive,
  AverageLogger.Count, AverageLogger,
  Support;

type
  TMainformReplacedSectorApplier = class
  private
    ReplacedSectorLog: TAverageCountLogger;
    ReplacedSectors: UInt64;
    procedure ApplyReplacedSectorsAsTotalWrite;
    procedure ApplyTodayUsageByLog;
    procedure SetUsageLabelByLogAndAvailableType;
    procedure CreateReplacedSectorLog;
    procedure FreeReplacedSectorLog;
    function IsTotalWriteNotSupported: Boolean;
    procedure SetHostWriteLabelAsReplacedSectors;
    procedure SetReplacedSectors;
    procedure RecoverAnalyticsLabel;
    procedure SetAnalyticsLabelAsLifeAnalysis;
    procedure ApplyUsageByLog;
    procedure ApplyReplacedSector;
    procedure RefreshAnalyticsSection;
  public
    procedure ApplyMainformReplacedSector;
  end;

implementation

uses Form.Main;

procedure TMainformReplacedSectorApplier.ApplyMainformReplacedSector;
begin
  ApplyReplacedSector;
  RefreshAnalyticsSection;
  FreeReplacedSectorLog;
end;

procedure TMainformReplacedSectorApplier.ApplyReplacedSector;
begin
  SetReplacedSectors;
  CreateReplacedSectorLog;
end;

procedure TMainformReplacedSectorApplier.SetReplacedSectors;
begin
  ReplacedSectors := fMain.SelectedDrive.SMARTInterpreted.ReplacedSectors;
  fMain.lSectors.Caption := CapRepSect[CurrLang];
  if fMain.SelectedDrive.SupportStatus.Supported = CDIInsufficient then
    fMain.lSectors.Caption := fMain.lSectors.Caption +
      CapUnsupported[CurrLang]
  else
    fMain.lSectors.Caption := fMain.lSectors.Caption +
      UIntToStr(ReplacedSectors) + CapCount[CurrLang];
end;

procedure TMainformReplacedSectorApplier.CreateReplacedSectorLog;
begin
  ReplacedSectorLog := TAverageCountLogger.Create(
    TAverageCountLogger.BuildFileName(
      EnvironmentVariable.AppPath,
      fMain.SelectedDrive.IdentifyDeviceResult.Serial + 'RSLog'));
  ReplacedSectorLog.ReadAndRefresh(UIntToStr(ReplacedSectors));
end;

procedure TMainformReplacedSectorApplier.FreeReplacedSectorLog;
begin
  if ReplacedSectorLog <> nil then
    FreeAndNil(ReplacedSectorLog);
end;

function TMainformReplacedSectorApplier.IsTotalWriteNotSupported: Boolean;
begin
  result :=
    fMain.SelectedDrive.SupportStatus.TotalWriteType =
    TTotalWriteType.WriteNotSupported;
end;

procedure TMainformReplacedSectorApplier.SetUsageLabelByLogAndAvailableType;
var
  MaxPeriodAverage: TPeriodAverage;
begin
  MaxPeriodAverage := ReplacedSectorLog.GetMaxPeriodFormattedAverage;
  fMain.l1Month.Caption :=
    CapAvg[Integer(MaxPeriodAverage.Period)][CurrLang] +
    MaxPeriodAverage.FormattedAverageValue +
    CapCount[CurrLang] + '/' +
    CapDay[CurrLang];
end;

procedure TMainformReplacedSectorApplier.ApplyTodayUsageByLog;
begin
  fMain.lTodayUsage.Caption := CapToday[CurrLang] +
    ReplacedSectorLog.GetFormattedTodayDelta +
    CapCount[CurrLang];
end;

procedure TMainformReplacedSectorApplier.ApplyUsageByLog;
begin
  SetUsageLabelByLogAndAvailableType;
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
end;

procedure TMainformReplacedSectorApplier.SetAnalyticsLabelAsLifeAnalysis;
begin
  fMain.lAnalytics.Caption := BtLifeAnaly[CurrLang];
  fMain.lAnaly.Caption := CapLifeAnaly[CurrLang];
end;

procedure TMainformReplacedSectorApplier.RecoverAnalyticsLabel;
begin
  fMain.lAnalytics.Caption := BtAnaly[CurrLang];
  fMain.lAnaly.Caption := CapAnaly[CurrLang];
end;

procedure TMainformReplacedSectorApplier.RefreshAnalyticsSection;
begin
  if IsTotalWriteNotSupported then
    ApplyReplacedSectorsAsTotalWrite
  else
    RecoverAnalyticsLabel;
end;

end.
