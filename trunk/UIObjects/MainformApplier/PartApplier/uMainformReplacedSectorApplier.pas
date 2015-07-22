unit uMainformReplacedSectorApplier;

interface

uses
  SysUtils,
  uPathManager, uLanguageSettings, uPhysicalDrive, uListChangeGetter,
  uAverageCountLogger, uAverageLogger, uNSTSupport;

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
  ReplacedSectorLog := TAverageCountLogger.Create(
    TAverageCountLogger.BuildFileName(
      TPathManager.AppPath,
      fMain.PhysicalDrive.IdentifyDeviceResult.Serial + 'RSLog'));
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
    fMain.PhysicalDrive.SupportStatus.TotalWriteType =
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
    CapCount[CurrLang] +
    CapDay[CurrLang];
end;

procedure TMainformReplacedSectorApplier.ApplyTodayUsageByLog;
begin
  fMain.lTodayUsage.Caption := CapToday[CurrLang] +
    ReplacedSectorLog.TodayUsage +
    CapCount[CurrLang];
end;

procedure TMainformReplacedSectorApplier.ApplyUsageByLog;
begin
  SetUsageLabelByLogAndAvailableType(ReplacedSectorLog, AvailableAverageType)
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
