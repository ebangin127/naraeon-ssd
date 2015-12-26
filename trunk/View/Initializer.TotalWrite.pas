unit Initializer.TotalWrite;

interface

uses
  SysUtils,
  uLanguageSettings, Device.PhysicalDrive, uListChangeGetter, uNSTSupport,
  AverageLogger.Write, AverageLogger, uPathManager, MeasureUnit.DataSize;

type
  TMainformTotalWriteApplier = class
  private
    procedure AppendWriteToWriteLabel;
    procedure ApplyTodayUsageByLog(WriteLog: TAverageWriteLogger);
    procedure ApplyTotalWriteAsCount;
    procedure ApplyTotalWriteAsValue;
    procedure ApplyTotalWriteByWriteType;
    procedure ApplyTotalWriteConvertedToMiB;
    procedure ApplyTotalWriteInCount;
    procedure SetUsageLabelByLogAndAvailableType(WriteLog: TAverageWriteLogger);
    function IsTotalWriteNotSupported: Boolean;
    function KiBtoMiB(SizeInKiB: UInt64): Double;
    procedure RefreshWriteLogAndApplyUsageByLog;
    procedure RestoreComponentsFromCountIfSet;
    procedure SetComponentsForCount;
    procedure SetWriteLabelByHostNANDInformation;
    procedure ApplyUsageByLog(WriteLog: TAverageWriteLogger);
  public
    procedure ApplyMainformTotalWrite;
  end;

implementation

uses Form.Main;

function TMainformTotalWriteApplier.IsTotalWriteNotSupported: Boolean;
begin
  result := fMain.PhysicalDrive.SupportStatus.TotalWriteType =
    TTotalWriteType.WriteNotSupported;
end;

procedure TMainformTotalWriteApplier.RestoreComponentsFromCountIfSet;
begin
  if fMain.l1Month.Visible = false then
  begin
    fMain.l1Month.Visible := false;
    fMain.lHost.Top := fMain.lHost.Top + 25;
    fMain.lTodayUsage.Top := fMain.lTodayUsage.Top + 15;
    fMain.lOntime.Top := fMain.lOntime.Top + 10;
  end;
end;

procedure TMainformTotalWriteApplier.SetComponentsForCount;
begin
  fMain.l1Month.Visible := false;
  fMain.lHost.Top := fMain.lHost.Top + 25;
  fMain.lTodayUsage.Top := fMain.lTodayUsage.Top + 15;
  fMain.lOntime.Top := fMain.lOntime.Top + 10;
end;

procedure TMainformTotalWriteApplier.SetWriteLabelByHostNANDInformation;
begin
  if fMain.PhysicalDrive.SMARTInterpreted.
      TotalWrite.InValue.TrueHostWriteFalseNANDWrite then
        fMain.lHost.Caption := CapHostWrite[CurrLang]
  else
    fMain.lHost.Caption := CapNandWrite[CurrLang];
end;

procedure TMainformTotalWriteApplier.AppendWriteToWriteLabel;
var
  BinaryPointOne: TFormatSizeSetting;
begin
  BinaryPointOne.FNumeralSystem := Binary;
  BinaryPointOne.FPrecision := 1;

  fMain.lHost.Caption :=
    fMain.lHost.Caption +
    FormatSizeInMB(
      fMain.PhysicalDrive.SMARTInterpreted.TotalWrite.InValue.ValueInMiB,
      BinaryPointOne);
end;

procedure TMainformTotalWriteApplier.SetUsageLabelByLogAndAvailableType(
  WriteLog: TAverageWriteLogger);
var
  MaxPeriodAverage: TPeriodAverage;
begin
  MaxPeriodAverage := WriteLog.GetMaxPeriodFormattedAverage;
  fMain.l1Month.Caption :=
    CapAvg[Integer(MaxPeriodAverage.Period)][CurrLang] +
    MaxPeriodAverage.FormattedAverageValue + 'GB/' +
    CapDay[CurrLang];
end;

procedure TMainformTotalWriteApplier.ApplyTodayUsageByLog(
  WriteLog: TAverageWriteLogger);
begin
  fMain.lTodayUsage.Caption := CapToday[CurrLang] +
    WriteLog.GetFormattedTodayDelta + 'GB';
end;

procedure TMainformTotalWriteApplier.ApplyUsageByLog(
  WriteLog: TAverageWriteLogger);
begin
  SetUsageLabelByLogAndAvailableType(WriteLog);
  ApplyTodayUsageByLog(WriteLog);
end;

procedure TMainformTotalWriteApplier.RefreshWriteLogAndApplyUsageByLog;
var
  WriteLog: TAverageWriteLogger;
begin   
  WriteLog :=
    TAverageWriteLogger.Create(
      TAverageWriteLogger.BuildFileName(
        PathManager.AppPath,
        fMain.PhysicalDrive.IdentifyDeviceResult.Serial));
  WriteLog.ReadAndRefresh(
    UIntToStr(MBToLiteONUnit(
      fMain.PhysicalDrive.SMARTInterpreted.TotalWrite.InValue.ValueInMiB)));
  ApplyUsageByLog(WriteLog);
  FreeAndNil(WriteLog);
end;
  
procedure TMainformTotalWriteApplier.ApplyTotalWriteAsValue;
begin
  SetWriteLabelByHostNANDInformation;
  AppendWriteToWriteLabel;
  RefreshWriteLogAndApplyUsageByLog;
end;

function TMainformTotalWriteApplier.KiBtoMiB(SizeInKiB: UInt64): Double;
var
  BinaryKiBtoMiB: TDatasizeUnitChangeSetting;
begin
  BinaryKiBtoMiB.FNumeralSystem := Binary;
  BinaryKiBtoMiB.FFromUnit := KiloUnit;
  BinaryKiBtoMiB.FFromUnit := MegaUnit;
  
  result :=
    ChangeDatasizeUnit(SizeInKiB, BinaryKiBtoMiB);
end;

procedure TMainformTotalWriteApplier.ApplyTotalWriteConvertedToMiB;
var
  HostWriteInMiB: UInt64;
begin
  HostWriteInMiB :=
    fMain.PhysicalDrive.SMARTInterpreted.TotalWrite.InCount.ValueInCount *
    round(KiBtoMiB(fMain.PhysicalDrive.IdentifyDeviceResult.UserSizeInKB));
        
  fMain.lHost.Caption := CapNandWrite[CurrLang] + UIntToStr(HostWriteInMiB);
end;

procedure TMainformTotalWriteApplier.ApplyTotalWriteInCount;
begin
  fMain.lTodayUsage.Caption :=
    CapWearLevel[CurrLang] +
    UIntToStr(fMain.PhysicalDrive.SMARTInterpreted.
      TotalWrite.InCount.ValueInCount);
end;

procedure TMainformTotalWriteApplier.ApplyTotalWriteAsCount;
begin
  SetComponentsForCount;
  ApplyTotalWriteConvertedToMiB;
  ApplyTotalWriteInCount;
end;

procedure TMainformTotalWriteApplier.ApplyTotalWriteByWriteType;
begin
  case fMain.PhysicalDrive.SupportStatus.TotalWriteType of
    TTotalWriteType.WriteSupportedAsValue:
      ApplyTotalWriteAsValue;
    TTotalWriteType.WriteSupportedAsCount:
      ApplyTotalWriteAsCount;
  end;
end;

procedure TMainformTotalWriteApplier.ApplyMainformTotalWrite;
begin
  if IsTotalWriteNotSupported then
    exit;

  RestoreComponentsFromCountIfSet;
  ApplyTotalWriteByWriteType;
end;

end.