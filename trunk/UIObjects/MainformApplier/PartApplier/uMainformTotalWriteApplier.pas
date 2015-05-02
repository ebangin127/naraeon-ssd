unit uMainformTotalWriteApplier;

interface

uses
  SysUtils,
  uLanguageSettings, uPhysicalDrive, uListChangeGetter, uNSTSupport, uLogSystem,
  uPathManager, uStrFunctions, uDatasizeUnit;

type
  TMainformTotalWriteApplier = class
  private
    procedure AppendWriteToWriteLabel;
    procedure ApplyTodayUsageByLog(WriteLog: TNSTLog);
    procedure ApplyTotalWriteAsCount;
    procedure ApplyTotalWriteAsValue;
    procedure ApplyTotalWriteByWriteType;
    procedure ApplyTotalWriteConvertedToMiB;
    procedure ApplyTotalWriteInCount;
    procedure SetUsageLabelByLogAndAvailableType(WriteLog: TNSTLog;
      AvailableAverageType: Integer);
    function GetAvailableAverageType(WriteLog: TNSTLog): Integer;
    function IsTotalWriteNotSupported: Boolean;
    function KiBtoMiB(SizeInKiB: UInt64): Double;
    procedure RefreshWriteLogAndApplyUsageByLog;
    procedure RestoreComponentsFromCountIfSet;
    procedure SetComponentsForCount;
    procedure SetUsageLabelAsUnknown;
    procedure SetWriteLabelByHostNANDInformation;
    procedure ApplyUsageByLog(WriteLog: TNSTLog);
  public
    procedure ApplyMainformTotalWrite;
  end;

implementation

uses uMain;

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
  BinaryPointOne: FormatSizeSetting;
begin
  BinaryPointOne.FNumeralSystem := Binary;
  BinaryPointOne.FPrecision := 1;

  fMain.lHost.Caption :=
    fMain.lHost.Caption +
    FormatSizeInMB(
      fMain.PhysicalDrive.SMARTInterpreted.TotalWrite.InValue.ValueInMiB,
      BinaryPointOne);
end;

function TMainformTotalWriteApplier.GetAvailableAverageType(WriteLog: TNSTLog):
  Integer;
var
  CurrentAverageType: Integer;
begin
  result := -1;
  for CurrentAverageType := AvgMax downto 0 do
    if Length(WriteLog.Average[IntToAvg[CurrentAverageType]]) > 0 then
      exit(CurrentAverageType);
end;

procedure TMainformTotalWriteApplier.SetUsageLabelByLogAndAvailableType(
  WriteLog: TNSTLog; AvailableAverageType: Integer);
begin
  if Length(WriteLog.Average[IntToAvg[AvailableAverageType]]) > 0 then
    fMain.l1Month.Caption :=
      CapAvg[AvailableAverageType][CurrLang] +
      WriteLog.Average[IntToAvg[AvailableAverageType]] + 'GB/' +
      CapDay[CurrLang];
end;

procedure TMainformTotalWriteApplier.SetUsageLabelAsUnknown;
begin
  fMain.l1Month.Caption :=
    CapAvg[0][CurrLang] + '0 GB/' + CapDay[CurrLang];
end;

procedure TMainformTotalWriteApplier.ApplyTodayUsageByLog(WriteLog: TNSTLog);
begin
  fMain.lTodayUsage.Caption := CapToday[CurrLang] +
    WriteLog.TodayUsage + 'GB';
end;

procedure TMainformTotalWriteApplier.ApplyUsageByLog(WriteLog: TNSTLog);
var
  AvailableAverageType: Integer;
begin
  AvailableAverageType := GetAvailableAverageType(WriteLog);
  if AvailableAverageType >= 0 then
    SetUsageLabelByLogAndAvailableType(WriteLog, AvailableAverageType)
  else
    SetUsageLabelAsUnknown;
  ApplyTodayUsageByLog(WriteLog);
end;

procedure TMainformTotalWriteApplier.RefreshWriteLogAndApplyUsageByLog;
var
  WriteLog: TNSTLog;
begin   
  WriteLog :=
    TNSTLog.Create(
      TPathManager.AppPath, fMain.PhysicalDrive.IdentifyDeviceResult.Serial,
      UIntToStr(MBToLiteONUnit(
        fMain.PhysicalDrive.SMARTInterpreted.TotalWrite.InValue.ValueInMiB)),
        false);
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