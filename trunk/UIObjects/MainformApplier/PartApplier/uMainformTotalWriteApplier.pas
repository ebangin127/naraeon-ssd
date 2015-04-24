unit uMainformTotalWriteApplier;

interface

uses
  uPhysicalDrive, uListChangeGetter;

type
  TMainformTotalWriteApplier = class
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
  if l1Month.Visible = false then
  begin
    l1Month.Visible := false;
    lHost.Top := lHost.Top + 25;
    lTodayUsage.Top := lTodayUsage.Top + 15;
    lOntime.Top := lOntime.Top + 10;
  end;
end;

procedure TMainformTotalWriteApplier.SetComponentsForCount;
begin
  l1Month.Visible := false;
  lHost.Top := lHost.Top + 25;
  lTodayUsage.Top := lTodayUsage.Top + 15;
  lOntime.Top := lOntime.Top + 10;
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

  lHost.Caption :=
    lHost.Caption +
    FormatSizeInMB(
      fMain.PhysicalDrive.SMARTInterpreted.TotalWrite.InValue.ValueInMiB,
      BinaryPointOne);
end;

function TMainformTotalWriteApplier.GetAvailableAverageType: Integer;
var
  CurrentAverageType: Integer;
begin
  result := -1;
  for CurrentAverageType := AvgMax downto 0 do
    if Length(WriteLog.Average[IntToAvg[CurrentAverageType]]) > 0 then
      exit(CurrentAverageType);
end;

procedure TMainformTotalWriteApplier.ApplyUsageByLog(WriteLog: TNSTLog;
  AvailableAverageType: Integer);
begin
  if Length(WriteLog.Average[IntToAvg[AvailableAverageType]]) > 0 then
    l1Month.Caption :=
      CapAvg[AvailableAverageType][CurrLang] +
      WriteLog.Average[IntToAvg[AvailableAverageType]] + 'GB/' +
      CapDay[CurrLang];
end;

procedure TMainformTotalWriteApplier.SetUsageLabelAsUnknown;
begin
  l1Month.Caption :=
    CapAvg[0][CurrLang] + '0 GB/' + CapDay[CurrLang];
end;

procedure TMainformTotalWriteApplier.ApplyTodayUsageByLog(WriteLog: TNSTLog);
begin
  lTodayUsage.Caption := CapToday[CurrLang] +
    WriteLog.TodayUsage + 'GB';
end;

procedure TMainformTotalWriteApplier.ApplyUsageByLog(WriteLog: TNSTLog);
var
  AvailableAverageType: Integer;
begin
  AvailableAverageType := GetAvailableAverageType;
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
      TPathManager.AppPath, PhysicalDrive.IdentifyDeviceResult.Serial,
      UIntToStr(MBToLiteONUnit(HostWriteInMiB)), false);
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
    PhysicalDrive.SMARTInterpreted.TotalWrite.InCount.ValueInCount *
    round(KiBtoMiB(PhysicalDrive.IdentifyDeviceResult.UserSizeInKB));
        
  lHost.Caption := CapNandWrite[CurrLang] + UIntToStr(HostWriteInMiB);
end;

procedure TMainformTotalWriteApplier.ApplyTotalWriteInCount;
var
  HostWriteInMiB: UInt64;
begin
  lTodayUsage.Caption :=
    CapWearLevel[CurrLang] +
    UIntToStr(PhysicalDrive.SMARTInterpreted.
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
  case PhysicalDrive.SupportStatus.TotalWriteType of
    TTotalWriteType.WriteSupportedAsValue:
      ApplyTotalWriteAsCount;
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