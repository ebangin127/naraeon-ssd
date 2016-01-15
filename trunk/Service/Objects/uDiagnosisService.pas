unit uDiagnosisService;

interface

uses
  Classes, SysUtils, ShlObj, Windows,
  Device.PhysicalDrive, Device.PhysicalDrive.List, Global.LanguageString,
  OS.EnvironmentVariable, Getter.PhysicalDrive.ListChange, MeasureUnit.Datasize, AverageLogger.Count,
  AverageLogger.Write, Support, Getter.PhysicalDrive.PartitionList,
  OS.WriteBufferSettingVerifier;

type
  TDiagnosisService = class
  private
    PhysicalDriveList: TPhysicalDriveList;
    LastChanges: TChangesList;
    IsFirstDiagnosis: Boolean;
    ErrorFilePath: String;
    procedure SetPhysicalDriveList;
    procedure FreeLastChanges;
    function GetLogLine(Timestamp: TDateTime; Content: String): String;
    function IsNeedDiagnosis: Boolean;
    procedure WriteBufferCheck;
    procedure LogAndCheckSMART;
    procedure RefreshReplacedSectorLog(Entry: IPhysicalDrive);
    procedure RefreshTotalWriteLog(Entry: IPhysicalDrive);
    procedure AlertIfNeedToReplacedSectors(Entry: IPhysicalDrive;
      IsReplacedSectorDifferent: Boolean);
    function GetAlertFile: TStringList;
    function GetPartitionLetters(Entry: IPhysicalDrive): String;
    procedure SaveWriteBufferCheckResult(WriteBufferErrors: TStringList);
    procedure RefreshReadWriteErrorLog(Entry: IPhysicalDrive);
    procedure AlertIfNeedToReadEraseError(Entry: IPhysicalDrive;
      IsReadWriteErrorDifferent: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitializePhysicalDriveList;
    procedure Diagnosis;
    function IsThereAnySupportedDrive: Boolean;
  end;

implementation

{ TDiagnosisService }

constructor TDiagnosisService.Create;
begin
  EnvironmentVariable.SetPath(nil);
  DetermineLanguage;
  ErrorFilePath := EnvironmentVariable.AllDesktopPath + '\!!!SSDError!!!.err';
  IsFirstDiagnosis := true;
end;

procedure TDiagnosisService.SetPhysicalDriveList;
var
  ListChangeGetter: TListChangeGetter;
begin
  FreeLastChanges;
  if PhysicalDriveList = nil then
    PhysicalDriveList := TPhysicalDriveList.Create;
  ListChangeGetter := TListChangeGetter.Create;
  ListChangeGetter.IsOnlyGetSupportedDrives := true;
  LastChanges :=
    ListChangeGetter.ServiceRefreshListWithResultFrom(PhysicalDriveList);
  FreeAndNil(ListChangeGetter);
end;

procedure TDiagnosisService.InitializePhysicalDriveList;
begin
  SetPhysicalDriveList;
end;

procedure TDiagnosisService.FreeLastChanges;
begin
  if LastChanges.Added <> nil then
    FreeAndNil(LastChanges.Added);
  if LastChanges.Deleted <> nil then
    FreeAndNil(LastChanges.Deleted);
end;

function TDiagnosisService.IsThereAnySupportedDrive: Boolean;
begin
  result := PhysicalDriveList.Count > 0;
end;

function TDiagnosisService.GetLogLine(Timestamp: TDateTime;
  Content: String): String;
begin
  result := FormatDateTime('[yy/mm/dd hh:nn:ss]', Timestamp) + Content;
end;

function TDiagnosisService.IsNeedDiagnosis: Boolean;
begin
  result := IsFirstDiagnosis or (FormatDateTime('mm', Now) = '00');
  if IsFirstDiagnosis = false then
    IsFirstDiagnosis := true;
end;

procedure TDiagnosisService.WriteBufferCheck;
var
  WriteBufferSettingVerifier: TWriteBufferSettingVerifier;
begin
  WriteBufferSettingVerifier := TWriteBufferSettingVerifier.Create;
  SaveWriteBufferCheckResult(
    WriteBufferSettingVerifier.CheckAndCorrect);
  FreeAndNil(WriteBufferSettingVerifier);
end;

procedure TDiagnosisService.SaveWriteBufferCheckResult(
  WriteBufferErrors: TStringList);
var
  AlertFile: TStringList;
  CurrentLine: Integer;
begin
  if WriteBufferErrors.Count > 0 then
  begin
    AlertFile := GetAlertFile;
    for CurrentLine := 0 to WriteBufferErrors.Count - 1 do
      AlertFile.Add(
        GetLogLine(Now, ' !!!!! ' +
          WriteBufferErrors[CurrentLine] + ' ' +
          CapWrongBuf[CurrLang] + ' !!!!! ' +
          CapWrongBuf2[CurrLang]));
    AlertFile.SaveToFile(ErrorFilePath);
    FreeAndNil(AlertFile);
  end;
  FreeAndNil(WriteBufferErrors);
end;

procedure TDiagnosisService.RefreshTotalWriteLog(Entry: IPhysicalDrive);
var
  TotalWriteLog: TAverageWriteLogger;
begin
  if Entry.SupportStatus.TotalWriteType <>
     TTotalWriteType.WriteSupportedAsValue then
      exit;
  TotalWriteLog := TAverageWriteLogger.Create(
    TAverageWriteLogger.BuildFileName(
      EnvironmentVariable.AppPath,
      Entry.IdentifyDeviceResult.Serial));
  TotalWriteLog.ReadAndRefresh(UIntToStr(
    MBToLiteONUnit(
      Entry.SMARTInterpreted.TotalWrite.InValue.ValueInMiB)));
  FreeAndNil(TotalWriteLog);
end;

function TDiagnosisService.GetAlertFile: TStringList;
begin
  result := TStringList.Create;
  if FileExists(ErrorFilePath) then
    result.LoadFromFile(ErrorFilePath);
end;

function TDiagnosisService.GetPartitionLetters(Entry: IPhysicalDrive): String;
var
  PartitionList: TPartitionList;
  CurrentPartition: Integer;
begin
  PartitionList := Entry.GetPartitionList;
  result := '';
  for CurrentPartition := 0 to (PartitionList.Count - 1) do
    result := result + ' ' + PartitionList[CurrentPartition].Letter;
  FreeAndNil(PartitionList);
end;

procedure TDiagnosisService.AlertIfNeedToReplacedSectors(Entry: IPhysicalDrive;
  IsReplacedSectorDifferent: Boolean);
var
  AlertFile: TStringList;
  AllMountedPartitions: String;
begin
  if (Entry.SMARTInterpreted.SMARTAlert.ReplacedSector = false) or
     (not IsReplacedSectorDifferent) then
     exit;
  AlertFile := GetAlertFile;
  AllMountedPartitions := GetPartitionLetters(Entry);
  AlertFile.Add(
    GetLogLine(Now, ' !!!!! ' +
      AllMountedPartitions + ' ' + CapBck[CurrLang] + ' !!!!! ' +
      CapBckRepSector[CurrLang] + '(' +
      UIntToStr(Entry.SMARTInterpreted.ReplacedSectors) +
      CapCount[CurrLang] + ') ' + CapOcc[CurrLang]));
  AlertFile.SaveToFile(ErrorFilePath);
  FreeAndNil(AlertFile);
end;

procedure TDiagnosisService.AlertIfNeedToReadEraseError(Entry: IPhysicalDrive;
  IsReadWriteErrorDifferent: Boolean);
var
  AlertFile: TStringList;
  AllMountedPartitions: String;
begin
  if (Entry.SMARTInterpreted.SMARTAlert.ReadEraseError = false) or
     (not IsReadWriteErrorDifferent) then
     exit;
  AlertFile := GetAlertFile;
  AllMountedPartitions := GetPartitionLetters(Entry);
  AlertFile.Add(
    GetLogLine(Now, ' !!!!! ' +
      AllMountedPartitions + ' ' + CapBck[CurrLang] + ' !!!!! ' +
      CapBckREError[CurrLang] + '(' +
      UIntToStr(Entry.SMARTInterpreted.ReadEraseError.Value) +
      CapCount[CurrLang] + ') ' + CapOcc[CurrLang]));
  AlertFile.SaveToFile(ErrorFilePath);
  FreeAndNil(AlertFile);
end;

procedure TDiagnosisService.RefreshReplacedSectorLog(Entry: IPhysicalDrive);
var
  ReplacedSectorLog: TAverageCountLogger;
begin
  ReplacedSectorLog := TAverageCountLogger.Create(
    TAverageCountLogger.BuildFileName(
      EnvironmentVariable.AppPath,
      Entry.IdentifyDeviceResult.Serial + 'RSLog'));
  ReplacedSectorLog.ReadAndRefresh(UIntToStr(
    Entry.SMARTInterpreted.ReplacedSectors));
  AlertIfNeedToReplacedSectors(Entry,
    ReplacedSectorLog.GetFormattedTodayDelta <> '0.0');
  FreeAndNil(ReplacedSectorLog);
end;

procedure TDiagnosisService.RefreshReadWriteErrorLog(Entry: IPhysicalDrive);
var
  ReadWriteErrorLog: TAverageCountLogger;
begin
  ReadWriteErrorLog := TAverageCountLogger.Create(
    TAverageCountLogger.BuildFileName(
      EnvironmentVariable.AppPath,
      Entry.IdentifyDeviceResult.Serial + 'RELog'));
  ReadWriteErrorLog.ReadAndRefresh(UIntToStr(
    Entry.SMARTInterpreted.ReplacedSectors));
  AlertIfNeedToReadEraseError(Entry,
    ReadWriteErrorLog.GetFormattedTodayDelta <> '0.0');
  FreeAndNil(ReadWriteErrorLog);
end;

procedure TDiagnosisService.LogAndCheckSMART;
var
  CurrentEntry: IPhysicalDrive;
begin
  for CurrentEntry in PhysicalDriveList do
  begin
    if not CurrentEntry.IsDriveAvailable then
    begin
      SetPhysicalDriveList;
      exit;
    end;
    RefreshTotalWriteLog(CurrentEntry);
    RefreshReplacedSectorLog(CurrentEntry);
    RefreshReadWriteErrorLog(CurrentEntry);
  end;
end;

procedure TDiagnosisService.Diagnosis;
begin
  if not IsNeedDiagnosis then
    exit;
  WriteBufferCheck;
  LogAndCheckSMART;
end;

destructor TDiagnosisService.Destroy;
begin
  FreeLastChanges;
  if PhysicalDriveList <> nil then
    FreeAndNil(PhysicalDriveList);
end;

end.
