unit AverageLogger;

interface

uses
  Classes, Sysutils, Dialogs, Windows, Math,
  AsciiCheck;

type
  TAveragePeriod = (Days30, Days90, Days180);
  
  TPeriodAverage = record
    Period: TAveragePeriod;
    FormattedAverageValue: String;
  end;

  TAverageLogger = class abstract
  private
    LastDateInLog: String;
    LastValueInLog: UInt64;
    MaxPeriodAverage: TPeriodAverage;
    PassedDaysFromFirst: Integer;
    TodayDelta: Double;
    UserDefaultFormat: TFormatSettings;
    TimestampedValueList: TStringList;
    FileName: String;
    procedure ChangeLastRecordedPeriodToNow;
    procedure AddNewRecordWithTimestamp(NewValue: String);
    procedure ReadFileOrCreateNew(FileName: String);
    function IsNewValueInvalid(NewValue: String): Boolean;
    procedure InitializeAverageTodayDelta;
    procedure ReadAndSetAverageTodayDelta(NewValue: String);
    procedure SetAverage(NewValue: String);
    procedure RefreshFile(NewValue: String);
    procedure DisposeExpiredRecords;
    procedure SaveToFile;
    function IsNewRecordNeeded: Boolean;
    function IsLastRecordExpired: Boolean;
  protected
    function GetUnit: Double; virtual; abstract;
  public
    constructor Create(FileName: String); overload;
    constructor Create(FileContents: TStringList); overload;
    procedure ReadAndRefresh(NewValue: String);
    function GetFormattedTodayDelta: String;
    function GetMaxPeriodFormattedAverage: TPeriodAverage;  
    class function BuildFileName(Folder, Serial: String): String;
    destructor Destroy; override;
  end;

implementation

procedure TAverageLogger.ReadFileOrCreateNew(FileName: String);
begin
  self.FileName := FileName;
  if not FileExists(FileName) then
    TimestampedValueList.SaveToFile(FileName)
  else
    TimestampedValueList.LoadFromFile(FileName);
end;

constructor TAverageLogger.Create(FileName: String);
begin
  TimestampedValueList := TStringList.Create;
  Create(TimestampedValueList);
  ReadFileOrCreateNew(FileName);
end;

destructor TAverageLogger.Destroy;
begin
  FreeAndNil(TimestampedValueList);
end;

function TAverageLogger.IsNewValueInvalid(NewValue: String): Boolean;
var
  NewValueInUInt64: UInt64;
begin
  result :=
    (not (TryStrToUInt64(NewValue, NewValueInUInt64))) or
    (NewValueInUInt64 = 0);
end;

procedure TAverageLogger.InitializeAverageTodayDelta;
begin
  LastDateInLog := '';
  LastValueInLog := 0;
  PassedDaysFromFirst := 0;
  MaxPeriodAverage.Period := TAveragePeriod.Days30;
  MaxPeriodAverage.FormattedAverageValue := '0.0';
  TodayDelta := 0;
end;

procedure TAverageLogger.SetAverage(NewValue: String);
const
  AvgMax = 2;
  AveragePeriodInSet: Array[0..AvgMax] of Set of Byte =
    ([0..29], [30..89], [90..180]);
  IntegerToAveragePeriod: Array[0..AvgMax] of TAveragePeriod =
    (Days30, Days90, Days180);
var
  FirstValueInLog: Integer;
  CurrentAveragePeriod: Integer;
  AverageValue: Double;
begin
  FirstValueInLog := 
    StrToUInt64(TimestampedValueList[TimestampedValueList.Count - 1]);
  if StrToUInt64(NewValue) = (FirstValueInLog) then
    exit;
    
  for CurrentAveragePeriod := AvgMax downto 0 do
    if PassedDaysFromFirst in AveragePeriodInSet[CurrentAveragePeriod] then
    begin
      MaxPeriodAverage.Period :=
        IntegerToAveragePeriod[CurrentAveragePeriod];
      AverageValue :=
        (StrToUInt64(NewValue) - FirstValueInLog) /
        PassedDaysFromFirst *
        GetUnit;
      MaxPeriodAverage.FormattedAverageValue :=
        Format('%.1f', [AverageValue]);
      break;  
    end;
end;

function TAverageLogger.GetFormattedTodayDelta: String;  
begin
  result := Format('%.1f', [TodayDelta]);
end;

function TAverageLogger.GetMaxPeriodFormattedAverage: TPeriodAverage;  
begin
  result := MaxPeriodAverage;
end;

procedure TAverageLogger.ReadAndSetAverageTodayDelta(NewValue: String);
begin
  if TimestampedValueList.Count = 0 then
    exit;
  
  LastDateInLog := TimestampedValueList[0];
  LastValueInLog := StrToUInt64(TimestampedValueList[1]);
  PassedDaysFromFirst :=
    Ceil(Now - StrToDateTime(
      TimestampedValueList[
        TimestampedValueList.Count - 2], UserDefaultFormat));
  TodayDelta :=
    (StrToUInt64(NewValue) -
     StrToUInt64(TimestampedValueList[1])) * GetUnit;
  SetAverage(NewValue);
end;

function TAverageLogger.IsNewRecordNeeded: Boolean;
begin
  result :=
    (TimestampedValueList.Count = 0) or
    (TodayDelta > 0);
end;

procedure TAverageLogger.RefreshFile(NewValue: String);
begin
  if LastDateInLog = FormatDateTime('yy/mm/dd', Now) then
    exit;
    
  LastDateInLog := FormatDateTime('yy/mm/dd', Now);
  LastValueInLog := StrToUInt64(NewValue);
  if IsNewRecordNeeded then AddNewRecordWithTimestamp(NewValue)
  else ChangeLastRecordedPeriodToNow;
end;

procedure TAverageLogger.ReadAndRefresh(NewValue: String);
begin
  InitializeAverageTodayDelta;
  if IsNewValueInvalid(NewValue) then
    exit;

  ReadAndSetAverageTodayDelta(NewValue);
  RefreshFile(NewValue);
end;

function TAverageLogger.IsLastRecordExpired: Boolean;
begin
  result := TimestampedValueList.Count > 0;
  result := result and
    ((Now - 180) >
      StrToDateTime(
        TimestampedValueList[TimestampedValueList.Count - 2],
        UserDefaultFormat));
end;

procedure TAverageLogger.DisposeExpiredRecords;
begin
  while IsLastRecordExpired do
  begin
    TimestampedValueList.Delete(TimestampedValueList.Count - 1);
    TimestampedValueList.Delete(TimestampedValueList.Count - 1);
  end;
end;

procedure TAverageLogger.ChangeLastRecordedPeriodToNow;
begin
  TimestampedValueList[0] := FormatDateTime('yy/mm/dd', Now);
  SaveToFile;
end;

constructor TAverageLogger.Create(FileContents: TStringList);
begin
  UserDefaultFormat := TFormatSettings.Create(GetUserDefaultLCID);
  UserDefaultFormat.DateSeparator := '-';
  TimestampedValueList := FileContents;
  if TimestampedValueList.Count > 0 then
    DisposeExpiredRecords;
end;

procedure TAverageLogger.AddNewRecordWithTimestamp(NewValue: String);
begin
  TimestampedValueList.Insert(0, NewValue);
  TimestampedValueList.Insert(0, FormatDateTime('yy/mm/dd', Now));
  SaveToFile;
end;

procedure TAverageLogger.SaveToFile;
begin
  {$IfNDef UNITTEST}
  TimestampedValueList.SaveToFile(FileName);
  {$EndIf}
end;

class function TAverageLogger.BuildFileName(Folder, Serial: String): String;
begin
  result := Folder + 'WriteLog' + Serial + '.txt';
end;
end.
