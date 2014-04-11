unit uLogSystem;

interface

uses Classes, Sysutils, Dialogs, Windows, Math, uIntFunctions;

type
  TAverage = (Avg30, Avg90, Avg180);

const
  AvgMax = 2;
  AverageDays: Array[TAverage] of Integer = (30, 90, 180);
  IntToAvg: Array[0..AvgMax] of TAverage = (Avg30, Avg90, Avg180);

type
  TNSTLog = class
    LastDay: String;
    LastOneGig, LastFiveGig: Integer;
    Average: Array[TAverage] of String;
    DayCount: Integer;
    TodayUsage: String;
    CurrOneGig, CurrFiveGig: String;
    ReadableList: TStringList;
    ConnFolder: String;
    ConnSerial: String;
    LastConnFolder: String;
    OnlyCount: Boolean;
    //1. 창조자와 파괴자
    constructor Create(Folder: String; Serial: String; CurrSMARTGig: String; isCount: Boolean; isLiteon85: Boolean);
    destructor Destroy; override;
    //2. 끊고 재연결하기
    procedure Disconnect;
    //3. 읽고 쓰기
    procedure ReadBothFiles(CurrGig: String);
    procedure ReleaseEqualGig;
    procedure ReleaseOneGig(CurrGig: String);
    protected
      OneGigList: TStringList;
  end;

procedure MigrateOldLog(OldPath, NewPath: String);

implementation

constructor TNSTLog.Create(Folder: String; Serial: String; CurrSMARTGig: String; isCount: Boolean; isLiteon85: Boolean);
var
  CurrAvgDays: Integer;
begin
  ConnFolder := '';
  LastConnFolder := '';
  LastDay := '';
  DayCount := 0;
  TodayUsage := '';
  for CurrAvgDays := 0 to AvgMax do
  begin
    Average[TAverage(CurrAvgDays)] := '';
  end;
  LastOneGig := 0;
  LastFiveGig := 0;
  OneGigList := TStringList.Create;
  ReadableList := TStringList.Create;
  OnlyCount := isCount;
  if isLiteon85 then
    Serial := Serial + '85';
  try
    //1기가 파일 로딩
    if FileExists(Folder + 'WriteLog' + Serial + '.txt') = false then
    begin
      if FileExists(Folder + 'UseLog' + Serial + '.txt') = false then
        OneGigList.SaveToFile(Folder + 'WriteLog' + Serial +'.txt')
      else
      begin
        MigrateOldLog(Folder + 'UseLog' + Serial +'.txt',
                      Folder + 'WriteLog' + Serial +'.txt');
      end;
    end;
    OneGigList.LoadFromFile(Folder + 'WriteLog' + Serial +'.txt');
    ConnSerial := Serial;
    ConnFolder := Folder;
    ReadBothFiles(CurrSMARTGig);
  except
    ConnFolder := '';
  end;
end;

destructor TNSTLog.Destroy;
begin
  FreeAndNil(OneGigList);
  FreeAndNil(ReadableList);
end;

procedure TNSTLog.Disconnect;
begin
  LastConnFolder := ConnFolder;
  ConnFolder := '';
  FreeAndNil(OneGigList);
  FreeAndNil(ReadableList);
end;

procedure TNSTLog.ReadBothFiles(CurrGig: String);
var
  CurrDay: Integer;
  FSet: TFormatSettings;
  CurrAvgDay: Integer;
begin
  Average[Avg30] := '0.0';
  TodayUsage := '0.0';
  DayCount := 0;
  if StrToUInt64(CurrGig) = 0 then exit;

  if OneGigList.Count > 0 then
  begin
    LastDay := OneGigList[0];
    LastOneGig := StrToUInt64(OneGigList[1]);
    FSet := TFormatSettings.Create(GetUserDefaultLCID);
    DayCount := Ceil(Now - StrToDateTime(OneGigList[OneGigList.Count - 2], FSet));
    FSet.DateSeparator := '-';
    if OnlyCount then
      TodayUsage := IntToStr((StrToUInt64(CurrGig) - StrToUInt64(OneGigList[1])))
    else
      TodayUsage := Format('%.1f',[(StrToUInt64(CurrGig) - StrToUInt64(OneGigList[1])) * 0.064]);

    for CurrDay := 0 to ((OneGigList.Count div 2) - 1) do
    begin
      for CurrAvgDay := 0 to AvgMax do
      begin
        if Now - AverageDays[IntToAvg[CurrAvgDay]] <= StrToDateTime(OneGigList[CurrDay * 2], FSet) then
        begin
          if StrToUInt64(CurrGig) <> (StrToUInt64(OneGigList[(CurrDay * 2) + 1])) then
            if OnlyCount then
              Average[IntToAvg[CurrAvgDay]] := Format('%.1f',[((StrToUInt64(CurrGig) - (StrToUInt64(OneGigList[(CurrDay * 2) + 1]))) / (Now - (StrToDateTime(OneGigList[CurrDay * 2], FSet))))])
            else
              Average[IntToAvg[CurrAvgDay]] := Format('%.1f',[((StrToUInt64(CurrGig) - (StrToUInt64(OneGigList[(CurrDay * 2) + 1]))) / (Now - (StrToDateTime(OneGigList[CurrDay * 2], FSet)))) * 0.064]);
        end;
      end;
    end;
  end
  else
  begin
    LastDay := '';
    LastOneGig := 0;
  end;
  if LastDay <> FormatDateTime('yy/mm/dd', Now) then
  begin
    if (StrToUInt64(CurrGig) = LastOneGig) and (CurrGig <> '0') then ReleaseEqualGig
    else ReleaseOneGig(CurrGig);
  end;
end;

procedure TNSTLog.ReleaseEqualGig;
var
  FSet: TFormatSettings;
begin
  FSet := TFormatSettings.Create(GetUserDefaultLCID);
  FSet.DateSeparator := '-';
  OneGigList[0] := FormatDateTime('yy/mm/dd', Now);
  while (Now - 181) > StrToDateTime(OneGigList[OneGigList.Count - 2], FSet) do
  begin
    OneGigList.Delete(OneGigList.Count - 1);
    OneGigList.Delete(OneGigList.Count - 1);
  end;
  OneGigList.SaveToFile(ConnFolder + 'WriteLog' + ConnSerial +'.txt');
end;

procedure TNSTLog.ReleaseOneGig(CurrGig: String);
var
  FSet: TFormatSettings;
begin
  LastOneGig := StrToUInt64(CurrGig);
  FSet := TFormatSettings.Create(GetUserDefaultLCID);
  FSet.DateSeparator := '-';
  OneGigList.Insert(0, CurrGig);
  OneGigList.Insert(0, FormatDateTime('yy/mm/dd', Now));
  while (Now - 181) > StrToDateTime(OneGigList[OneGigList.Count - 2], FSet) do
  begin
    OneGigList.Delete(OneGigList.Count - 1);
    OneGigList.Delete(OneGigList.Count - 1);
  end;
  OneGigList.SaveToFile(ConnFolder + 'WriteLog' + ConnSerial +'.txt');
end;

procedure MigrateOldLog(OldPath, NewPath: String);
var
  NewLog: TStringList;
  CurrLine: Integer;
begin
  NewLog := TStringList.Create;
  NewLog.LoadFromFile(OldPath);
  for CurrLine := 1 to NewLog.Count div 2 do
    NewLog[CurrLine * 2 - 1] := NewLog[CurrLine * 2 - 1] + '0';
  NewLog.SaveToFile(NewPath);
  DeleteFile(PChar(OldPath));
  FreeAndNil(NewLog);
end;
end.
