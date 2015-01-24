unit uSemiAuto;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  uTrimThread, uTrimList, uDiskFunctions, uStrFunctions,
  uSSDInfo, uSSDList, uSSDSupport, uLanguageSettings;

type
  TSemiAuto = class
  public
    class procedure SemiAutoTrim(Model, Serial: String);
  private
    class function GetDeviceEntry(Model, Serial: String): TSSDEntry;
    class function GetTrimList(SSDEntry: TSSDEntry): TTrimList;
    class procedure ExecuteTrim(TrimList: TTrimList);
  end;

implementation

class procedure TSemiAuto.SemiAutoTrim(Model, Serial: String);
var
  SSDEntry: TSSDEntry;
  PartToTrim: TTrimList;
begin
  SSDEntry := GetDeviceEntry(Model, Serial);
  PartToTrim := GetTrimList(SSDEntry);
  ExecuteTrim(PartToTrim);
  FreeAndNil(PartToTrim);
end;

class function TSemiAuto.GetDeviceEntry(Model, Serial: String): TSSDEntry;
var
  SSDList: TSSDList;
begin
  SSDList := TSSDList.Create;
  TraverseDevice(false, false, SSDList);
  result := SSDList[SSDList.IndexOf(Model, Serial)];
  FreeAndNil(SSDList);
end;

class function TSemiAuto.GetTrimList(SSDEntry: TSSDEntry): TTrimList;
var
  Drives: TDriveLetters;
  CurrPartition: Integer;
begin
  result := TTrimList.Create;
  Drives :=
    GetPartitionList(ExtractDeviceNum(
      '\\.\PhysicalDrive' + SSDEntry.DeviceName));
  for CurrPartition := 1 to Length(Drives.Letters) do
    result.Add(Drives.Letters[CurrPartition] + ':');
end;

class procedure TSemiAuto.ExecuteTrim(TrimList: TTrimList);
var
  TrimThread: TTrimThread;
begin
  TTrimThread.IsSemiAuto := true;
  TTrimThread.TrimStage := TRIMSTAGE_NONE;
  if TrimThread <> Nil then FreeAndNil(TrimThread);
  TrimThread := TTrimThread.Create(true);
  TrimThread.ApplyPartList(TrimList);
  TrimThread.Priority := tpLower;
  TrimThread.Start;
  while TTrimThread.TrimStage < TRIMSTAGE_END do Sleep(10);
  Sleep(10);
  FreeAndNil(TrimThread);
end;

end.
