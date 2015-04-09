unit uSemiAuto;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  uTrimThread, uTrimList, uDiskFunctions, uStrFunctions,
  uPhysicalDriveList, uLanguageSettings,
  uPhysicalDrive, uPartitionListGetter;

type
  TSemiAuto = class
  public
    class procedure SemiAutoTrim(Model, Serial: String);
  private
    class function GetDeviceEntry(Model, Serial: String): TPhysicalDrive;
    class function GetTrimList(SSDEntry: TPhysicalDrive): TTrimList;
    class procedure ExecuteTrim(TrimList: TTrimList);
  end;

implementation

class procedure TSemiAuto.SemiAutoTrim(Model, Serial: String);
var
  SSDEntry: TPhysicalDrive;
  PartToTrim: TTrimList;
begin
  SSDEntry := GetDeviceEntry(Model, Serial);
  PartToTrim := GetTrimList(SSDEntry);
  ExecuteTrim(PartToTrim);
  FreeAndNil(PartToTrim);
end;

class function TSemiAuto.GetDeviceEntry(Model, Serial: String):
  TPhysicalDrive;
var
  SSDList: TPhysicalDriveList;
begin
  SSDList := TPhysicalDriveList.Create;
  TraverseDevice(false, false, SSDList);
  result := SSDList[SSDList.IndexOf(Model, Serial)];
  FreeAndNil(SSDList);
end;

class function TSemiAuto.GetTrimList(SSDEntry: TPhysicalDrive):
  TTrimList;
var
  PhysicalDrive: TPhysicalDrive;
  Drives: TPartitionList;
  CurrPartition: Integer;
begin
  result := TTrimList.Create;
  PhysicalDrive := TPhysicalDrive.Create(
    StrToInt(SSDEntry.GetPathOfFileAccessingWithoutPrefix));
  Drives :=
    PhysicalDrive.GetPartitionList;
  for CurrPartition := 0 to Drives.Count - 1 do
    result.Add(Drives[CurrPartition].Letter + ':');
  FreeAndNil(Drives);
  FreeAndNil(PhysicalDrive);
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
