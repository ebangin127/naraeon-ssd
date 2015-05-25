unit uSemiAutoTrimmer;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  uTrimThread, uTrimList,
  uPhysicalDriveList, uLanguageSettings,
  uPhysicalDrive, uPartitionListGetter, uListChangeGetter;

type
  TSemiAutoTrimmer = class
  public
    procedure SemiAutoTrim(Model, Serial: String);

  private
    function GetDeviceEntry(Model, Serial: String): TPhysicalDrive;
    function GetTrimList(SSDEntry: TPhysicalDrive): TTrimList;
    procedure ExecuteTrim(TrimList: TTrimList);
  end;

implementation

procedure TSemiAutoTrimmer.SemiAutoTrim(Model, Serial: String);
var
  SSDEntry: TPhysicalDrive;
  PartToTrim: TTrimList;
begin
  SSDEntry := GetDeviceEntry(Model, Serial);
  PartToTrim := GetTrimList(SSDEntry);
  ExecuteTrim(PartToTrim);
  FreeAndNil(PartToTrim);
end;

function TSemiAutoTrimmer.GetDeviceEntry(Model, Serial: String):
  TPhysicalDrive;
var
  SSDList: TPhysicalDriveList;
  ListChangeGetter: TListChangeGetter;
begin
  SSDList := TPhysicalDriveList.Create;
  ListChangeGetter := TListChangeGetter.Create;
  ListChangeGetter.IsOnlyGetSupportedDrives := true;
  ListChangeGetter.RefreshListWithoutResultFrom(SSDList);
  result := SSDList[SSDList.IndexOf(Model, Serial)];
  FreeAndNil(ListChangeGetter);
  FreeAndNil(SSDList);
end;

function TSemiAutoTrimmer.GetTrimList(SSDEntry: TPhysicalDrive):
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

procedure TSemiAutoTrimmer.ExecuteTrim(TrimList: TTrimList);
var
  TrimThread: TTrimThread;
begin
  if TrimThread <> Nil then FreeAndNil(TrimThread);
  TrimThread := TTrimThread.Create(true, false);
  TrimThread.SetPartitionList(TrimList);
  TrimThread.Priority := tpLower;
  TrimThread.Start;
  while TTrimThread.TrimStage < TTrimStage.Finished do Sleep(10);
  Sleep(10);
end;

end.
