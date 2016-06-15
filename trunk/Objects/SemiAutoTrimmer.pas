unit SemiAutoTrimmer;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  Thread.Trim, TrimList,
  Device.PhysicalDrive.List,
  Device.PhysicalDrive, Getter.PhysicalDrive.PartitionList, Getter.PhysicalDrive.ListChange;

type
  TSemiAutoTrimmer = class
  public
    procedure SemiAutoTrim(const Model, Serial: String);

  private
    function GetDeviceEntry(const Model, Serial: String): IPhysicalDrive;
    function GetTrimList(SSDEntry: IPhysicalDrive): TTrimList;
    procedure ExecuteTrim(TrimList: TTrimList);
  end;

implementation

procedure TSemiAutoTrimmer.SemiAutoTrim(const Model, Serial: String);
var
  SSDEntry: IPhysicalDrive;
  PartToTrim: TTrimList;
begin
  SSDEntry := GetDeviceEntry(Model, Serial);
  PartToTrim := GetTrimList(SSDEntry);
  ExecuteTrim(PartToTrim);
  FreeAndNil(PartToTrim);
end;

function TSemiAutoTrimmer.GetDeviceEntry(const Model, Serial: String):
  IPhysicalDrive;
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

function TSemiAutoTrimmer.GetTrimList(SSDEntry: IPhysicalDrive):
  TTrimList;
var
  PhysicalDrive: IPhysicalDrive;
  Drives: TPartitionList;
  CurrPartition: Integer;
begin
  result := TTrimList.Create;
  PhysicalDrive := SSDEntry;
  Drives :=
    PhysicalDrive.GetPartitionList;
  for CurrPartition := 0 to Drives.Count - 1 do
    result.Add(Drives[CurrPartition].Letter + ':');
  FreeAndNil(Drives);
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
