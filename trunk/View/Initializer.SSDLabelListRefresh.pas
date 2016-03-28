unit Initializer.SSDLabelListRefresh;

interface

uses
  Classes, Forms, SysUtils, Generics.Collections, Windows, ShellAPI,
  Global.LanguageString, OS.EnvironmentVariable, Form.Alert,
  Device.PhysicalDrive, Getter.PhysicalDrive.ListChange, Component.SSDLabel,
  Component.SSDLabel.List;

type
  TSSDLabelListRefresher = class
  private
    SSDLabel: TSSDLabelList;
    ChangesList: TChangesList;
    procedure AddByAddedList;
    procedure AddDevice(const Entry: IPhysicalDrive);
    procedure AlertAndExecuteNewDiagnosisInstance;
    procedure DeleteAndAddDevicesByResultList;
    procedure DeleteByDeletedList;
    procedure DeleteDevice(const Path: String);
    procedure FreeChangesList;
    function ChangeExists: Boolean;
    function IsNoSupportedDriveExists: Boolean;
    procedure RefreshMainFormAndSetNewSelection;
    procedure SetChangesList;
    procedure SetFirstDeviceAsSelected;
    procedure SetFirstDeviceAsSelectedIfNoDeviceSelected;
  public
    procedure RefreshDrives(const SSDLabelToRefresh: TSSDLabelList);
  end;

implementation

uses Form.Main;

type
  THackMainform = TForm;

procedure TSSDLabelListRefresher.AlertAndExecuteNewDiagnosisInstance;
begin
  AlertCreate(fMain, AlrtNoSupport[CurrLang]);
  ShellExecute(0, 'open',
    PChar(EnvironmentVariable.AppPath + 'SSDTools.exe'),
    PChar('/diag'), nil, SW_SHOW);
end;

procedure TSSDLabelListRefresher.FreeChangesList;
begin
  FreeAndNil(ChangesList.Added);
  FreeAndNil(ChangesList.Deleted);
end;

procedure TSSDLabelListRefresher.SetChangesList;
var
  ListChangeGetter: TListChangeGetter;
begin
  ListChangeGetter := TListChangeGetter.Create;
  ListChangeGetter.IsOnlyGetSupportedDrives := true;
  ChangesList :=
    ListChangeGetter.RefreshListWithResultFrom(fMain.IdentifiedDriveList);
  FreeAndNil(ListChangeGetter);
end;

function TSSDLabelListRefresher.IsNoSupportedDriveExists: Boolean;
begin
  result := fMain.IdentifiedDriveList.Count = 0;
end;

procedure TSSDLabelListRefresher.DeleteAndAddDevicesByResultList;
begin
  DeleteByDeletedList;
  AddByAddedList;
end;

procedure TSSDLabelListRefresher.DeleteByDeletedList;
var
  CurrentEntry: String;
begin
  for CurrentEntry in ChangesList.Deleted do
    DeleteDevice(CurrentEntry);
end;

procedure TSSDLabelListRefresher.AddByAddedList;
var
  CurrentEntry: IPhysicalDrive;
begin
  for CurrentEntry in ChangesList.Added do
    AddDevice(CurrentEntry);
end;

procedure TSSDLabelListRefresher.DeleteDevice(const Path: String);
begin
  if not SSDLabel.IsExistsByPath(Path) then
    exit;
  SSDLabel.Delete(SSDLabel.IndexOfByPath(Path));
end;

procedure TSSDLabelListRefresher.AddDevice(const Entry: IPhysicalDrive);
begin
  SSDLabel.Add(TSSDLabel.Create(Entry));
end;

function TSSDLabelListRefresher.ChangeExists: Boolean;
begin
  result :=
    (ChangesList.Added.Count > 0) or
    (ChangesList.Deleted.Count > 0);
end;

procedure TSSDLabelListRefresher.SetFirstDeviceAsSelected;
begin
  SSDLabel[0].OnClick(SSDLabel[0]);
end;

procedure TSSDLabelListRefresher.SetFirstDeviceAsSelectedIfNoDeviceSelected;
begin
  if ChangeExists then
    SetFirstDeviceAsSelected;
end;

procedure TSSDLabelListRefresher.RefreshMainFormAndSetNewSelection;
begin
  DeleteAndAddDevicesByResultList;
  SetFirstDeviceAsSelectedIfNoDeviceSelected;
end;

procedure TSSDLabelListRefresher.RefreshDrives(
  const SSDLabelToRefresh: TSSDLabelList);
begin
  SSDLabel := SSDLabelToRefresh;
  SetChangesList;
  if IsNoSupportedDriveExists then
    AlertAndExecuteNewDiagnosisInstance
  else
    RefreshMainFormAndSetNewSelection;
  FreeChangesList;
end;

end.
