unit uSSDLabelListRefresher;

interface

uses
  SysUtils, Generics.Collections,
  uPhysicalDrive, uListChangeGetter;

type
  TSSDLabelListRefresher = class
  private
    ChangesList: TChangesList;
  public
    procedure RefreshDrives;  
  end;

implementation

uses uMain;

procedure TSSDLabelListRefresher.AlertAndExecuteNewDiagnosisInstance;
begin
  AlertCreate(fMain, AlrtNoSupport[CurrLang]);
  ShellExecute(fMain.Handle, 'open',
    PChar(TPathManager.AppPath + 'SSDTools.exe'),
    PChar('/diag'), nil, SW_SHOW);
end;

procedure TSSDLabelListRefresher.FreeChangesList;
begin
  FreeAndNil(ChangesList.Added);
  FreeAndNil(ChangesList.Deleted);
end;

procedure TSSDLabelListRefresher.SetChangesList;
var
  ListChangesGetter: TListChangesGetter;
begin
  ListChangesGetter := TListChangesGetter.Create;
  ListChangesGetter.IsOnlyGetSupportedDrives := true;
  ChangesList :=
    ListChangesGetter.RefreshListWithResultFrom(fMain.PhysicalDriveList);
  FreeAndNil(ListChangesGetter);
end;

function TSSDLabelListRefresher.IsNoSupportedDriveExists: Boolean;
begin
  result := fMain.PhysicalDriveList.Count = 0;
end;

procedure TSSDLabelListRefresher.DeleteAndAddDevicesByResultList;
begin
  DeleteByDeletedList;
  AddByAddedList;
end;

procedure TSSDLabelListRefresher.DeleteByDeletedList;
var
  CurrentEntry: TPhysicalDrive;
begin
  for CurrentEntry in SSDList do
    DeleteDevice(CurrentEntry);
end;

procedure TSSDLabelListRefresher.AddByAddedList;
var
  CurrentEntry: TPhysicalDrive;
begin
  for CurrentEntry in SSDList do
    AddDevice(CurrentEntry);
end;

procedure TSSDLabelListRefresher.DeleteDevice(Entry: TPhysicalDrive);
begin
  if not fMain.SSDLabel.IsExists(Entry) then
    exit;
    
  fMain.SSDLabel.Delete(fMain.SSDLabel.IndexOf(Entry));
end;

procedure TSSDLabelListRefresher.AddDevice(Entry: TPhysicalDrive);
begin
  fMain.SSDLabel.Add(Entry);
end;

function TSSDLabelListRefresher.IsNoDeviceSelected: Boolean;
begin
  result := fMain.CurrDrive = '';
end;

procedure TSSDLabelListRefresher.SetFirstDeviceAsSelcted;
begin
  fMain.SSDLabel[0].OnClick(fMain.SSDLabel[0]);
end;

procedure TSSDLabelListRefresher.SetFirstDeviceAsSelectedIfNoDeviceSelected;
begin
  if IsNoDriveSelectedYet then
    SetFirstDeviceAsSelcted;
end;

procedure TSSDLabelListRefresher.RefreshMainFormAndSetNewSelection;
begin
  DeleteAndAddDevicesByResultList;
  SetFirstDeviceAsSelected;
end;

procedure TSSDLabelListRefresher.RefreshDrives;
begin
  SetChangesList;
  if IsNoSupportedDriveExists then
    AlertAndExecuteNewDiagnosisInstance;
  else
    RefreshMainFormAndSetNewSelection;
  FreeChangesList;
end;

end.
