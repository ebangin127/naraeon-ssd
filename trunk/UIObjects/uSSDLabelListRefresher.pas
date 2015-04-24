unit uSSDLabelListRefresher;

interface

uses
  Forms, SysUtils, Generics.Collections, Windows, ShellAPI,
  uLanguageSettings, uPathManager, uAlert, uPhysicalDrive, uListChangeGetter,
  uSSDLabel;

type
  TSSDLabelListRefresher = class
  private
    ChangesList: TChangesList;
    procedure AddByAddedList;
    procedure AddDevice(Entry: TPhysicalDrive);
    procedure AlertAndExecuteNewDiagnosisInstance;
    procedure DeleteAndAddDevicesByResultList;
    procedure DeleteByDeletedList;
    procedure DeleteDevice(Entry: TPhysicalDrive);
    procedure FreeChangesList;
    function IsNoDeviceSelected: Boolean;
    function IsNoSupportedDriveExists: Boolean;
    procedure RefreshMainFormAndSetNewSelection;
    procedure SetChangesList;
    procedure SetFirstDeviceAsSelected;
    procedure SetFirstDeviceAsSelectedIfNoDeviceSelected;
  public
    procedure RefreshDrives;  
  end;

implementation

uses uMain;

type
  THackMainform = TForm;

procedure TSSDLabelListRefresher.AlertAndExecuteNewDiagnosisInstance;
begin
  AlertCreate(fMain, AlrtNoSupport[CurrLang]);
  ShellExecute(0, 'open',
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
  ListChangeGetter: TListChangeGetter;
begin
  ListChangeGetter := TListChangeGetter.Create;
  ListChangeGetter.IsOnlyGetSupportedDrives := true;
  ChangesList :=
    ListChangeGetter.RefreshListWithResultFrom(fMain.PhysicalDriveList);
  FreeAndNil(ListChangeGetter);
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
  for CurrentEntry in ChangesList.Deleted do
    DeleteDevice(CurrentEntry);
end;

procedure TSSDLabelListRefresher.AddByAddedList;
var
  CurrentEntry: TPhysicalDrive;
begin
  for CurrentEntry in ChangesList.Added do
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
  fMain.SSDLabel.Add(TSSDLabel.Create(Entry));
end;

function TSSDLabelListRefresher.IsNoDeviceSelected: Boolean;
begin
  result := fMain.CurrDrive = '';
end;

procedure TSSDLabelListRefresher.SetFirstDeviceAsSelected;
begin
  fMain.SSDLabel[0].OnClick(fMain.SSDLabel[0]);
end;

procedure TSSDLabelListRefresher.SetFirstDeviceAsSelectedIfNoDeviceSelected;
begin
  if IsNoDeviceSelected then
    SetFirstDeviceAsSelected;
end;

procedure TSSDLabelListRefresher.RefreshMainFormAndSetNewSelection;
begin
  DeleteAndAddDevicesByResultList;
  SetFirstDeviceAsSelectedIfNoDeviceSelected;
end;

procedure TSSDLabelListRefresher.RefreshDrives;
begin
  SetChangesList;
  if IsNoSupportedDriveExists then
    AlertAndExecuteNewDiagnosisInstance
  else
    RefreshMainFormAndSetNewSelection;
  FreeChangesList;
end;

end.
