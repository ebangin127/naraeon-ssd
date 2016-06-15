unit Getter.PhysicalDriveList.WMI;

interface

uses
  Classes, Windows, ActiveX, ComObj, Variants, SysUtils, Dialogs,
  Generics.Collections, Threading,
  Getter.PhysicalDriveList, Device.PhysicalDrive,
  Device.PhysicalDrive.List, CommandSet.Factory, WMI;

type
  TWMIPhysicalDriveListGetter = class sealed(TPhysicalDriveListGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    CurrentDrive: OleVariant;
    PhysicalDriveList: TThreadedPhysicalDriveList;
    DeviceIDList: TStringList;
    procedure AddDriveToDeviceIDList;
    procedure CheckMediaTypeAndAddIfRequirementMet;
    function GetDiskDriveSearchResult(WMIObject: IDispatch): IEnumVARIANT;
    function GetNextDriveAndReturnResult(
      DiskDriveSearchResult: IEnumVARIANT): Boolean;
    procedure IfDriveConnectedByKnownInterfaceAddToList;
    procedure IfFixedOrUSBDriveAddToList;
    function IsCurrentDriveAvailable: Boolean;
    function IsDriveConnectedBy(const InterfaceName: String): Boolean;
    function IsDriveConnectedByKnownInterface: Boolean;
    function IsDriveLoaded: Boolean;
    function IsDriveSetValidID: Boolean;
    function IsDriveSetValidType: Boolean;
    function IsHarddrive(const MediaType: String): Boolean;
    procedure TraverseResultAndAddFixedOrUSBDrive(
      DiskDriveSearchResult: IEnumVARIANT);
    procedure TryToGetPhysicalDriveList;
    function LockAndTransfer: TPhysicalDriveList;
    procedure AddDriveToList(const DeviceID: String);
  end;

implementation

{ TWMIPhysicalDriveListGetter }

function TWMIPhysicalDriveListGetter.GetDiskDriveSearchResult
  (WMIObject: IDispatch): IEnumVARIANT;
const
  SelectAllDiskDrive = 'Select * from Win32_DiskDrive';
var
  ResultAsOleVariant: OleVariant;
begin
  ResultAsOleVariant :=
    OleVariant(WMIObject).ExecQuery(SelectAllDiskDrive);
  result :=
    IUnknown(ResultAsOleVariant._NewEnum) as IEnumVARIANT;
end;

function TWMIPhysicalDriveListGetter.GetNextDriveAndReturnResult
  (DiskDriveSearchResult: IEnumVARIANT): Boolean;
const
  OneDriveInformationNeeded = 1;
  ThereIsAnotherDrive = 0;
var
  DriveReturned: Cardinal;
begin
  result :=
    DiskDriveSearchResult.Next(
      OneDriveInformationNeeded,
      CurrentDrive,
      DriveReturned) =
      ThereIsAnotherDrive;
end;

function TWMIPhysicalDriveListGetter.IsHarddrive(const MediaType: String):
  Boolean;
begin
  //Refer https://msdn.microsoft.com/en-us/library/aa394132%28v=vs.85%29.aspx
  result := Pos('hard', LowerCase(MediaType)) >= 0;
end;

procedure TWMIPhysicalDriveListGetter.AddDriveToDeviceIDList;
begin
  DeviceIDList.Add(CurrentDrive.DeviceID);
end;

procedure TWMIPhysicalDriveListGetter.AddDriveToList(const DeviceID: String);
var
  PhysicalDrive: TPhysicalDrive;
begin
  try
    PhysicalDrive := TPhysicalDrive.Create(String(DeviceID));
    PhysicalDriveList.Add(PhysicalDrive);
  except
    on E: ENoCommandSetException do;
    on E: ENoNVMeDriverException do;
    on OSError: EOSError do
      if OSError.ErrorCode <> 2 then raise;
    else raise;
  end;
end;

function TWMIPhysicalDriveListGetter.IsDriveConnectedBy
  (const InterfaceName: String): Boolean;
begin
  result := CurrentDrive.InterfaceType = InterfaceName;
end;

function TWMIPhysicalDriveListGetter.IsDriveConnectedByKnownInterface: Boolean;
begin
  result :=
    IsDriveConnectedBy('IDE') or
    IsDriveConnectedBy('SCSI') or
    IsDriveConnectedBy('USB');
end;

procedure TWMIPhysicalDriveListGetter.IfDriveConnectedByKnownInterfaceAddToList;
begin
  if IsDriveConnectedByKnownInterface then
     AddDriveToDeviceIDList;
end;

procedure TWMIPhysicalDriveListGetter.CheckMediaTypeAndAddIfRequirementMet;
begin
  if IsHarddrive(CurrentDrive.MediaType) then
    IfDriveConnectedByKnownInterfaceAddToList;
end;

function TWMIPhysicalDriveListGetter.IsDriveSetValidType: Boolean;
begin
  result := (not VarIsNull(CurrentDrive.MediaType));
end;

function TWMIPhysicalDriveListGetter.IsDriveLoaded: Boolean;
begin
  result := (CurrentDrive.MediaLoaded);
end;

function TWMIPhysicalDriveListGetter.IsDriveSetValidID: Boolean;
begin
  result := (not VarIsNull(CurrentDrive.DeviceID));
end;

function TWMIPhysicalDriveListGetter.IsCurrentDriveAvailable: Boolean;
begin
  result :=
    IsDriveSetValidID and
    IsDriveLoaded and
    IsDriveSetValidType;
end;

procedure TWMIPhysicalDriveListGetter.IfFixedOrUSBDriveAddToList;
begin
  if IsCurrentDriveAvailable then
    CheckMediaTypeAndAddIfRequirementMet;
  CurrentDrive := Unassigned;
end;

procedure TWMIPhysicalDriveListGetter.TraverseResultAndAddFixedOrUSBDrive(
  DiskDriveSearchResult: IEnumVARIANT);
begin
  DeviceIDList := TStringList.Create;
  try
    while GetNextDriveAndReturnResult(DiskDriveSearchResult) do
      IfFixedOrUSBDriveAddToList;
    TParallel.For(0, DeviceIDList.Count - 1, procedure (DeviceIDIndex: Integer)
    begin
      AddDriveToList(DeviceIDList[DeviceIDIndex]);
    end);
  finally
    FreeAndNil(DeviceIDList);
  end;
end;

procedure TWMIPhysicalDriveListGetter.TryToGetPhysicalDriveList;
var
  WMIObject: IDispatch;
  DiskDriveSearchResult: IEnumVARIANT;
begin
  WMIObject := WMIConnection.GetWMIConnection;
  DiskDriveSearchResult := GetDiskDriveSearchResult(WMIObject);
  TraverseResultAndAddFixedOrUSBDrive(DiskDriveSearchResult);
end;

function TWMIPhysicalDriveListGetter.LockAndTransfer:
  TPhysicalDriveList;
var
  LockedList: TList<IPhysicalDrive>;
begin
  LockedList := PhysicalDriveList.LockList;
  result := TPhysicalDriveList.Create;
  result.AddRange(LockedList.ToArray);
  PhysicalDriveList.UnlockList;
end;

function TWMIPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  PhysicalDriveList := TThreadedPhysicalDriveList.Create;
  try
    TryToGetPhysicalDriveList;
  except
    PhysicalDriveList.Clear;
  end;
  result := LockAndTransfer;
  FreeAndNil(PhysicalDriveList);
end;

end.
