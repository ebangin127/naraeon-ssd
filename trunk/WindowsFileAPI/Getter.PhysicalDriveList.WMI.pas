unit Getter.PhysicalDriveList.WMI;

interface

uses
  Windows, ActiveX, ComObj, Variants, SysUtils, Dialogs,
  OSFile, Getter.PhysicalDriveList, Device.PhysicalDrive,
  Device.PhysicalDrive.List, CommandSet.Factory;

type
  TWMIPhysicalDriveListGetter = class sealed(TPhysicalDriveListGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    CurrentDrive: OleVariant;
    PhysicalDriveList: TPhysicalDriveList;
    procedure AddDriveToList;
    procedure CheckMediaTypeAndAddIfRequirementMet;
    function ConnectToWMIObjectByMoniker: IDispatch;
    function GetDefaultMonikerFromObjectPath(ObjectPath: String;
      BindableContext: IBindCtx): IMoniker;
    function GetDiskDriveSearchResult(WMIObject: IDispatch): IEnumVARIANT;
    function GetLocalhostWMIRepositoryURI: String;
    function GetMonikerBindableContext: IBindCtx;
    function GetNextDriveAndReturnResult(
      DiskDriveSearchResult: IEnumVARIANT): Boolean;
    function GetReferredObjectByMoniker(DefaultMoniker: IMoniker;
      BindableContext: IBindCtx): IDispatch;
    procedure IfDriveConnectedByKnownInterfaceAddToList;
    procedure IfFixedOrUSBDriveAddToList;
    function IsCurrentDriveAvailable: Boolean;
    function IsDriveConnectedBy(InterfaceName: String): Boolean;
    function IsDriveConnectedByKnownInterface: Boolean;
    function IsDriveLoaded: Boolean;
    function IsDriveSetValidID: Boolean;
    function IsDriveSetValidType: Boolean;
    function IsHarddrive(MediaType: String): Boolean;
    procedure TraverseResultAndAddFixedOrUSBDrive(
      DiskDriveSearchResult: IEnumVARIANT);
    procedure TryToGetPhysicalDriveList;
  end;

implementation

{ TWMIPhysicalDriveListGetter }

function TWMIPhysicalDriveListGetter.GetMonikerBindableContext: IBindCtx;
const
  ReservedAndMustBeZero = 0;
begin
  OleCheck(CreateBindCtx(ReservedAndMustBeZero, result));
end;

function TWMIPhysicalDriveListGetter.GetDefaultMonikerFromObjectPath
  (ObjectPath: String; BindableContext: IBindCtx): IMoniker;
var
  LengthOfURISuccessfullyParsed: Integer;
begin
  OleCheck(
    MkParseDisplayName(BindableContext,
      PWideChar(ObjectPath),
      LengthOfURISuccessfullyParsed,
      result));
end;

function TWMIPhysicalDriveListGetter.GetReferredObjectByMoniker
  (DefaultMoniker: IMoniker; BindableContext: IBindCtx): IDispatch;
begin
  OleCheck(
    DefaultMoniker.BindToObject(BindableContext, nil, IUnknown, result));
end;

function TWMIPhysicalDriveListGetter.GetLocalhostWMIRepositoryURI: String;
const
  WMIService = 'winmgmts:\\';
  Localhost = 'localhost\';
  WMIRepositoryPrefix = 'root\cimv2';
begin
  result := WMIService + Localhost + WMIRepositoryPrefix;
end;

function TWMIPhysicalDriveListGetter.ConnectToWMIObjectByMoniker: IDispatch;
var
  ContextToBindMoniker: IBindCtx;
  DefaultMoniker: IMoniker;
begin
  ContextToBindMoniker := GetMonikerBindableContext;
  DefaultMoniker :=
    GetDefaultMonikerFromObjectPath(
      GetLocalhostWMIRepositoryURI,
      ContextToBindMoniker);
  result :=
    GetReferredObjectByMoniker(
      DefaultMoniker,
      ContextToBindMoniker);
end;

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
    DiskDriveSearchResult.Next
      (OneDriveInformationNeeded,
       CurrentDrive,
       DriveReturned) =
       ThereIsAnotherDrive;
end;

function TWMIPhysicalDriveListGetter.IsHarddrive(MediaType: String): Boolean;
begin
  //Refer https://msdn.microsoft.com/en-us/library/aa394132%28v=vs.85%29.aspx
  result := Pos('hard', LowerCase(MediaType)) >= 0;
end;

procedure TWMIPhysicalDriveListGetter.AddDriveToList;
var
  PhysicalDrive: TPhysicalDrive;
begin
  try
    PhysicalDrive := TPhysicalDrive.Create(String(CurrentDrive.DeviceID));
    PhysicalDriveList.Add(PhysicalDrive);
  except
    on E: ENoCommandSetException do
    else raise;
  end;
end;

function TWMIPhysicalDriveListGetter.IsDriveConnectedBy
  (InterfaceName: String): Boolean;
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
     AddDriveToList;
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
  result := (not VarIsNull(CurrentDrive.DeviceID <> ''));
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

procedure TWMIPhysicalDriveListGetter.TraverseResultAndAddFixedOrUSBDrive
  (DiskDriveSearchResult: IEnumVARIANT);
begin
  while GetNextDriveAndReturnResult(DiskDriveSearchResult) do
    IfFixedOrUSBDriveAddToList;
end;

procedure TWMIPhysicalDriveListGetter.TryToGetPhysicalDriveList;
var
  WMIObject: IDispatch;
  DiskDriveSearchResult: IEnumVARIANT;
begin
  WMIObject := ConnectToWMIObjectByMoniker;
  DiskDriveSearchResult := GetDiskDriveSearchResult(WMIObject);
  TraverseResultAndAddFixedOrUSBDrive(DiskDriveSearchResult);
end;

function TWMIPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  PhysicalDriveList := TPhysicalDriveList.Create;
  try
    TryToGetPhysicalDriveList;
  except
    PhysicalDriveList.Clear;
  end;
  result := PhysicalDriveList;
end;

end.
