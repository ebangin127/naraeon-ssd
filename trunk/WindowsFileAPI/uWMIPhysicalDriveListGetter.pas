unit uWMIPhysicalDriveListGetter;

interface

uses
  Windows, ActiveX, ComObj, Variants, SysUtils,
  uOSFile, uPhysicalDriveGetter, uPhysicalDrive, uPhysicalDriveList;

type
  TWMIPhysicalDriveListGetter = class(TPhysicalDriveGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    CurrentDrive: OleVariant;
    PhysicalDriveList: TPhysicalDriveList;
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
    MkParseDisplayName(OleCtx,
      PWideChar(ObjectPath),
      LengthOfURISuccessfullyParsed,
      result));
end;

function TWMIPhysicalDriveListGetter.GetReferredObjectByMoniker
  (DefaultMoniker: IMoniker; BindableContext: IBindCtx): IDispatch;
var
  LengthOfURISuccessfullyParsed: Integer;
begin
  OleCheck(
    OleMoniker.BindToObject(OleCtx, nil, IUnknown, result));
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
  result := Pos('hard', LowerCase(IsFixedMedia)) >= 0;
end;

procedure TWMIPhysicalDriveListGetter.AddDriveToList;
var
  PhysicalDrive: TPhysicalDrive;
  EntryToAdd: TPhysicalDriveEntry;
begin
  PhysicalDrive := TPhysicalDrive.Create(CurrentDrive.DeviceID);

  CurrEntry.DeviceName := PhysicalDrive.GetPathOfFileAccessingWithoutPrefix;
  CurrEntry.IsUSBDevice := OleDrives.InterfaceType = 'USB';
  PhysicalDriveList.Add(EntryToAdd);
  
  FreeAndNil(PhysicalDrive);
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

procedure TWMIPhysicalDriveListGetter.CheckMediaTypeAndAddIfRequirementMet
  (DiskDriveSearchResult: IEnumVARIANT);
begin
  if IsHarddrive(CurrentDrive.MediaType) then
    IfDriveConnectedByKnownInterfaceAddToList;
end;

function TWMIPhysicalDriveListGetter.IsDriveSetValidType: Boolean;
begin
  result := (not VarIsNull(OleDrives.MediaType));
end;

function TWMIPhysicalDriveListGetter.IsDriveLoaded: Boolean;
begin
  result := (CurrentDrive.MediaLoaded);
end;

function TWMIPhysicalDriveListGetter.IsDriveSetValidID: Boolean;
begin
  result := (not VarIsNull(CurrentDrive.DeviceID <> ''));
end;

function TWMIPhysicalDriveListGetter.IsCurrentDriveAvailable
  (DiskDriveSearchResult: IEnumVARIANT): Boolean;
begin
  result :=
    IsDriveSetValidID and
    IsDriveLoaded and
    IsDriveSetValidType;
end;

function TWMIPhysicalDriveListGetter.IfFixedOrUSBDriveAddToList
  (DiskDriveSearchResult: IEnumVARIANT): Boolean;
begin
  if IsCurrentDriveAvailable then
    CheckMediaTypeAndAddIfRequirementMet;
  
  CurrentDrive := Unassigned;
end;

function TWMIPhysicalDriveListGetter.TraverseResultAndAddFixedOrUSBDrive
  (DiskDriveSearchResult: IEnumVARIANT);
var
  DriveReturned: LongWord;
begin
  while GetNextDriveAndReturnResult do
    IfFixedOrUSBDriveAddToList;
end;

function TWMIPhysicalDriveListGetter.TryToGetPhysicalDriveList:
  TPhysicalDriveList;
var
  WMIObject: IDispatch;
  DiskDriveSearchResult: IEnumVARIANT;
begin
  WMIObject := ConnectToWMIObjectByMoniker;
  DiskDriveSearchResult := GetDiskDriveSearchResult(WMIObject);
  TraverseResultAndAddFixedAndUSBDrive(DiskDriveSearchResult);
end;

function TWMIPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  try
    PhysicalDriveList := TPhysicalDriveList.Create;
    TryToGetPhysicalDriveList;
  except
    FreeAndNil(PhysicalDriveList);
  end;
  result := PhysicalDriveList;
end;

end.
