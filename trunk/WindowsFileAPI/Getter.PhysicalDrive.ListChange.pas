unit Getter.PhysicalDrive.ListChange;

interface

uses
  SysUtils, Classes,
  Device.PhysicalDrive, Device.PhysicalDrive.List, Getter.PhysicalDriveList;

type
  TChangesList = record
    Added: TPhysicalDriveList;
    Deleted: TStringList;
  end;
  
  TListChangeGetter = class
  private
    type
      TRefreshedListAndChanges = record
        RefreshedList: TPhysicalDriveList;
        Changes: TChangesList;
      end;
  private
    InnerIsOnlyGetSupportedDrives: Boolean;
    IsResultNeeded: Boolean;
    ListToRefresh: TPhysicalDriveList;
    CurrentPhysicalDriveList: TPhysicalDriveList;
    function GetListChangeByCurrentPhysicalDriveList:
      TRefreshedListAndChanges;
    function IsSupportedOrNotNeededToCheck(
      IsSupported: Boolean): Boolean;
    function ReturnAddedListAndRefreshList(
      var NewList: TPhysicalDriveList): TPhysicalDriveList;
    function ReturnDeletedListAndRefreshList(
      var NewList: TPhysicalDriveList): TStringList;
    function InnerRefreshListWithResultFrom(
      var ListToRefresh: TPhysicalDriveList; IsService: Boolean): TChangesList;
    function GetPhysicalDriveList(
      IsService: Boolean): TPhysicalDriveList;
  public
    property IsOnlyGetSupportedDrives: Boolean
      read InnerIsOnlyGetSupportedDrives write InnerIsOnlyGetSupportedDrives;
    procedure RefreshListWithoutResultFrom(
      var ListToRefresh: TPhysicalDriveList);
    function RefreshListWithResultFrom(
      var ListToRefresh: TPhysicalDriveList): TChangesList;
    function ServiceRefreshListWithResultFrom(
      var ListToRefresh: TPhysicalDriveList): TChangesList;
  end;

implementation

uses
  Getter.PhysicalDriveList.Auto, Getter.PhysicalDriveList.BruteForce;

function TListChangeGetter.IsSupportedOrNotNeededToCheck(
  IsSupported: Boolean): Boolean;
begin
  result :=
    (IsSupported) or
    (not IsOnlyGetSupportedDrives);
end;
  
procedure TListChangeGetter.RefreshListWithoutResultFrom(
  var ListToRefresh: TPhysicalDriveList);
begin
  IsResultNeeded := false;
  RefreshListWithResultFrom(ListToRefresh);
end;

function TListChangeGetter.GetPhysicalDriveList(IsService: Boolean):
  TPhysicalDriveList;
begin
  if not IsService then
    result := AutoPhysicalDriveListGetter.GetPhysicalDriveList
  else
    result := AutoPhysicalDriveListGetter.GetPhysicalDriveListInService;
end;
  
function TListChangeGetter.RefreshListWithResultFrom(
  var ListToRefresh: TPhysicalDriveList): TChangesList;
begin
  result := InnerRefreshListWithResultFrom(ListToRefresh, false);
end;

function TListChangeGetter.ServiceRefreshListWithResultFrom(
  var ListToRefresh: TPhysicalDriveList): TChangesList;
begin
  result := InnerRefreshListWithResultFrom(ListToRefresh, true);
end;

function TListChangeGetter.InnerRefreshListWithResultFrom(
  var ListToRefresh: TPhysicalDriveList; IsService: Boolean): TChangesList;
var
  RefreshedListAndChanges: TRefreshedListAndChanges;
begin
  IsResultNeeded := true;
  self.ListToRefresh := ListToRefresh;
  CurrentPhysicalDriveList := GetPhysicalDriveList(IsService);
  RefreshedListAndChanges := GetListChangeByCurrentPhysicalDriveList;
  FreeAndNil(ListToRefresh);
  ListToRefresh := RefreshedListAndChanges.RefreshedList;
  result := RefreshedListAndChanges.Changes;
  FreeAndNil(CurrentPhysicalDriveList);
end;
  
function TListChangeGetter.ReturnAddedListAndRefreshList(
  var NewList: TPhysicalDriveList): TPhysicalDriveList;
var
  CurrentEntry: IPhysicalDrive;
  IsExistsInPreviousList: Boolean;
  CanBeListed: Boolean;
  NewPath: String;
begin
  result := TPhysicalDriveList.Create;

  for CurrentEntry in CurrentPhysicalDriveList do
  begin
    IsExistsInPreviousList := ListToRefresh.IsExists(CurrentEntry);
    CanBeListed :=
      IsSupportedOrNotNeededToCheck(CurrentEntry.SupportStatus.Supported);
    NewPath :=
      TPhysicalDrive.BuildFileAddressByNumber(
        StrToInt(CurrentEntry.GetPathOfFileAccessingWithoutPrefix));
    if CanBeListed then
      NewList.Add(TPhysicalDrive.Create(NewPath));
    if not IsResultNeeded then
      Continue;
    if (not IsExistsInPreviousList) and (CanBeListed) then
      result.Add(TPhysicalDrive.Create(NewPath));
  end;
end;
  
function TListChangeGetter.ReturnDeletedListAndRefreshList(
  var NewList: TPhysicalDriveList): TStringList;
var
  ItemIndexOfListToRefresh: Integer;
  IsExistsInNewList: Boolean;
begin
  result := nil;
  if not IsResultNeeded then
    exit;
  result := TStringList.Create;
  for ItemIndexOfListToRefresh := 0 to ListToRefresh.Count - 1 do
  begin
    IsExistsInNewList :=
      NewList.IsExists(ListToRefresh[ItemIndexOfListToRefresh]);
    if not IsExistsInNewList then
      result.Add(ListToRefresh[ItemIndexOfListToRefresh].
        GetPathOfFileAccessing);
  end;
end;

function TListChangeGetter.GetListChangeByCurrentPhysicalDriveList:
  TRefreshedListAndChanges;
begin
  result.RefreshedList := TPhysicalDriveList.Create;
  result.Changes.Added :=
    ReturnAddedListAndRefreshList(result.RefreshedList);
  result.Changes.Deleted :=
    ReturnDeletedListAndRefreshList(result.RefreshedList);
end;
end.
