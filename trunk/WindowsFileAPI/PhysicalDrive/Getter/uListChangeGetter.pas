unit uListChangeGetter;

interface

uses
  SysUtils, Classes,
  uPhysicalDrive, uPhysicalDriveList;

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
    procedure GetCurrentPhysicalDriveList;
    function GetListChangeByCurrentPhysicalDriveList:
      TRefreshedListAndChanges;
    function IsSupportedOrNotNeededToCheck(
      IsSupported: Boolean): Boolean;
    function ReturnAddedListAndRefreshList(
      var NewList: TPhysicalDriveList): TPhysicalDriveList;
    function ReturnDeletedListAndRefreshList(
      var NewList: TPhysicalDriveList): TStringList;
  public
    property IsOnlyGetSupportedDrives: Boolean
      read InnerIsOnlyGetSupportedDrives write InnerIsOnlyGetSupportedDrives;
    procedure RefreshListWithoutResultFrom(
      var ListToRefresh: TPhysicalDriveList);
    function RefreshListWithResultFrom(
      var ListToRefresh: TPhysicalDriveList): TChangesList;
  end;

implementation

uses uAutoPhysicalDriveListGetter;

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

procedure TListChangeGetter.GetCurrentPhysicalDriveList;
var
  AutoPhysicalDriveListGetter: TAutoPhysicalDriveListGetter;
begin
  AutoPhysicalDriveListGetter := TAutoPhysicalDriveListGetter.Create;
  CurrentPhysicalDriveList :=
    AutoPhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(AutoPhysicalDriveListGetter);
end;
  
function TListChangeGetter.RefreshListWithResultFrom(
  var ListToRefresh: TPhysicalDriveList): TChangesList;
var
  RefreshedListAndChanges: TRefreshedListAndChanges;
begin
  IsResultNeeded := true;
  self.ListToRefresh := ListToRefresh;
  GetCurrentPhysicalDriveList;
  RefreshedListAndChanges := GetListChangeByCurrentPhysicalDriveList;
  FreeAndNil(ListToRefresh);
  ListToRefresh := RefreshedListAndChanges.RefreshedList;
  result := RefreshedListAndChanges.Changes;
  FreeAndNil(CurrentPhysicalDriveList);
end;
  
function TListChangeGetter.ReturnAddedListAndRefreshList(
  var NewList: TPhysicalDriveList): TPhysicalDriveList;
var
  CurrentEntry: TPhysicalDrive;
  IsExistsInPreviousList: Boolean;
begin
  result := TPhysicalDriveList.Create;

  for CurrentEntry in CurrentPhysicalDriveList do
  begin
    IsExistsInPreviousList := ListToRefresh.IsExists(CurrentEntry);

    if IsSupportedOrNotNeededToCheck(CurrentEntry.SupportStatus.Supported) then
      NewList.Add(TPhysicalDrive.Create
        (StrToInt(CurrentEntry.GetPathOfFileAccessingWithoutPrefix)));

    if not IsResultNeeded then
      Continue;
      
    if (not IsExistsInPreviousList) and
       (IsSupportedOrNotNeededToCheck(CurrentEntry.SupportStatus.Supported))
       then
      result.Add(TPhysicalDrive.Create
        (StrToInt(CurrentEntry.GetPathOfFileAccessingWithoutPrefix)));
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
