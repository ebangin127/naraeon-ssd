unit uListChangeGetter;

interface

uses
  SysUtils, 
  uPhysicalDrive, uPhysicalDriveList;

type
  TChangesList = record
    Added: TPhysicalDriveList;
    Deleted: TPhysicalDriveList;
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
      var ListToRefresh: TPhysicalDriveList): TPhysicalDriveList;
    function ReturnDeletedListAndRefreshList(
      var ListToRefresh: TPhysicalDriveList): TPhysicalDriveList;
  public
    property IsOnlyGetSupportedDrives: Boolean
      read InnerIsOnlyGetSupportedDrives write InnerIsOnlyGetSupportedDrives;
    procedure RefeshListWithoutResultFrom(
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
  
procedure TListChangeGetter.RefeshListWithoutResultFrom(
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
  var ListToRefresh: TPhysicalDriveList): TPhysicalDriveList;
var
  CurrentEntry: TPhysicalDrive;
  IsExistsInPreviousList: Boolean;
begin
  result := TPhysicalDriveList.Create;

  for CurrentEntry in CurrentPhysicalDriveList do
  begin
    IsExistsInPreviousList := ListToRefresh.IsExists(CurrentEntry);

    if IsSupportedOrNotNeededToCheck(CurrentEntry.SupportStatus.Supported) then
      ListToRefresh.Add(TPhysicalDrive.Create
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
  var ListToRefresh: TPhysicalDriveList): TPhysicalDriveList;
var
  ItemIndexOfListToRefresh: Integer;
  IsExistsInCurrentList: Boolean;
begin
  result := nil;
  if not IsResultNeeded then
    exit;
    
  result := TPhysicalDriveList.Create;

  for ItemIndexOfListToRefresh := 0 to ListToRefresh.Count - 1 do
  begin
    IsExistsInCurrentList :=
      ListToRefresh.IsExists(ListToRefresh[ItemIndexOfListToRefresh]);

    if not IsExistsInCurrentList then
      result.Add(TPhysicalDrive.Create
        (StrToInt(
          ListToRefresh[ItemIndexOfListToRefresh].
            GetPathOfFileAccessingWithoutPrefix)));
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
