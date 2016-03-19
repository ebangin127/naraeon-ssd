unit Getter.DriveList.Removable;

interface

uses
  Windows,
  Getter.DriveList;

type
  TRemovableDriveListGetter = class sealed(TDriveListGetter)
  protected
    function GetDriveTypeToGet: TDriveSet; override;
  end;

implementation

function TRemovableDriveListGetter.GetDriveTypeToGet: TDriveSet;
begin
  result := [DRIVE_REMOVABLE];
end;

end.

