unit Getter.DriveList.Fixed;

interface

uses
  Windows,
  Getter.DriveList;

type
  TFixedDriveListGetter = class sealed(TDriveListGetter)
  protected
    function GetDriveTypeToGet: TDriveSet; override;
  end;

implementation

function TFixedDriveListGetter.GetDriveTypeToGet: TDriveSet;
begin
  result := [DRIVE_FIXED, DRIVE_REMOVABLE];
end;

end.

