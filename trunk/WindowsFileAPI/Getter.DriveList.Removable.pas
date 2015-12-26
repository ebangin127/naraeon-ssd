unit Getter.DriveList.Removable;

interface

uses
  Windows,
  Getter.DriveList;

type
  TRemovableDriveListGetter = class sealed(TDriveListGetter)
  protected
    function GetDriveTypeToGet: Cardinal; override;
  end;

implementation

function TRemovableDriveListGetter.GetDriveTypeToGet: Cardinal; 
begin
  result := DRIVE_REMOVABLE;
end;

end.

