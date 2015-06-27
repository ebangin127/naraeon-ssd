unit uFixedDriveListGetter;

interface

uses
  Windows,
  uDriveListGetter;

type
  TFixedDriveListGetter = class sealed(TDriveListGetter)
  protected
    function GetDriveTypeToGet: Cardinal; override;
  end;

implementation

function TFixedDriveListGetter.GetDriveTypeToGet: Cardinal; 
begin
  result := DRIVE_FIXED;
end;

end.

