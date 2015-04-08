unit uPhysicalDriveListGetter;

interface

uses
  uOSFile, uPhysicalDriveList;

type
  TPhysicalDriveListGetter = class abstract
  public
    PhysicalDriveList: TPhysicalDriveList;
    function GetPhysicalDriveList: TPhysicalDriveList; virtual; abstract;
  end;

implementation

end.
