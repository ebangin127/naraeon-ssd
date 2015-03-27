unit uPhysicalDriveGetter;

interface

uses
  uOSFile, uPhysicalDriveList;

type
  TPhysicalDriveGetter = class abstract
  public
    PhysicalDriveList: TPhysicalDriveList;
    function GetPhysicalDriveList: TPhysicalDriveList; virtual; abstract;
  end;

implementation

end.
