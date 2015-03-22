unit uPhysicalDriveGetter;

interface

uses
  uOSFile, uPhysicalDriveList;

type
  TPhysicalDriveGetter = class abstract(TOSFile)
  public
    PhysicalDriveList: TPhysicalDriveList;
    function GetPhysicalDriveList: TPhysicalDriveList; virtual; abstract;
  end;

implementation

end.
