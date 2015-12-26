unit Getter.PhysicalDriveList;

interface

uses
  OSFile, Device.PhysicalDrive.List;

type
  TPhysicalDriveListGetter = class abstract
  public
    PhysicalDriveList: TPhysicalDriveList;
    function GetPhysicalDriveList: TPhysicalDriveList; virtual; abstract;
  end;

implementation

end.
