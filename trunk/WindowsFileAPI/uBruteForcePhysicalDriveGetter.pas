unit uBruteForcePhysicalDriveGetter;

interface

uses
  uOSFile, uPhysicalDriveList;

type
  TBruteForcePhysicalDriveGetter = class abstract(TOSFile)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  end;

implementation

{ TBruteForcePhysicalDriveGetter }

function TBruteForcePhysicalDriveGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin

end;

end.
