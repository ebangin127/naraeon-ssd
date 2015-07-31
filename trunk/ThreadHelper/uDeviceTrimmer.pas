unit uDeviceTrimmer;

interface

uses
  uOSFile, uPartitionExtentGetter, uAutoCommandSet;

type
  TPendingTrimOperation = record
    IsUnusedSpaceFound: Boolean;
    StartLBA: UInt64;
    LengthInLBA: UInt64;
  end;
  
  TDeviceTrimmer = class(TOSFile)
  private
    PendingTrimOperation: TPendingTrimOperation;
    AutoCommandSet: TAutoCommandSet;

    function GetMotherDrivePath: String;
    procedure IncreaseOrSetTrimPosition;
    procedure SetThisPositionAsStart;
  public
    constructor Create(FileToGetAccess: String); override;
    destructor Destroy; override;
    procedure Flush;
    procedure SetStartPoint(StartLBA, LengthInLBA: UInt64);
    procedure IncreaseLength(LengthInLBA: UInt64);
    function IsUnusedSpaceFound: Boolean;
    function IsLBACountOverLimit: Boolean;
  end;

implementation

{ TDeviceTrimmer }

constructor TDeviceTrimmer.Create(FileToGetAccess: String); 
begin
  inherited;
  SetCommandSet;
  ZeroMemory(@PendingTrimOperation, SizeOf(PendingTrimOperation));
end;

destructor TDeviceTrimmer.Destroy; override;
begin
  FreeAndNil(AutoCommandSet);
end;

procedure TDeviceTrimmer.SetCommandSet;
begin
  AutoCommandSet := TAutoCommandSet.Create(GetMotherDrivePath);
  AutoCommandSet.IdentifyDevice;
end;

function TDeviceTrimmer.GetMotherDrivePath: String;
var
  PartitionExtentGetter: TPartitionExtentGetter;
  PartitionExtentList: TPartitionExtentList;
begin
  PartitionExtentGetter :=
    TPartitionExtentGetter.Create(GetPathOfFileAccessing);
  PartitionExtentList := PartitionExtentGetter.GetPartitionExtentList;
  result :=
    ThisComputerPrefix +
    PhysicalDrivePrefix +
    IntToStr(PartitionExtentList[0].DriveNumber);
  FreeAndNil(PartitionExtentList);
  FreeAndNil(PartitionExtentGetter);
end;

procedure TDeviceTrimmer.SetStartPoint(StartLBA, LengthInLBA: UInt64);
begin
  PendingTrimOperation.IsUnusedSpaceFound := true;
  PendingTrimOperation.StartLBA := StartLBA;
  PendingTrimOperation.LengthInLBA := LengthInLBA;
end;

procedure TDeviceTrimmer.IncreaseLength(LengthInLBA: UInt64);
begin
  PendingTrimOperation.LengthInLBA := 
    PendingTrimOperation.LengthInLBA +
    LengthInLBA;
end;

procedure TDeviceTrimmer.Flush;
begin
  AutoCommandSet.DataSetManagement(
    PendingTrimOperation.StartLBA,
    PendingTrimOperation.LengthInLBA);
  ZeroMemory(@PendingTrimOperation, SizeOf(PendingTrimOperation));
end;

function TDeviceTrimmer.IsLBACountOverLimit: Boolean;
const
  LimitLengthInLBA = 65500;
begin
  result := PendingTrimOperation.LengthInLBA > LimitLengthInLBA;
end;

function TDeviceTrimmer.IsUnusedSpaceFound: Boolean;
begin
  result := PendingTrimOperation.IsUnusedSpaceFound;
end;

end.
