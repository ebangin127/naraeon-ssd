unit Thread.Trim.Helper.Device;

interface

uses
  SysUtils, Windows,
  OSFile, OSFile.ForInternal, Getter.PartitionExtent, CommandSet,
  CommandSet.Factory;

type
  TPendingTrimOperation = record
    IsUnusedSpaceFound: Boolean;
    StartLBA: UInt64;
    LengthInLBA: UInt64;
  end;

  TDeviceTrimmer = class(TOSFileForInternal)
  private
    PendingTrimOperation: TPendingTrimOperation;
    CommandSet: TCommandSet;
    function GetMotherDrivePath: String;
    procedure SetCommandSet;
  public
    constructor Create(const FileToGetAccess: String); override;
    destructor Destroy; override;
    procedure Flush;
    procedure SetStartPoint(StartLBA, LengthInLBA: UInt64);
    procedure IncreaseLength(LengthInLBA: UInt64);
    function IsUnusedSpaceFound: Boolean;
    function IsLBACountOverLimit: Boolean;
  end;

implementation

{ TDeviceTrimmer }

constructor TDeviceTrimmer.Create(const FileToGetAccess: String); 
begin
  inherited;
  SetCommandSet;
  ZeroMemory(@PendingTrimOperation, SizeOf(PendingTrimOperation));
end;

destructor TDeviceTrimmer.Destroy;
begin
  FreeAndNil(CommandSet);
  inherited;
end;

procedure TDeviceTrimmer.SetCommandSet;
begin
  if CommandSet <> nil then
    exit;
  CommandSet := CommandSetFactory.GetSuitableCommandSet(GetMotherDrivePath);
  CommandSet.IdentifyDevice;
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
  CommandSet.DataSetManagement(
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
