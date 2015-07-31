unit uMockDeviceTrimmer;

interface

uses
  Generics.Collections,
  uOSFile;

type
  TPendingTrimOperation = record
    IsUnusedSpaceFound: Boolean;
    StartLBA: UInt64;
    LengthInLBA: UInt64;
  end;
  TTrimOperationList = TList<TPendingTrimOperation>;
  
  TDeviceTrimmer = class(TOSFile)
  private
    PendingTrimOperation: TPendingTrimOperation;
    TrimOperationList: TTrimOperationList;

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
    function GetOperationList: TTrimOperationList;
  end;

implementation

{ TDeviceTrimmer }

constructor TDeviceTrimmer.Create(FileToGetAccess: String); 
begin
  inherited;
  TrimOperationList := TTrimOperationList.Create;
  ZeroMemory(@PendingTrimOperation, SizeOf(PendingTrimOperation));
end;

destructor TDeviceTrimmer.Destroy; override;
begin
  FreeAndNil(TrimOperationList);
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
  TrimOperationList.Add(PendingTrimOperation);
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

function TDeviceTrimmer.GetOperationList: TTrimOperationList;
begin
  result := TrimOperationList;
end;
end.
