unit uMockDeviceTrimmer;

interface

uses
  SysUtils, Generics.Collections, Windows,
   OSFile;

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
    class var TrimOperationList: TTrimOperationList;
  public
    constructor Create(const FileToGetAccess: String); override;
    procedure Flush;
    procedure SetStartPoint(StartLBA, LengthInLBA: UInt64);
    procedure IncreaseLength(LengthInLBA: UInt64);
    function IsUnusedSpaceFound: Boolean;
    function IsLBACountOverLimit: Boolean;
    class procedure CreateTrimOperationLogger;
    class procedure FreeTrimOperationLogger;
    class function GetTrimOperationLogger: TTrimOperationList;
  end;

implementation

{ TDeviceTrimmer }

constructor TDeviceTrimmer.Create(const FileToGetAccess: String); 
begin
  inherited;
  ZeroMemory(@PendingTrimOperation, SizeOf(PendingTrimOperation));
end;

class procedure TDeviceTrimmer.CreateTrimOperationLogger;
begin
  TrimOperationList := TTrimOperationList.Create;
end;

class procedure TDeviceTrimmer.FreeTrimOperationLogger;
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

class function TDeviceTrimmer.GetTrimOperationLogger: TTrimOperationList;
begin
  result := TrimOperationList
end;

end.
