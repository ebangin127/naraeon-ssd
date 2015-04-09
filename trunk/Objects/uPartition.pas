unit uPartition;

interface

uses
  Windows, SysUtils,
  uOSFile, uPartitionExtentGetter;

type
  TPartition = class(TOSFile)
  private
    PartitionExtentList: TPartitionExtentList;
    PartitionLengthReadWrite: TLargeInteger;
    
    function GetPartitionLengthOrRequestAndReturn: TLargeInteger;
    procedure AddThisEntryToPartitionLength(
      PartitionExtentEntry: TPartitionExtentEntry);

    procedure SetPartitionLengthFromPartitionExtentList;
    function RequestPartitionExtentList: TPartitionExtentList;
    procedure RequestPartitionLength;

  public
    property PartitionLength: TLargeInteger 
      read GetPartitionLengthOrRequestAndReturn;
    
    function GetPartitionExtentList: TPartitionExtentList;
  end;

implementation

function TPartition.RequestPartitionExtentList: TPartitionExtentList;
var
  PartitionExtentGetter: TPartitionExtentGetter;
begin
  PartitionExtentGetter := TPartitionExtentGetter.Create
    (GetPathOfFileAccessing);
  result := PartitionExtentGetter.GetPartitionExtentList;
  FreeAndNil(PartitionExtentGetter);
end;

procedure TPartition.AddThisEntryToPartitionLength
  (PartitionExtentEntry: TPartitionExtentEntry);
begin
  PartitionLengthReadWrite :=
    PartitionLengthReadWrite +
    PartitionExtentEntry.ExtentLength;
end;

procedure TPartition.SetPartitionLengthFromPartitionExtentList;
var
  PartitionExtentEntry: TPartitionExtentEntry;
  PartitionExtentListToGetLength: TPartitionExtentList;
begin
  PartitionLengthReadWrite := 0;
  PartitionExtentListToGetLength := RequestPartitionExtentList;
  for PartitionExtentEntry in PartitionExtentListToGetLength do
    AddThisEntryToPartitionLength(PartitionExtentEntry);
  FreeAndNil(PartitionExtentListToGetLength);
end;

procedure TPartition.RequestPartitionLength;
begin
  SetPartitionLengthFromPartitionExtentList;
end;

function TPartition.GetPartitionLengthOrRequestAndReturn: TLargeInteger;
begin
  if PartitionLengthReadWrite = 0 then
    RequestPartitionLength;
  exit(PartitionLengthReadWrite);
end;

function TPartition.GetPartitionExtentList: TPartitionExtentList;
begin
  if PartitionExtentList = nil then
    PartitionExtentList := RequestPartitionExtentList;
  result := PartitionExtentList;
end;

end.
