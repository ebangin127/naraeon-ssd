unit uPartition;

interface

uses
  uOSFile,
  uPartitionExtentGetter;

type
  TPartition = class(TOSFile)
  private
    PartitionExtentList: TPartitionExtentList;
    PartitionLengthReadWrite: TLargeInteger;
    
    function GetPartitionLengthOrRequestAndReturn: TLargeInteger;
  public
    property PartitionLength: TLargeInteger 
      read GetPartitionLengthOrRequestAndReturn;
    
    function GetPartitionExtentList: TPartitionExtentList;
  end;

implementation

function TPartition.RequestPartitionExtentList: PartitionExtentList;
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

procedure TPartition.GetPartitionLengthFromPartitionExtentList;
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
  PartitionLengthReadWrite := GetPartitionLengthFromPartitionExtentList;
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
