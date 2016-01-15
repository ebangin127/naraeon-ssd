unit TrimList;

interface

uses
  Generics.Collections;

type
  TNextPartitionStatus = (Success, CompleteLastPartitionFirst);
  TTrimListEntry = record
    Status: TNextPartitionStatus;
    PartitionPath: String;
  end;
  TTrimList = class(TList<String>)
  private
    CurrentPartitionReadWrite: Integer;
    CompletedPartitionReadWrite: Integer;
    AtomicNextPartitionMonitor: TMonitor;
    function GetNextPartitionInCriticalSection: TTrimListEntry;
    function IsLastPartitionCompletedProperly: Boolean;
    function ChangePointerToNextPartitionAndGet: TTrimListEntry;
  public
    property CurrentPartition: Integer read CurrentPartitionReadWrite;
    property CompletedPartition: Integer read CompletedPartitionReadWrite;
    procedure PointerToFirst;
    function GetNextPartition: TTrimListEntry;
    procedure CompleteCurrentPartition;
  end;

implementation


procedure TTrimList.PointerToFirst;
begin
  CurrentPartitionReadWrite := -1;
  CompletedPartitionReadWrite := -1;
end;

function TTrimList.IsLastPartitionCompletedProperly: Boolean;
begin
  result := CurrentPartitionReadWrite >= CompletedPartitionReadWrite;
end;

function TTrimList.ChangePointerToNextPartitionAndGet: TTrimListEntry;
begin
  result.Status := TNextPartitionStatus.Success;
  result.PartitionPath := self[CurrentPartitionReadWrite];
  CurrentPartitionReadWrite := CurrentPartitionReadWrite + 1;
end;

function TTrimList.GetNextPartitionInCriticalSection: TTrimListEntry;
begin
  if not IsLastPartitionCompletedProperly then
  begin
    result.Status := TNextPartitionStatus.CompleteLastPartitionFirst;
    exit;
  end;

  result := ChangePointerToNextPartitionAndGet;
end;


function TTrimList.GetNextPartition: TTrimListEntry;
begin
  AtomicNextPartitionMonitor.Enter(self);
  result := GetNextPartitionInCriticalSection;
  AtomicNextPartitionMonitor.Exit(self);
end;

procedure TTrimList.CompleteCurrentPartition;
begin
  AtomicNextPartitionMonitor.Enter(self);
  CompletedPartitionReadWrite := CurrentPartitionReadWrite;
  AtomicNextPartitionMonitor.Exit(self);
end;

end.
