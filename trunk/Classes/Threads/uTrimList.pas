unit uTrimList;

interface

uses
  Generics.Collections;

type
  TTrimListNextStat = (NEXTSTAT_SUCCESS, NEXTSTAT_NOTCMPL);
  TTrimListNext = record
    Status: TTrimListNextStat;
    PartitionName: String;
  end;
  TTrimList = class(TList<String>)
  private
    FCurrPartition: Integer;
    FCompletedPartition: Integer;
  public
    property CurrPartition: Integer
      read FCurrPartition;
    property CompletedPartition: Integer
      read FCompletedPartition;

    procedure PointerToFirst;
    function GetNextPartition: TTrimListNext;
    procedure MarkAsCompleted;
  end;

implementation


procedure TTrimList.PointerToFirst;
begin
  FCurrPartition := -1;
  FCompletedPartition := -1;
end;

function TTrimList.GetNextPartition: TTrimListNext;
begin
  if FCurrPartition < FCompletedPartition then
  begin
    result.Status := NEXTSTAT_NOTCMPL;
    exit;
  end;

  FCurrPartition := FCurrPartition + 1;
  result.Status := NEXTSTAT_SUCCESS;
  result.PartitionName := self[CurrPartition];
end;

procedure TTrimList.MarkAsCompleted;
begin
  FCompletedPartition := FCurrPartition;
end;

end.
