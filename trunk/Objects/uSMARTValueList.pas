unit uSMARTValueList;

interface

uses
  Generics.Collections;

type
  TSMARTValueEntry = record
    ID: Byte;
    Current: Byte;
    Worst: Byte;
    RAW: UInt64;
  end;

  TSMARTValueList = class(TList<TSMARTValueEntry>)
  public
    function IndexByID(ID: Byte): Integer;
  end;

implementation

{ TSMARTValueList }

function TSMARTValueList.IndexByID(ID: Byte): Integer;
var
  CurrentEntryNumber: Integer;
begin
  for CurrentEntryNumber := 0 to (Self.Count - 1) do
    if Self[CurrentEntryNumber].ID = ID then
      exit(CurrentEntryNumber);
end;

end.
