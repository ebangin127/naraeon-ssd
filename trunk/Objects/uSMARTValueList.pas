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
    function GetIndexByID(ID: Byte): Integer;
    function GetRAWByID(ID: Byte): UInt64;
  end;

implementation

{ TSMARTValueList }

function TSMARTValueList.GetIndexByID(ID: Byte): Integer;
var
  CurrentEntryNumber: Integer;
begin
  for CurrentEntryNumber := 0 to (Self.Count - 1) do
    if Self[CurrentEntryNumber].ID = ID then
      exit(CurrentEntryNumber);
end;

function TSMARTValueList.GetRAWByID(ID: Byte): UInt64;
var
  CurrentEntryNumber: Integer;
begin
  for CurrentEntryNumber := 0 to (Self.Count - 1) do
    if Self[CurrentEntryNumber].ID = ID then
      exit(Self[CurrentEntryNumber].RAW);
end;

end.
