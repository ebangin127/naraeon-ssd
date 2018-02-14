unit Device.SMART.List;

interface

uses
  SysUtils, Generics.Collections;

type
  EEntryNotFound = class(EArgumentException);
  TSMARTValueEntry = record
    ID: Byte;
    Current: Byte;
    Worst: Byte;
    Threshold: Byte;
    RAW: UInt64;
  end;
  TSMARTValueList = class(TList<TSMARTValueEntry>)
  public
    function GetIndexByID(ID: Byte): Integer;
    function GetRAWByID(ID: Byte): UInt64;
    function ExceptionFreeGetRAWByID(ID: Byte): UInt64;
    procedure MergeThreshold(const ThresholdList: TSMARTValueList);
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
  raise EEntryNotFound.Create('Entry not found with ID: ' + IntToStr(ID));
end;

function TSMARTValueList.GetRAWByID(ID: Byte): UInt64;
var
  CurrentEntryNumber: Integer;
begin
  for CurrentEntryNumber := 0 to (Self.Count - 1) do
    if Self[CurrentEntryNumber].ID = ID then
      exit(Self[CurrentEntryNumber].RAW);
  raise EEntryNotFound.Create('Entry not found with ID: ' + IntToStr(ID));
end;

function TSMARTValueList.ExceptionFreeGetRAWByID(ID: Byte): UInt64;
var
  CurrentEntryNumber: Integer;
begin
  for CurrentEntryNumber := 0 to (Self.Count - 1) do
    if Self[CurrentEntryNumber].ID = ID then
      exit(Self[CurrentEntryNumber].RAW);
  result := 0;
end;

procedure TSMARTValueList.MergeThreshold(const ThresholdList: TSMARTValueList);
var
  CurrentItem: TSMARTValueEntry;
  IndexInSelf: Integer;
  EntryToChange: TSMARTValueEntry;
begin
  for CurrentItem in ThresholdList do
  begin
    IndexInSelf := self.GetIndexByID(CurrentItem.ID);
    EntryToChange := self[IndexInSelf];
    EntryToChange.Threshold := CurrentItem.Threshold;
    self[IndexInSelf] := EntryToChange;
  end;
end;

end.
