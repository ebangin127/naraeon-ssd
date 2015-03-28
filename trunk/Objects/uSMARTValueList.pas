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

  TSMARTValueList = TList<TSMARTValueEntry>;

implementation

end.
