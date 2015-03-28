unit uBufferInterpreter;

interface

uses
  uSMARTValueList;

type
  T512Buffer = Array[0..511] of Byte;
  TStorageInterface =
    (MODEL_NULL, MODEL_ATA, MODEL_SCSI, MODEL_DETERMINE);
  TSATASpeed =
    (NOT_SATA, SATA_UNKNOWN, SATA_150, SATA_300, SATA_600);
  TIdentifyDeviceResult = record
    Model: String;
    Firmware: String;
    Serial: String;
    UserSizeInKB: UInt64;
    SATASpeed: TSATASpeed;
    LBASize: Cardinal;
    StorageInterface: TStorageInterface;
  end;

  TBufferInterpreter = class abstract
  public
    function BufferToIdentifyDeviceResult
      (Buffer: T512Buffer): TIdentifyDeviceResult; virtual; abstract;
    function BufferToSMARTValueList
      (Buffer: T512Buffer): TSMARTValueList; virtual; abstract;
  end;

implementation

end.
