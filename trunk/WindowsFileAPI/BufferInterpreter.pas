unit BufferInterpreter;

interface

uses
  Device.SMART.List, Device.SlotSpeed;

type
  TSmallBuffer = Array[0..511] of Byte;
  TLargeBuffer = Array[0..4096] of Byte;
  TStorageInterface =
    (Probing, ATA, SAT, SCSI, NVMe, UnknownInterface);
  TSATASpeed =
    (NotSATA, UnknownSATASpeed, SATA150, SATA300, SATA600);
  TIdentifyDeviceResult = record
    Model: String;
    Firmware: String;
    Serial: String;
    UserSizeInKB: UInt64;
    SATASpeed: TSATASpeed;
    SlotSpeed: TSlotSpeed;
    LBASize: Cardinal;
    StorageInterface: TStorageInterface;
    IsDataSetManagementSupported: Boolean;
  end;

  TBufferInterpreter = class abstract
  public
    function BufferToIdentifyDeviceResult
      (Buffer: TSmallBuffer): TIdentifyDeviceResult; virtual; abstract;
    function BufferToSMARTValueList
      (Buffer: TSmallBuffer): TSMARTValueList; virtual; abstract;
    function LargeBufferToIdentifyDeviceResult
      (Buffer: TLargeBuffer): TIdentifyDeviceResult; virtual; abstract;
    function LargeBufferToSMARTValueList
      (Buffer: TLargeBuffer): TSMARTValueList; virtual; abstract;
  end;

implementation

end.
