unit BufferInterpreter;

interface

uses
  Device.SMART.List, Device.SlotSpeed;

type
  TSmallBuffer = Array[0..511] of Byte;
  TLargeBuffer = Array[0..4095] of Byte;
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
    function BufferToIdentifyDeviceResult(
      const Buffer: TSmallBuffer): TIdentifyDeviceResult; virtual; abstract;
    function BufferToSMARTValueList(
      const Buffer: TSmallBuffer): TSMARTValueList; virtual; abstract;
    function LargeBufferToIdentifyDeviceResult(
      const Buffer: TLargeBuffer): TIdentifyDeviceResult; virtual; abstract;
    function LargeBufferToSMARTValueList(
      const Buffer: TLargeBuffer): TSMARTValueList; virtual; abstract;
  end;

implementation

end.
