unit BufferInterpreter;

interface

uses
  SysUtils,
  Device.SMART.List, Device.SlotSpeed;

type
  TSmallBuffer = Array[0..511] of Byte;
  TLargeBuffer = Array[0..4095] of Byte;
  TStorageInterface =
    (Probing, ATA, SAT, SCSI, NVMe, UnknownInterface);
  TSATASpeed =
    (NotSATA, UnknownSATASpeed, SATA150, SATA300, SATA600);
  TRotationRate = record
    Supported: Boolean;
    Value: Integer;
  end;
  TIdentifyDeviceResult = record
    Model: String;
    Firmware: String;
    Serial: String;
    UserSizeInKB: UInt64;
    SATASpeed: TSATASpeed;
    SlotSpeed: TSlotSpeed;
    LBASize: Cardinal;
    RotationRate: TRotationRate;
    StorageInterface: TStorageInterface;
    IsDataSetManagementSupported: Boolean;
  end;

  TBufferInterpreter = class
  public
    function BufferToIdentifyDeviceResult(
      const Buffer: TSmallBuffer): TIdentifyDeviceResult; virtual; abstract;
    function BufferToSMARTValueList(
      const Buffer: TSmallBuffer): TSMARTValueList; virtual; abstract;
    function LargeBufferToIdentifyDeviceResult(
      const Buffer: TLargeBuffer): TIdentifyDeviceResult; virtual; abstract;
    function LargeBufferToSMARTValueList(
      const Buffer: TLargeBuffer): TSMARTValueList; virtual; abstract;
    class function BufferToString(const Buffer: Array of Byte): String;
  end;

implementation

{ TBufferInterpreter }

class function TBufferInterpreter.BufferToString(const Buffer: Array of Byte):
  String;
var
  CurrentCharacter: Byte;
begin
  result := ': Array[0..' + IntToStr(Length(Buffer) - 1) + '] of Byte = (';
  for CurrentCharacter in Buffer do
    result := result + '$' + IntToHex(Ord(CurrentCharacter), 2) + ',';
  result[Length(result)] := ')';
end;

end.
