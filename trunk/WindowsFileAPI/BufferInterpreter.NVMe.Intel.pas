unit BufferInterpreter.NVMe.Intel;

interface

uses
  SysUtils,
  BufferInterpreter, BufferInterpreter.NVMe, Device.SMART.List;

type
  ESmallBufferException = class(ENotImplemented);
  TSMARTValueID = (
    CriticalWarning,
    TemperatureInKelvin,
    AvailableSpare,
    PercentageUsed,
    DataUnitsReadInLBA,
    DataUnitsWrittenInLBA,
    HostReadCommands,
    HostWriteCommands,
    ControllerBusyTime,
    PowerCycles,
    PowerOnHours,
    UnsafeShutdowns,
    MediaErrors,
    NumberOfErrorInformationLogEntries);

  TIntelBufferInterpreter = class(TBufferInterpreter)
  public
    function BufferToIdentifyDeviceResult(
      const Buffer: TSmallBuffer): TIdentifyDeviceResult; override;
    function BufferToSMARTValueList(
      const Buffer: TSmallBuffer): TSMARTValueList; override;
    function LargeBufferToIdentifyDeviceResult(
      const Buffer: TLargeBuffer): TIdentifyDeviceResult; override;
    function LargeBufferToSMARTValueList(
      const Buffer: TLargeBuffer): TSMARTValueList; override;
    function BufferToCapacityAndLBA(const Buffer: TSmallBuffer):
      TIdentifyDeviceResult;
    function VendorSpecificSMARTValueList(
      const Buffer: TLargeBuffer): TSMARTValueList;
    constructor Create;
    destructor Destroy; override;
  private
    NVMeInterpreter: TNVMeBufferInterpreter;
    SMARTValueList: TSMARTValueList;
    BufferInterpreting: TLargeBuffer;
    procedure IfValidSMARTAddToList(CurrentRow: Integer);
    function GetCurrentOfRow(CurrentRowStart: Integer): Byte;
    function GetIDOfRow(CurrentRowStart: Integer): Byte;
    function GetRAWOfRow(CurrentRowStart: Integer): UInt64;
    function AdditionalSMARTValueList(const Buffer: TLargeBuffer):
      TSMARTValueList;
    function TemperatureSMARTValueList(const Buffer: TLargeBuffer):
      TSMARTValueList;
    procedure AddTemperatureToList(const CurrentRow, StartPoint,
      LengthOfValue: Integer);
  end;

implementation

{ TNVMeBufferInterpreter }

function TIntelBufferInterpreter.BufferToIdentifyDeviceResult(
  const Buffer: TSmallBuffer): TIdentifyDeviceResult;
begin
  raise ESmallBufferException.Create('Small Buffer cannot be interpreted in ' +
    'Intel NVMe Way');
end;

function TIntelBufferInterpreter.BufferToSMARTValueList(
  const Buffer: TSmallBuffer): TSMARTValueList;
begin
  raise ESmallBufferException.Create('Small Buffer cannot be interpreted in ' +
    'Intel NVMe Way');
end;

constructor TIntelBufferInterpreter.Create;
begin
  inherited Create;
  NVMeInterpreter := TNVMeBufferInterpreter.Create;
end;

destructor TIntelBufferInterpreter.Destroy;
begin
  FreeAndNil(NVMeInterpreter);
  inherited;
end;

function TIntelBufferInterpreter.LargeBufferToIdentifyDeviceResult(
  const Buffer: TLargeBuffer): TIdentifyDeviceResult;
begin
  result := NVMeInterpreter.LargeBufferToIdentifyDeviceResult(Buffer);
end;

function TIntelBufferInterpreter.LargeBufferToSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
begin
  result := NVMeInterpreter.LargeBufferToSMARTValueList(Buffer);
end;

function TIntelBufferInterpreter.VendorSpecificSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
const
  FirstByteOfAdditionalAttribute = $AB;
begin
  if Buffer[0] = FirstByteOfAdditionalAttribute then
    result := AdditionalSMARTValueList(Buffer)
  else
    result := TemperatureSMARTValueList(Buffer);
end;

function TIntelBufferInterpreter.AdditionalSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
const
  SMARTValueLength = 12;
var
  CurrentRow: Integer;
begin
  SMARTValueList := TSMARTValueList.Create;
  BufferInterpreting := Buffer;
  for CurrentRow := 0 to Length(BufferInterpreting) div SMARTValueLength do
    IfValidSMARTAddToList(CurrentRow * SMARTValueLength);
  result := SMARTValueList;
end;

function TIntelBufferInterpreter.TemperatureSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
const
  TemperatureValueLength = 8;
  CurrentTemperature = 0;
  HighestTemperature = 24;
  LowestTemperature = 32;
  MaximumTemperature = 80;
  MinimumTemperature = 96;
  TemperatureStart = $40;
var
  CurrentRow: Integer;
begin
  SMARTValueList := TSMARTValueList.Create;
  BufferInterpreting := Buffer;
  CurrentRow := TemperatureStart;
  AddTemperatureToList(CurrentRow, CurrentTemperature, TemperatureValueLength);
  Inc(CurrentRow);
  AddTemperatureToList(CurrentRow, HighestTemperature, TemperatureValueLength);
  Inc(CurrentRow);
  AddTemperatureToList(CurrentRow, LowestTemperature, TemperatureValueLength);
  Inc(CurrentRow);
  AddTemperatureToList(CurrentRow, MaximumTemperature, TemperatureValueLength);
  Inc(CurrentRow);
  AddTemperatureToList(CurrentRow, MinimumTemperature, TemperatureValueLength);
  result := SMARTValueList;
end;

procedure TIntelBufferInterpreter.AddTemperatureToList(
  const CurrentRow, StartPoint, LengthOfValue: Integer);
var
  Entry: TSMARTValueEntry;
  RAWStart, RAWEnd, CurrentRAW: Integer;
begin
  Entry.ID := CurrentRow;
  Entry.Current := 0;
  Entry.Worst := 0;
  Entry.Threshold := 0;
  Entry.RAW := 0;
  RAWStart := StartPoint;
  RAWEnd := RAWStart + LengthOfValue - 1;
  for CurrentRAW := RAWEnd downto RAWStart do
  begin
    Entry.RAW := Entry.RAW shl 8;
    Entry.RAW :=
      Entry.RAW +
      BufferInterpreting[CurrentRAW];
  end;
  SMARTValueList.Add(Entry);
end;

procedure TIntelBufferInterpreter.IfValidSMARTAddToList(CurrentRow: Integer);
var
  SMARTValueEntry: TSMARTValueEntry;
begin
  SMARTValueEntry.ID := GetIDOfRow(CurrentRow);
  if SMARTValueEntry.ID = 0 then
    exit;

  SMARTValueEntry.Worst := 0;
  SMARTValueEntry.Threshold := 0;
  SMARTValueEntry.Current := GetCurrentOfRow(CurrentRow);
  SMARTValueEntry.RAW := GetRAWOfRow(CurrentRow);
  SMARTValueList.Add(SMARTValueEntry);
end;

function TIntelBufferInterpreter.GetIDOfRow(CurrentRowStart: Integer): Byte;
begin
  result := BufferInterpreting[CurrentRowStart];
end;

function TIntelBufferInterpreter.GetCurrentOfRow(CurrentRowStart: Integer): Byte;
const
  CurrentValuePosition = 3;
begin
  result := BufferInterpreting[CurrentRowStart + CurrentValuePosition];
end;

function TIntelBufferInterpreter.GetRAWOfRow(CurrentRowStart: Integer): UInt64;
const
  RAWValueStart = 5;
  RAWValueLength = 6;
var
  RAWStart, RAWEnd: Integer;
  CurrentRAW: Integer;
begin
  RAWStart := CurrentRowStart + RAWValueStart;
  RAWEnd := RAWStart + RAWValueLength - 1;

  result := 0;
  for CurrentRAW := RAWEnd downto RAWStart do
  begin
    result := result shl 8;
    result :=
      result +
      BufferInterpreting[CurrentRAW];
  end;
end;

function TIntelBufferInterpreter.BufferToCapacityAndLBA(
  const Buffer: TSmallBuffer): TIdentifyDeviceResult;
begin
  result := NVMeInterpreter.BufferToCapacityAndLBA(Buffer);
end;

end.
