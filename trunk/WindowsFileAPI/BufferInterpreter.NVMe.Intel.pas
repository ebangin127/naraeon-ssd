unit BufferInterpreter.NVMe.Intel;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, MeasureUnit.DataSize;

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
  TIntelBufferInterpreter = class sealed(TBufferInterpreter)
  public
    function BufferToIdentifyDeviceResult
      (Buffer: TSmallBuffer): TIdentifyDeviceResult; override;
    function BufferToSMARTValueList
      (Buffer: TSmallBuffer): TSMARTValueList; override;
    function LargeBufferToIdentifyDeviceResult
      (Buffer: TLargeBuffer): TIdentifyDeviceResult; override;
    function LargeBufferToSMARTValueList
      (Buffer: TLargeBuffer): TSMARTValueList; override;
    function BufferToCapacityAndLBA(Buffer: TSmallBuffer):
      TIdentifyDeviceResult;
    function VendorSpecificSMARTValueList
      (Buffer: TLargeBuffer): TSMARTValueList;
  private
    SMARTValueList: TSMARTValueList;
    BufferInterpreting: TLargeBuffer;
    function GetFirmwareFromBuffer: String;
    function GetLBASizeFromBuffer: Cardinal;
    function GetModelFromBuffer: String;
    function GetSerialFromBuffer: String;
    function GetLBASize(Buffer: TSmallBuffer): Integer;
    function SeperateCriticalWarningFrom(Buffer: TLargeBuffer):
      TSMARTValueEntry;
    function SeperateTemperatureFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperateAvailableSpareFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperatePercentageUsedFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperateDataUnitsReadFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperateDataUnitsWrittenFrom(Buffer: TLargeBuffer):
      TSMARTValueEntry;
    function SeperateHostReadCommandsFrom(Buffer: TLargeBuffer):
      TSMARTValueEntry;
    function SeperateHostWriteCommandsFrom(
      Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperateControllerBusyTimeFrom(
      Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperatePowerCyclesFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperatePowerOnHoursFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperateUnsafeShutdownsFrom(Buffer: TLargeBuffer):
      TSMARTValueEntry;
    function SeperateMediaErrorsFrom(Buffer: TLargeBuffer): TSMARTValueEntry;
    function SeperateNumberOfErrorsFrom(Buffer: TLargeBuffer):
      TSMARTValueEntry;
    procedure IfValidSMARTAddToList(CurrentRow: Integer);
    function GetCurrentOfRow(CurrentRowStart: Integer): Byte;
    function GetIDOfRow(CurrentRowStart: Integer): Byte;
    function GetRAWOfRow(CurrentRowStart: Integer): UInt64;
    function AdditionalSMARTValueList(Buffer: TLargeBuffer): TSMARTValueList;
    function TemperatureSMARTValueList(Buffer: TLargeBuffer): TSMARTValueList;
    procedure AddTemperatureToList(const CurrentRow, StartPoint,
      LengthOfValue: Integer);
  end;

implementation

{ TNVMeBufferInterpreter }

function TIntelBufferInterpreter.BufferToIdentifyDeviceResult
  (Buffer: TSmallBuffer): TIdentifyDeviceResult;
begin
  raise ESmallBufferException.Create('Small Buffer cannot be interpreted in ' +
    'Intel NVMe Way');
end;

function TIntelBufferInterpreter.BufferToSMARTValueList
  (Buffer: TSmallBuffer): TSMARTValueList;
begin
  raise ESmallBufferException.Create('Small Buffer cannot be interpreted in ' +
    'Intel NVMe Way');
end;

function TIntelBufferInterpreter.LargeBufferToIdentifyDeviceResult(
  Buffer: TLargeBuffer): TIdentifyDeviceResult;
begin
  BufferInterpreting := Buffer;
  result.Model := GetModelFromBuffer;
  result.Firmware := GetFirmwareFromBuffer;
  result.Serial := GetSerialFromBuffer;
  result.UserSizeInKB := 0;
  result.SATASpeed := TSATASpeed.NotSATA;
  result.LBASize := GetLBASizeFromBuffer;
end;

function TIntelBufferInterpreter.LargeBufferToSMARTValueList(
  Buffer: TLargeBuffer): TSMARTValueList;
begin
  result := TSMARTValueList.Create;
  result.Add(SeperateCriticalWarningFrom(Buffer));
  result.Add(SeperateTemperatureFrom(Buffer));
  result.Add(SeperateAvailableSpareFrom(Buffer));
  result.Add(SeperatePercentageUsedFrom(Buffer));
  result.Add(SeperateDataUnitsReadFrom(Buffer));
  result.Add(SeperateDataUnitsWrittenFrom(Buffer));
  result.Add(SeperateHostReadCommandsFrom(Buffer));
  result.Add(SeperateHostWriteCommandsFrom(Buffer));
  result.Add(SeperateControllerBusyTimeFrom(Buffer));
  result.Add(SeperatePowerCyclesFrom(Buffer));
  result.Add(SeperatePowerOnHoursFrom(Buffer));
  result.Add(SeperateUnsafeShutdownsFrom(Buffer));
  result.Add(SeperateMediaErrorsFrom(Buffer));
  result.Add(SeperateNumberOfErrorsFrom(Buffer));
end;

function TIntelBufferInterpreter.VendorSpecificSMARTValueList(
  Buffer: TLargeBuffer): TSMARTValueList;
const
  FirstByteOfAdditionalAttribute = $AB;
begin
  if Buffer[0] = FirstByteOfAdditionalAttribute then
    result := AdditionalSMARTValueList(Buffer)
  else
    result := TemperatureSMARTValueList(Buffer);
end;

function TIntelBufferInterpreter.AdditionalSMARTValueList(
  Buffer: TLargeBuffer): TSMARTValueList;
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
  Buffer: TLargeBuffer): TSMARTValueList;
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

function TIntelBufferInterpreter.SeperateCriticalWarningFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.CriticalWarning);
  result.RAW := Buffer[0];
end;

function TIntelBufferInterpreter.SeperateTemperatureFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  TemperatureStart = 1;
  TemperatureEnd = 2;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.TemperatureInKelvin);
  result.RAW := 0;
  for CurrentByte := TemperatureEnd downto TemperatureStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateAvailableSpareFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.AvailableSpare);
  result.RAW := Buffer[3];
  result.Threshold := Buffer[4];
end;

function TIntelBufferInterpreter.SeperatePercentageUsedFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.PercentageUsed);
  result.RAW := Buffer[5];
end;

function TIntelBufferInterpreter.SeperateDataUnitsReadFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  ReadStart = 32;
  ReadEnd = 47;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.DataUnitsReadInLBA);
  result.RAW := 0;
  for CurrentByte := ReadEnd downto ReadStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateDataUnitsWrittenFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  WrittenStart = 48;
  WrittenEnd = 63;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.DataUnitsWrittenInLBA);
  result.RAW := 0;
  for CurrentByte := WrittenEnd downto WrittenStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateHostReadCommandsFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  ReadStart = 64;
  ReadEnd = 79;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.HostReadCommands);
  result.RAW := 0;
  for CurrentByte := ReadEnd downto ReadStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateHostWriteCommandsFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  WriteStart = 80;
  WriteEnd = 95;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.HostWriteCommands);
  result.RAW := 0;
  for CurrentByte := WriteEnd downto WriteStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateControllerBusyTimeFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  BusyTimeStart = 96;
  BusyTimeEnd = 111;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.ControllerBusyTime);
  result.RAW := 0;
  for CurrentByte := BusyTimeEnd downto BusyTimeStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperatePowerCyclesFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  PowerCycleStart = 112;
  PowerCycleEnd = 127;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.PowerCycles);
  result.RAW := 0;
  for CurrentByte := PowerCycleEnd downto PowerCycleStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperatePowerOnHoursFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  PowerOnHoursStart = 128;
  PowerOnHoursEnd = 143;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.PowerOnHours);
  result.RAW := 0;
  for CurrentByte := PowerOnHoursEnd downto PowerOnHoursStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateUnsafeShutdownsFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  UnsafeShutdownsStart = 144;
  UnsafeShutdownsEnd = 159;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.UnsafeShutdowns);
  result.RAW := 0;
  for CurrentByte := UnsafeShutdownsEnd downto UnsafeShutdownsStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateMediaErrorsFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  MediaErrorsStart = 160;
  MediaErrorsEnd = 175;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.MediaErrors);
  result.RAW := 0;
  for CurrentByte := MediaErrorsEnd downto MediaErrorsStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.SeperateNumberOfErrorsFrom
  (Buffer: TLargeBuffer): TSMARTValueEntry;
var
  CurrentByte: Integer;
const
  NumberOfErrorsStart = 176;
  NumberOfErrorsEnd = 191;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.NumberOfErrorInformationLogEntries);
  result.RAW := 0;
  for CurrentByte := NumberOfErrorsEnd downto NumberOfErrorsStart do
  begin
    result.RAW := result.RAW shl 8;
    result.RAW := result.RAW + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.GetModelFromBuffer: String;
const
  ModelStart = 24;
  ModelEnd = 63;
var
  CurrentByte: Integer;
begin
  result := '';
  for CurrentByte := ModelStart to ModelEnd do
    result := result + Chr(BufferInterpreting[CurrentByte]);
  result := Trim(result);
end;

function TIntelBufferInterpreter.GetFirmwareFromBuffer: String;
const
  FirmwareStart = 64;
  FirmwareEnd = 71;
var
  CurrentByte: Integer;
begin
  result := '';
  for CurrentByte := FirmwareStart to FirmwareEnd do
    result := result + Chr(BufferInterpreting[CurrentByte]);
  result := Trim(result);
end;

function TIntelBufferInterpreter.GetSerialFromBuffer: String;
const
  SerialStart = 4;
  SerialEnd = 23;
var
  CurrentByte: Integer;
begin
  result := '';
  for CurrentByte := SerialStart to SerialEnd do
    result := result + Chr(BufferInterpreting[CurrentByte]);
  result := Trim(result);
end;

function TIntelBufferInterpreter.GetLBASizeFromBuffer: Cardinal;
const
  ATA_LBA_SIZE = 512;
begin
  result := ATA_LBA_SIZE;
end;

function TIntelBufferInterpreter.GetLBASize(Buffer: TSmallBuffer): Integer;
var
  CurrentByte: Integer;
const
  LBASizeStart = 8;
  LBASizeEnd = 11;
begin
  result := 0;
  for CurrentByte := LBASizeStart to LBASizeEnd do
  begin
    result := result shl 8;
    result := result + Buffer[CurrentByte];
  end;
end;

function TIntelBufferInterpreter.BufferToCapacityAndLBA(Buffer:
  TSmallBuffer): TIdentifyDeviceResult;
  function ByteToDenaryKB: TDatasizeUnitChangeSetting;
  begin
    result.FNumeralSystem := Denary;
    result.FFromUnit := ByteUnit;
    result.FToUnit := KiloUnit;
  end;
var
  CurrentByte: Integer;
  ResultInByte: UInt64;
const
  LBAStart = 0;
  LBAEnd = 7;
begin
  ResultInByte := 0;
  for CurrentByte := LBAStart to LBAEnd do
  begin
    ResultInByte := ResultInByte shl 8;
    ResultInByte := ResultInByte + Buffer[CurrentByte];
  end;
  result.LBASize := GetLBASize(Buffer);
  result.UserSizeInKB := round(
    ChangeDatasizeUnit(ResultInByte * result.LBASize, ByteToDenaryKB));
end;

end.
