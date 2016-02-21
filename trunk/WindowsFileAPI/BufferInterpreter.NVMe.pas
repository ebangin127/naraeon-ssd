unit BufferInterpreter.NVMe;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, MeasureUnit.DataSize;

type
  TSMARTValueID = (
    None,
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

  TNVMeBufferInterpreter = class(TBufferInterpreter)
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
  private
    BufferInterpreting: TSmallBuffer;
    function GetFirmwareFromBuffer: String;
    function GetLBASizeFromBuffer: Cardinal;
    function GetModelFromBuffer: String;
    function GetSerialFromBuffer: String;
    function GetLBASize(const Buffer: TSmallBuffer): Integer;
    function SeperateCriticalWarningFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateTemperatureFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateAvailableSpareFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperatePercentageUsedFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateDataUnitsReadFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateDataUnitsWrittenFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateHostReadCommandsFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateHostWriteCommandsFrom(
      const Buffer: TSmallBuffer): TSMARTValueEntry;
    function SeperateControllerBusyTimeFrom(
      const Buffer: TSmallBuffer): TSMARTValueEntry;
    function SeperatePowerCyclesFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperatePowerOnHoursFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateUnsafeShutdownsFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateMediaErrorsFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
    function SeperateNumberOfErrorsFrom(const Buffer: TSmallBuffer):
      TSMARTValueEntry;
  end;

implementation

{ TNVMeBufferInterpreter }

function TNVMeBufferInterpreter.BufferToSMARTValueList(
  const Buffer: TSmallBuffer): TSMARTValueList;
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

function TNVMeBufferInterpreter.SeperateCriticalWarningFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.CriticalWarning);
  result.RAW := Buffer[0];
end;

function TNVMeBufferInterpreter.SeperateTemperatureFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateAvailableSpareFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.AvailableSpare);
  result.RAW := Buffer[3];
  result.Threshold := Buffer[4];
end;

function TNVMeBufferInterpreter.SeperatePercentageUsedFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
begin
  FillChar(result, SizeOf(result), 0);
  result.ID := Ord(TSMARTValueID.PercentageUsed);
  result.RAW := Buffer[5];
end;

function TNVMeBufferInterpreter.SeperateDataUnitsReadFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateDataUnitsWrittenFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateHostReadCommandsFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateHostWriteCommandsFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateControllerBusyTimeFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperatePowerCyclesFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperatePowerOnHoursFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateUnsafeShutdownsFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateMediaErrorsFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.SeperateNumberOfErrorsFrom(
  const Buffer: TSmallBuffer): TSMARTValueEntry;
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

function TNVMeBufferInterpreter.GetModelFromBuffer: String;
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

function TNVMeBufferInterpreter.GetFirmwareFromBuffer: String;
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

function TNVMeBufferInterpreter.GetSerialFromBuffer: String;
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

function TNVMeBufferInterpreter.LargeBufferToIdentifyDeviceResult(
  const Buffer: TLargeBuffer): TIdentifyDeviceResult;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToIdentifyDeviceResult(SmallBuffer);
end;

function TNVMeBufferInterpreter.LargeBufferToSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToSMARTValueList(SmallBuffer);
end;

function TNVMeBufferInterpreter.GetLBASizeFromBuffer: Cardinal;
const
  ATA_LBA_SIZE = 512;
begin
  result := ATA_LBA_SIZE;
end;

function TNVMeBufferInterpreter.GetLBASize(const Buffer: TSmallBuffer): Integer;
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

function TNVMeBufferInterpreter.BufferToCapacityAndLBA(
  const Buffer: TSmallBuffer): TIdentifyDeviceResult;
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

function TNVMeBufferInterpreter.BufferToIdentifyDeviceResult(
  const Buffer: TSmallBuffer): TIdentifyDeviceResult;
const
  SSDRate = 1;
begin
  BufferInterpreting := Buffer;
  result.Model := GetModelFromBuffer;
  result.Firmware := GetFirmwareFromBuffer;
  result.Serial := GetSerialFromBuffer;
  result.UserSizeInKB := 0;
  result.SATASpeed := TSATASpeed.NotSATA;
  result.LBASize := GetLBASizeFromBuffer;
  result.RotationRate.Supported := true;
  result.RotationRate.Value := SSDRate;
end;

end.
