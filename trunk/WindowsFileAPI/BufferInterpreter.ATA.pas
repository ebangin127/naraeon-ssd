unit BufferInterpreter.ATA;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List;

type
  TATABufferInterpreter = class sealed(TBufferInterpreter)
  public
    function BufferToIdentifyDeviceResult(
      const Buffer: TSmallBuffer): TIdentifyDeviceResult; override;
    function BufferToSMARTValueList(
      const Buffer: TSmallBuffer): TSMARTValueList; override;
    function BufferToSMARTThresholdValueList(
      const Buffer: TSmallBuffer): TSMARTValueList;
    function LargeBufferToIdentifyDeviceResult(
      const Buffer: TLargeBuffer): TIdentifyDeviceResult; override;
    function LargeBufferToSMARTValueList(
      const Buffer: TLargeBuffer): TSMARTValueList; override;
  private
    SMARTValueList: TSMARTValueList;
    BufferInterpreting: TSmallBuffer;
    function GetFirmwareFromBuffer: String;
    function GetLBASizeFromBuffer: Cardinal;
    function GetModelFromBuffer: String;
    function GetSATASpeedFromBuffer: TSATASpeed;
    function GetSerialFromBuffer: String;
    function GetUserSizeInKBFromBuffer: UInt64;
    function GetCurrentOfRow(CurrentRowStart: Integer): Byte;
    function GetIDOfRow(CurrentRowStart: Integer): Byte;
    function GetRAWOfRow(CurrentRowStart: Integer): UInt64;
    function GetWorstOfRow(CurrentRowStart: Integer): Byte;
    function GetThresholdOfRow(CurrentRowStart: Integer): Byte;
    function IfValidSMARTAddToList(CurrentRow: Integer): Boolean;
    procedure IfValidSMARTThresholdAddToList(CurrentRow: Integer);
    function GetRotationRateFromBuffer: TRotationRate;
  end;

implementation

{ TATABufferInterpreter }

function TATABufferInterpreter.GetIDOfRow
  (CurrentRowStart: Integer): Byte;
begin
  result := BufferInterpreting[CurrentRowStart];
end;

function TATABufferInterpreter.GetCurrentOfRow
  (CurrentRowStart: Integer): Byte;
const
  CurrentValuePosition = 3;
begin
  result := BufferInterpreting[CurrentRowStart + CurrentValuePosition];
end;

function TATABufferInterpreter.GetWorstOfRow
  (CurrentRowStart: Integer): Byte;
const
  WorstValuePosition = 4;
begin
  result := BufferInterpreting[CurrentRowStart + WorstValuePosition];
end;

function TATABufferInterpreter.GetThresholdOfRow
  (CurrentRowStart: Integer): Byte;
const
  ThresholdValuePosition = 1;
begin
  result := BufferInterpreting[CurrentRowStart + ThresholdValuePosition];
end;

function TATABufferInterpreter.GetRAWOfRow
  (CurrentRowStart: Integer): UInt64;
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

procedure TATABufferInterpreter.IfValidSMARTThresholdAddToList(
  CurrentRow: Integer);
var
  SMARTValueEntry: TSMARTValueEntry;
begin
  SMARTValueEntry.ID := GetIDOfRow(CurrentRow);
  if SMARTValueEntry.ID = 0 then
    exit;

  SMARTValueEntry.Threshold := GetThresholdOfRow(CurrentRow);
  SMARTValueList.Add(SMARTValueEntry);
end;

function TATABufferInterpreter.IfValidSMARTAddToList(
  CurrentRow: Integer): Boolean;
var
  SMARTValueEntry: TSMARTValueEntry;
begin
  SMARTValueEntry.ID := GetIDOfRow(CurrentRow);
  if SMARTValueEntry.ID = 0 then
    exit(false);

  SMARTValueEntry.Current := GetCurrentOfRow(CurrentRow);
  SMARTValueEntry.Worst := GetWorstOfRow(CurrentRow);
  SMARTValueEntry.RAW := GetRAWOfRow(CurrentRow);
  SMARTValueEntry.Threshold := 0;
  SMARTValueList.Add(SMARTValueEntry);
  result := true;
end;

function TATABufferInterpreter.LargeBufferToIdentifyDeviceResult(
  const Buffer: TLargeBuffer): TIdentifyDeviceResult;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToIdentifyDeviceResult(SmallBuffer);
end;

function TATABufferInterpreter.LargeBufferToSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToSMARTValueList(SmallBuffer);
end;

function TATABufferInterpreter.BufferToSMARTThresholdValueList(
  const Buffer: TSmallBuffer): TSMARTValueList;
const
  SMARTStartPadding = 2;
  SMARTValueLength = 12;
var
  CurrentRow: Integer;
begin
  SMARTValueList := TSMARTValueList.Create;
  BufferInterpreting := Buffer;
  for CurrentRow := 0 to
    (Length(BufferInterpreting) - SMARTStartPadding) div SMARTValueLength do
    IfValidSMARTThresholdAddToList(
      (CurrentRow * SMARTValueLength) + SMARTStartPadding);
  result := SMARTValueList;
end;

function TATABufferInterpreter.BufferToSMARTValueList(
  const Buffer: TSmallBuffer): TSMARTValueList;
const
  SMARTStartPadding = 2;
  SMARTValueLength = 12;
  function CalculateRow(const CurrentRow: Integer): Integer;
  begin
    result := (CurrentRow * SMARTValueLength) + SMARTStartPadding;
  end;
var
  CurrentRow: Integer;
  MaxRow: Integer;
begin
  SMARTValueList := TSMARTValueList.Create;
  BufferInterpreting := Buffer;
  MaxRow :=
    (Length(BufferInterpreting) - SMARTStartPadding) div SMARTValueLength;
  for CurrentRow := 0 to MaxRow do
    if not IfValidSMARTAddToList(CalculateRow(CurrentRow)) then
      break;
  result := SMARTValueList;
end;

function TATABufferInterpreter.GetModelFromBuffer: String;
const
  ModelStart = 27;
  ModelEnd = 46;
var
  CurrentWord: Integer;
begin
  result := '';
  for CurrentWord := ModelStart to ModelEnd do
    result :=
      result +
      Chr(BufferInterpreting[CurrentWord * 2 + 1]) +
      Chr(BufferInterpreting[CurrentWord * 2]);
  result := Trim(result);
end;

function TATABufferInterpreter.GetFirmwareFromBuffer: String;
const
  FirmwareStart = 23;
  FirmwareEnd = 26;
var
  CurrentWord: Integer;
begin
  result := '';
  for CurrentWord := FirmwareStart to FirmwareEnd do
    result :=
      result +
      Chr(BufferInterpreting[CurrentWord * 2 + 1]) +
      Chr(BufferInterpreting[CurrentWord * 2]);
  result := Trim(result);
end;

function TATABufferInterpreter.GetSerialFromBuffer: String;
const
  SerialStart = 10;
  SerialEnd = 19;
var
  CurrentWord: Integer;
begin
  result := '';
  for CurrentWord := SerialStart to SerialEnd do
    result :=
      result +
      Chr(BufferInterpreting[CurrentWord * 2 + 1]) +
      Chr(BufferInterpreting[CurrentWord * 2]);
  result := Trim(result);
end;

function TATABufferInterpreter.GetUserSizeInKBFromBuffer: UInt64;
const
  UserSizeStart = 100;
  UserSizeEnd = 103;
var
  CurrentWord: Integer;
begin
  result := 0;
  for CurrentWord := UserSizeStart to UserSizeEnd do
  begin
    result :=
      result +
      BufferInterpreting[CurrentWord * 2] shl
        (((CurrentWord - UserSizeStart) * 2) * 8) +
      BufferInterpreting[CurrentWord * 2 + 1]  shl
        ((((CurrentWord - UserSizeStart) * 2) + 1) * 8);
  end;
  result := result div 2;
end;

function TATABufferInterpreter.GetLBASizeFromBuffer: Cardinal;
const
  ATA_LBA_SIZE = 512;
begin
  result := ATA_LBA_SIZE;
end;

function TATABufferInterpreter.GetSATASpeedFromBuffer: TSATASpeed;
const
  SataNegStart = 77;
var
  SATASpeedInNum: Cardinal;
begin
  SATASpeedInNum := BufferInterpreting[SataNegStart * 2 + 1] +
                    BufferInterpreting[SataNegStart * 2];
  SATASpeedInNum := (SATASpeedInNum shr 1 and 3) + 1;
  result := TSATASpeed(SATASpeedInNum);
end;

function TATABufferInterpreter.GetRotationRateFromBuffer: TRotationRate;
const
  MajorVersionStart = 80;
  RotationRateStart = 217;
begin
  result.Supported :=
    (BufferInterpreting[MajorVersionStart * 2 + 1] +
    BufferInterpreting[MajorVersionStart * 2]) >= 7;
  result.Value :=
    BufferInterpreting[RotationRateStart * 2 + 1] +
    BufferInterpreting[RotationRateStart * 2];
end;

function TATABufferInterpreter.BufferToIdentifyDeviceResult(
  const Buffer: TSmallBuffer): TIdentifyDeviceResult;
begin
  BufferInterpreting := Buffer;
  result.Model := GetModelFromBuffer;
  result.Firmware := GetFirmwareFromBuffer;
  result.Serial := GetSerialFromBuffer;
  result.UserSizeInKB := GetUserSizeInKBFromBuffer;
  result.SATASpeed := GetSATASpeedFromBuffer;
  result.LBASize := GetLBASizeFromBuffer;
  result.RotationRate := GetRotationRateFromBuffer;
end;

end.
