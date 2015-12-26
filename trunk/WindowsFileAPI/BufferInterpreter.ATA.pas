unit BufferInterpreter.ATA;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List;

type
  TATABufferInterpreter = class sealed(TBufferInterpreter)
  public
    function BufferToIdentifyDeviceResult
      (Buffer: TSmallBuffer): TIdentifyDeviceResult; override;
    function BufferToSMARTValueList
      (Buffer: TSmallBuffer): TSMARTValueList; override;
    function LargeBufferToIdentifyDeviceResult
      (Buffer: TLargeBuffer): TIdentifyDeviceResult; override;
    function LargeBufferToSMARTValueList
      (Buffer: TLargeBuffer): TSMARTValueList; override;

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
    procedure IfValidSMARTAddToList(CurrentRow: Integer);
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
  ThresholdValuePosition = 11;
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

procedure TATABufferInterpreter.IfValidSMARTAddToList
  (CurrentRow: Integer);
var
  SMARTValueEntry: TSMARTValueEntry;
begin
  SMARTValueEntry.ID := GetIDOfRow(CurrentRow);
  if SMARTValueEntry.ID = 0 then
    exit;

  SMARTValueEntry.Current := GetCurrentOfRow(CurrentRow);
  SMARTValueEntry.Worst := GetWorstOfRow(CurrentRow);
  SMARTValueEntry.RAW := GetRAWOfRow(CurrentRow);
  SMARTValueEntry.Threshold := GetThresholdOfRow(CurrentRow);
  SMARTValueList.Add(SMARTValueEntry);
end;

function TATABufferInterpreter.LargeBufferToIdentifyDeviceResult(
  Buffer: TLargeBuffer): TIdentifyDeviceResult;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToIdentifyDeviceResult(SmallBuffer);
end;

function TATABufferInterpreter.LargeBufferToSMARTValueList(
  Buffer: TLargeBuffer): TSMARTValueList;

var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToSMARTValueList(SmallBuffer);
end;

function TATABufferInterpreter.BufferToSMARTValueList
  (Buffer: TSmallBuffer): TSMARTValueList;
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
    IfValidSMARTAddToList((CurrentRow * SMARTValueLength) + SMARTStartPadding);
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

function TATABufferInterpreter.BufferToIdentifyDeviceResult
  (Buffer: TSmallBuffer): TIdentifyDeviceResult;
begin
  BufferInterpreting := Buffer;
  result.Model := GetModelFromBuffer;
  result.Firmware := GetFirmwareFromBuffer;
  result.Serial := GetSerialFromBuffer;
  result.UserSizeInKB := GetUserSizeInKBFromBuffer;
  result.SATASpeed := GetSATASpeedFromBuffer;
  result.LBASize := GetLBASizeFromBuffer;
end;

end.
