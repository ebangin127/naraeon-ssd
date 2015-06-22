unit uUINT64;

interface

function TryStrToUInt64(UInt64InString: String; var Output: UInt64): Boolean;
function StrToUInt64(UInt64InString: String): UInt64;

implementation

uses SysUtils, Character;

{$R-}

type
  TUInt64ConvertSettings = record
    FBase: Integer;
    FStartPoint: Integer;
  end;

function IsInvalidUInt64Value(UInt64InString: String): Boolean;
begin
  result := false;
  if (UInt64InString = '') or
     (UInt64InString[1] = '-') or
     (Length(UInt64InString) > 17) then
    result := true;
end;

function GetNewDigit(UInt64InChar: Char; Base: Integer): Integer;
begin
  if UInt64InChar.IsNumber then
    result := Ord(UInt64InChar) - Ord('0')
  else if  (Base = 16) and (UInt64InChar >= 'A') and (UInt64InChar <= 'F') then
    result := (Ord(UInt64InChar) - Ord('A')) + 10
  else
    result := -1;
end;

function IsNewDigitInvalid(Output, NextOutput: UInt64; DigitToAdd: Integer):
  Boolean;
begin
  result := (DigitToAdd = -1) or (Output >= NextOutput);
end;

function CharToUInt64ByBase(UInt64InChar: Char;
  var Output: UInt64; Base: Integer): Boolean;
var
  NextOutput: UInt64;
  DigitToAdd: Integer;
begin
  result := false;
  
  DigitToAdd := GetNewDigit(UInt64InChar, Base);
  NextOutput := (Output * Base) + DigitToAdd;
  if IsNewDigitInvalid(Output, NextOutput, DigitToAdd) then
    exit;
  
  Output := NextOutput;
  result := true;
end;
    
function ConvertToUInt64ByNumeralSystem(UInt64InString: String;
  var Output: UInt64; NumeralSystem: TUInt64ConvertSettings): Boolean;
var
  CurrentChar: Integer;
begin
  result := false;
  Output := 0;
  
  for CurrentChar := NumeralSystem.StartPoint to Length(UInt64InString) do
    if not CharToUInt64ByBase(UInt64InString[CurrentChar],
      Output, NumeralSystem.Base) then
        exit;
        
  result := true;
end;
  
function GetNumeralSystem(UInt64InString: String): TUInt64ConvertSettings;
begin
  UInt64InString := Trim(UpperCase(UInt64InString));
  
  if UInt64InString[1] = HexadecimalKeyword then
    exit(Hexadecimal)
  else
    exit(Decimal);
end;

function TryStrToUInt64(UInt64InString: String; var Output: UInt64): Boolean;
begin
  result := false;
  if IsInvalidUInt64Value(UInt64InString) then
    exit(false)
  else
    exit(ConvertToUInt64ByNumeralSystem(UInt64InString, Output,
      GetNumeralSystem(UInt64InString)));
end;

function StrToUInt64(UInt64InString: String): UInt64;
begin
  if not TryStrToUINT64(UInt64InString, result) then
    raise EConvertError.Create('Invalid UInt64 value: ' + UInt64InString);
end;

end.
