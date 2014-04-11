unit uIntFunctions;

interface
  function TryStrToUINT64(StrValue: String; var uValue: UInt64 ): Boolean;
  function StrToUINT64(Value: String): UInt64;

implementation

uses SysUtils, Character;

{$R-}

function TryStrToUInt64(StrValue: String; var uValue: UInt64): Boolean;
var
  Start,Base,Digit: Integer;
  n: Integer;
  Nextvalue: UInt64;
begin
  result := false;
  Base := 10;
  Start := 1;
  digit := 0;
  StrValue := Trim(UpperCase(StrValue));
  if StrValue = '' then
    exit;
  if StrValue[1] = '-' then
    exit;
  if StrValue[1] = '$' then
  begin
    Base := 16;
    Start := 2;
    if Length(StrValue) > 17 then
        exit;
  end;
  uValue := 0;
  for n := Start to Length(StrValue) do
  begin
      if Character.IsDigit(StrValue[n]) then
          Digit := Ord(StrValue[n]) - Ord('0')
      else if  (Base = 16) and (StrValue[n] >= 'A') and (StrValue[n] <= 'F') then
          Digit := (Ord(StrValue[n]) - Ord('A'))+10
      else
          exit;
      Nextvalue := (uValue * base) + digit;
      if (Nextvalue < uValue) then
          exit;
      uValue := Nextvalue;
  end;
  result := true;
end;

function StrToUInt64(Value:String): UInt64;
begin
  if not TryStrToUINT64(Value,result) then
    raise EConvertError.Create('Invalid uint64 value');
end;

end.
