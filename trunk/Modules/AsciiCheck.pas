unit AsciiCheck;

interface

uses
  SysUtils, Character;

type
  TStringHelper = class
  public
    function IsAscii(PathToValidate: String): Boolean;
  end;

var
  StringHelper: TStringHelper;

implementation

{$R-}

function TStringHelper.IsAscii(PathToValidate: String): Boolean;
var
  CurrentCharacter: Char;
begin
  result := true;
  for CurrentCharacter in PathToValidate do
    if Ord(CurrentCharacter) > Byte.MaxValue then
      exit(false);
end;

initialization
  StringHelper := TStringHelper.Create;
finalization
  StringHelper.Free;
end.
