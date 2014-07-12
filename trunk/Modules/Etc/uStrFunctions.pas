unit uStrFunctions;

interface

uses SysUtils;

function TrimEx(input: String): String; //중간 공백까지 삭제
function ExtractDeviceNum(const Input: String): String;{PhysicalDrive0 -> 0
                                                        \\.\PhysicalDrive0 -> 0}

implementation


function TrimEx(input: String): String;
var
  i: integer;
begin
  result := '';
  for i := 1 to Length(input) do
    if input[i] <> ' ' then
      result := result + input[i];
end;

function ExtractDeviceNum(const Input: String): String;
begin
  if Input[1] = '\' then result := Copy(Input, 18, Length(Input) - 17)
  else if Input[1] = 'P' then result := Copy(Input, 14, Length(Input) - 13);
end;

end.
