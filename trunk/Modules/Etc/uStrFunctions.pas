unit uStrFunctions;

interface

uses SysUtils;

function ExtractDeviceNum(const Input: String): String;{PhysicalDrive0 -> 0
                                                        \\.\PhysicalDrive0 -> 0}

implementation

function ExtractDeviceNum(const Input: String): String;
begin
  if Input[1] = '\' then result := Copy(Input, 18, Length(Input) - 17)
  else if Input[1] = 'P' then result := Copy(Input, 14, Length(Input) - 13);
end;

end.
