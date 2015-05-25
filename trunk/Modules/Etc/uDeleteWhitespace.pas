unit uDeleteWhitespace;

interface

function DeleteWhitespace(input: String): String;

implementation

function DeleteWhitespace(input: String): String;
var
  i: integer;
begin
  result := '';
  for i := 1 to Length(input) do
    if input[i] <> ' ' then
      result := result + input[i];
end;

end.
