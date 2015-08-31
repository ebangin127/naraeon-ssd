unit uTrimBasicsGetterFactory;

interface

uses
  SysUtils,
  uTrimBasicsGetter,
  uFATTrimBasicsGetter, uNTFSTrimBasicsGetter;

type
  TMetaTrimBasicsGetter = class of TTrimBasicsGetter;

  TTrimBasicsGetterFactory = class
  public
    function GetSuitableTrimBasicsGetter(FileToGetAccess: String):
      TTrimBasicsGetter;
    class function Create: TTrimBasicsGetterFactory;

  private
    function TryAndGetRightGetter: TTrimBasicsGetter;
    function TestGetterCompatibility(TTrimBasicsGetterToTry:
      TMetaTrimBasicsGetter; LastResult: TTrimBasicsGetter):
      TTrimBasicsGetter;
    var
      FFileToGetAccess: String;
  end;

var
  TrimBasicsGetterFactory: TTrimBasicsGetterFactory;

implementation

class function TTrimBasicsGetterFactory.Create: TTrimBasicsGetterFactory;
begin
  if TrimBasicsGetterFactory = nil then
    result := inherited Create as self
  else
    result := TrimBasicsGetterFactory;
end;

function TTrimBasicsGetterFactory.GetSuitableTrimBasicsGetter(
  FileToGetAccess: String): TTrimBasicsGetter;
begin
  FFileToGetAccess := FileToGetAccess;
  result := TryAndGetRightGetter;
  if result = nil then
    raise EArgumentNilException.Create('Argument Nil: ' +
      'TrimBasicsGetter is not set');
end;

function TTrimBasicsGetterFactory.TryAndGetRightGetter: TTrimBasicsGetter;
begin
  result := nil;
  result := TestGetterCompatibility(TFATTrimBasicsGetter, result);
  result := TestGetterCompatibility(TNTFSTrimBasicsGetter, result);
end;

function TTrimBasicsGetterFactory.TestGetterCompatibility(
  TTrimBasicsGetterToTry: TMetaTrimBasicsGetter;
  LastResult: TTrimBasicsGetter): TTrimBasicsGetter;
begin
  if LastResult <> nil then
    exit(LastResult);

  result := TTrimBasicsGetterToTry.Create(FFileToGetAccess);

  if not result.IsPartitionMyResponsibility then
    FreeAndNil(result);
end;

initialization
  TrimBasicsGetterFactory := TTrimBasicsGetterFactory.Create;
finalization
  TrimBasicsGetterFactory.Free;
end.
