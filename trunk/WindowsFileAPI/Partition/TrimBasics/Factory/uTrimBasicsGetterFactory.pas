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
    class function GetSuitableTrimBasicsGetter(FileToGetAccess: String):
      TTrimBasicsGetter;

  private
    constructor Create;
    class var FFileToGetAccess: String;
    class function TryAndGetRightGetter: TTrimBasicsGetter;
    class function TestGetterCompatibility(TTrimBasicsGetterToTry:
      TMetaTrimBasicsGetter; LastResult: TTrimBasicsGetter):
      TTrimBasicsGetter;
  end;

implementation

constructor TTrimBasicsGetterFactory.Create;
begin

end;

class function TTrimBasicsGetterFactory.GetSuitableTrimBasicsGetter(
  FileToGetAccess: String): TTrimBasicsGetter;
begin
  FFileToGetAccess := FileToGetAccess;
  result := TryAndGetRightGetter;
  if result = nil then
    raise EArgumentNilException.Create('Argument Nil: ' +
      'TrimBasicsGetter is not set');
end;

class function TTrimBasicsGetterFactory.TryAndGetRightGetter: TTrimBasicsGetter;
begin
  result := nil;
  result := TestGetterCompatibility(TFATTrimBasicsGetter, result);
  result := TestGetterCompatibility(TNTFSTrimBasicsGetter, result);
end;

class function TTrimBasicsGetterFactory.TestGetterCompatibility(
  TTrimBasicsGetterToTry: TMetaTrimBasicsGetter;
  LastResult: TTrimBasicsGetter): TTrimBasicsGetter;
begin
  if LastResult <> nil then
    exit(LastResult);

  result := TTrimBasicsGetterToTry.Create(FFileToGetAccess);

  if not result.IsPartitionMyResponsibility then
    FreeAndNil(result);
end;
end.
