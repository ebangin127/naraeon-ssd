unit uParameter;

interface

uses
  SysUtils,
  uDiag, uExeFunctions, uSemiAuto;

type
  TParameter = class
  public
    function ProcessParameterAndIfNormalReturnTrue(
      ParameterString: String): Boolean;
  end;

implementation

{ TParameter }

function TParameter.ProcessParameterAndIfNormalReturnTrue(
  ParameterString: String): Boolean;
const
  PointsErrFilePath = ':\';
begin
  result := ParameterString = '';
  if result then
    exit;

  ParameterString := UpperCase(ParameterString);
  if ParameterString = '/DIAG' then
    TDiag.Diagnosis
  else if ParameterString = '/UNINSTALL' then
    DeletePrevSvc
  else if Pos(PointsErrFilePath, ParameterString) = 0 then
    TSemiAuto.SemiAutoTrim(ParamStr(1), ParamStr(2));
end;

end.
