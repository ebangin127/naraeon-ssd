unit Parameter;

interface

uses
  SysUtils,
  OS.ServiceController, IdentifyDiagnosis, SemiAutoTrimmer;

type
  TParameter = class
  private
    ServiceController: TServiceController;
    procedure DeleteNaraeonSSDToolsServices;
    procedure OpenStopDeleteService(ServiceName: String);
    procedure DiagnoseAndSetClipboardResult;
    procedure SemiAutoTrim(Model, Serial: String);
  public
    function ProcessParameterAndIfNormalReturnTrue(
      ParameterString: String): Boolean;
  end;

implementation

{ TParameter }

procedure TParameter.OpenStopDeleteService(ServiceName: String);
begin
  ServiceController.OpenService(ServiceName);
  ServiceController.StopService;
  ServiceController.DeleteAndCloseService;
end;

procedure TParameter.DeleteNaraeonSSDToolsServices;
begin
  ServiceController := TServiceController.Create;
  OpenStopDeleteService('NareonSSDToolsDiag');
  OpenStopDeleteService('NaraeonSSDToolsDiag');
  FreeAndNil(ServiceController);
end;

procedure TParameter.DiagnoseAndSetClipboardResult;
var
  IdentifyDiagnosis: TIdentifyDiagnosis;
begin
  IdentifyDiagnosis := TIdentifyDiagnosis.Create;
  IdentifyDiagnosis.DiagnoseAndSetClipboardResult;
  FreeAndNil(IdentifyDiagnosis);
end;

procedure TParameter.SemiAutoTrim(Model, Serial: String);
var
  SemiAutoTrimmer: TSemiAutoTrimmer;
begin
  SemiAutoTrimmer := TSemiAutoTrimmer.Create;
  SemiAutoTrimmer.SemiAutoTrim(Model, Serial);
  FreeAndNil(SemiAutoTrimmer);
end;

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
    DiagnoseAndSetClipboardResult
  else if ParameterString = '/UNINSTALL' then
    DeleteNaraeonSSDToolsServices
  else if Pos(PointsErrFilePath, ParameterString) = 0 then
    SemiAutoTrim(ParamStr(1), ParamStr(2));
end;

end.
