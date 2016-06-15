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
    procedure OpenStopDeleteService(const ServiceName: String);
    procedure DiagnoseAndSetClipboardResult;
    procedure SemiAutoTrim(const Model, Serial: String);
  public
    function ProcessParameterAndIfNormalReturnTrue(
      const ParameterString: String): Boolean;
  end;

implementation

{ TParameter }

procedure TParameter.OpenStopDeleteService(const ServiceName: String);
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

procedure TParameter.SemiAutoTrim(const Model, Serial: String);
var
  SemiAutoTrimmer: TSemiAutoTrimmer;
begin
  SemiAutoTrimmer := TSemiAutoTrimmer.Create;
  SemiAutoTrimmer.SemiAutoTrim(Model, Serial);
  FreeAndNil(SemiAutoTrimmer);
end;

function TParameter.ProcessParameterAndIfNormalReturnTrue(
  const ParameterString: String): Boolean;
const
  PointsErrFilePath = ':\';
var
  UpperParameterString: String;
begin
  result := ParameterString = '';
  if result then
    exit;

  UpperParameterString := UpperCase(ParameterString);
  if UpperParameterString = '/DIAG' then
    DiagnoseAndSetClipboardResult
  else if UpperParameterString = '/UNINSTALL' then
    DeleteNaraeonSSDToolsServices
  else if Pos(PointsErrFilePath, UpperParameterString) = 0 then
    SemiAutoTrim(ParamStr(1), ParamStr(2));
end;

end.
