unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls,
  uDiagnosisService;

type
  TNaraeonSSDToolsDiag = class(TService)
    tDiagnosis: TTimer;
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure tDiagnosisTimer(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
  private
    DiagnosisService: TDiagnosisService;
    procedure WaitForTerminate;
    procedure InitializeAndWaitForEnd;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  NaraeonSSDToolsDiag: TNaraeonSSDToolsDiag;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  NaraeonSSDToolsDiag.Controller(CtrlCode);
end;

function TNaraeonSSDToolsDiag.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TNaraeonSSDToolsDiag.InitializeAndWaitForEnd;
begin
  DiagnosisService.InitializePhysicalDriveList;
  WaitForTerminate;
end;

procedure TNaraeonSSDToolsDiag.ServiceCreate(Sender: TObject);
begin
  DiagnosisService := TDiagnosisService.Create;
  {$IFDEF DEBUG} InitializeAndWaitForEnd; {$ENDIF}
end;

procedure TNaraeonSSDToolsDiag.ServiceDestroy(Sender: TObject);
begin
  if DiagnosisService <> nil then
    FreeAndNil(DiagnosisService);
end;

procedure TNaraeonSSDToolsDiag.ServiceExecute(Sender: TService);
begin
  InitializeAndWaitForEnd;
end;

procedure TNaraeonSSDToolsDiag.WaitForTerminate;
begin
  tDiagnosis.Enabled := true;
  tDiagnosis.OnTimer(self);
  {$IFNDEF DEBUG}
  while not Terminated do
    ServiceThread.ProcessRequests(true);
  {$ELSE}
  while true do
    Sleep(1);
  {$ENDIF}
  tDiagnosis.Enabled := false;
end;

procedure TNaraeonSSDToolsDiag.tDiagnosisTimer(Sender: TObject);
begin
  DiagnosisService.Diagnosis;
  if not DiagnosisService.IsThereAnySupportedDrive then
    DoStop;
end;

end.
