unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls,
  uDiagnosisService;

type
  PDevBroadcastHdr = ^TDevBroadcastHdr;
  TDevBroadcastHdr = packed record
    dbcd_size: DWORD;
    dbcd_devicetype: DWORD;
    dbcd_reserved: DWORD;
  end;

type
  PDevBroadcastPortA = ^TDevBroadcastPortA;
  TDevBroadcastPortA = packed record
    dbcp_size: DWORD;
    dbcp_devicetype: DWORD;
    dbcp_reserved: DWORD;
    dbcp_name: Array[0..4] of Char;
  end;

type
  TNaraeonSSDToolsDiag = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
  private
    DiagnosisService: TDiagnosisService;
    procedure WaitForMessage;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
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

procedure TNaraeonSSDToolsDiag.ServiceCreate(Sender: TObject);
begin
  DiagnosisService := TDiagnosisService.Create;
end;

procedure TNaraeonSSDToolsDiag.ServiceDestroy(Sender: TObject);
begin
  if DiagnosisService <> nil then
    FreeAndNil(DiagnosisService);
end;

procedure TNaraeonSSDToolsDiag.WaitForMessage;
const
  WaitingTimeInMillisecond = 500;
var
  ElapsedMillisecond: Integer;
begin
  for ElapsedMillisecond := 0 to WaitingTimeInMillisecond - 1 do
  begin
    ServiceThread.ProcessRequests(False);
    Sleep(1);
  end;
end;

procedure TNaraeonSSDToolsDiag.ServiceExecute(Sender: TService);
begin
  DiagnosisService.InitializePhysicalDriveList;

  repeat
    DiagnosisService.Diagnosis;
    WaitForMessage;
    if not DiagnosisService.IsThereAnySupportedDrive then
      exit;
  until Terminated;
end;
end.
