unit OS.ServiceController;

interface

uses
  Windows, WinSvc;

type
  TServiceController = class
  private
    SCManagerHandle: THandle;
    ServiceHandle: THandle;
    ServiceStatus: TServiceStatus;
    procedure OpenSCManager;
    procedure CloseSCManager;
    function StopServiceAndSetServiceStatus: Boolean;
    function WaitForStopServiceAndIfNeedBreakFalse: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenService(const ServiceName: String);
    procedure CloseService;
    procedure StopService;
    procedure DeleteAndCloseService;
  end;

implementation

{ TServiceController }

procedure TServiceController.OpenSCManager;
begin
  SCManagerHandle := WinSvc.OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
end;

procedure TServiceController.CloseSCManager;
begin
  if SCManagerHandle <> 0 then
    CloseServiceHandle(SCManagerHandle);
  SCManagerHandle := 0;
end;

procedure TServiceController.OpenService(const ServiceName: String);
begin
  ServiceHandle :=
    WinSvc.OpenService(SCManagerHandle, PChar(ServiceName), SERVICE_ALL_ACCESS);
end;

procedure TServiceController.CloseService;
begin
  if ServiceHandle <> 0 then
    CloseServiceHandle(ServiceHandle);
  ServiceHandle := 0;
end;

constructor TServiceController.Create;
begin
  OpenSCManager;
end;

function TServiceController.StopServiceAndSetServiceStatus: Boolean;
begin
  result :=
    (ControlService(ServiceHandle, SERVICE_CONTROL_STOP, ServiceStatus)) and
    (QueryServiceStatus(ServiceHandle, ServiceStatus));
end;

function TServiceController.WaitForStopServiceAndIfNeedBreakFalse: Boolean;
var
  ValueIncreasedWhenStopped: DWORD;
begin
  ValueIncreasedWhenStopped := ServiceStatus.dwCheckPoint;
  Sleep(ServiceStatus.dwWaitHint);

  result := true;
  if (not QueryServiceStatus(ServiceHandle, ServiceStatus)) or
     (ServiceStatus.dwCheckPoint < ValueIncreasedWhenStopped) then
    result := false;
end;

procedure TServiceController.StopService;
begin
  if not StopServiceAndSetServiceStatus then
    exit;

  while SERVICE_STOPPED <> ServiceStatus.dwCurrentState do
    if not WaitForStopServiceAndIfNeedBreakFalse then
      break;
end;

procedure TServiceController.DeleteAndCloseService;
begin
  DeleteService(ServiceHandle);
  CloseService;
end;

destructor TServiceController.Destroy;
begin
  CloseService;
  CloseSCManager;
  inherited;
end;

end.
