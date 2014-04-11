program NSTDiagSvc;

uses
  Vcl.SvcMgr,
  uMain in 'uMain.pas' {NareonSSDToolsDiag: TService},
  uEasySMART in '..\EasySMART\uEasySMART.pas',
  hddInfo in '..\HddInfo\hddInfo.pas',
  WbemScripting_TLB in '..\HddInfo\WbemScripting_TLB.pas',
  Windows,
  uLogSystem in '..\uLogSystem.pas',
  uAlert in '..\uAlert.pas' {fAlert};

{$R *.RES}

var
  MutexAppear: Integer;

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  MutexAppear := OpenMutex(MUTEX_ALL_ACCESS, False, 'NSToolServiceOpened');
  if MutexAppear = 0 then
  begin
    MutexAppear := CreateMutex(Nil, True, 'NSToolServiceOpened');
    if not Application.DelayInitialize or Application.Installing then
      Application.Initialize;
    Application.CreateForm(TNareonSSDToolsDiag, NareonSSDToolsDiag);
  Application.CreateForm(TfAlert, fAlert);
  Application.Run;
    ReleaseMutex(MutexAppear);
    CloseHandle(MutexAppear);
  end;
end.
