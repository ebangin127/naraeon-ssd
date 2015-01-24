program SSDTools;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fMain},
  uAlert in 'uAlert.pas' {fAlert},
  Windows,
  SysUtils,
  Classes,
  ShellAPI,
  uMessage in 'uMessage.pas' {fMessage},
  uBrowser in 'uBrowser.pas' {fBrowser},
  uDiskFunctions in 'Modules\Disk\uDiskFunctions.pas',
  uPartitionFunctions in 'Modules\Disk\uPartitionFunctions.pas',
  uSMARTFunctions in 'Modules\Disk\uSMARTFunctions.pas',
  uIntFunctions in 'Modules\Etc\uIntFunctions.pas',
  uLanguageSettings in 'Modules\Language\uLanguageSettings.pas',
  uRegFunctions in 'Modules\Windows\uRegFunctions.pas',
  uFileFunctions in 'Modules\Windows\uFileFunctions.pas',
  uStrFunctions in 'Modules\Etc\uStrFunctions.pas',
  uExeFunctions in 'Modules\Windows\uExeFunctions.pas',
  uLogSystem in 'Classes\LogSystem\uLogSystem.pas',
  uOptimizer in 'Classes\Optimizer\uOptimizer.pas',
  uSSDInfo in 'Classes\SSDInfo\uSSDInfo.pas',
  uSSDSupport in 'Classes\SSDInfo\uSSDSupport.pas',
  uTrimThread in 'Classes\Threads\uTrimThread.pas',
  uUpdateThread in 'Classes\Threads\uUpdateThread.pas',
  uVersion in 'Classes\Version\uVersion.pas',
  uDownloadPath in 'Classes\DownloadPath\uDownloadPath.pas',
  uPlugAndPlay in 'Modules\Windows\uPlugAndPlay.pas',
  uATALowOps in 'Classes\ATALowOps\uATALowOps.pas',
  uFirmware in 'ModulesForUI\Firmware\uFirmware.pas',
  uUSBDrive in 'ModulesForUI\USBDrive\uUSBDrive.pas',
  uRefresh in 'ModulesForUI\Refresh\uRefresh.pas',
  uUAWebbrowser in 'Classes\UAWebbrowser\uUAWebbrowser.pas',
  uButtonGroup in 'ModulesForUI\ButtonGroup\uButtonGroup.pas',
  uInit in 'ModulesForUI\Init\uInit.pas',
  uGetFirm in 'Classes\GetFirm\uGetFirm.pas',
  uSevenZip in 'Classes\SevenZip\uSevenZip.pas',
  uTrimList in 'Classes\Threads\uTrimList.pas',
  uRufus in 'Classes\Rufus\uRufus.pas',
  uDiag in 'Classes\Diag\uDiag.pas',
  uSSDList in 'Classes\SSDList\uSSDList.pas',
  uPathManager in 'Classes\PathManager\uPathManager.pas',
  uSemiAuto in 'Classes\SemiAuto\uSemiAuto.pas';

type
  TRunMode = (RM_NORMAL, RM_DIAG, RM_SEMIAUTO);

{$R *.res}
var
  //캡션 생성 및 뮤텍스 찾기
  Cap: String;

  //명령행 해석
  ParamInUpper: String;
  RunMode: TRunMode;

  //현재 프로세스 뮤텍스 관리
  MutexAppear: LongInt;

begin
  Application.Initialize;
  Cap := 'Naraeon SSD Tools ' + CurrentVersion + CapToSeeSerial[CurrLang];

  TPathManager.GetPath(Application);
  DetermineLanguage;

  ParamInUpper := UpperCase(ParamStr(1));

  SimulationMode := ParamInUpper = '/SIMULMODE';
  RunMode := RM_NORMAL;

  if ParamInUpper = '/DIAG' then
    RunMode := RM_DIAG
  else if (not SimulationMode) and (ParamInUpper <> '') then
    RunMode := RM_SEMIAUTO;

  MutexAppear := 0;

  TGetFirm.CreateCache;
  case RunMode of
  RM_NORMAL:
  begin
    SimulationMode := ParamInUpper = '/SIMULMODE';

    MutexAppear := OpenMutex(MUTEX_ALL_ACCESS, False, 'NSToolsOpened');

    if MutexAppear <> 0 then
    begin
      MutexAppear := FindWindow(PChar('TfMain'), PChar(Cap));
      if (MutexAppear <> 0)
      and (Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) <> '.err') then
      begin
        ShowWindow(MutexAppear, SW_RESTORE);
        SetForegroundWindow(MutexAppear);
        CloseHandle(MutexAppear);
      end;
    end
    else
    begin
      MutexAppear := CreateMutex(Nil, True, 'NSToolsOpened');
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfMain, fMain);
      fMain.Caption := Cap;
      Application.Run;
    end;
  end;

  RM_DIAG:
  begin
    TDiag.Diagnosis;
  end;

  RM_SEMIAUTO:
  begin
    TSemiAuto.SemiAutoTrim(ParamStr(1), ParamStr(2));
    TGetFirm.DestroyCache;
  end;

  end;
  TGetFirm.DestroyCache;

  if MutexAppear <> 0 then
  begin
    ReleaseMutex(MutexAppear);
    CloseHandle(MutexAppear);
  end;
end.
