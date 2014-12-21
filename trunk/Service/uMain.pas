unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Vcl.ExtCtrls, WinInet, Registry, IdHttp, SHFolder, ShellAPI, ShlObj,
  TlHelp32, WinSvc,
  uDiskFunctions, uExeFunctions, uLogSystem, uSSDInfo, uLanguageSettings,
  uRegFunctions, uSSDSupport;

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
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceBeforeUnInstall(Sender: TService);
    procedure ServiceBeforeInstall(Sender: TService);
  private
    procedure RefreshDrives;
    procedure CheckDrives;
    procedure DeletePrevSvc;
    procedure LoggerCreate(CurrDrv: Integer);
    procedure MainWorks(SSDInfo: TSSDInfo_NST);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

function GetServiceExecutablePath(strServiceName: string): String;
function DeleteServiceNST(strServiceName: string): String;
function KillAllSvc(): Boolean;
function KillSelf(): Boolean;
function KillProcess(const ProcName: String; Suicide: Boolean): Boolean;

var
  NaraeonSSDToolsDiag: TNaraeonSSDToolsDiag;
  WinDir, WinDrive: String;
  AppPath: String;
  DeskPath: String;
  DriveList: Array[0..99] of String;
  DriveWritInfoList: Array[0..99] of TNSTLog;
  DriveSectInfoList: Array[0..99] of TNSTLog;
  DriveCount: Integer;
  OnceSMARTInvestigated: Integer;
  NSTLog: TNSTLog;
  NeedRefresh: Boolean;
  Cap: String;

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

procedure TNaraeonSSDToolsDiag.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('SYSTEM\CurrentControlSet\Services\' + Name, false) then
    begin
      Reg.WriteString('Description',
        'Naraeon SSD Tools - SSD life alerter service.');
      Reg.CloseKey;
    end;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey('.' + 'err', True) then
    begin
      Reg.WriteString('', 'errfile') ;
      Reg.CloseKey;
      Reg.CreateKey('errfile') ;
      Reg.OpenKey('errfile\DefaultIcon', True) ;
      WinDir := GetEnvironmentVariable('windir');
      WinDrive := ExtractFileDrive(WinDir);
      AppPath :=
        ExtractFilePath(GetRegStr('LM',
          'Software\Microsoft\Windows\CurrentVersion\' +
          'Uninstall\Naraeon SSD Tools\', 'UninstallString'));
      Reg.WriteString('', AppPath + 'Image\warning.ico') ;
      Reg.CloseKey;
      Reg.OpenKey('err' + 'file\shell\open\command', True) ;
      Reg.WriteString('', AppPath + 'SSDTools.exe "%1"') ;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TNaraeonSSDToolsDiag.ServiceAfterUninstall(Sender: TService);
var
  Reg: TRegistry;
  aHandle: THandle;
begin
  if GetSystemDefaultLangID = 1042 then
    CurrLang := LANG_HANGUL
  else
    CurrLang := LANG_ENGLISH;
  Cap := 'Naraeon SSD Tools ' + CurrentVersion + CapToSeeSerial[CurrLang];

  aHandle := FindWindow(Nil, PChar(Cap));

  while aHandle <> 0 do
  begin
    Sleep(10);
    SendMessage(aHandle, WM_CLOSE, 0, 0);
    aHandle := FindWindow(Nil, PChar(Cap));
  end;

  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.DeleteKey('.' + 'err');
    Reg.DeleteKey('errfile');
  finally
    Reg.Free;
  end;
end;

procedure TNaraeonSSDToolsDiag.ServiceBeforeInstall(Sender: TService);
begin
  KillAllSvc();
  DeletePrevSvc;
end;

procedure TNaraeonSSDToolsDiag.ServiceBeforeUnInstall(Sender: TService);
var
  NewSvc: String;
begin
  NewSvc := ExtractFileName(GetServiceExecutablePath('NaraeonSSDToolsDiag'));
  KillAllSvc();
  if Length(NewSvc) = 0 then
  begin
    DeletePrevSvc;
    KillSelf();
  end;
  DeletePrevSvc;
end;

procedure TNaraeonSSDToolsDiag.ServiceCreate(Sender: TObject);
begin
  DriveCount := 0;
  WinDir := GetEnvironmentVariable('windir');
  WinDrive := ExtractFileDrive(WinDir);
  AppPath :=
    ExtractFilePath(GetRegStr('LM',
      'Software\Microsoft\Windows\CurrentVersion\Uninstall\Naraeon SSD Tools\',
      'UninstallString'));
  OnceSMARTInvestigated := 0;
end;

procedure TNaraeonSSDToolsDiag.ServiceExecute(Sender: TService);
var
  SSDInfo: TSSDInfo_NST;
  DesktopPath: array[0..MAX_PATH] of char;
begin
  RefreshDrives;

  if DriveCount = 0 then
    FreeAndNil(Application);

  OnceSMARTInvestigated := 0;

  if GetSystemDefaultLangID = 1042 then
    CurrLang := LANG_HANGUL
  else
    CurrLang := LANG_ENGLISH;

  SSDInfo := TSSDInfo_NST.Create;
  RefreshDrives;
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0, @DesktopPath[0]);
  DeskPath := DesktopPath;

  while not Terminated do
    MainWorks(SSDInfo);

  FreeAndNil(SSDInfo);
end;

function GetLogLine(MsgTime: TDateTime; MsgContents: String): String;
begin
  result := FormatDateTime('[yy/mm/dd hh:nn:ss]', Now) + MsgContents;
end;

procedure TNaraeonSSDToolsDiag.MainWorks(SSDInfo: TSSDInfo_NST);
var
  CurrDrive, CurrPart: Integer;
  ReplacedSectors: UInt64;
  CurrDrvPartitions: TDriveLetters;
  AllReadablePartition: String;
  SaveLog: TStringList;
  HostWrites: UInt64;
  ErrList: TStringList;
  CurrLine: Integer;
  MessageCount: Integer;
  ErrFilePath: String;
const
  MessageWaitingTime = 500;
begin
  //서비스 종료 등 요청 처리
  for MessageCount := 0 to MessageWaitingTime - 1 do
  begin
    ServiceThread.ProcessRequests(False);
    Sleep(1);
  end;

  CheckDrives;
  if NeedRefresh then
    RefreshDrives;

  //드라이브 없으면 종료
  if DriveCount = 0 then
    FreeAndNil(Application);

  Dec(OnceSMARTInvestigated, 1);
  if FormatDateTime('HH:mm', Now) = '00:00' then
    OnceSMARTInvestigated := 0;

  //시간 안 되면 다음 회차 진행
  if OnceSMARTInvestigated > 0 then
    exit;

  OnceSMARTInvestigated := 3600;
  ErrFilePath := DeskPath + '\!!!SSDError!!!.err';

  //쓰기 버퍼 체크
  ErrList := WriteBufferCheck;
  if ErrList.Count > 0 then
  begin
    SaveLog := TStringList.Create;
    if FileExists(ErrFilePath) then
      SaveLog.LoadFromFile(ErrFilePath);

    for CurrLine := 0 to ErrList.Count - 1 do
      SaveLog.Add(GetLogLine(Now,
        ' !!!!! ' + ErrList[CurrLine] + ' ' + CapWrongBuf[CurrLang] +
        ' !!!!! ' + CapWrongBuf2[CurrLang]));

    SaveLog.SaveToFile(ErrFilePath);
    FreeAndNil(SaveLog);
  end;
  FreeAndNil(ErrList);

  for CurrDrive := 0 to DriveCount - 1 do
  begin
    //HostWrite 처리
    SSDInfo.ATAorSCSI := MODEL_DETERMINE;
    SSDInfo.SetDeviceName(StrToInt(DriveList[CurrDrive]));
    SSDInfo.CollectAllSmartData;

    HostWrites := SSDInfo.HostWrites;

    if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_FULL then
      DriveWritInfoList[CurrDrive].ReadBothFiles(UIntToStr(HostWrites));

    //ReplacedSectors 처리
    ReplacedSectors := SSDInfo.ReplacedSectors;
    DriveSectInfoList[CurrDrive].ReadBothFiles(
      UIntToStr(ReplacedSectors));

    //ReplacedSectors 경고문
    if (SSDInfo.RepSectorAlert = false) or
       (DriveSectInfoList[CurrDrive].LastOneGig = ReplacedSectors) then
       Continue;

    SaveLog := TStringList.Create;
    if FileExists(ErrFilePath) then
      SaveLog.LoadFromFile(ErrFilePath);

    CurrDrvPartitions := GetPartitionList(DriveList[CurrDrive]);
    AllReadablePartition := '';
    for CurrPart := 0 to (CurrDrvPartitions.LetterCount - 1) do
      AllReadablePartition := AllReadablePartition + ' ' +
                              CurrDrvPartitions.Letters[CurrPart];

    SaveLog.Add(
      GetLogLine(Now, ' !!!!! ' +
        AllReadablePartition + ' ' + CapBck[CurrLang] + ' !!!!! ' +
        CapBck2[CurrLang] + '(' + UIntToStr(ReplacedSectors) +
        CapCount[CurrLang] + ') ' + CapOcc[CurrLang]));

    SaveLog.SaveToFile(ErrFilePath);
    FreeAndNil(SaveLog);
  end;
end;

procedure TNaraeonSSDToolsDiag.ServiceShutdown(Sender: TService);
var
  CurrDrv: Integer;
begin
  for CurrDrv := 0 to DriveCount - 1 do
    FreeAndNil(DriveWritInfoList[CurrDrv]);
  for CurrDrv := 0 to DriveCount - 1 do
    FreeAndNil(DriveSectInfoList[CurrDrv]);
end;

procedure TNaraeonSSDToolsDiag.ServiceStop(Sender: TService;
  var Stopped: Boolean);
var
  CurrDrv: Integer;
begin
  for CurrDrv := 0 to DriveCount - 1 do
    FreeAndNil(DriveWritInfoList[CurrDrv]);
  for CurrDrv := 0 to DriveCount - 1 do
    FreeAndNil(DriveSectInfoList[CurrDrv]);
end;

procedure TNaraeonSSDToolsDiag.RefreshDrives;
var
  CurrDrv: Integer;
  hdrive: Integer;
begin
  DriveCount := 0;
  for CurrDrv := 0 to 99 do
  begin
    DriveList[CurrDrv] := '';
    hdrive := CreateFile(PChar('\\.\PhysicalDrive' + IntToStr(CurrDrv)),
                                GENERIC_READ or GENERIC_WRITE,
                                FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                                OPEN_EXISTING, 0, 0);
    LoggerCreate(CurrDrv);
    CloseHandle(hdrive);
  end;
end;


procedure TNaraeonSSDToolsDiag.LoggerCreate(CurrDrv: Integer);
var
  TempSSDInfo: TSSDInfo_NST;
begin
  if GetLastError <> 0 then
    exit;

  try
    TempSSDInfo := TSSDInfo_NST.Create;
    TempSSDInfo.ATAorSCSI := MODEL_DETERMINE;
    TempSSDInfo.SetDeviceName(StrToInt(IntToStr(CurrDrv)));

    if TempSSDInfo.SupportedDevice <> SUPPORT_NONE then
    begin
      TempSSDInfo.CollectAllSmartData;
      if TempSSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_FULL then
      begin
        DriveWritInfoList[DriveCount] :=
          TNSTLog.Create(AppPath, TempSSDInfo.Serial,
            UIntToStr(TempSSDInfo.HostWrites), false, TempSSDInfo.S10085);
      end;
      DriveSectInfoList[DriveCount] :=
        TNSTLog.Create(AppPath, TempSSDInfo.Serial + 'RSLog',
          UIntToStr(TempSSDInfo.ReplacedSectors), true, false);
      DriveList[DriveCount] := IntToStr(CurrDrv);
      DriveCount := DriveCount + 1;
    end;
  finally
    FreeAndNil(TempSSDInfo);
  end;
end;

procedure TNaraeonSSDToolsDiag.CheckDrives;
var
  TempSSDInfo: TSSDInfo_NST;
  CurrDrv: Integer;
  hdrive: Integer;
begin
  NeedRefresh := false;
  TempSSDInfo := TSSDInfo_NST.Create;
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    hdrive := CreateFile(PChar('\\.\PhysicalDrive' + DriveList[CurrDrv]),
                                GENERIC_READ or GENERIC_WRITE,
                                FILE_SHARE_READ or FILE_SHARE_WRITE,
                                nil, OPEN_EXISTING, 0, 0);
    TempSSDInfo.SetDeviceName(StrToInt(DriveList[CurrDrv]));
    if GetLastError = 0 then
    begin
      if TempSSDInfo.SupportedDevice <> SUPPORT_NONE then
        NeedRefresh := true;
    end
    else
      NeedRefresh := true;
    CloseHandle(hDrive)
  end;
  FreeAndNil(TempSSDInfo);
end;

procedure TNaraeonSSDToolsDiag.DeletePrevSvc;
var
  Reg: TRegistry;
begin
  DeleteServiceNST('NareonSSDToolsDiag');
  DeleteServiceNST('NaraeonSSDToolsDiag');
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('SYSTEM\CurrentControlSet\Services\NareonSSDToolsDiag',
                    false) then
    begin
      Reg.CloseKey;
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey('SYSTEM\CurrentControlSet\Services', false);
      Reg.DeleteKey('NareonSSDToolsDiag');
      Reg.CloseKey;
    end;
    Reg.CloseKey;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('SYSTEM\CurrentControlSet\Services\NaraeonSSDToolsDiag',
                    false) then
    begin
      Reg.CloseKey;
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKey('SYSTEM\CurrentControlSet\Services', false);
      Reg.DeleteKey('NaraeonSSDToolsDiag');
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

function DeleteServiceNST(strServiceName: string): String;
var
  hSCManager,hSCService: SC_Handle;
begin
  Result := '';
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (hSCManager > 0) then
  begin
    hSCService := OpenService(hSCManager, PChar(strServiceName),
                              SERVICE_QUERY_CONFIG);
    if (hSCService > 0) then
    begin
      DeleteService(hSCService);
      CloseServiceHandle(hSCService);
    end;
  end;
end;

function GetServiceExecutablePath(strServiceName: string): String;
var
  hSCManager,hSCService: SC_Handle;
  lpServiceConfig: LPQUERY_SERVICE_CONFIGW;
  nSize, nBytesNeeded: DWord;
begin
  Result := '';
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (hSCManager > 0) then
  begin
    hSCService := OpenService(hSCManager, PChar(strServiceName),
                              SERVICE_QUERY_CONFIG);
    if (hSCService > 0) then
    begin
      QueryServiceConfig(hSCService, nil, 0, nSize);
      lpServiceConfig := AllocMem(nSize);
      try
        if not QueryServiceConfig(
          hSCService, lpServiceConfig, nSize, nBytesNeeded) Then Exit;
          Result := lpServiceConfig^.lpBinaryPathName;
      finally
        Dispose(lpServiceConfig);
      end;
      CloseServiceHandle(hSCService);
    end;
  end;
end;

function KillAllSvc(): Boolean;
var
  OldSvc: String;
  NewSvc: String;
begin
  OldSvc := ExtractFileName(GetServiceExecutablePath('NareonSSDToolsDiag'));
  NewSvc := ExtractFileName(GetServiceExecutablePath('NaraeonSSDToolsDiag'));
  Result := KillProcess(OldSvc, False) and KillProcess(NewSvc, False);
end;

function KillSelf(): Boolean;
var
  hProcess: THandle;
begin
  Result := True;

  hProcess := OpenProcess(PROCESS_TERMINATE, True, GetCurrentProcessId());
  if not TerminateProcess(hProcess, 0) then Result := False;
  CloseHandle(hProcess);
end;

function KillProcess(const ProcName: String; Suicide: Boolean): Boolean;
var
  Process32: TProcessEntry32;
  SHandle: THandle;
  Next: Boolean;
  hProcess: THandle;
begin
  Result := True;

  Process32.dwSize := SizeOf(TProcessEntry32);
  Process32.th32ProcessID := 0;
  SHandle := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if Process32First(SHandle, Process32) then begin
    repeat
      Next := Process32Next(SHandle, Process32);
      if ((AnsiCompareText(Process32.szExeFile, Trim(ProcName)) = 0) and
          ((GetCurrentProcessId() <> Process32.th32ProcessID) or
            (Suicide))) then
      begin
        if Process32.th32ProcessID <> 0 then
        begin
          hProcess := OpenProcess(PROCESS_TERMINATE, True,
                                  Process32.th32ProcessID);
          if hProcess <> 0 then begin
            if not TerminateProcess(hProcess, 0) then Result := False;
          end
          else Result := False;

          CloseHandle(hProcess);
        end
        else Result := False;
      end;
    until not Next;
  end;
  CloseHandle(SHandle);
end;
end.
