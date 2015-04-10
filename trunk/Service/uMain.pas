unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Vcl.ExtCtrls, WinInet, Registry, IdHttp, SHFolder, ShellAPI, ShlObj,
  TlHelp32, WinSvc,
  uDiskFunctions, uExeFunctions, uLogSystem, uLanguageSettings,
  uRegFunctions, uNSTSupport, uDatasizeUnit,
  uPhysicalDrive, uPartitionListGetter;

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
  private
    procedure RefreshDrives;
    procedure CheckDrives;
    procedure CreateLogger(CurrDrv: Integer);
    procedure DestroyLogger(CurrDrv: Integer);
    procedure MainWorks;
    procedure DestroyDrives;
    procedure LoadDrives;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

function GetServiceExecutablePath(strServiceName: string): String;

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
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.DeleteKey('.' + 'err');
    Reg.DeleteKey('errfile');
  finally
    Reg.Free;
  end;
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
end;

procedure TNaraeonSSDToolsDiag.ServiceExecute(Sender: TService);
var
  DesktopPath: array[0..MAX_PATH] of char;
begin
  RefreshDrives;
  OnceSMARTInvestigated := 0;

  if DriveCount = 0 then
    FreeAndNil(Application);

  OnceSMARTInvestigated := 0;

  if GetSystemDefaultLangID = 1042 then
    CurrLang := LANG_HANGUL
  else
    CurrLang := LANG_ENGLISH;

  RefreshDrives;
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0, @DesktopPath[0]);
  DeskPath := DesktopPath;

  while not Terminated do
    MainWorks;
end;

function GetLogLine(MsgTime: TDateTime; MsgContents: String): String;
begin
  result := FormatDateTime('[yy/mm/dd hh:nn:ss]', Now) + MsgContents;
end;

procedure TNaraeonSSDToolsDiag.MainWorks;
var
  CurrDrive, CurrPart: Integer;
  ReplacedSectors: UInt64;
  CurrDrvPartitions: TPartitionList;
  AllReadablePartition: String;
  SaveLog: TStringList;
  HostWrites: UInt64;
  ErrList: TStringList;
  CurrLine: Integer;
  MessageCount: Integer;
  ErrFilePath: String;
  PhysicalDrive: TPhysicalDrive;
const
  MessageWaitingTime = 500;
  SecondsPerHour = 3600;
  MillisecondPerSecond = 1000;
  SMARTInvestigateInterval = SecondsPerHour * MillisecondPerSecond;
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

  OnceSMARTInvestigated := SMARTInvestigateInterval;
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
    PhysicalDrive := TPhysicalDrive.Create(StrToInt(DriveList[CurrDrive]));
    HostWrites := MBToLiteONUnit(
      PhysicalDrive.SMARTInterpreted.TotalWrite.ValueInMiB);

    if PhysicalDrive.SupportStatus.TotalWriteType =
      TTotalWriteType.WriteSupportedAsValue then
        DriveWritInfoList[CurrDrive].ReadBothFiles(UIntToStr(HostWrites));

    //ReplacedSectors 처리
    ReplacedSectors := PhysicalDrive.SMARTInterpreted.ReplacedSectors;
    DriveSectInfoList[CurrDrive].ReadBothFiles(
      UIntToStr(ReplacedSectors));

    //ReplacedSectors 경고문
    if (PhysicalDrive.SMARTInterpreted.SMARTAlert.ReplacedSector = false) or
       (DriveSectInfoList[CurrDrive].LastOneGig = ReplacedSectors) then
       Continue;

    SaveLog := TStringList.Create;
    if FileExists(ErrFilePath) then
      SaveLog.LoadFromFile(ErrFilePath);

    PhysicalDrive := TPhysicalDrive.Create(StrToInt(DriveList[CurrDrive]));
    CurrDrvPartitions := PhysicalDrive.GetPartitionList;
    AllReadablePartition := '';
    for CurrPart := 0 to (CurrDrvPartitions.Count - 1) do
      AllReadablePartition := AllReadablePartition + ' ' +
                              CurrDrvPartitions[CurrPart].Letter;
    FreeAndNil(CurrDrvPartitions);
    FreeAndNil(PhysicalDrive);

    SaveLog.Add(
      GetLogLine(Now, ' !!!!! ' +
        AllReadablePartition + ' ' + CapBck[CurrLang] + ' !!!!! ' +
        CapBck2[CurrLang] + '(' + UIntToStr(ReplacedSectors) +
        CapCount[CurrLang] + ') ' + CapOcc[CurrLang]));

    SaveLog.SaveToFile(ErrFilePath);
    FreeAndNil(SaveLog);
    FreeAndNil(PhysicalDrive);
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
begin
  DestroyDrives;
  LoadDrives;
end;

procedure TNaraeonSSDToolsDiag.DestroyDrives;
var
  CurrDrv: Integer;
begin
  DriveCount := 0;
  for CurrDrv := 0 to 99 do
  begin
    DestroyLogger(CurrDrv);
  end;
end;

procedure TNaraeonSSDToolsDiag.LoadDrives;
var
  CurrDrv: Integer;
  hdrive: Integer;
begin
  for CurrDrv := 0 to 99 do
  begin
    DriveList[CurrDrv] := '';
    hdrive := CreateFile(PChar('\\.\PhysicalDrive' + IntToStr(CurrDrv)),
                                GENERIC_READ or GENERIC_WRITE,
                                FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                                OPEN_EXISTING, 0, 0);
    CreateLogger(CurrDrv);
    CloseHandle(hdrive);
  end;
end;

procedure TNaraeonSSDToolsDiag.CheckDrives;
var
  PhysicalDrive: TPhysicalDrive;
  CurrDrv: Integer;
begin
  NeedRefresh := false;
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    try
      PhysicalDrive := TPhysicalDrive.Create(StrToInt(DriveList[CurrDrv]));
    except
      NeedRefresh := true;
      FreeAndNil(PhysicalDrive);
    end;
  end;
end;

procedure TNaraeonSSDToolsDiag.CreateLogger(CurrDrv: Integer);
var
  PhysicalDrive: TPhysicalDrive;
begin
  if GetLastError <> 0 then
    exit;

  try
    PhysicalDrive := TPhysicalDrive.Create(CurrDrv);

    if PhysicalDrive.SupportStatus.Supported then
    begin
      if PhysicalDrive.SupportStatus.TotalWriteType <>
        TTotalWriteType.WriteNotSupported then
      begin
        DriveWritInfoList[DriveCount] :=
          TNSTLog.Create(AppPath, PhysicalDrive.IdentifyDeviceResult.Serial,
            UIntToStr(
              MBToLiteONUnit(
                PhysicalDrive.SMARTInterpreted.TotalWrite.ValueInMiB)),
            false);
      end
      else
        DriveWritInfoList[DriveCount] := nil;

      DriveSectInfoList[DriveCount] :=
        TNSTLog.Create(AppPath, PhysicalDrive.IdentifyDeviceResult.Serial +
          'RSLog', UIntToStr(PhysicalDrive.SMARTInterpreted.ReplacedSectors),
          true);

      DriveList[DriveCount] := IntToStr(CurrDrv);
      DriveCount := DriveCount + 1;
    end;
  finally
    FreeAndNil(PhysicalDrive);
  end;
end;

procedure TNaraeonSSDToolsDiag.DestroyLogger(CurrDrv: Integer);
begin
  if DriveWritInfoList[CurrDrv] <> nil then
    FreeAndNil(DriveWritInfoList[CurrDrv]);
  if DriveSectInfoList[CurrDrv] <> nil then
    FreeAndNil(DriveSectInfoList[CurrDrv]);
end;

function GetServiceExecutablePath(strServiceName: string): String;
var
  hSCManager, hSCService: SC_Handle;
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

function KillSelf: Boolean;
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
