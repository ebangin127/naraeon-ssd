//기능
//수명 감시&호스트쓰기 기록
unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  uEasySMART, HddInfo, Vcl.ExtCtrls, WinInet, Registry, IdHttp, uAlert, SHFolder, ShellAPI,
  uLogSystem, ShlObj;

type
  TNareonSSDToolsDiag = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  NareonSSDToolsDiag: TNareonSSDToolsDiag;
  WinDir, WinDrive: String;
  AppPath: String;
  DeskPath: String;
  DriveList: Array[0..99] of String;
  DriveInfoList: Array[0..99] of TNSTLog;
  DriveCount: Integer;
  OnceAlertCreated: Integer;
  NSTLog: TNSTLog;

const
  DebugMode = False;
  CurrentVersion = '2.1';

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  NareonSSDToolsDiag.Controller(CtrlCode);
end;

function TNareonSSDToolsDiag.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TNareonSSDToolsDiag.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegistry;
  AppData: Array[0..MAX_PATH] of Char;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, false) then
    begin
      Reg.WriteString('Description', 'Naraeon SSD Tools의 보조 서비스로서, 수명 관리를 맡고 있습니다.' + Chr(13) + Chr(10) +
                                      '즉, 하루종일 SMART 정보 들여다보고 있지 않아도 NST가 알아서 경고를 해주니 안심하세요.');
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
      AppPath := WinDrive + '\Naraeon\SSDTools\';
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

procedure TNareonSSDToolsDiag.ServiceAfterUninstall(Sender: TService);
var
  Reg: TRegistry;
  aHandle: THandle;
begin
  aHandle := FindWindow(Nil, 'Naraeon SSD Tools ' + CurrentVersion +
                              ' (도움말 : F1)');
  while aHandle <> 0 do
  begin
    Sleep(10);
    SendMessage(aHandle, WM_CLOSE, 0, 0);
    aHandle := FindWindow(Nil, 'Naraeon SSD Tools ' + CurrentVersion +
                                ' (도움말 : F1)');
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

procedure TNareonSSDToolsDiag.ServiceCreate(Sender: TObject);
var
  TempHDDInfo: THddInfo;
  CurrDrv: Integer;
  AppData: Array[0..MAX_PATH] of Char;
begin
  for CurrDrv := 0 to 99 do
    DriveList[CurrDrv] := '';
  DriveCount := 0;
  TempHDDInfo := THDDInfo.Create(Self);
  TempHDDInfo.CanonicalScsiAddressing := false;
  WinDir := GetEnvironmentVariable('windir');
  WinDrive := ExtractFileDrive(WinDir);
  AppPath := WinDrive + '\Naraeon\SSDTools\';
  for CurrDrv := 0 to 99 do
  begin
    //if Length(GetPartitionList(IntToStr(CurrDrv)).Letters) > 0  then
    //begin
      CreateFile(PChar('\\.\PhysicalDrive' + IntToStr(CurrDrv)), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      if GetLastError <> ERROR_FILE_NOT_FOUND then
      begin
        TempHDDInfo.Method := TGetInfoMethod(gimByName);
        try
          TempHDDInfo.DeviceName := 'PhysicalDrive' + IntToStr(CurrDrv);
        finally
          if (Pos('LITEONIT', TempHDDInfo.Model) > 0) or
             (Pos('PLEXTOR', TempHDDInfo.Model) > 0) or
             ((Pos('SAMSUNG', TempHDDInfo.Model) > 0) and (Pos('830', TempHDDInfo.Model) > 0)) or
             (DebugMode) then
          begin
            if Pos('SAMSUNG', TempHDDInfo.Model) > 0 then
              DriveInfoList[DriveCount] := TNSTLog.Create(AppPath, TempHDDInfo.Serial,
                                                IntToStr(round(ExtractSMART('F1', IntToStr(CurrDrv)) / 1024 / 2048 * 10 * 1.56)))
            else
              DriveInfoList[DriveCount] := TNSTLog.Create(AppPath, TempHDDInfo.Serial,
                                                IntToStr(round(ExtractSMART(177, IntToStr(CurrDrv)))));
            DriveList[DriveCount] := IntToStr(CurrDrv);
            DriveCount := DriveCount + 1;
          end;
        end;
      //end;
    end;
  end;
  FreeAndNil(TempHddInfo);
  if (DriveCount = 0) or (DebugMode = true) then
  begin
    AlertCreate(nil, 'SSD가 검출되지 않았습니다.' + Chr(13) + Chr(10) + '이방인(ebangin127@gmail.com)에게 패치를 요청해보세요.');
    FreeAndNil(Application);
  end
  else
  begin
    OnceAlertCreated := 0;
  end;
end;

procedure TNareonSSDToolsDiag.ServiceExecute(Sender: TService);
var
  CurrDrive, CurrPart: Integer;
  ReplacedSectors: Integer;
  CurrDrvPartitions: TDriveLetters;
  AllReadablePartition: String;
  SaveLog: TStringList;
  HDDInfo: THddInfo;
  HostWrites: Integer;
  DesktopPath: array[0..MAX_PATH] of char;
begin
  HDDInfo := THDDInfo.Create(nil);
  while not Terminated do
  begin
    ServiceThread.ProcessRequests(False);
    Sleep(1000);
    SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0, @DesktopPath[0]);
    DeskPath := DesktopPath;
    if DriveCount > 0 then
    begin
      if OnceAlertCreated = 0 then
      begin
        OnceAlertCreated := 1800;

        for CurrDrive := 0 to DriveCount - 1 do
        begin
          HDDInfo.CanonicalScsiAddressing := false;
          HDDInfo.Method := TGetInfoMethod(gimByName);
          HDDInfo.DeviceName := 'PhysicalDrive' + DriveList[CurrDrive];
          if Pos('SAMSUNG', HDDInfo.Model) > 0 then HostWrites := round(ExtractSMART('F1', DriveList[CurrDrive]) / 1024 / 2048 * 10 * 1.56)
          else HostWrites := ExtractSMART(177, DriveList[CurrDrive]);
          DriveInfoList[CurrDrive].ReadBothFiles(IntToStr(HostWrites));
        end;
        SaveLog := TStringList.Create;
        for CurrDrive := 0 to DriveCount - 1 do
        begin
          HDDInfo.CanonicalScsiAddressing := false;
          HDDInfo.Method := TGetInfoMethod(gimByName);
          HDDInfo.DeviceName := 'PhysicalDrive' + DriveList[CurrDrive];
          if Pos('SAMSUNG', HDDInfo.Model) > 0 then ReplacedSectors := ExtractSMART(5, DriveList[CurrDrive])
          else ReplacedSectors := ExtractSMART(196, DriveList[CurrDrive]);

          DriveInfoList[CurrDrive].ReadBothFiles(IntToStr(HostWrites));
          if ReplacedSectors >= 150 then
          begin
            CurrDrvPartitions.Letters := '';
            CurrDrvPartitions := GetPartitionList(DriveList[CurrDrive]);
            AllReadablePartition := '';
            for CurrPart := 1 to Length(CurrDrvPartitions.Letters) do
              AllReadablePartition := AllReadablePartition + ' ' +
                                      CurrDrvPartitions.Letters[CurrPart] + ':';
            SaveLog.Add(FormatDateTime('[yy/mm/dd hh:nn:ss]', Now) + ' !!!!! ' + AllReadablePartition + ' 즉시 백업 요망 !!!!! ' +
                                        'SSD에서 죽은 섹터(' + IntToStr(ReplacedSectors) + '개) 발생');
            SaveLog.SaveToFile(DeskPath + '\!!!SSDError!!!.err');
            OnceAlertCreated := 3600;
          end;
        end;
      end
      else if (OnceAlertCreated > 0) then
        Dec(OnceAlertCreated, 1);
      FreeAndNil(SaveLog);
    end;
  end;
  FreeAndNil(HDDInfo);
end;

procedure TNareonSSDToolsDiag.ServiceShutdown(Sender: TService);
var
  CurrDrv: Integer;
begin
  for CurrDrv := 0 to DriveCount - 1 do
    FreeAndNil(DriveInfoList[CurrDrv]);
end;

procedure TNareonSSDToolsDiag.ServiceStop(Sender: TService;
  var Stopped: Boolean);
var
  CurrDrv: Integer;
begin
  for CurrDrv := 0 to DriveCount - 1 do
    FreeAndNil(DriveInfoList[CurrDrv]);
end;

end.
