unit uDiagThread;

interface

uses Classes;

type
  TMakeThread = class(TThread)
  public
    FormMessage: String;
    AllStageNum: Integer;
    AppPath: String;
    NeedDebug: Boolean;
    NoSave: Boolean;
    Debugged: Byte;
  protected
    Stagenum: Integer;
    RobotPort: String;
    procedure Execute; override;
    procedure GetAppPath;
    procedure ChangeStage;
    procedure EndTask;
    procedure Debug;
  end;

implementation

uses uMain;

procedure TMakeThread.Execute;
var
  Output, CleanFile, Command: String;
  i: integer;
begin
    Synchronize(ChangeStage);
    Debugged := 0;
    FormMessage := '';
    Synchronize(GetAppPath);
    for i := 0 to Template.CleanList.Count - 1 do
    begin
      CleanFile := Template.ConnFolder + Template.MakeFolder + '\' + Template.CleanList.Strings[i];
      if FileExists(CleanFile) then DeleteFile(PChar(CleanFile));
    end;
    Template.ReadMission(Mission);
    if NoSave = false then Template.Save;
    if NeedDebug = True then
    begin
      Synchronize(Debug);
      while Debugged = 0 do ;
    end else Debugged := 1;
    Stagenum := 1;
    Synchronize(ChangeStage);
    if Debugged = 1 then
    begin
      for i := 1 to Template.MakeList.Count do
      begin
        AllStageNum := Template.MakeList.Count;
        StageNum := i;
        Synchronize(ChangeStage);
        Command := Template.MakeList.Strings[i - 1];
        Output := OpenProcWithOutput(Template.ConnFolder + Template.MakeFolder, Command);
        if Output <> '' then
        begin
          FormMessage := IntToStr(i) + '단계에서 에러' + Chr(13) + Chr(10) + '명령어 : ' + Template.MakeList.Strings[i - 1] + Chr(13) + Chr(10) + '메시지 : ' + Output + Chr(13) + Chr(10) + '위 메시지를 starrygyu450@gmail.com로 보내주세요';
          Synchronize(EndTask);
          Synchronize(fMain.CreateForm);
        end;
      end;
      FormMessage := FormMessage + Template.ConnFolder + Template.MakeFolder + '\' + Template.ResultFile + '에 컴파일 완료!';
      Stagenum := Stagenum + 1;
      Synchronize(ChangeStage);
    end
    else
    begin
      Synchronize(EndTask);
    end;
end;

procedure TMakeThread.GetAppPath;
begin
  AppPath := fMain.AppPath;
end;

procedure TMakeThread.ChangeStage;
var
  aHandle : THandle;
  Progressbar : THandle;
  Editbox : THandle;
  PID, i : Integer;
  BufProgress: integer;
  BufEdit: String;
  ProgramRegistry: TRegistry;
  ValueList: TStringList;
begin
  if (Stagenum = 0) or (Stagenum < AllStageNum) then
  begin
    fAlert.tTransparent.Enabled := false;
    if Stagenum = 0 then fAlert.lMessage.Caption := '준비중'
    else fAlert.lMessage.Caption := 'Stage ' + IntToStr(Stagenum) + '/' + IntToStr(AllStageNum) + ' 진행중';
  end
  else if Stagenum = AllStageNum then
  begin
    //Find Robot
    ValueList := TStringList.Create;
    ProgramRegistry := TRegistry.Create;
    if Template.RootKey = 'CR' then ProgramRegistry.RootKey := HKEY_CLASSES_ROOT
    else if Template.RootKey = 'CU' then ProgramRegistry.RootKey := HKEY_CURRENT_USER
    else if Template.RootKey = 'LM' then ProgramRegistry.RootKey := HKEY_LOCAL_MACHINE
    else if Template.RootKey = 'U' then ProgramRegistry.RootKey := HKEY_USERS
    else if Template.RootKey = 'PD' then ProgramRegistry.RootKey := HKEY_PERFORMANCE_DATA
    else if Template.RootKey = 'CC' then ProgramRegistry.RootKey := HKEY_CURRENT_CONFIG
    else if Template.RootKey = 'DD' then ProgramRegistry.RootKey := HKEY_DYN_DATA;
    ProgramRegistry.OpenKey(Template.OpenKey, False);
    ProgramRegistry.GetValueNames(ValueList);
    for i := 0 to ValueList.Count - 1 do
    begin
      if Pos(Template.Valuename, ValueList[i]) <> 0 then
      begin
        RobotPort := ProgramRegistry.ReadString(ValueList[i]);
      end;
    end;
    ProgramRegistry.CloseKey;
    //FreeAndNil
    FreeAndNil(ValueList);
    FreeAndNil(ProgramRegistry);
    if Pos('아카데미', Template.Name) <> 0 then
    begin
      aHandle := FindWindow(Nil, 'PRO 다운로더');
      while aHandle <> 0 do
      begin
        Sleep(100);
        aHandle := FindWindow(Nil, 'PRO 다운로더');
        Progressbar := FindWindowEx(aHandle, 0, 'msctls_progress32', Nil);
        SendMessage(Progressbar, PBM_GETPOS, 32, Integer(@BufProgress));
        if BufProgress = 0 then SendMessage(aHandle, WM_CLOSE, 32, Integer(@BufProgress));
      end;
    end;
    fAlert.tTransparent.Enabled := true;
    fAlert.lMessage.Caption := '컴파일 완료!';
    if Template.Programmer = 'Custom' then ShellExecute(0, Nil, PChar(fMain.AppPath + 'Downloader\' + Template.DownloaderFileName),Nil, Nil, SW_NORMAL )
    else if Template.Programmer = 'AVRDUDE' then
    begin
      if RobotPort = '' then RobotPort := 'COM3';
      ShellExecute(0, Nil, PChar('avrdude -c stk500  -p m16 -P ' + RobotPort + ' -U flash:w:main.hex'), Nil, PChar(Template.ConnFolder + Template.MakeFolder), SW_NORMAL );
    end;
    if Pos('아카데미', Template.Name) <> 0 then
    begin
      PID := 0;
      aHandle := FindWindow(Nil, 'PRO 다운로더');
      while aHandle = 0 do
      begin
        Sleep(100);
        aHandle := FindWindow(Nil, 'PRO 다운로더');
      end;
      GetWindowThreadProcessId(aHandle, @PID);
      Editbox := FindWindowEx(aHandle, 0, 'Edit', Nil);
      BufEdit := Template.ConnFolder + 'default\main.hex';
      SendMessage(Editbox, WM_SETTEXT, Length(BufEdit), Integer(@BufEdit[1]));
      aHandle := FindWindow(Nil, 'PRO 다운로더');
      if aHandle <> 0 then GetWindowThreadProcessId(aHandle, @PID);
      Editbox := FindWindowEx(aHandle, 0, 'ComboBox', Nil);
      BufEdit := RobotPort;
      SendMessage(Editbox, CB_SELECTSTRING, Length(BufEdit), Integer(@BufEdit[1]));
    end;
  end;
end;

procedure TMakeThread.EndTask;
begin
  fAlert.Close;
end;

procedure TMakeThread.Debug;
begin
  try
    fDebug := TfDebug.Create(fMain);
    fDebug.Source := Template.ConnFolder + '\' + Template.SrcFileName;
    fDebug.Template := Template;
    fDebug.eCode.Lines.LoadFromFile(Template.ConnFolder + '\' + Template.SrcFileName);
    fDebug.Caption := '소스 수정중 (현재 : ' + Template.SrcFileName + ' 작업중)';
    fDebug.bTrans.Caption := '현재 파일 저장 후 ' + Template.LinkFileName + ' 열기';
    fDebug.ShowModal;
  finally
    if fDebug.Saved = True then Debugged := 1
    else Debugged := 2;
    FreeAndNil(fDebug);
  end;
end;
end.
