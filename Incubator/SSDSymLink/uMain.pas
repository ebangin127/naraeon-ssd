//SSD Setting HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\Enum\IDE
unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ShellAPI, Vcl.StdCtrls, FileCtrl, SHFolder, uFolderFunctions,
  Vcl.CheckLst, ShlObj, uAlert, uEasyReg, Vcl.ExtCtrls, uEasySMART;

type
  TfMain = class(TForm)
    bRamdisk: TButton;
    cRamdisk: TComboBox;
    cRamList: TCheckListBox;
    lRamdisk: TLabel;
    lStatus: TLabel;
    tRam: TTimer;
    procedure bRamdiskClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cRamdiskChange(Sender: TObject);
  private
    procedure HideControls;
    procedure ShowControls;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;
  FirstRam: String;

implementation

{$R *.dfm}

procedure TfMain.bRamdiskClick(Sender: TObject);
var
  Succeed: Array[0..3] of Boolean;
  ChromeDataChr: array [0..MAX_PATH] of char;
  Startup: array [0..MAX_PATH] of char;
  ChromeData: String;
  OriginalDownloads: String;
  DeleteScript, CreateScript, SucceedList: TStringList;
begin
  if cRamdisk.Items[cRamdisk.ItemIndex] <> '' then
  begin
    HideControls;
    SHGetFolderPath(0, CSIDL_STARTUP, 0, 0, @Startup[0]);
    Succeed[0] := false;
    Succeed[1] := false;
    Succeed[2] := false;
    Succeed[3] := false;
    DeleteScript := TStringList.Create;
    CreateScript := TStringList.Create;

    if FileExists(Startup + '\DeleteFoldersScript.cmd') then
      DeleteScript.LoadFromFile(Startup + '\DeleteFoldersScript.cmd')
    else
      DeleteScript.Add('@echo off');

    if FileExists(Startup + '\CreateFoldersScript.cmd') then
      CreateScript.LoadFromFile(Startup + '\CreateFoldersScript.cmd')
    else
      CreateScript.Add('@echo off');

    try
      if cRamList.Checked[0] then
      begin
        if cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp' <>
          GetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'Cache') then
        begin
          lStatus.Caption := 'IE - 폴더 옮기는 중... (1/2)';
          Application.ProcessMessages;
          OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex],
                             GetEnvironmentVariable('windir') + '\System32\cmd.exe /C mkdir "' + cRamdisk.Items[cRamdisk.ItemIndex] +
                             'IETemp"');
          CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp');
          DeleteScript.Add('rmdir /S /Q' + GetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'Cache'));
          SetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'Cache',
                          cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp');

          lStatus.Caption := 'IE - 폴더 옮기는 중... (2/2)';
          Application.ProcessMessages;
          DeleteScript.Add('rmdir /S /Q' + GetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders', 'Cache'));
          SetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders', 'Cache',
                          cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp');
          Succeed[0] := true;
        end
        else
        begin
          if CreateScript.IndexOf('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp') = -1 then
            CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp');
          Succeed[0] := true;
        end;
      end;

      if (cRamList.Checked[1]) and (cRamList.Count = 4) then
      begin
        SHGetFolderPath(0,CSIDL_LOCAL_APPDATA ,0,0,@ChromeDataChr[0]);
        ChromeData := ChromeDataChr + '\Google\Chrome\User Data\Default\Cache';
        if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd') then
          DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd');
        CreateScript.SaveToFile(ChromeData + '\CreateFoldersScript.cmd');
        if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd') = false then
        begin
          DeleteFile(ChromeData + 'Chrome\CreateFoldersScript.cmd');
          lStatus.Caption := '크롬 - 기존 폴더 삭제중... (1/2)';
          Application.ProcessMessages;
          OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex],
                             GetEnvironmentVariable('windir') + '\System32\cmd.exe /C rmdir "' + ChromeData +
                             '" /S /Q');
          if DirectoryExists(ChromeData) then
          begin
            AlertCreate(Self, '크롬을 모두 종료하신 뒤 다시 시도해주세요.');
          end
          else
          begin
            lStatus.Caption := '크롬 - 심볼릭 링크 생성중... (2/2)';
            Application.ProcessMessages;
            OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex],
                               GetEnvironmentVariable('windir') + '\System32\cmd.exe /C mkdir "' + cRamdisk.Items[cRamdisk.ItemIndex] +
                               'Chrome"');
            CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome');
            OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex],
                               GetEnvironmentVariable('windir') + '\System32\cmd.exe /C mklink /D "' + ChromeData +
                               '" "' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\"');
            Succeed[1] := true;
          end;
        end
        else
        begin
          DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd');
          if CreateScript.IndexOf('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome') = -1 then
            CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome');
          Succeed[1] := true;
        end;
      end;

      if ((cRamList.Checked[2]) and (cRamList.Count = 4)) or
         ((cRamList.Checked[1]) and (cRamList.Count = 3)) then
      begin
        if cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp' <>
          GetRegStr('CU', 'Environment', 'TEMP') then
        begin
          lStatus.Caption := '운영체제 임시 파일 - 사용자 임시 폴더 옮기는 중... (1/2)';
          Application.ProcessMessages;
          OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex],
                             GetEnvironmentVariable('windir') + '\System32\cmd.exe /C mkdir "' + cRamdisk.Items[cRamdisk.ItemIndex] +
                             'Temp"');
          CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp');
          DeleteScript.Add('rmdir /S /Q' + GetRegStr('CU', 'Environment', 'TEMP'));
          DeleteScript.Add('rmdir /S /Q' + GetRegStr('CU', 'Environment', 'TMP'));
          SetRegStr('CU', 'Environment', 'TEMP',
                          cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp');
          SetRegStr('CU', 'Environment', 'TMP',
                          cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp');

          lStatus.Caption := '운영체제 임시 파일 - 전체 임시 폴더 옮기는 중... (2/2)';
          Application.ProcessMessages;
          DeleteScript.Add('rmdir /S /Q' + GetRegStr('LM', 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'TEMP'));
          DeleteScript.Add('rmdir /S /Q' + GetRegStr('LM', 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'TMP'));
          SetRegStr('LM', 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'TEMP',
                          cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp');
          SetRegStr('LM', 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'TMP',
                          cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp');
          Succeed[2] := true;
        end
        else
        begin
          if CreateScript.IndexOf('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp') = -1 then
            CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp');
          Succeed[2] := true;
        end;
      end;

      if ((cRamList.Checked[3]) and (cRamList.Count = 4)) or
         ((cRamList.Checked[2]) and (cRamList.Count = 3)) then
      begin
        OriginalDownloads := GetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
                                      '{374DE290-123F-4565-9164-39C4925E467B}');
        if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd') then
          DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd');
        CreateScript.SaveToFile(OriginalDownloads + '\CreateFoldersScript.cmd');
        if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd') = false then
        begin
          DeleteFile(OriginalDownloads + '\CreateFoldersScript.cmd');
          lStatus.Caption := '다운로드 - 폴더 복사(B → A)중... (1/3)';
          Application.ProcessMessages;
          OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex],
                             GetEnvironmentVariable('windir') + '\System32\cmd.exe /C mkdir "' + cRamdisk.Items[cRamdisk.ItemIndex] +
                             'Downloads"');
          CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads');
          OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads', GetEnvironmentVariable('windir') + '\System32\cmd.exe /C robocopy "' + OriginalDownloads
                                             + '" "' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads' + '" /E');
          lStatus.Caption := '다운로드 - 기존 폴더(B) 삭제중... (2/3)';
          Application.ProcessMessages;
          OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads', GetEnvironmentVariable('windir') + '\System32\cmd.exe /C rmdir "' + OriginalDownloads +
                                             '" /S /Q');
          if DirectoryExists(OriginalDownloads) then
          begin
            ShowMessage('다운로드중인 프로그램을 모두 종료하신 뒤 다시 시도해주세요.');
          end
          else
          begin
            lStatus.Caption := '다운로드 - 심볼릭 링크 생성중... (3/3)';
            Application.ProcessMessages;
            OpenProcWithOutput(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads', GetEnvironmentVariable('windir') + '\System32\cmd.exe /C mklink /D "' + OriginalDownloads +
                                               '" "' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads' + '"');
            Succeed[3] := true;
          end;
        end
        else
        begin
          DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd');
          if CreateScript.IndexOf('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads') = -1 then
            CreateScript.Add('mkdir ' + cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads');
          Succeed[3] := true;
        end;
      end;
    Finally
      if DeleteScript.Count > 0 then
        if DeleteScript[DeleteScript.Count - 1] <> 'del %0' then
          DeleteScript.Add('del %0');
      DeleteScript.SaveToFile(Startup + '\DeleteFoldersScript.cmd');
      CreateScript.SaveToFile(Startup + '\CreateFoldersScript.cmd');
      FreeAndNil(DeleteScript);
      FreeAndNil(CreateScript);
    End;
    SucceedList := TStringList.Create;
    if Succeed[0] = true then SucceedList.Add(' * IE 임시파일');
    if Succeed[1] = true then SucceedList.Add(' * 크롬 임시파일');
    if Succeed[2] = true then SucceedList.Add(' * 운영체제 임시파일');
    if Succeed[3] = true then SucceedList.Add(' * 다운로드 폴더');

    if SucceedList.Count > 0 then AlertCreate(Self, '램디스크로의 다음 폴더 이동이 완료되었습니다.' + Chr(13) + Chr(10)
                                                      + SucceedList.Text);
    FreeAndNil(SucceedList);
    cRamdisk.OnChange(nil);
    ShowControls;
  end;
end;

procedure TfMain.cRamdiskChange(Sender: TObject);
var
  ChromeDataChr: array [0..MAX_PATH] of char;
  ChromeData, OriginalDownloads: String;
  CreateScript: TStringList;
  Startup: array [0..MAX_PATH] of char;
  SelectedRamdisk: String;
begin
  if cRamdisk.ItemIndex >= 0 then
  begin
    CreateScript := TStringList.Create;
    CreateScript.Add('TESTING! TESTING! TESTING!');
    cRamList.Items.Text := FirstRam;

    if cRamdisk.Items[cRamdisk.ItemIndex] + 'IETemp' =
      GetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'Cache') then
    begin
      cRamList.Items[0] := cRamList.Items[0] + ' (이미 완료됨)';
      cRamList.Checked[0] := false;
    end;

    if cRamList.Items.Count = 4 then
    begin
      SHGetFolderPath(0,CSIDL_LOCAL_APPDATA ,0,0,@ChromeDataChr[0]);
      ChromeData := ChromeDataChr + '\Google\Chrome\User Data\Default\Cache';
      if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd') then
        DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd');
      CreateScript.SaveToFile(ChromeData + '\CreateFoldersScript.cmd');
      if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd') = true then
      begin
        DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Chrome\CreateFoldersScript.cmd');
        cRamList.Items[1] := cRamList.Items[1] + ' (이미 완료됨)';
        cRamList.Checked[1] := false;
      end
      else
        DeleteFile(ChromeData + '\CreateFoldersScript.cmd');
    end;

    if cRamdisk.Items[cRamdisk.ItemIndex] + 'Temp' =
        GetRegStr('CU', 'Environment', 'TEMP') then
    begin
      if cRamList.Items.Count = 4 then
      begin
        cRamList.Items[2] := cRamList.Items[2] + ' (이미 완료됨)';
        cRamList.Checked[2] := false;
      end
      else
      begin
        cRamList.Items[1] := cRamList.Items[1] + ' (이미 완료됨)';
        cRamList.Checked[1] := false;
      end;
    end;

    OriginalDownloads := GetRegStr('CU', 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
                                  '{374DE290-123F-4565-9164-39C4925E467B}');
    if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd') then
      DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd');
    CreateScript.SaveToFile(OriginalDownloads + '\CreateFoldersScript.cmd');
    if FileExists(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd') = true then
    begin
      DeleteFile(cRamdisk.Items[cRamdisk.ItemIndex] + 'Downloads\CreateFoldersScript.cmd');
      if cRamList.Items.Count = 4 then
      begin
        cRamList.Items[3] := cRamList.Items[3] + ' (이미 완료됨)';
        cRamList.Checked[3] := false;
      end
      else
      begin
        cRamList.Items[2] := cRamList.Items[2] + ' (이미 완료됨)';
        cRamList.Checked[2] := false;
      end;
    end
    else
      DeleteFile(OriginalDownloads + '\CreateFoldersScript.cmd');
    FreeAndNil(CreateScript);
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FirstRam := cRamList.Items.Text;
  GetRamDrives(cRamdisk.Items);
  if (cRamdisk.Items.Count > 0) and (cRamdisk.ItemIndex = -1) then
  begin
    cRamdisk.ItemIndex := 0;
    cRamdiskChange(nil);
  end;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
  Constraints.MaxWidth := Width;
  Constraints.MinWidth := Width;
end;

procedure TfMain.HideControls;
begin
  lRamdisk.Visible := false;
  cRamdisk.Visible := false;
  cRamList.Visible := false;
  bRamdisk.Visible := false;
end;

procedure TfMain.ShowControls;
begin
  lRamdisk.Visible := true;
  cRamdisk.Visible := true;
  cRamList.Visible := true;
  bRamdisk.Visible := true;
end;

end.
