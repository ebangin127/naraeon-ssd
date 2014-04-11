unit uFolderFunctions;

interface

uses Windows, ShellApi, SysUtils, Dialogs, FileCtrl, ActiveX, ShlObj;

type
  KNOWNFOLDERID = TGuid;

const
  FOLDERID_ProgramData: KNOWNFOLDERID =
    '{374DE290-123F-4565-9164-39C4925E467B}';

var
  SHGetKnownFolderPathFunc: function( const rfid: KNOWNFOLDERID;
    dwFlags: DWORD; hToken: THandle; var ppszPath: PWideChar ): HResult; stdcall;
  SHGetKnownFolderIDListFunc: function( const rfid: KNOWNFOLDERID;
    dwFlags: DWORD; hToken: THandle; var ppidl: PItemIDList ): HResult; stdcall;

function GetDownloadsFolderPath: string;
function AdvSelectDirectory(WindowTitle: String): String;
function DeleteDirectory(Const DirPath: String): Boolean;

function OpenProcWithOutput(Path: String; Command: String): String;

implementation

function AdvSelectDirectory(WindowTitle: String): String;
begin
  if Win32MajorVersion >= 6 then
  begin
    with TFileOpenDialog.Create(nil) do
      try
        Title := WindowTitle;
        Options := [fdoPickFolders];
        if Execute then
          Result := FileName;
      finally
        Free;
      end;
  end
  else
  begin
    SelectDirectory(WindowTitle, 'C:\', Result, [sdNewUI, sdNewFolder]);
  end;
end;

function OpenProcWithOutput(Path: String; Command: String): String;
var
  start: TStartupInfo;
  sec: TSecurityAttributes;
  pinfo: TProcessInformation;
  hwrite, hread: THandle;
  BytesRead: DWORD;
  Buffer: array[0..512] of Ansichar;
  ResultString: string;
  PathW, CommandW: WideString;
begin
  sec.nLength := sizeof(sec);
  sec.lpSecurityDescriptor := nil;
  sec.bInheritHandle := true;
  if CreatePipe(hread, hwrite, @sec, 0)<>true then
  begin
    exit;
  end;
  FillChar(start, sizeof(STARTUPINFO), 0);
  start.cb := sizeof(STARTUPINFO);
  start.wShowWindow := SW_HIDE;
  start.dwFlags := STARTF_USESHOWWINDOW;
  start.dwFlags := start.dwFlags or STARTF_USESTDHANDLES;
  start.hStdOutput := hwrite;
  start.hStdError := hwrite;
  PathW := Path;
  CommandW := Command;
  if not CreateProcess(nil, PWideChar(CommandW), nil, nil, True, 0, nil, PWideChar(PathW), start, pinfo) then ShowMessage('Error!');
  CloseHandle(hwrite);
  while ReadFile(hread, Buffer, Length(buffer)-1, BytesRead, nil) and (BytesRead>0) do
  begin
    Buffer[BytesRead] := #0;
    ResultString := ResultString + Buffer;
  end;
  CloseHandle(hread);
  Result := ResultString;
end;

function PathFromIDList( Pidl: ShlObj.PItemIdList ): string;
var
  Path: array[ 0..MAX_PATH ] of Char;
begin
  if SHGetPathFromIDList( Pidl, Path ) then
    Result := Path
  else
    Result := '';
end;

function GetDownloadsFolderPath: string;
var
  Path: PWideChar;
  Pidl: PItemIdList;
begin
  Result := '';
  if @SHGetKnownFolderPathFunc <> nil then
  begin
    if Succeeded( SHGetKnownFolderPathFunc( FOLDERID_ProgramData, 0, 0, Path ) ) then
      begin
        try
          Result := Path;
        finally; CoTaskMemFree( Path ); end;
        Exit;
      end;
  end
  else if @SHGetKnownFolderIDListFunc <> nil then
  begin
    if Succeeded( SHGetKnownFolderIDListFunc( FOLDERID_ProgramData, 0, 0, Pidl ) ) then
      begin
        try
          Result := PathFromIDList( Pidl );
        finally; CoTaskMemFree( Pidl ); end;
        Exit;
      end;
  end;
  if Succeeded( SHGetFolderLocation( 0, CSIDL_PROFILE, 0, 0, Pidl ) ) then
    try
      Result := PathFromIDList( Pidl ) + '\Downloads';
    finally; CoTaskMemFree( Pidl ); end;
end;

procedure InitVistaFunctions;
var
  hShell32: THandle;
begin
  hShell32 := GetModuleHandle( 'SHELL32' );
  @SHGetKnownFolderPathFunc := Windows.GetProcAddress(hShell32, 'SHGetKnownFolderPath');
  @SHGetKnownFolderIDListFunc := Windows.GetProcAddress(hShell32, 'SHGetKnownFolderIDList');
end;

function DeleteDirectory(Const DirPath: String): Boolean;
var
  SHFileOpStruct: TSHFileOpStruct;
  DirBuf: array [0..255] of char;
  Directory: string;
  iFindResult: integer;
 srSchRec : TSearchRec;
begin
  iFindResult := FindFirst(DirPath + '*.*', faAnyFile, srSchRec);
  while iFindResult = 0 do
  begin
    try
      Directory := ExcludeTrailingPathDelimiter(DirPath + srSchRec.Name);
      Fillchar(SHFileOpStruct, sizeof(SHFileOpStruct), 0);
      FillChar(DirBuf, sizeof(DirBuf), 0);
      StrPCopy(DirBuf, Directory);
      with SHFileOpStruct do
      begin
        Wnd := 0;
        pFrom := @DirBuf;
        wFunc := FO_DELETE;
        fFlags := fFlags or FOF_NOCONFIRMATION;
        fFlags := fFlags or FOF_SILENT;
      end;
      Result := (SHFileOperation(SHFileOpStruct) = 0);
    except
      Result := False;
    end;
    iFindResult := FindNext(srSchRec);
  end;
  FindClose(srSchRec);
end;

end.
