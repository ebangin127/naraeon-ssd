unit uExeFunctions;

interface

uses
  SysUtils, Classes, Windows, Dialogs,
  uRegFunctions;

function WriteBufferCheck: TStringList; //쓰기 버퍼 체크되어있나 확인
function OpenProcWithOutput(Path: String; Command: String): AnsiString; //프로그램 열기
procedure OpenProcWOOutput(Path: String; Command: String);
function Is64Bit: Boolean;

function CheckUNetbootin: Boolean;

implementation

uses
  uMain;

function WriteBufferCheck: TStringList;
var
  DevicesList, SerialsList: TStringList;
  ValueList: TStringList;
  CurrDev, CurrSer: Integer;
begin
  result := TStringList.Create;
  DevicesList := TStringList.Create;
  SerialsList := TStringList.Create;
  ValueList := TStringList.Create;
  GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\IDE', DevicesList);
  for CurrDev := 0 to DevicesList.Count - 1 do
  begin
    if ((Pos('SAMSUNG', DevicesList[CurrDev]) > 0) and (Pos('SSD', DevicesList[CurrDev]) > 0)) or
        (Pos('LITEONIT', DevicesList[CurrDev]) > 0) or
        ((Pos('PLEXTOR', DevicesList[CurrDev]) > 0) and (Pos('PX', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('JT', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('MMY', DevicesList[CurrDev]) > 0)) then
    begin
      GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev], SerialsList);
      for CurrSer := 0 to SerialsList.Count - 1 do
      begin
        GetValueList('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', ValueList);
        if GetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', 'UserWriteCacheSetting') = 0 then
        begin
          SetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                              '\Device Parameters\Disk', 'UserWriteCacheSetting', 1);
          result.Add(GetRegStr('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer]
                    , 'FriendlyName'));
        end;
      end;
    end;
  end;
  GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI', DevicesList);
  for CurrDev := 0 to DevicesList.Count - 1 do
  begin
    if ((Pos('SAMSUNG', DevicesList[CurrDev]) > 0) and (Pos('SSD', DevicesList[CurrDev]) > 0)) or
        (Pos('LITEONIT', DevicesList[CurrDev]) > 0) or
        ((Pos('PLEXTOR', DevicesList[CurrDev]) > 0) and (Pos('PX', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('JT', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('MMY', DevicesList[CurrDev]) > 0)) then
    begin
      GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev], SerialsList);
      for CurrSer := 0 to SerialsList.Count - 1 do
      begin
        GetValueList('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', ValueList);
        if GetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', 'UserWriteCacheSetting') = 0 then
        begin
          SetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                              '\Device Parameters\Disk', 'UserWriteCacheSetting', 1);
          result.Add(GetRegStr('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer]
                    , 'FriendlyName'));
        end;
      end;
    end;
  end;
  FreeAndNil(DevicesList);
  FreeAndNil(SerialsList);
  FreeAndNil(ValueList);
end;

function OpenProcWithOutput(Path: String; Command: String): AnsiString;
var
  start: TStartupInfo;
  sec: TSecurityAttributes;
  pinfo: TProcessInformation;
  hwrite, hread: THandle;
  BytesRead: DWORD;
  Buffer: array[0..512] of Ansichar;
  ResultString: AnsiString;
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

procedure OpenProcWOOutput(Path: String; Command: String);
var
  start: TStartupInfo;
  sec: TSecurityAttributes;
  pinfo: TProcessInformation;
  hwrite, hread: THandle;
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
  CloseHandle(hread);
end;

function Is64Bit: Boolean;
type
  TIsWow64Process = function(AHandle:THandle; var AIsWow64: BOOL): BOOL; stdcall;
var
  vKernel32Handle: DWORD;
  vIsWow64Process: TIsWow64Process;
  vIsWow64: BOOL;
begin
  Result := False;

  vKernel32Handle := LoadLibrary('kernel32.dll');
  if (vKernel32Handle = 0) then Exit;
  try
    @vIsWow64Process := GetProcAddress(vKernel32Handle, 'IsWow64Process');
    if not Assigned(vIsWow64Process) then Exit;
    vIsWow64 := False;
    if (vIsWow64Process(GetCurrentProcess, vIsWow64)) then
      Result := vIsWow64;
  finally
    FreeLibrary(vKernel32Handle);
  end;
end;

function CheckUNetbootin: Boolean;
begin
  result := FileExists(AppPath + 'Unetbootin\unetbootin.exe');
end;
end.
