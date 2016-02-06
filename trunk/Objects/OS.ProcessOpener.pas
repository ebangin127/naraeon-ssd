unit OS.ProcessOpener;

interface

uses
  SysUtils, Windows, Dialogs,
  OS.SecurityDescriptor;

type
  TProcessBuffer = reference to procedure (const CurrentBuffer: String;
    var CurrentResult: AnsiString);
  TProcessOpener = class
  public
    function OpenProcWithProcessFunction(const Path, Command: String;
      const ProcessBufferFunction: TProcessBuffer): AnsiString;
    function OpenProcWithOutput(const Path, Command: String): AnsiString;
    class function Create: TProcessOpener;
  private
    ProcessBuffer: TProcessBuffer;
    function GetSecurityDescriptor: TSecurityAttributes;
    procedure CreatePipeWithHandles(
      var ReadHandle, WriteHandle: THandle);
    function ReadFromHandle(const ReadHandle: THandle): AnsiString;
    var
      SecurityDescriptorManipulator: TSecurityDescriptorManipulator;
  end;
  procedure DefaultProcessBuffer(const CurrentBuffer: String;
    var CurrentResult: AnsiString);

var
  ProcessOpener: TProcessOpener;

implementation

function TProcessOpener.GetSecurityDescriptor: TSecurityAttributes;
begin
  result.nLength := sizeof(result);
  result.lpSecurityDescriptor :=
    SecurityDescriptorManipulator.GetSecurityDescriptor;
  result.bInheritHandle := true;
end;

class function TProcessOpener.Create: TProcessOpener;
begin
  if ProcessOpener = nil then
    result := inherited Create as self
  else
    result := ProcessOpener;
end;

procedure TProcessOpener.CreatePipeWithHandles(
  var ReadHandle, WriteHandle: THandle);
var
  SecurityAttributes: TSecurityAttributes;
begin
  SecurityAttributes := GetSecurityDescriptor;
  if not CreatePipe(ReadHandle, WriteHandle, @SecurityAttributes, 0) then
    raise EOSError.Create('CreatePipe Error (' + UIntToStr(GetLastError) + ')');
end;

function TProcessOpener.ReadFromHandle(
  const ReadHandle: THandle): AnsiString;
var
  Buffer: array[0..512] of AnsiChar;
  BytesRead: DWORD;
begin
  result := '';
  while
    (ReadFile(ReadHandle, Buffer, Length(Buffer) - 1, BytesRead, nil)) and
    (BytesRead > 0) do
  begin
    Buffer[BytesRead] := #0;
    ProcessBuffer(String(Buffer), result);
  end;
end;

function TProcessOpener.OpenProcWithOutput(const Path, Command: String
  ): AnsiString;
begin
  result := OpenProcWithProcessFunction(Path, Command, DefaultProcessBuffer);
end;

function TProcessOpener.OpenProcWithProcessFunction(
  const Path, Command: String;
  const ProcessBufferFunction: TProcessBuffer): AnsiString;
const
  StartupSettingsTemplate: TStartupInfo =
    (cb: sizeof(STARTUPINFO);
     dwFlags: STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
     wShowWindow: SW_HIDE);
var
  StartupSettings: TStartupInfo;
  WriteHandle, ReadHandle: THandle;
  ProcessInformation: _PROCESS_INFORMATION;
begin
  ProcessBuffer := ProcessBufferFunction;
  SecurityDescriptorManipulator := TSecurityDescriptorManipulator.Create;
  CreatePipeWithHandles(ReadHandle, WriteHandle);
  StartupSettings := StartupSettingsTemplate;
  StartupSettings.hStdOutput := WriteHandle;
  StartupSettings.hStdError := WriteHandle;
  if not CreateProcess(nil,
    PWideChar(WideString(Command)), nil, nil, True, 0, nil,
    PWideChar(WideString(Path)), StartupSettings, ProcessInformation) then
      raise EOSError.Create(
        'CreateProcess Error(Command: ' + Command + ' Path: ' + Path +
        ' ErrorCode: ' + UIntToStr(GetLastError) + ')');
  CloseHandle(WriteHandle);
  result := ReadFromHandle(ReadHandle);
  CloseHandle(ReadHandle);
  FreeAndNil(SecurityDescriptorManipulator);
end;

procedure DefaultProcessBuffer(const CurrentBuffer: String;
  var CurrentResult: AnsiString);
begin
  CurrentResult := AnsiString(String(CurrentResult) + CurrentBuffer);
end;

initialization
  ProcessOpener := TProcessOpener.Create;
finalization
  ProcessOpener.Free;
end.
