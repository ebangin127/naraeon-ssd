unit OS.MutexManager;

interface

uses
  Windows;

type
  TMutexManager = class
  private
    MutexHandle: THandle;
    MutexName: String;
  public
    constructor Create(const MutexNameToUse: String);
    destructor Destroy; override;
    function OpenMutex: Boolean;
    function CreateMutex: Boolean;
    procedure ReleaseMutex;
  end;

implementation

{ TMutexManager }

constructor TMutexManager.Create(const MutexNameToUse: String);
begin
  MutexName := MutexNameToUse;
end;

procedure TMutexManager.ReleaseMutex;
begin
  Windows.ReleaseMutex(MutexHandle);
  CloseHandle(MutexHandle);
  MutexHandle := 0;
end;

function TMutexManager.CreateMutex: Boolean;
begin
  MutexHandle := Windows.CreateMutex(nil, true, PChar(MutexName));
  result :=
    (MutexHandle <> INVALID_HANDLE_VALUE) and
    (MutexHandle <> 0);
end;

destructor TMutexManager.Destroy;
begin
  if MutexHandle <> 0 then
    ReleaseMutex;
  inherited;
end;

function TMutexManager.OpenMutex: Boolean;
begin
  MutexHandle := Windows.OpenMutex(MUTEX_ALL_ACCESS, false, PChar(MutexName));
  result :=
    (MutexHandle <> INVALID_HANDLE_VALUE) and
    (MutexHandle <> 0);
end;

end.
