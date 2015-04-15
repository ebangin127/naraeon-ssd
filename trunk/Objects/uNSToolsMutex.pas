unit uNSToolsMutex;

interface

uses
  Windows;

type
  TNSToolsMutex = class
  private
    MutexHandle: THandle;
    MutexName: String;

  public
    constructor Create(MutexNameToUse: String);
    destructor Destroy; override;

    function OpenMutex: Boolean;
    function CreateMutex: Boolean;
    procedure ReleaseMutex;
  end;

implementation

{ TNSToolsMutex }

constructor TNSToolsMutex.Create(MutexNameToUse: String);
begin
  MutexName := MutexNameToUse;
end;

procedure TNSToolsMutex.ReleaseMutex;
begin
  Windows.ReleaseMutex(MutexHandle);
  CloseHandle(MutexHandle);
  MutexHandle := 0;
end;

function TNSToolsMutex.CreateMutex: Boolean;
begin
  MutexHandle := Windows.CreateMutex(nil, true, PChar(MutexName));
  result :=
    (MutexHandle <> INVALID_HANDLE_VALUE) and
    (MutexHandle <> 0);
end;

destructor TNSToolsMutex.Destroy;
begin
  if MutexHandle <> 0 then
    ReleaseMutex;
  inherited;
end;

function TNSToolsMutex.OpenMutex: Boolean;
begin
  MutexHandle := Windows.OpenMutex(MUTEX_ALL_ACCESS, false, PChar(MutexName));
  result :=
    (MutexHandle <> INVALID_HANDLE_VALUE) and
    (MutexHandle <> 0);
end;

end.
