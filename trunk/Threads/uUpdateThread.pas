unit uUpdateThread;

interface

uses
  Classes, SysUtils,
  uUpdater;

type
  TUpdateThread = class(TThread)
  private
    class var InnerUpdateNotice: String;
    CheckUpdateResult: TCheckUpdateResult;
    Updater: TUpdater;
    procedure StartUpdate;
  protected
    procedure Execute; override;
  public
    class property UpdateNotice: String read InnerUpdateNotice;
    constructor Create; overload;
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
  end;

implementation

uses
  uMain;


constructor TUpdateThread.Create;
begin
  Create(true);
end;

constructor TUpdateThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  Updater := TUpdater.Create;
  FreeOnTerminate := true;
end;

destructor TUpdateThread.Destroy;
begin
  FreeAndNil(Updater);
  inherited Destroy;
end;

procedure TUpdateThread.StartUpdate;
begin
  InnerUpdateNotice := CheckUpdateResult.UpdateNotice;
  Synchronize(Updater.StartUpdate);
  while not Terminated do
    Sleep(100);
end;

procedure TUpdateThread.Execute;
begin
  CheckUpdateResult := Updater.CheckUpdate;
  if not CheckUpdateResult.IsUpdateNeeded then
    exit
  else
    StartUpdate;
end;
end.
