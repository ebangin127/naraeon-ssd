unit uHibernationOptimizer;

interface

uses
  SysUtils,
  uPathManager, uOptimizationUnit, uLanguageSettings,
  uProcessOpener;

type
  THibernationOptimizer = class(IOptimizationUnit)
  public
    function IsOptional: Boolean;
    function IsAppliable: Boolean;
    function IsApplied: Boolean;
    function GetName: String;
    procedure Apply;
    procedure Undo;
  end;

implementation

{ THibernationOptimizer }

function THibernationOptimizer.IsOptional: Boolean;
begin
  exit(false);
end;

function THibernationOptimizer.IsAppliable: Boolean;
begin
  exit(IsBelowWindows8);
end;

function THibernationOptimizer.IsApplied: Boolean;
begin
  if not IsBelowWindows8 then
    exit(false);
  result :=
    not FileExists(PathManager.WinDrive + '\hiberfil.sys');
end;

function THibernationOptimizer.GetName: String;
begin
  exit(CapOptHiber[CurrLang]);
end;

procedure THibernationOptimizer.Apply;
begin
  ProcessOpener.OpenProcWithOutput(
    PathManager.WinDrive,
    PathManager.WinDir + 'System32\cmd.exe /C powercfg -h off');
end;

procedure THibernationOptimizer.Undo;
begin
  ProcessOpener.OpenProcWithOutput(
    PathManager.WinDrive,
    PathManager.WinDir + 'System32\cmd.exe /C powercfg -h on');
end;

end.
