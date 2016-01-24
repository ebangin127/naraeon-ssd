unit Optimizer.Hibernation;

interface

uses
  SysUtils,
  OS.EnvironmentVariable, Optimizer.Template, Global.LanguageString,
  OS.ProcessOpener;

type
  THibernationOptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function THibernationOptimizer.IsOptional: Boolean;
begin
  exit(false);
end;

function THibernationOptimizer.IsCompatible: Boolean;
begin
  exit(IsBelowWindows8);
end;

function THibernationOptimizer.IsApplied: Boolean;
begin
  if not IsBelowWindows8 then
    exit(false);
  result :=
    not FileExists(EnvironmentVariable.WinDrive + '\hiberfil.sys');
end;

function THibernationOptimizer.GetName: String;
begin
  exit(CapOptHiber[CurrLang]);
end;

procedure THibernationOptimizer.Apply;
begin
  ProcessOpener.OpenProcWithOutput(
    EnvironmentVariable.WinDrive,
    EnvironmentVariable.WinDir + '\System32\cmd.exe /C powercfg -h off');
end;

procedure THibernationOptimizer.Undo;
begin
  ProcessOpener.OpenProcWithOutput(
    EnvironmentVariable.WinDrive,
    EnvironmentVariable.WinDir + '\System32\cmd.exe /C powercfg -h on');
end;

end.
