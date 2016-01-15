unit Optimizer.LastAccess;

interface

uses
  SysUtils,
  OS.EnvironmentVariable, Optimizer.Template, Global.LanguageString,
  OS.ProcessOpener;

type
  TLastAccessOptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function TLastAccessOptimizer.IsOptional: Boolean;
begin
  exit(false);
end;

function TLastAccessOptimizer.IsCompatible: Boolean;
begin
  exit(true);
end;

function TLastAccessOptimizer.IsApplied: Boolean;
begin
  result := not (Pos('= 0',
    string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDrive,
      'FSUTIL behavior query disablelastaccess'))) > 0);
end;

function TLastAccessOptimizer.GetName: String;
begin
  exit(CapOptLastAccess[CurrLang]);
end;

procedure TLastAccessOptimizer.Apply;
begin
  ProcessOpener.OpenProcWithOutput(
    EnvironmentVariable.WinDrive, 'FSUTIL behavior set disablelastaccess 1');
end;

procedure TLastAccessOptimizer.Undo;
begin
  ProcessOpener.OpenProcWithOutput(
    EnvironmentVariable.WinDrive, 'FSUTIL behavior set disablelastaccess 0');
end;

end.
