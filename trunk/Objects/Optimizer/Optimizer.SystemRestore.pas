unit Optimizer.SystemRestore;

interface

uses
  SysUtils,
  uPathManager, uOptimizationUnit, uLanguageSettings,
  uProcessOpener, uNSTRegistry;

type
  TSystemRestoreOptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function TSystemRestoreOptimizer.IsOptional: Boolean;
begin
  exit(true);
end;

function TSystemRestoreOptimizer.IsCompatible: Boolean;
begin
  exit(true);
end;

function TSystemRestoreOptimizer.IsApplied: Boolean;
begin
  result :=
    ((NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
                'SOFTWARE\Microsoft\Windows NT\CurrentVersion\SystemRestore',
                'DisableConfig')) = 1) and
     (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
                'SOFTWARE\Microsoft\Windows NT\CurrentVersion\SystemRestore',
                'DisableSR')) = 1));
end;

function TSystemRestoreOptimizer.GetName: String;
begin
  exit(CapOptRes[CurrLang]);
end;

procedure TSystemRestoreOptimizer.Apply;
var
  ProcOutput: String;
begin
  ProcOutput :=
    string(ProcessOpener.OpenProcWithOutput(
      PathManager.WinDir + '\System32',
      'schtasks /change /TN "Microsoft\Windows\SystemRestore\SR" /disable'));
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Windows NT\CurrentVersion' +
      '\SystemRestore', 'DisableConfig'), 1);
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\' +
    'SystemRestore', 'DisableSR'), 1);
end;

procedure TSystemRestoreOptimizer.Undo;
var
  ProcOutput: String;
begin
  ProcOutput :=
    string(ProcessOpener.OpenProcWithOutput(
      PathManager.WinDir + '\System32',
      'schtasks /change /TN "Microsoft\Windows\SystemRestore\SR" /enable'));
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Windows NT\CurrentVersion' +
      '\SystemRestore', 'DisableConfig'), 0);
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\' +
    'SystemRestore', 'DisableSR'), 0);
end;

end.
