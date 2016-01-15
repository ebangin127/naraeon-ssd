unit Optimizer.Indexer;

interface

uses
  SysUtils,
  OS.EnvironmentVariable, Optimizer.Template, Global.LanguageString,
  OS.ProcessOpener, Registry.Helper;

type
  TIndexerOptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function TIndexerOptimizer.IsOptional: Boolean;
begin
  exit(true);
end;

function TIndexerOptimizer.IsCompatible: Boolean;
begin
  exit(true);
end;

function TIndexerOptimizer.IsApplied: Boolean;
begin
  result := false;
  if Win32MajorVersion = 6 then
  begin
    result :=
      (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\WSearch',
        'Start')) = 4);
  end
  else if Win32MajorVersion = 5 then
  begin
    result :=
      (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\CiSvc',
        'Start')) = 4);
  end;
end;

function TIndexerOptimizer.GetName: String;
begin
  exit(CapOptIndex[CurrLang]);
end;

procedure TIndexerOptimizer.Apply;
var
  ProcOutput: String;
begin
  if Win32MajorVersion = 6 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
      'LM', 'SYSTEM\CurrentControlSet\services\WSearch', 'Start'), 4);
    ProcOutput :=
      string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDrive,
        'net stop WSearch'));
  end
  else if Win32MajorVersion = 5 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
      'LM', 'SYSTEM\CurrentControlSet\services\CiSvc', 'Start'), 4);
    ProcOutput :=
      string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDrive,
        'net stop CiSvc'));
  end;
end;

procedure TIndexerOptimizer.Undo;
var
  ProcOutput: String;
begin
  if Win32MajorVersion = 6 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
      'LM', 'SYSTEM\CurrentControlSet\services\WSearch', 'Start'), 2);
    ProcOutput :=
      string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDrive,
        'net start WSearch'));
  end
  else if Win32MajorVersion = 5 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
      'LM', 'SYSTEM\CurrentControlSet\services\CiSvc', 'Start'), 2);
    ProcOutput :=
      string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDrive,
        'net start CiSvc'));
  end;
end;

end.
