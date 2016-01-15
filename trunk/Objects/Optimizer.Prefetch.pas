unit Optimizer.Prefetch;

interface

uses
  SysUtils,
  OS.EnvironmentVariable, Optimizer.Template, Global.LanguageString,
  OS.ProcessOpener, Registry.Helper;

type
  TPrefetchOptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function TPrefetchOptimizer.IsOptional: Boolean;
begin
  exit(false);
end;

function TPrefetchOptimizer.IsCompatible: Boolean;
begin
  exit(IsBelowWindows8);
end;

function TPrefetchOptimizer.IsApplied: Boolean;
begin
  if not IsBelowWindows8 then
    exit(false);
  result :=
    not (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager' +
      '\Memory Management\PrefetchParameters',
      'EnablePrefetcher')) > 0);
end;

function TPrefetchOptimizer.GetName: String;
begin
  if Win32MajorVersion = 6 then
    exit(CapOptSupFetch[CurrLang])
  else if Win32MajorVersion = 5 then
    exit(CapOptPrefetch[CurrLang]);
end;

procedure TPrefetchOptimizer.Apply;
var
  ProcOutput: String;
begin
  if Win32MajorVersion = 6 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager' +
      '\Memory Management\PrefetchParameters', 'EnablePrefetcher'), 0);
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager' +
      '\Memory Management\PrefetchParameters', 'EnableSuperfetch'), 0);
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\services\SysMain',
      'Start'), 2);
    ProcOutput := string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDir +
      '\System32',
      'net start SysMain'));
  end
  else if Win32MajorVersion = 5 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager\' +
      'Memory Management\PrefetchParameters', 'EnablePrefetcher'), 0);
  end;
end;

procedure TPrefetchOptimizer.Undo;
var
  ProcOutput: String;
begin
  if Win32MajorVersion = 6 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager' +
      '\Memory Management\PrefetchParameters', 'EnablePrefetcher'), 3);
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager' +
      '\Memory Management\PrefetchParameters', 'EnableSuperfetch'), 3);
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\services\SysMain',
      'Start'), 4);
    ProcOutput := string(ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDir +
      '\System32',
      'net stop SysMain'));
  end
  else if Win32MajorVersion = 5 then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SYSTEM\CurrentControlSet\Control\Session Manager\' +
      'Memory Management\PrefetchParameters', 'EnablePrefetcher'), 3);
  end;
end;

end.
