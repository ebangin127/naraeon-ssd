unit Optimizer.Defrag;

interface

uses
  SysUtils,
  uPathManager, uOptimizationUnit, uLanguageSettings,
  uProcessOpener, uNSTRegistry;

type
  TDefragOptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function TDefragOptimizer.IsOptional: Boolean;
begin
  exit(false);
end;

function TDefragOptimizer.IsCompatible: Boolean;
begin
  exit(IsBelowWindows8);
end;

function TDefragOptimizer.IsApplied: Boolean;
begin
  if not IsBelowWindows8 then
    exit(false);
  result :=
    not
      ((NSTRegistry.GetRegStr(NSTRegistry.LegacyPathToNew('LM',
        'SOFTWARE\Microsoft\Dfrg\BootOptimizeFunction', 'Enable')) <> 'N')
      and
      (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
        'SOFTWARE\Microsoft\Windows\CurrentVersion\OptimalLayout',
        'EnableAutoLayout')) <> 0));
end;

function TDefragOptimizer.GetName: String;
begin
  exit(CapOptDfrg[CurrLang]);
end;

procedure TDefragOptimizer.Apply;
begin
  NSTRegistry.SetRegStr(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Dfrg\BootOptimizeFunction',
    'Enable'), 'N');
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
    'SOFTWARE\Microsoft\Windows\CurrentVersion' +
    '\OptimalLayout', 'EnableAutoLayout'), 0);
end;

procedure TDefragOptimizer.Undo;
begin
  NSTRegistry.SetRegStr(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Dfrg\BootOptimizeFunction', 'Enable'), 'Y');
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
    'LM', 'SOFTWARE\Microsoft\Windows\CurrentVersion' +
    '\OptimalLayout', 'EnableAutoLayout'), 1);
end;

end.
