unit Optimizer.P2P;

interface

uses
  SysUtils,
  Optimizer.Template, Global.LanguageString, Registry.Helper;

type
  TP2POptimizer = class(TOptimizationUnit)
  public
    function IsOptional: Boolean; override;
    function IsCompatible: Boolean; override;
    function IsApplied: Boolean; override;
    function GetName: String; override;
    procedure Apply; override;
    procedure Undo; override;
  end;

implementation

function TP2POptimizer.IsOptional: Boolean;
begin
  exit(false);
end;

function TP2POptimizer.IsCompatible: Boolean;
begin
  exit(IsAtLeastWindows10);
end;

function TP2POptimizer.IsApplied: Boolean;
begin
  if not IsAtLeastWindows10 then
    exit(false);
  result :=
    NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
      'SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config',
      'DODownloadMode')) = 0;
end;

function TP2POptimizer.GetName: String;
begin
  exit(CapOptP2P[CurrLang]);
end;

procedure TP2POptimizer.Apply;
begin
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
    'SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config',
    'DODownloadMode'), 0);
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('CU',
    'SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization',
    'SystemSettingsDownloadMode'), 3);
end;

procedure TP2POptimizer.Undo;
begin
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
    'SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config',
    'DODownloadMode'), 3);
  NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('CU',
    'SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization',
    'DODownloadMode'), 1);
end;

end.
