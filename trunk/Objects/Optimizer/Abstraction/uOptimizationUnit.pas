unit uOptimizationUnit;

interface

uses
  Generics.Collections,
  uVersionHelper;

type
  IOptimizationUnit = interface
    function IsOptional: Boolean;
    function IsCompatible: Boolean;
    function IsApplied: Boolean;
    function GetName: String;
    procedure Apply;
    procedure Undo;
  end;
  TOptimizationUnit = class abstract(TInterfacedObject, IOptimizationUnit)
  public
    function IsOptional: Boolean; virtual; abstract;
    function IsCompatible: Boolean; virtual; abstract;
    function IsApplied: Boolean; virtual; abstract;
    function GetName: String; virtual; abstract;
    procedure Apply; virtual; abstract;
    procedure Undo; virtual; abstract;
  protected
    function IsBelowWindows8: Boolean;
  end;
  TOptimizerList = TList<IOptimizationUnit>;

implementation

{ TOptimizationUnit }

function TOptimizationUnit.IsBelowWindows8: Boolean;
begin
  result :=
    (VersionHelper.MajorVersion < 6) or
    ((VersionHelper.MajorVersion = 6) and (VersionHelper.MinorVersion = 1));
end;

end.
