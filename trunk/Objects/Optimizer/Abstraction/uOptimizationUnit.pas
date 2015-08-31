unit uOptimizationUnit;

interface

uses
  Generics.Collections;

type
  IOptimizationUnit = interface
    function IsOptional: Boolean;
    function IsAppliable: Boolean;
    function IsApplied: Boolean;
    function GetName: String;
    procedure Apply;
    procedure Undo;
  end;
  TOptimizerList = TList<IOptimizationUnit>;

implementation

end.
