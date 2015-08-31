unit uOptimizer;

interface

uses
  uOptimizationUnit;

type
  TOptimizer = class
  public
    procedure Apply(OptimizerListToApply: TOptimizerList);
    procedure Undo(OptimizerListToUndo: TOptimizerList);
  end;

implementation

{ TOptimizer }

procedure TOptimizer.Apply(OptimizerListToApply: TOptimizerList);
var
  CurrentOptimizerToApply: IOptimizationUnit;
begin
  for CurrentOptimizerToApply in OptimizerListToApply do
    CurrentOptimizerToApply.Apply;
end;

procedure TOptimizer.Undo(OptimizerListToUndo: TOptimizerList);
var
  CurrentOptimizerToUndo: IOptimizationUnit;
begin
  for CurrentOptimizerToUndo in OptimizerListToUndo do
    CurrentOptimizerToUndo.Undo;
end;

end.
