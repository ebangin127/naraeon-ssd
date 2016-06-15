unit Optimizer;

interface

uses
  SysUtils, Classes, Forms, Generics.Collections,
  Optimizer.Template, Optimizer.Hibernation, Optimizer.LastAccess,
  Optimizer.Prefetch, Optimizer.Defrag, Optimizer.Indexer,
  Optimizer.SystemRestore, Optimizer.P2P;

type
  TOptimizeList = TList<Boolean>;
  TMetaOptimizationUnit = class of TOptimizationUnit;
  TNSTOptimizer = class
  private
    Optimizers: TOptimizerList;
    Descriptions: TStringList;
    Applied: TOptimizeList;
    IsOptional: TOptimizeList;
    procedure RefreshOptList;
    procedure CheckOptimized;
    procedure IfCompatibleAddToList(
      TOptimizationUnitToTry: TMetaOptimizationUnit);
    procedure AddOptimizers;
    procedure ClearOptimizers;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Optimize(OptList: TOptimizeList);
    procedure CancelOptimization;
    function GetDescriptions: TStringList;
    function GetApplied: TOptimizeList;
    function GetIsOptional: TOptimizeList;
  end;

implementation



constructor TNSTOptimizer.Create;
begin
  Descriptions := TStringList.Create;
  Applied := TOptimizeList.Create;
  IsOptional := TOptimizeList.Create;
  Optimizers := TOptimizerList.Create;

  RefreshOptList;
  CheckOptimized;
end;

destructor TNSTOptimizer.Destroy;
begin
  if Descriptions <> nil then
    FreeAndNil(Descriptions);

  if Applied <> nil then
    FreeAndNil(Applied);

  if IsOptional <> nil then
    FreeAndNil(IsOptional);

  if Optimizers <> nil then
    FreeAndNil(Optimizers);
  inherited;
end;

procedure TNSTOptimizer.Optimize(OptList: TList<Boolean>);
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Optimizers.Count - 1 do
    if (OptList[CurrentItem]) and
      (not Optimizers[CurrentItem].IsApplied) then
        Optimizers[CurrentItem].Apply;
  CheckOptimized;
end;


procedure TNSTOptimizer.CancelOptimization;
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Optimizers.Count - 1 do
    if Optimizers[CurrentItem].IsApplied then
        Optimizers[CurrentItem].Undo;
  CheckOptimized;
end;

procedure TNSTOptimizer.CheckOptimized;
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Optimizers.Count - 1 do
    Applied[CurrentItem] := Optimizers[CurrentItem].IsApplied;
  RefreshOptList;
end;

procedure TNSTOptimizer.RefreshOptList;
begin
  ClearOptimizers;
  AddOptimizers;
end;

procedure TNSTOptimizer.AddOptimizers;
begin
  IfCompatibleAddToList(THibernationOptimizer);
  IfCompatibleAddToList(TLastAccessOptimizer);
  IfCompatibleAddToList(TPrefetchOptimizer);
  IfCompatibleAddToList(TDefragOptimizer);
  IfCompatibleAddToList(TP2POptimizer);
  IfCompatibleAddToList(TIndexerOptimizer);
  IfCompatibleAddToList(TSystemRestoreOptimizer);
end;

procedure TNSTOptimizer.IfCompatibleAddToList(
  TOptimizationUnitToTry: TMetaOptimizationUnit);
var
  OptimizationUnit: IOptimizationUnit;
begin
  OptimizationUnit := TOptimizationUnitToTry.Create;
  if OptimizationUnit.IsCompatible then
  begin
    Optimizers.Add(OptimizationUnit);
    Descriptions.Add(OptimizationUnit.GetName);
    IsOptional.Add(OptimizationUnit.IsOptional);
    Applied.Add(OptimizationUnit.IsApplied);
  end;
end;

function TNSTOptimizer.GetDescriptions: TStringList;
begin
  result := Descriptions;
end;

function TNSTOptimizer.GetIsOptional: TOptimizeList;
begin
  result := IsOptional;
end;

procedure TNSTOptimizer.ClearOptimizers;
begin
  Descriptions.Clear;
  IsOptional.Clear;
  Applied.Clear;
  Optimizers.Clear;
end;

function TNSTOptimizer.GetApplied: TOptimizeList;
begin
  result := Applied;
end;
end.
