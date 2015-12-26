unit Initializer.PartitionAlign;

interface

uses
  SysUtils, Windows, Classes, Graphics,
  uLanguageSettings, Device.PhysicalDrive, uListChangeGetter,
  uPartitionListGetter;

type
  TMainformPartitionAlignApplier = class
  private
    PartitionList: TPartitionList;
    procedure AppendThisEntryAsInvalid(CurrentEntry: TPartitionEntry);
    procedure AppendThisEntryAsValid(CurrentEntry: TPartitionEntry);
    function ApplyAndGetPartitionAlignStatus: Boolean;
    procedure FreePartitionList;
    function IsThisStartingOffsetInvalid(
      StartingOffset: TLargeInteger): Boolean;
    procedure SetPartitionList;
    function AppendThisEntryToAlignLabel(
      CurrentEntry: TPartitionEntry): Boolean;
  public
    function ApplyAlignAndGetMisalignedExists: Boolean;
  end;

implementation

uses Form.Main;

procedure TMainformPartitionAlignApplier.SetPartitionList;
begin
  PartitionList := fMain.PhysicalDrive.GetPartitionList;
end;

procedure TMainformPartitionAlignApplier.FreePartitionList;
begin
  FreeAndNil(PartitionList);
end;

function TMainformPartitionAlignApplier.IsThisStartingOffsetInvalid(
  StartingOffset: TLargeInteger): Boolean;
const
  ValidAlignUnit = 4096;
begin
  result := (StartingOffset / ValidAlignUnit) <>
    (StartingOffset div ValidAlignUnit);
end;

procedure TMainformPartitionAlignApplier.AppendThisEntryAsInvalid(
  CurrentEntry: TPartitionEntry);
const
  DisplayUnit = 1024;
begin
  fMain.lPartitionAlign.Font.Color := clRed;
  fMain.lPartitionAlign.Caption := fMain.lPartitionAlign.Caption +
    CurrentEntry.Letter + ' (' +
    IntToStr(CurrentEntry.StartingOffset div DisplayUnit) +
    CapBad[CurrLang];
end;

procedure TMainformPartitionAlignApplier.AppendThisEntryAsValid(
  CurrentEntry: TPartitionEntry);
begin
  fMain.lPartitionAlign.Caption := fMain.lPartitionAlign.Caption +
    CurrentEntry.Letter + CapGood[CurrLang];
end;

function TMainformPartitionAlignApplier.AppendThisEntryToAlignLabel(
  CurrentEntry: TPartitionEntry): Boolean;
begin
  result := IsThisStartingOffsetInvalid(CurrentEntry.StartingOffset);
  if result then
    AppendThisEntryAsInvalid(CurrentEntry)
  else
    AppendThisEntryAsValid(CurrentEntry);
end;

function TMainformPartitionAlignApplier.ApplyAndGetPartitionAlignStatus:
  Boolean;
var
  CurrentEntry: TPartitionEntry;
begin
  fMain.lPartitionAlign.Caption := CapAlign[CurrLang];
  result := false;
  for CurrentEntry in PartitionList do
    result := result or AppendThisEntryToAlignLabel(CurrentEntry);
end;

function TMainformPartitionAlignApplier.ApplyAlignAndGetMisalignedExists:
  Boolean;
begin
  SetPartitionList;
  result := ApplyAndGetPartitionAlignStatus;
  FreePartitionList;
end;

end.