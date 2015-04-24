unit uMainformPartitionAlignApplier;

interface

uses
  uPhysicalDrive, uListChangeGetter;

type
  TMainformPartitionAlignApplier = class
  private
    PartitionList: TPartitionList;
  public
    function ApplyAlignAndGetMisalignedExists: Boolean;
  end;

implementation

uses uMain;

procedure TMainformPartitionAlignApplier.SetPartitionList;
begin
  PartitionList := PhysicalDrive.GetPartitionList;
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

function TMainformPartitionAlignApplier.ApplyAndGetPartitionAlignStatus(
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