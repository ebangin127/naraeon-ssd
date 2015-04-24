unit uSSDLabel;

interface

uses
  Classes, Controls, SysUtils, Windows, StdCtrls,
  uDatasizeUnit, uStrFunctions, uPartitionListGetter, uPhysicalDrive;

type
  TSSDLabel = class(TLabel)
  private
    type
      TPartitionPosition = (First, Last, Mid);
    procedure AppendCurrentPartitionToCaption(Position: TPartitionPosition;
      Letter: String);
    procedure AppendPartitionListToCaption(PartitionList: TPartitionList);
    procedure SetCaption;
    procedure SetCaptionAsModelAndDiskSizeInMB;
    procedure SetCaptionByPartitionList(PartitionList: TPartitionList);
    procedure SetCursorAsHandPoint;
    procedure SetEvent;
    procedure SetFontAsInherited;
    procedure SetParentAsgSSDSel;
    procedure SetPhysicalDrive(PhysicalDriveToReplicate: TPhysicalDrive);
    procedure SetProperty;
  public
    PhysicalDrive: TPhysicalDrive;
    constructor Create(PhysicalDriveToReplicate: TPhysicalDrive); reintroduce;
    destructor Destroy; override;
  end;
  
implementation

uses uMain;

destructor TSSDLabel.Destroy;
begin
  FreeAndNil(PhysicalDrive);
  inherited;
end;

constructor TSSDLabel.Create(PhysicalDriveToReplicate: TPhysicalDrive);
begin
  inherited Create(fMain.gSSDSel);
  SetPhysicalDrive(PhysicalDriveToReplicate);
  SetProperty;
  SetEvent;
  SetCaption;
end;

procedure TSSDLabel.SetPhysicalDrive(PhysicalDriveToReplicate: TPhysicalDrive);
begin
  PhysicalDrive :=
    TPhysicalDrive.Create(
      StrToInt(PhysicalDriveToReplicate.GetPathOfFileAccessingWithoutPrefix));
end;

procedure TSSDLabel.SetFontAsInherited;
begin
  Font.Name := fMain.Font.Name;
  Font.Size := 10;
end;

procedure TSSDLabel.SetParentAsgSSDSel;
begin
  Parent := fMain.gSSDSel;
end;

procedure TSSDLabel.SetCursorAsHandPoint;
begin
  Cursor := crHandPoint;
end;

procedure TSSDLabel.SetProperty;
begin
  SetParentAsgSSDSel;
  SetFontAsInherited;
  SetCursorAsHandPoint;
end;

procedure TSSDLabel.SetEvent;
begin
  OnClick := fMain.SSDLabelClick;
  OnMouseEnter := fMain.SSDSelLblMouseEnter;
  OnMouseLeave := fMain.SSDSelLblMouseLeave;
end;

function DenaryByteToMB(SizeInByte: Double): Double;
var
  DenaryByteToMB: TDatasizeUnitChangeSetting;
begin
  DenaryByteToMB.FNumeralSystem := Denary;
  DenaryByteToMB.FFromUnit := ByteUnit;
  DenaryByteToMB.FToUnit := MegaUnit;
  
  result :=
    ChangeDatasizeUnit(SizeInByte, DenaryByteToMB);
end;

function FormatSizeInMBAsDenaryInteger(SizeInDenaryFloat: Double): String;
var
  DenaryInteger: FormatSizeSetting;
begin
  DenaryInteger.FNumeralSystem := Denary;
  DenaryInteger.FPrecision := 0;
  
  result := FormatSizeInMB(SizeInDenaryFloat, DenaryInteger);
end;

procedure TSSDLabel.SetCaptionAsModelAndDiskSizeInMB;
var
  DiskSizeInMB: Double;
begin
  DiskSizeInMB := DenaryByteToMB(PhysicalDrive.DiskSizeInByte);
  Caption :=
    PhysicalDrive.IdentifyDeviceResult.Model + ' ' +
    FormatSizeInMBAsDenaryInteger(DiskSizeInMB);
end;

procedure TSSDLabel.AppendCurrentPartitionToCaption(
  Position: TPartitionPosition; Letter: String);
begin
  if Position = TPartitionPosition.First then
    Caption := Caption + '(';

  Caption := Caption + Letter;

  if Position = TPartitionPosition.Mid then
    Caption := Caption + ' '
  else
    Caption := Caption + ') ';
end;

procedure TSSDLabel.AppendPartitionListToCaption(PartitionList: TPartitionList);
var
  PartitionEntryNumber: Integer;
begin
  for PartitionEntryNumber := 0 to (PartitionList.Count - 1) do
    if PartitionEntryNumber = 0 then
      AppendCurrentPartitionToCaption(TPartitionPosition.First,
        PartitionList[PartitionEntryNumber].Letter)
    else if PartitionEntryNumber = (PartitionList.Count - 1) then
      AppendCurrentPartitionToCaption(TPartitionPosition.Last,
        PartitionList[PartitionEntryNumber].Letter)
    else
      AppendCurrentPartitionToCaption(TPartitionPosition.Mid,
        PartitionList[PartitionEntryNumber].Letter);
end;

procedure TSSDLabel.SetCaptionByPartitionList(PartitionList: TPartitionList);
begin
  SetCaptionAsModelAndDiskSizeInMB;
  AppendPartitionListToCaption(PartitionList);
end;

procedure TSSDLabel.SetCaption;
var
  PartitionList: TPartitionList;
begin
  PartitionList := PhysicalDrive.GetPartitionList;
  SetCaptionByPartitionList(PartitionList);
  FreeAndNil(PartitionList);
end;

end.