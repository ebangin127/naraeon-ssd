unit Component.SSDLabel;

interface

uses
  Classes, Controls, SysUtils, Windows, StdCtrls,
  MeasureUnit.DataSize, Getter.PhysicalDrive.PartitionList, Device.PhysicalDrive;

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
    procedure SetPhysicalDrive(PhysicalDriveToReplicate: IPhysicalDrive);
    procedure SetProperty;
    procedure AppendFirst(PartitionList: TPartitionList);
  public
    PhysicalDrive: IPhysicalDrive;
    constructor Create(PhysicalDriveToReplicate: IPhysicalDrive); reintroduce;
    destructor Destroy; override;
  end;
  
implementation

uses Form.Main;

destructor TSSDLabel.Destroy;
begin
  inherited;
end;

constructor TSSDLabel.Create(PhysicalDriveToReplicate: IPhysicalDrive);
begin
  inherited Create(fMain.gSSDSel);
  SetPhysicalDrive(PhysicalDriveToReplicate);
  SetProperty;
  SetEvent;
  SetCaption;
end;

procedure TSSDLabel.SetPhysicalDrive(PhysicalDriveToReplicate: IPhysicalDrive);
begin
  PhysicalDrive := PhysicalDriveToReplicate;
end;

procedure TSSDLabel.SetFontAsInherited;
begin
  Font.Name := fMain.Font.Name;
  Font.Size := fMain.SSDSelLbl.Font.Size;
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
  DenaryInteger: TFormatSizeSetting;
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
  Caption := Caption + Letter;

  if Position = TPartitionPosition.Mid then
    Caption := Caption + ', '
  else
    Caption := Caption + ') ';
end;

procedure TSSDLabel.AppendFirst(PartitionList: TPartitionList);
begin
  if PartitionList.Count > 0 then
    Caption := Caption + '(';
end;

procedure TSSDLabel.AppendPartitionListToCaption(PartitionList: TPartitionList);
var
  PartitionEntryNumber: Integer;
begin
  AppendFirst(PartitionList);
  for PartitionEntryNumber := 0 to (PartitionList.Count - 1) do
    if PartitionEntryNumber = (PartitionList.Count - 1) then
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