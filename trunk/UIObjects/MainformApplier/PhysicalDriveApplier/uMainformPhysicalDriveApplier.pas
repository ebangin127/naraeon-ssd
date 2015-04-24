unit uMainformPhysicalDriveApplier;

interface

uses
  uMainformMainpartApplier, uMainformPartitionAlignApplier,
  uMainformReplacedSectorApplier, uMainformSMARTApplier,
  uMainformTotalWriteApplier;

type
  TMainformAlert = record
    SMARTAlert: TSMARTAlert;
    IsMisalignedPartitionExists: Boolean;
  end;
  
  TMainformPhysicalDriveApplier = class
  private
    MainformAlert: TMainformAlert;
    IsSerialOpened: Boolean;
  public
    procedure ApplyMainformPhysicalDrive(IsSerialOpened: Boolean);
  end;

implementation

uses uMain;

procedure TMainformPhysicalDriveApplier.ApplyMainpart;
var
  MainpartApplier: TMainformMainpartApplier;
begin
  MainpartApplier := TMainformMainpartApplier.Create;
  MainpartApplier.ApplyMainformMainpart(IsSerialOpened);
  FreeAndNil(MainpartApplier);
end;

procedure TMainformPhysicalDriveApplier.ApplyPartitionAlign;
var
  PartitionAlignApplier: TMainformPartitionAlignApplier;
begin
  PartitionAlignApplier := TMainformPartitionAlignApplier.Create;
  MainformAlert.IsMisalignedPartitionExists :=
    PartitionAlignApplier.ApplyAlignAndGetMisalignedExists;
  FreeAndNil(PartitionAlignApplier);
end;

procedure TMainformPhysicalDriveApplier.ApplyReplacedSector;
var
  ReplacedSectorApplier: TMainformReplacedSectorApplier;
begin
  ReplacedSectorApplier := TMainformReplacedSectorApplier.Create;
  ReplacedSectorApplier.ApplyMainformReplacedSector;
  FreeAndNil(ReplacedSectorApplier);
end;

procedure TMainformPhysicalDriveApplier.ApplySMARTAndSetSMARTAlert;
var
  SMARTApplier: TMainformSMARTApplier;
begin
  SMARTApplier := TMainformSMARTApplier.Create;
  SMARTApplier.ApplyMainformSMART;
  FreeAndNil(SMARTApplier);
  SetSMARTAlert;
end;

procedure TMainformPhysicalDriveApplier.ApplyTotalWrite;
var
  TotalWriteApplier: TMainformTotalWriteApplier;
begin
  TotalWriteApplier := TMainformTotalWriteApplier.Create;
  TotalWriteApplier.ApplyMainformTotalWrite;
  FreeAndNil(TotalWriteApplier);
end;

procedure TMainformPhysicalDriveApplier.SetAlertLabelBadPartition;
begin
  fMain.lNotSafe.Font.Color := clRed;
  fMain.lNotsafe.Caption := CapStatus[CurrLang] + CapBadPartition[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.SetAlertLabelBadReplacedSector;
begin
  fMain.lSectors.Font.Color := clRed;
  fMain.lNotsafe.Font.Color := clRed;
  fMain.lSectors.Caption := CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
    CapCount[CurrLang];
  fMain.lNotsafe.Caption := CapStatus[CurrLang] + CapNotSafeRepSect[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.SetAlertLabelBadReadWriteError;
begin
  fMain.lPError.Font.Color := clRed;
  fMain.lNotsafe.Font.Color := clRed;
  fMain.lNotsafe.Caption := CapNotSafeEraseErrors[CurrLang] +
    CapNotSafeRepSect[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.SetSMARTAlert;
begin
  MainformAlert.SMARTAlert :=
    fMain.PhysicalDrive.SMARTInterpreted.SMARTAlert;
end;

procedure TMainformPhysicalDriveApplier.ApplyAlert;
begin
  if MainformAlert.IsMisalignedPartitionExists then
    SetAlertLabelBadPartition;
    
  if MainformAlert.SMARTAlert.ReplacedSector then
    SetAlertLabelBadReplacedSector;
  
  if MainformAlert.SMARTAlert.ReadEraseError then
    SetAlertLabelBadReadWriteError;
end;

procedure TMainformPhysicalDriveApplier.ApplyDataSetManagementSetting;
begin
  fMain.lTrim.Visible := 
    fMain.PhysicalDrive.IdentifyDeviceResult.IsDataSetManagementSupported;
  fMain.iTrim.Visible := 
    fMain.PhysicalDrive.IdentifyDeviceResult.IsDataSetManagementSupported;
end;

procedure TMainformPhysicalDriveApplier.RevertLastChangesToMainform;
begin
  if (not gFirmware.Visible) and
     (not gDownload.Visible) then
    lFirmware.Font.Style := [];
    
  lSectors.Font.Color := clWindowText;
  lPError.Font.Color := clWindowText;
  lNotsafe.Font.Color := clWindowText;
  lPartitionAlign.Font.Color := clWindowText;
  
  lNotsafe.Caption := CapStatus[CurrLang] + CapSafe[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.ApplyMainformPhysicalDrive(
  IsSerialOpened: Boolean);
begin
  self.IsSerialOpened := IsSerialOpened;
  
  RevertLastChangesToMainform;
  ApplyMainpart;
  ApplyPartitionAlign;
  ApplyReplacedSector;
  ApplySMARTAndSetSMARTAlert;
  ApplyTotalWrite;
  ApplyDataSetManagementSetting;
  ApplyAlert;
end;

end.