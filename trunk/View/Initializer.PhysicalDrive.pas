unit Initializer.PhysicalDrive;

interface

uses
  SysUtils, Graphics,
  uLanguageSettings,
  Initializer.Mainpart, Initializer.PartitionAlign,
  Initializer.ReplacedSector, Initializer.SMART,
  Initializer.TotalWrite, uNSTSupport, uWindowsVersion;

type
  TMainformAlert = record
    SMARTAlert: TSMARTAlert;
    IsMisalignedPartitionExists: Boolean;
  end;
  
  TMainformPhysicalDriveApplier = class
  private
    MainformAlert: TMainformAlert;
    IsSerialOpened: Boolean;
    procedure ApplyAlert;
    procedure ApplyDataSetManagementSetting;
    procedure ApplyMainpart;
    procedure ApplyPartitionAlign;
    procedure ApplyReplacedSector;
    procedure ApplySMARTAndSetSMARTAlert;
    procedure ApplyTotalWrite;
    procedure RevertLastChangesOfMainform;
    procedure SetAlertLabelBadPartition;
    procedure SetAlertLabelBadReadWriteError;
    procedure SetAlertLabelBadReplacedSector;
    procedure SetSMARTAlert;
  public
    procedure ApplyMainformPhysicalDrive(IsSerialOpened: Boolean);
  end;

implementation

uses Form.Main;

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
    (fMain.PhysicalDrive.IdentifyDeviceResult.IsDataSetManagementSupported) and
    (IsBelowWindows8);
  fMain.iTrim.Visible := fMain.lTrim.Visible;
end;

procedure TMainformPhysicalDriveApplier.RevertLastChangesOfMainform;
begin
  if (not fMain.gFirmware.Visible) and
     (not fMain.gDownload.Visible) then
    fMain.lFirmware.Font.Style := [];

  fMain.lSectors.Font.Color := clWindowText;
  fMain.lPError.Font.Color := clWindowText;
  fMain.lNotsafe.Font.Color := clWindowText;
  fMain.lPartitionAlign.Font.Color := clWindowText;

  fMain.lNotsafe.Caption := CapStatus[CurrLang] + CapSafe[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.ApplyMainformPhysicalDrive(
  IsSerialOpened: Boolean);
begin
  self.IsSerialOpened := IsSerialOpened;
  
  RevertLastChangesOfMainform;
  ApplyMainpart;
  ApplyPartitionAlign;
  ApplyReplacedSector;
  ApplySMARTAndSetSMARTAlert;
  ApplyTotalWrite;
  ApplyDataSetManagementSetting;
  ApplyAlert;
end;

end.