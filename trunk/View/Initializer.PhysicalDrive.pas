unit Initializer.PhysicalDrive;

interface

uses
  SysUtils, Graphics,
  Global.LanguageString,
  Initializer.Mainpart, Initializer.PartitionAlign,
  Initializer.ReplacedSector, Initializer.SMART, BufferInterpreter,
  Initializer.TotalWrite, Initializer.CriticalWarning, Support,
  OS.WindowsVersion;

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
    function IsNotNVMe: Boolean;
    procedure ApplyCriticalWarning;
    function IsNVMeCriticalWarning: Boolean;
    procedure SetAlertLabelBadCriticalWarning;
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

procedure TMainformPhysicalDriveApplier.ApplyCriticalWarning;
var
  CriticalWarningApplier: TMainformCriticalWarningApplier;
begin
  CriticalWarningApplier := TMainformCriticalWarningApplier.Create;
  CriticalWarningApplier.ApplyMainformCriticalWarning;
  FreeAndNil(CriticalWarningApplier);
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
  fMain.lNotsafe.Caption := CapStatus[CurrLang] +
    CapNotSafeCritical[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.SetAlertLabelBadCriticalWarning;
begin
  fMain.lSectors.Font.Color := clRed;
  fMain.lNotsafe.Font.Color := clRed;
  fMain.lNotsafe.Caption := CapStatus[CurrLang] +
    CapNotSafeCritical[CurrLang];
end;

procedure TMainformPhysicalDriveApplier.SetSMARTAlert;
begin
  MainformAlert.SMARTAlert :=
    fMain.PhysicalDrive.SMARTInterpreted.SMARTAlert;
end;

function TMainformPhysicalDriveApplier.IsNVMeCriticalWarning: Boolean;
begin
  result :=
    (fMain.PhysicalDrive.IdentifyDeviceResult.StorageInterface =
      TStorageInterface.NVMe) and
    (MainformAlert.SMARTAlert.CriticalError);
end;

procedure TMainformPhysicalDriveApplier.ApplyAlert;
begin
  if MainformAlert.IsMisalignedPartitionExists then
    SetAlertLabelBadPartition;

  if MainformAlert.SMARTAlert.ReplacedSector then
    SetAlertLabelBadReplacedSector;

  if MainformAlert.SMARTAlert.ReadEraseError then
    SetAlertLabelBadReadWriteError;

  if IsNVMeCriticalWarning then
    SetAlertLabelBadCriticalWarning;
end;

procedure TMainformPhysicalDriveApplier.ApplyDataSetManagementSetting;
begin
  fMain.lTrim.Visible :=
    fMain.PhysicalDrive.IdentifyDeviceResult.IsDataSetManagementSupported;
  fMain.iTrim.Visible := fMain.lTrim.Visible;
  fMain.bSchedule.Visible := IsBelowWindows8;
  if fMain.bSchedule.Visible = false then
  begin
    fMain.bTrimStart.Width :=
      fMain.bSchedule.Left - fMain.bTrimStart.Left + fMain.bSchedule.Width;
  end;
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

function TMainformPhysicalDriveApplier.IsNotNVMe: Boolean;
begin
  result := fMain.PhysicalDrive.IdentifyDeviceResult.StorageInterface
    <> TStorageInterface.NVMe;
end;

procedure TMainformPhysicalDriveApplier.ApplyMainformPhysicalDrive(
  IsSerialOpened: Boolean);
begin
  self.IsSerialOpened := IsSerialOpened;
  
  RevertLastChangesOfMainform;
  ApplyMainpart;
  ApplyPartitionAlign;
  if IsNotNVMe then
    ApplyReplacedSector
  else
    ApplyCriticalWarning;
  ApplySMARTAndSetSMARTAlert;
  ApplyTotalWrite;
  ApplyDataSetManagementSetting;
  ApplyAlert;
end;

end.