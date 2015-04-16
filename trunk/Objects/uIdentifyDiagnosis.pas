unit uIdentifyDiagnosis;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  uPhysicalDriveList, uPhysicalDrive, uLanguageSettings,
  uGlobalSettings;

type
  TIdentifyDiagnosis = class
  public
    procedure DiagnoseAndSetClipboardResult;
  private
    procedure Header(Contents: TStringList);
    procedure Body(Contents: TStringList);
    procedure Footer(Contents: TStringList);
  end;

implementation

procedure TIdentifyDiagnosis.DiagnoseAndSetClipboardResult;
var
  DiagnosisResult: TStringList;
begin
  DiagnosisResult := TStringList.Create;

  Header(DiagnosisResult);
  Body(DiagnosisResult);
  Footer(DiagnosisResult);

  Clipboard.AsText := DiagnosisResult.Text;
  MessageBox(0, PChar(DiagContents[CurrLang]), PChar(DiagName[CurrLang]),
    MB_OK or MB_IConInformation);

  FreeAndNil(DiagnosisResult);
end;

procedure TIdentifyDiagnosis.Header(Contents: TStringList);
begin
  Contents.Add('DiagStart, ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
  Contents.Add('Version, ' + CurrentVersion);
end;

procedure TIdentifyDiagnosis.Body(Contents: TStringList);
var
  SSDList: TPhysicalDriveList;
  CurrEntry: TPhysicalDrive;
begin
  SSDList := TPhysicalDriveList.Create;

  TraverseDevice(false, false, SSDList);
  for CurrEntry in SSDList do
  begin
    Contents.Add('Probe, ' + CurrEntry.GetPathOfFileAccessing + ', ');

    Contents[Contents.Count - 1] :=
      Contents[Contents.Count - 1] +
      CurrEntry.IdentifyDeviceResult.Model + ', ' +
      CurrEntry.IdentifyDeviceResult.Firmware + ', ';

    if CurrEntry.SupportStatus.FirmwareUpdate then
    begin
      Contents[Contents.Count - 1] :=
        Contents[Contents.Count - 1] + 'Full';
    end
    else if CurrEntry.SupportStatus.Supported then
    begin
      Contents[Contents.Count - 1] :=
        Contents[Contents.Count - 1] + 'Semi';
    end
    else
    begin
      Contents[Contents.Count - 1] :=
        Contents[Contents.Count - 1] + 'None';
    end;
  end;

  FreeAndNil(SSDList);
end;

procedure TIdentifyDiagnosis.Footer(Contents: TStringList);
begin
  Contents.Add('DiagEnd, ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
end;

end.
