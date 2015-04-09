unit uDiag;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  uPhysicalDriveList, uPhysicalDrive, uLanguageSettings,
  uGlobalSettings;

type
  TDiag = class
  public
    class procedure Diagnosis;
  private
    class procedure Header(Contents: TStringList);
    class procedure Body(Contents: TStringList);
    class procedure Footer(Contents: TStringList);
  end;

implementation

class procedure TDiag.Diagnosis;
var
  DiagClipBrd: TStringList;
begin
  DiagClipBrd := TStringList.Create;

  Header(DiagClipBrd);
  Body(DiagClipBrd);
  Footer(DiagClipBrd);

  Clipboard.AsText := DiagClipBrd.Text;
  MessageBox(0, PChar(DiagContents[CurrLang]), PChar(DiagName[CurrLang]),
    MB_OK or MB_IConInformation);

  FreeAndNil(DiagClipBrd);
end;

class procedure TDiag.Header(Contents: TStringList);
begin
  Contents.Add('DiagStart, ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
  Contents.Add('Version, ' + CurrentVersion);
end;

class procedure TDiag.Body(Contents: TStringList);
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

class procedure TDiag.Footer(Contents: TStringList);
begin
  Contents.Add('DiagEnd, ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
end;

end.
