unit uDiag;

interface

uses
  SysUtils, Classes, ClipBrd, Windows,
  uSSDInfo, uSSDList, uSSDSupport, uLanguageSettings;

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
  DiagContents: TStringList;
begin
  DiagContents := TStringList.Create;

  Header(DiagContents);
  Body(DiagContents);
  Footer(DiagContents);

  Clipboard.AsText := DiagContents.Text;
  MessageBox(0, PChar(DiagContents[CurrLang]), PChar(DiagName[CurrLang]),
    MB_OK or MB_IConInformation);

  FreeAndNil(DiagContents);
end;

class procedure TDiag.Header(Contents: TStringList);
begin
  Contents.Add('DiagStart, ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
  Contents.Add('Version, ' + CurrentVersion);
end;

class procedure TDiag.Body(Contents: TStringList);
var
  SSDList: TSSDList;
  CurrEntry: TSSDEntry;
  CurrSSDInfo: TSSDInfo_NST;
begin
  SSDList := TSSDList.Create;
  CurrSSDInfo := TSSDInfo_NST.Create;

  TraverseDevice(false, false, SSDList);
  for CurrEntry in SSDList do
  begin
    Contents.Add('Probe, \\.\PhysicalDrive' + CurrEntry.DeviceName + ', ');
    CurrSSDInfo.SetDeviceName(StrToInt(CurrEntry.DeviceName));

    Contents[Contents.Count - 1] :=
      Contents[Contents.Count - 1] +
      CurrSSDInfo.Model + ', ' +
      CurrSSDInfo.Firmware + ', ';

    case CurrSSDInfo.SupportedDevice of
    SUPPORT_FULL:
      Contents[Contents.Count - 1] :=
        Contents[Contents.Count - 1] + 'Full';
    SUPPORT_SEMI:
      Contents[Contents.Count - 1] :=
        Contents[Contents.Count - 1] + 'Semi';
    SUPPORT_NONE:
      Contents[Contents.Count - 1] :=
        Contents[Contents.Count - 1] + 'None';
    end;
  end;

  FreeAndNil(SSDList);
  FreeAndNil(CurrSSDInfo);
end;

class procedure TDiag.Footer(Contents: TStringList);
begin
  Contents.Add('DiagEnd, ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
end;

end.
