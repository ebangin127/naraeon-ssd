unit uInit;

interface

uses
  Forms, SysUtils, Windows, Classes, Graphics, Controls,
  uAlert, uButtonGroup, uLanguageSettings;

procedure InitMainForm;
procedure RefreshOptList;

implementation

uses uMain;

type
  THackControl = class(TControl);
  THackMainForm = class(TfMain);

procedure CheckVersion;
begin
  if Win32MajorVersion < 5 then
  begin
    AlertCreate(fMain, AlrtOSError[CurrLang]);
    Application.Terminate;
  end;
end;

procedure CreateButtonGroup;
begin
  with THackMainForm(fMain) do
  begin
    ButtonGroup :=
      TButtonGroup.Create(fMain, MaximumSize, MinimumSize,
        ClientWidth, ClientWidth);
    ButtonGroup.AddEntry(False, iFirmUp, lFirmUp, gFirmware, nil);
    ButtonGroup.AddEntry(False, iErase, lErase, gErase, nil);
    ButtonGroup.AddEntry(False, iAnalytics, lAnalytics, gAnalytics, nil);
    ButtonGroup.AddEntry(False, iTrim, lTrim, gTrim, nil);
    ButtonGroup.AddEntry(False, iOptimize, lOptimize, gOpt, nil);
  end;
end;

procedure InitFormSizeAndIcon;
begin
  with fMain do
  begin
    Icon := Application.Icon;
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    ClientHeight := MinimumSize;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end;
end;

procedure CheckEssentialDir;
begin
  if FileExists(AppPath + 'Setup.exe') then
    SysUtils.DeleteFile(AppPath + 'Setup.exe');
  if DirectoryExists(AppPath + 'Image') = false then
    CreateDirectory(PChar(AppPath + 'Image'), nil);
  if DirectoryExists(AppPath + 'Erase') = false then
    CreateDirectory(PChar(AppPath + 'Erase'), nil);
  if DirectoryExists(AppPath + 'Unetbootin') = false then
    CreateDirectory(PChar(AppPath + 'Unetbootin'), nil);
end;

procedure RefreshOptList;
var
  CurrItem: Integer;
begin
  with THackMainForm(fMain) do
  begin
    lList.Items.Assign(Optimizer.Descriptions);
    for CurrItem := 0 to (Optimizer.Descriptions.Count - 1) do
    begin
      lList.Checked[CurrItem] := (not Optimizer.Optimized[CurrItem])
                                  and (not Optimizer.Selective[CurrItem]);

      if Optimizer.Optimized[CurrItem] then
        lList.Items[CurrItem] := lList.Items[CurrItem]
                                  + CapAlreadyCompleted[CurrLang];
    end;
  end;
end;

procedure SetLanguage;
begin
  with fMain do
  begin
    bTrimStart.Caption := CapStartManTrim[CurrLang];

    lFirmUp.Caption := BtFirmware[CurrLang];
    lErase.Caption := BtErase[CurrLang];
    lOptimize.Caption := BtOpt[CurrLang];
    lHelp.Caption := BtHelp[CurrLang];
    lAnalytics.Caption := BtAnaly[CurrLang];
    lTrim.Caption := BtTrim[CurrLang];

    lFirmUp.left := iFirmUp.Left + (iFirmUp.Width div 2) - (lFirmUp.Width div 2);
    lErase.left := iErase.Left + (iErase.Width div 2) - (lErase.Width div 2);
    lOptimize.left  := iOptimize.Left + (iOptimize.Width div 2)
                        - (lOptimize.Width div 2);
    lAnalytics.left := iAnalytics.Left + (iAnalytics.Width div 2)
                        - (lAnalytics.Width div 2);
    lHelp.left := iHelp.Left + (iHelp.Width div 2) - (lHelp.Width div 2);
    lTrim.left := iTrim.Left + (iTrim.Width div 2) - (lTrim.Width div 2);

    lUpdate.Caption := CapFirm[CurrLang];
    lUSB.Caption := CapSelUSB[CurrLang];
    lNewFirm.Caption := CapNewFirm[CurrLang];
    cAgree.Caption := CapWarnErase[CurrLang];
    bFirmStart.Caption := BtDoUpdate[CurrLang];
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];

    lNameOpt.Caption := CapNameOpt[CurrLang];
    bStart.Caption := BtDoOpt[CurrLang];
    bRtn.Caption := BtRollback[CurrLang];

    lAnaly.Caption := CapAnaly[CurrLang];
    lEraseUSB.Caption := CapErase[CurrLang];
    lUSBErase.Caption := CapSelUSB[CurrLang];
    cEraseAgree.Caption := CapWarnErase[CurrLang];
    bEraseUSBStart.Caption := BtDoErase[CurrLang];

    lTrimName.Caption := CapTrimName[CurrLang];
    lAnaly.Caption := CapAnaly[CurrLang];
    bSchedule.Caption := BtSemiAutoTrim[CurrLang];
    bCancel.Caption := BtDnldCncl[CurrLang];

    lSchName.Caption := CapSemiAutoTrim[CurrLang];
    lSchExp.Caption := CapSemiAutoTrimExp[CurrLang];
    cTrimRunning.Caption := ChkSemiAutoTrim[CurrLang];
    bReturn.Caption := BtRtn[CurrLang];
  end;
end;

procedure FontAndSATrimSet;
  procedure SetFontName(Control: TControl; const FontName: String);
  begin
    THackControl(Control).Font.Name := FontName;
  end;
var
  CurrFont: String;
  CurrCompNum: Integer;
  CurrComponent: TComponent;
begin
  with THackMainForm(fMain) do
  begin
    if Win32MajorVersion = 5 then
    begin
      CurrFont := XPFont[CurrLang];

      lAnaly.Font.Style := [fsBold];
      lUpdate.Font.Style := [fsBold];
      lNameOpt.Font.Style := [fsBold];
      lEraseUSB.Font.Style := [fsBold];
      lName.Font.Style := [fsBold];
      lTrimName.Font.Style := [fsBold];
      lSchName.Font.Style := [fsBold];
      lDownload.Font.Style := [fsBold];
    end
    else
    begin
      CurrFont := VistaFont[CurrLang];
      if Win32MinorVersion >= 2 then
      begin
        bSchedule.Visible := false;
        bTrimStart.Width := bFirmStart.Width;
      end;
    end;

    Font.Name := CurrFont;
    for CurrCompNum := 0 to fMain.ComponentCount - 1 do
    begin
      CurrComponent := fMain.Components[CurrCompNum];
      if CurrComponent is TControl then
        SetFontName(TControl(CurrComponent), CurrFont);
    end;

    FirstOpt := lList.Items.Text;
    RefreshOptList;
    Constraints.MaxWidth := Width;
    Constraints.MaxHeight := Height;
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
  end;
end;

procedure LoadBGImage;
begin
  with fMain do
  begin
    if FileExists(AppPath + 'Image\bg.png') then
      iBG.Picture.LoadFromFile(AppPath + 'Image\bg.png');
    if FileExists(AppPath + 'Image\logo.png') then
      iLogo.Picture.LoadFromFile(AppPath + 'Image\logo.png');
  end;
end;

procedure InitMainForm;
begin
  CheckVersion;
  CreateButtonGroup;
  InitFormSizeAndIcon;
  CheckEssentialDir;
  RefreshOptList;
  SetLanguage;
  FontAndSATrimSet;
  LoadBGImage;
end;
end.
