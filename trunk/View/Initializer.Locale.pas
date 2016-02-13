unit Initializer.Locale;

interface

uses
  Forms, SysUtils, StdCtrls, ExtCtrls, Windows, Classes, Graphics, Controls,
  Global.LanguageString;

procedure ApplyLocaleToMainformAndArrangeButton;

implementation

uses Form.Main;

type
  THackControl = class(TControl);
  THackMainForm = class(TfMain);

  TLocaleApplier = class
  public
    constructor Create(MainformToApply: TfMain);
    procedure ApplyLocale;
  private
    Mainform: THackMainForm;
    procedure ApplyLocaleToTrim;
    procedure ApplyLocaleToAnalytics;
    procedure ApplyLocaleToErase;
    procedure ApplyLocaleToFirmware;
    procedure ApplyLocaleToSemiautoTrim;
    procedure ApplyLocaleToDownloader;
    procedure ApplyLocaleToHelp;
    procedure ApplyLocaleToOptimizer;
    procedure ApplyLocaleToSSDSelector;
    procedure ArrangeButton(ImageToFit: TImage; LabelToArrange: TLabel);
    procedure ArrangeButtonsInMain;
  end;

procedure ApplyLocaleToMainformAndArrangeButton;
var
  LocaleApplier: TLocaleApplier;
begin
  LocaleApplier := TLocaleApplier.Create(fMain);
  LocaleApplier.ApplyLocale;
  LocaleApplier.ArrangeButtonsInMain;
  FreeAndNil(LocaleApplier);
end;

constructor TLocaleApplier.Create(MainformToApply: TfMain);
begin
  Mainform := THackMainForm(MainformToApply);
end;

procedure TLocaleApplier.ApplyLocale;
begin
  ApplyLocaleToFirmware;
  ApplyLocaleToErase;
  ApplyLocaleToOptimizer;
  ApplyLocaleToAnalytics;
  ApplyLocaleToTrim;
  ApplyLocaleToSemiautoTrim;
  ApplyLocaleToDownloader;
  ApplyLocaleToHelp;
  ApplyLocaleToSSDSelector;
  ArrangeButtonsInMain;
end;

procedure TLocaleApplier.ApplyLocaleToFirmware;
begin
  Mainform.lFirmUp.Caption := BtFirmware[CurrLang];
  Mainform.lUpdate.Caption := CapFirm[CurrLang];
  Mainform.lUSB.Caption := CapSelUSB[CurrLang];
  Mainform.lNewFirm.Caption := CapNewFirm[CurrLang];
  Mainform.cAgree.Caption := CapWarnErase[CurrLang];
  Mainform.bFirmStart.Caption := BtDoUpdate[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToErase;
begin
  Mainform.lErase.Caption := BtErase[CurrLang];
  Mainform.lEraseUSB.Caption := CapErase[CurrLang];
  Mainform.lUSBErase.Caption := CapSelUSB[CurrLang];
  Mainform.cEraseAgree.Caption := CapWarnErase[CurrLang];
  Mainform.bEraseUSBStart.Caption := BtDoErase[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToOptimizer;
begin
  Mainform.lOptimize.Caption := BtOpt[CurrLang];
  Mainform.lNameOpt.Caption := CapNameOpt[CurrLang];
  Mainform.bStart.Caption := BtDoOpt[CurrLang];
  Mainform.bRtn.Caption := BtRollback[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToAnalytics;
begin
  Mainform.lAnalytics.Caption := BtAnaly[CurrLang];
  Mainform.lAnaly.Caption := CapAnaly[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToTrim;
begin
  Mainform.lTrim.Caption := BtTrim[CurrLang];
  Mainform.lTrimName.Caption := CapTrimName[CurrLang];
  Mainform.bTrimStart.Caption := CapStartManTrim[CurrLang];
  Mainform.bSchedule.Caption := BtSemiAutoTrim[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToSemiautoTrim;
begin
  Mainform.lSchName.Caption := CapSemiAutoTrim[CurrLang];
  Mainform.lSchExp.Caption := CapSemiAutoTrimExp[CurrLang];
  Mainform.cTrimRunning.Caption := ChkSemiAutoTrim[CurrLang];
  Mainform.bReturn.Caption := BtRtn[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToSSDSelector;
begin
  Mainform.SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToDownloader;
begin
  Mainform.bCancel.Caption := BtDnldCncl[CurrLang];
end;

procedure TLocaleApplier.ApplyLocaleToHelp;
begin
  Mainform.lHelp.Caption := BtHelp[CurrLang];
end;

procedure TLocaleApplier.ArrangeButton
  (ImageToFit: TImage; LabelToArrange: TLabel);
begin
  LabelToArrange.left :=
    ImageToFit.Left + (ImageToFit.Width div 2) - (LabelToArrange.Width div 2);
end;

procedure TLocaleApplier.ArrangeButtonsInMain;
begin
  ArrangeButton(Mainform.iFirmUp, Mainform.lFirmUp);
  ArrangeButton(Mainform.iErase, Mainform.lErase);
  ArrangeButton(Mainform.iOptimize, Mainform.lOptimize);
  ArrangeButton(Mainform.iAnalytics, Mainform.lAnalytics);
  ArrangeButton(Mainform.iHelp, Mainform.lHelp);
  ArrangeButton(Mainform.iTrim, Mainform.lTrim);
end;
end.
