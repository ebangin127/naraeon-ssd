unit uInit;

interface

uses
  Forms, SysUtils, StdCtrls, ExtCtrls, Windows, Classes, Graphics, Controls,
  uAlert, uButtonGroup, uGetFirm, uPathManager, uLanguageSettings;

procedure InitializeMainForm;
procedure RefreshOptimizeList;

implementation

uses uMain;

type
  THackControl = class(TControl);
  THackMainForm = class(TfMain);

  TMainformInitializer = class
  public
    constructor Create(MainformToInitialize: TfMain);
    procedure InitializeMainform;
  private
    Mainform: THackMainForm;

    procedure CreateButtonGroup;
    procedure RefreshOptimizeList;
    procedure LoadBGImage;
    procedure ApplyLocale;
    procedure SetFormSize;
    procedure SetIcon;
    procedure AddButtonsToButtonGroup;
    procedure SetMessageFontAsApplicationFont;
    procedure FixFontToApplicationFont;
    procedure FixMainformSize;
  end;

procedure InitializeMainForm;
var
  MainformInitializer: TMainformInitializer;
begin
  MainformInitializer := TMainformInitializer.Create(fMain);
  MainformInitializer.InitializeMainform;
  FreeAndNil(MainformInitializer);
end;

procedure RefreshOptimizeList;
var
  MainformInitializer: TMainformInitializer;
begin
  MainformInitializer := TMainformInitializer.Create(fMain);
  MainformInitializer.RefreshOptimizeList;
  FreeAndNil(MainformInitializer);
end;

{ TMainformInitializer }

constructor TMainformInitializer.Create(MainformToInitialize: TfMain);
begin
  Mainform := THackMainForm(MainformToInitialize);
end;

procedure TMainformInitializer.InitializeMainform;
begin
  CreateButtonGroup;
  AddButtonsToButtonGroup;
  LoadBGImage;
  SetFormSize;
  SetIcon;
  ApplyLocale;
  RefreshOptimizeList;
  SetMessageFontAsApplicationFont;
  FixFontToApplicationFont;
  FixMainformSize;
end;

procedure TMainformInitializer.CreateButtonGroup;
begin
  Mainform.ButtonGroup :=
      TButtonGroup.Create(fMain, MaximumSize, MinimumSize,
        Mainform.ClientWidth, Mainform.ClientWidth);
end;

procedure TMainformInitializer.AddButtonsToButtonGroup;
begin
  Mainform.ButtonGroup.AddEntry(
    False, Mainform.iFirmUp, Mainform.lFirmUp, Mainform.gFirmware, nil);
  Mainform.ButtonGroup.AddEntry(
    False, Mainform.iErase, Mainform.lErase, Mainform.gErase, nil);
  Mainform.ButtonGroup.AddEntry(
    False, Mainform.iAnalytics, Mainform.lAnalytics, Mainform.gAnalytics, nil);
  Mainform.ButtonGroup.AddEntry(
    False, Mainform.iTrim, Mainform.lTrim, Mainform.gTrim, nil);
  Mainform.ButtonGroup.AddEntry(
    False, Mainform.iOptimize, Mainform.lOptimize, Mainform.gOpt, nil);
end;

procedure TMainformInitializer.SetFormSize;
begin
  Mainform.Constraints.MaxHeight := 0;
  Mainform.Constraints.MinHeight := 0;
  Mainform.ClientHeight := MinimumSize;
  Mainform.Constraints.MaxHeight := Mainform.Height;
  Mainform.Constraints.MinHeight := Mainform.Height;
end;

procedure TMainformInitializer.SetIcon;
begin
  Mainform.Icon := Application.Icon;
end;

procedure TMainformInitializer.RefreshOptimizeList;
var
  CurrItem: Integer;
begin
  Mainform.lList.Items.Assign(Mainform.Optimizer.Descriptions);
  for CurrItem := 0 to (Mainform.Optimizer.Descriptions.Count - 1) do
  begin
    Mainform.lList.Checked[CurrItem] :=
      (not Mainform.Optimizer.Optimized[CurrItem]) and
      (not Mainform.Optimizer.Selective[CurrItem]);

    if Mainform.Optimizer.Optimized[CurrItem] then
      Mainform.lList.Items[CurrItem] :=
        Mainform.lList.Items[CurrItem] +
        CapAlreadyCompleted[CurrLang];
  end;
end;

procedure TMainformInitializer.ApplyLocale;
begin
end;

procedure TMainformInitializer.SetMessageFontAsApplicationFont;
begin
  Application.DefaultFont := Screen.MessageFont;
end;


procedure TMainformInitializer.FixFontToApplicationFont;
  procedure SetFontName(Control: TControl; const FontName: String);
  begin
    THackControl(Control).Font.Name := FontName;
  end;
var
  CurrCompNum: Integer;
  CurrComponent: TComponent;
begin
  Mainform.Font.Name := Application.DefaultFont.Name;

  for CurrCompNum := 0 to Mainform.ComponentCount - 1 do
  begin
    CurrComponent := Mainform.Components[CurrCompNum];
    if CurrComponent is TControl then
      SetFontName(TControl(CurrComponent), Mainform.Font.Name);
  end;
end;

procedure TMainformInitializer.FixMainformSize;
begin
  Mainform.Constraints.MaxWidth := Mainform.Width;
  Mainform.Constraints.MaxHeight := Mainform.Height;
  Mainform.Constraints.MinWidth := Mainform.Width;
  Mainform.Constraints.MinHeight := Mainform.Height;
end;

procedure TMainformInitializer.LoadBGImage;
begin
  with fMain do
  begin
    if FileExists(TPathManager.AppPath + 'Image\bg.png') then
      iBG.Picture.LoadFromFile(TPathManager.AppPath + 'Image\bg.png');
    if FileExists(TPathManager.AppPath + 'Image\logo.png') then
      iLogo.Picture.LoadFromFile(TPathManager.AppPath + 'Image\logo.png');
  end;
end;

end.
