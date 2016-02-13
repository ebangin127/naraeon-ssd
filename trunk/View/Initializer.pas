unit Initializer;

interface

uses
  Forms, SysUtils, StdCtrls, ExtCtrls, Windows, Classes, Graphics, Controls,
  WinCodec,
  Form.Alert, Component.ButtonGroup, OS.EnvironmentVariable, Global.LanguageString,
  OS.VersionHelper;

procedure InitializeMainForm;
procedure RefreshOptimizeList;

implementation

uses Form.Main;

type
  THackControl = class(TControl);
  THackMainForm = class(TfMain);

  TMainformInitializer = class
  public
    constructor Create(MainformToInitialize: TfMain);
    procedure InitializeMainform;
  private
    const
      LogoPath = 'Image\logo.png';
      BackgroundPath = 'Image\bg.png';
  private
    Mainform: THackMainForm;
    procedure RefreshOptimizeList;
    procedure LoadBackground;
    procedure SetFormSize;
    procedure SetIcon;
    procedure AddButtonsToButtonGroup;
    procedure SetMessageFontAsApplicationFont;
    procedure FixFontToApplicationFont;
    procedure FixMainformSize;
    procedure LoadLogoImage;
    procedure LoadAndProportionalStretchLogo;
    procedure LoadAndProportionalStretchLogoXP;
    function StretchImage(ImagePath: String;
      NewWidth, NewHeight: Integer): TPersistent;
    procedure LoadAndProportionalStretchBackground;
    procedure LoadAndProportionalStretchBackgroundXP;
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
  AddButtonsToButtonGroup;
  LoadBackground;
  LoadLogoImage;
  SetFormSize;
  SetIcon;
  RefreshOptimizeList;
  SetMessageFontAsApplicationFont;
  FixFontToApplicationFont;
  FixMainformSize;
end;

procedure TMainformInitializer.AddButtonsToButtonGroup;
begin
  Mainform.AddEntryToButtonGroup(
    False, Mainform.iFirmUp, Mainform.lFirmUp, Mainform.gFirmware, nil);
  Mainform.AddEntryToButtonGroup(
    False, Mainform.iErase, Mainform.lErase, Mainform.gErase, nil);
  Mainform.AddEntryToButtonGroup(
    False, Mainform.iAnalytics, Mainform.lAnalytics, Mainform.gAnalytics, nil);
  Mainform.AddEntryToButtonGroup(
    False, Mainform.iTrim, Mainform.lTrim, Mainform.gTrim, nil);
  Mainform.AddEntryToButtonGroup(
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
  Mainform.lList.Items.Assign(Mainform.GetOptimizer.GetDescriptions);
  for CurrItem := 0 to (Mainform.GetOptimizer.GetDescriptions.Count - 1) do
  begin
    Mainform.lList.Checked[CurrItem] :=
      (not Mainform.GetOptimizer.GetApplied[CurrItem]) and
      (not Mainform.GetOptimizer.GetIsOptional[CurrItem]);

    if Mainform.GetOptimizer.GetApplied[CurrItem] then
      Mainform.lList.Items[CurrItem] :=
        Mainform.lList.Items[CurrItem] +
        CapAlreadyCompleted[CurrLang];
  end;
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

function TMainformInitializer.StretchImage(ImagePath: String;
  NewWidth, NewHeight: Integer): TPersistent;
var
  ImageToStretch: TWICImage;
  Scaler: IWICBitmapScaler;
  ScaledImage: IWICBitmap;
begin
  if fMain.WICImage <> nil then
    ImageToStretch := fMain.WICImage
  else
    ImageToStretch := TWICImage.Create;

  ImageToStretch.LoadFromFile(ImagePath);
  ImageToStretch.ImagingFactory.CreateBitmapScaler
    (Scaler);
  Scaler.Initialize(ImageToStretch.Handle,
    NewWidth, NewHeight,
    WICBitmapInterpolationModeFant);
  ImageToStretch.ImagingFactory.CreateBitmapFromSourceRect(
    Scaler, 0, 0, NewWidth, NewHeight, ScaledImage);
  ImageToStretch.Handle := ScaledImage;
  result := ImageToStretch;

  fMain.WICImage := ImageToStretch;
end;

procedure TMainformInitializer.LoadAndProportionalStretchBackgroundXP;
begin
  fMain.iBG.Proportional := true;
  if FileExists(EnvironmentVariable.AppPath + BackgroundPath) then
    fMain.iBG.Picture.LoadFromFile(EnvironmentVariable.AppPath + BackgroundPath);
end;

procedure TMainformInitializer.LoadAndProportionalStretchBackground;
begin
  fMain.iBG.Picture.Bitmap.Assign(
    StretchImage(EnvironmentVariable.AppPath + BackgroundPath,
    fMain.ClientWidth, fMain.ClientHeight));
end;

procedure TMainformInitializer.LoadBackground;
begin
  if VersionHelper.MajorVersion = 5 then
    LoadAndProportionalStretchBackgroundXP
  else
    LoadAndProportionalStretchBackground;
end;

procedure TMainformInitializer.LoadAndProportionalStretchLogo;
begin
  fMain.iLogo.Picture.Bitmap.Assign(
    StretchImage(EnvironmentVariable.AppPath + LogoPath,
    fMain.iLogo.Width, fMain.iLogo.Height));
end;

procedure TMainformInitializer.LoadAndProportionalStretchLogoXP;
begin
  fMain.iLogo.Proportional := true;
  if FileExists(EnvironmentVariable.AppPath + LogoPath) then
    fMain.iLogo.Picture.LoadFromFile(EnvironmentVariable.AppPath + LogoPath);
end;

procedure TMainformInitializer.LoadLogoImage;
begin
  if VersionHelper.MajorVersion = 5 then
    LoadAndProportionalStretchLogoXP
  else
    LoadAndProportionalStretchLogo;
end;

end.
