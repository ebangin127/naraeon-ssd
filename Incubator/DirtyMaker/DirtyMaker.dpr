program DirtyMaker;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fMain},
  uSSDInfo in 'SSDInfo\uSSDInfo.pas',
  uSSDVersion in 'SSDInfo\uSSDVersion.pas',
  uDiskFunctions in 'Modules\uDiskFunctions.pas',
  uIntFunctions in 'Modules\uIntFunctions.pas',
  uLanguageSettings in 'Modules\uLanguageSettings.pas',
  uLogSystem in 'Modules\uLogSystem.pas',
  uRegFunctions in 'Modules\uRegFunctions.pas',
  uTrimCommand in 'Modules\uTrimCommand.pas',
  uMTforDel in 'Modules\uMTforDel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
