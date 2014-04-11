program SymbolicLinker;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fMain},
  uEasyReg in 'EasyRegistry\uEasyReg.pas',
  uEasySMART in 'EasySMART\uEasySMART.pas',
  hddInfo in 'HddInfo\hddInfo.pas',
  WbemScripting_TLB in 'HddInfo\WbemScripting_TLB.pas',
  uAlert in 'uAlert.pas' {fAlert},
  uFolderFunctions in 'SymbolicLinker\uFolderFunctions.pas';

{$R 'SymbolicLinker.res'}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfAlert, fAlert);
  Application.Run;
end.
