program Tarkov;

{$R *.dres}

uses
  Vcl.Forms,
  App.Constants in 'Source\App.Constants.pas',
  Vcl.Themes,
  Vcl.Styles,
  FilesMonitor in 'Source\Service\FilesMonitor.pas',
  MapWrapper in 'Source\GUI\MapWrapper.pas',
  uMainForm in 'Source\GUI\Form\uMainForm.pas' {MainForm},
  eduSettings in 'Source\GUI\Dialog\eduSettings.pas' {edSettings},
  FormWrapper in 'Source\GUI\FormWrapper.pas',
  fruLocation in 'Source\GUI\Frame\fruLocation.pas' {frLocation: TFrame},
  fruTagFilter in 'Source\GUI\Frame\fruTagFilter.pas' {frTagFilter: TFrame},
  GraphicButton in 'Source\GUI\Base\GraphicButton.pas',
  MapTagButton in 'Source\GUI\Base\MapTagButton.pas',
  QuestTagButton in 'Source\GUI\Base\QuestTagButton.pas',
  ResUIWrapper in 'Source\GUI\ResUIWrapper.pas',
  LocalMap in 'Source\Domain\LocalMap.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TedSettings, edSettings);
  Application.Run;
end.
