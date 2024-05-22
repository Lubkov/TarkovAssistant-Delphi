program MapEditor;

uses
  Vcl.Forms,
  ME.MainForm in 'Source\ME.MainForm.pas' {MainForm},
  ME.Connection in 'Source\ME.Connection.pas',
  ME.AppService in 'Source\ME.AppService.pas',
  ME.Point in 'Source\Entity\ME.Point.pas',
  ME.PointDAO in 'Source\DAO\ME.PointDAO.pas',
  ME.PointService in 'Source\Service\ME.PointService.pas',
  ME.LocalMap in 'Source\Entity\ME.LocalMap.pas',
  ME.LocalMapDAO in 'Source\DAO\ME.LocalMapDAO.pas',
  ME.LocalMapService in 'Source\Service\ME.LocalMapService.pas',
  ME.DB.Entity in 'Source\Common\ME.DB.Entity.pas',
  ME.DB.DAO in 'Source\Common\ME.DB.DAO.pas',
  ME.DB.Utils in 'Source\Common\ME.DB.Utils.pas',
  ME.DB.Service in 'Source\Common\ME.DB.Service.pas',
  ME.MapLevel in 'Source\Entity\ME.MapLevel.pas',
  ME.MapLevelDAO in 'Source\DAO\ME.MapLevelDAO.pas',
  ME.MapLevelService in 'Source\Service\ME.MapLevelService.pas',
  ME.Dialog.Presenter in 'Source\Common\ME.Dialog.Presenter.pas',
  ME.Presenter.LocalMap in 'Source\GUI\ME.Presenter.LocalMap.pas',
  ME.Edit.Form.Presenter in 'Source\Common\ME.Edit.Form.Presenter.pas',
  ME.Edit.LocalMap in 'Source\GUI\ME.Edit.LocalMap.pas' {edLocalMap},
  ME.Del.Form.Presenter in 'Source\Common\ME.Del.Form.Presenter.pas',
  ME.Dialog.Message in 'Source\GUI\Dialog\ME.Dialog.Message.pas' {edMessage},
  ME.Frame.MapLevel in 'Source\GUI\Frame\ME.Frame.MapLevel.pas' {frMapLevel: TFrame},
  ME.Frame.LocalMap in 'Source\GUI\Frame\ME.Frame.LocalMap.pas' {frLocalMap: TFrame};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // app initialization
  AppService := TMEService.Create(Application);

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TedLocalMap, edLocalMap);
  Application.Run;
end.
