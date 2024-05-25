program MapEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  ME.MainForm in 'Source\ME.MainForm.pas' {MainForm},
  ME.DB.DAO in '..\Source\Common\ME.DB.DAO.pas',
  ME.DB.Entity in '..\Source\Common\ME.DB.Entity.pas',
  ME.DB.Service in '..\Source\Common\ME.DB.Service.pas',
  ME.DB.Utils in '..\Source\Common\ME.DB.Utils.pas',
  ME.Del.Form.Presenter in '..\Source\Common\ME.Del.Form.Presenter.pas',
  ME.Dialog.Presenter in '..\Source\Common\ME.Dialog.Presenter.pas',
  ME.Edit.Form.Presenter in '..\Source\Common\ME.Edit.Form.Presenter.pas',
  ME.Connection in 'Source\ME.Connection.pas',
  ME.AppService in 'Source\ME.AppService.pas',
  ME.LocalMapDAO in '..\Source\DAO\ME.LocalMapDAO.pas',
  ME.MapLevelDAO in '..\Source\DAO\ME.MapLevelDAO.pas',
  ME.PointDAO in '..\Source\DAO\ME.PointDAO.pas',
  ME.LocalMap in '..\Source\Entity\ME.LocalMap.pas',
  ME.MapLevel in '..\Source\Entity\ME.MapLevel.pas',
  ME.Point in '..\Source\Entity\ME.Point.pas',
  ME.LocalMapService in '..\Source\Service\ME.LocalMapService.pas',
  ME.MapLevelService in '..\Source\Service\ME.MapLevelService.pas',
  ME.PointService in '..\Source\Service\ME.PointService.pas',
  ME.Frame.LocalMap in 'Source\GUI\Frame\ME.Frame.LocalMap.pas' {frLocalMap: TFrame},
  ME.Presenter.LocalMap in 'Source\GUI\ME.Presenter.LocalMap.pas',
  ME.Dialog.Message in 'Source\GUI\Dialog\ME.Dialog.Message.pas' {edMessage},
  ME.Frame.MapLevel in 'Source\GUI\Frame\ME.Frame.MapLevel.pas' {frMapLevel: TFrame},
  ME.Presenter.MapLevel in 'Source\GUI\ME.Presenter.MapLevel.pas',
  ME.Edit.Form in '..\Source\Common\ME.Edit.Form.pas' {EditForm},
  ME.Edit.LocalMap in 'Source\GUI\Dialog\ME.Edit.LocalMap.pas' {edLocalMap},
  ME.Edit.MapLevel in 'Source\GUI\Dialog\ME.Edit.MapLevel.pas' {edMapLevel};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;

  // app initialization
  AppService := TMEService.Create(Application);

  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
