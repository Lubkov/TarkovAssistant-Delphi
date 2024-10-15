program TarkovMap;

uses
  System.StartUpCopy,
  FMX.Forms,
  TM.MainForm in 'Source\GUI\Form\TM.MainForm.pas' {MainForm},
  ME.DB.DAO in 'Source\Common\ME.DB.DAO.pas',
  ME.DB.Entity in 'Source\Common\ME.DB.Entity.pas',
  ME.DB.Service in 'Source\Common\ME.DB.Service.pas',
  ME.DB.Utils in 'Source\Common\ME.DB.Utils.pas',
  ME.Del.Form.Presenter in 'Source\Common\ME.Del.Form.Presenter.pas',
  ME.Dialog.Presenter in 'Source\Common\ME.Dialog.Presenter.pas',
  ME.Edit.Form in 'Source\Common\ME.Edit.Form.pas' {EditForm},
  ME.Edit.Form.Presenter in 'Source\Common\ME.Edit.Form.Presenter.pas',
  ME.DAO.Layer in 'Source\DAO\ME.DAO.Layer.pas',
  ME.DAO.Map in 'Source\DAO\ME.DAO.Map.pas',
  ME.DAO.Marker in 'Source\DAO\ME.DAO.Marker.pas',
  ME.DAO.Quest in 'Source\DAO\ME.DAO.Quest.pas',
  ME.DB.Layer in 'Source\Entity\ME.DB.Layer.pas',
  ME.DB.Map in 'Source\Entity\ME.DB.Map.pas',
  ME.DB.Marker in 'Source\Entity\ME.DB.Marker.pas',
  ME.DB.Quest in 'Source\Entity\ME.DB.Quest.pas',
  TM.FilesMonitor in 'Source\Service\TM.FilesMonitor.pas',
  ME.Service.Layer in 'Source\Service\ME.Service.Layer.pas',
  ME.Service.Map in 'Source\Service\ME.Service.Map.pas',
  ME.Service.Marker in 'Source\Service\ME.Service.Marker.pas',
  ME.Service.Quest in 'Source\Service\ME.Service.Quest.pas',
  TM.Form.Wrapper in 'Source\GUI\TM.Form.Wrapper.pas',
  TM.Map.Wrapper in 'Source\GUI\TM.Map.Wrapper.pas',
  TM.Form.Location in 'Source\GUI\Form\TM.Form.Location.pas' {LocationForm},
  TM.Frame.MarkerFilter in 'Source\GUI\Frame\TM.Frame.MarkerFilter.pas' {MarkerFilterList: TFrame},
  ME.MarkerFilter in 'Source\Entity\ME.MarkerFilter.pas',
  App.DB.Connection in 'Source\Common\App.DB.Connection.pas',
  App.SQLite.Connection in 'Source\Common\App.SQLite.Connection.pas',
  App.Service in 'Source\Common\App.Service.pas',
  TM.Frame.Location in 'Source\GUI\Frame\TM.Frame.Location.pas' {LocationGrid: TFrame},
  Map.Data.Classes in 'Source\Data\Map.Data.Classes.pas',
  Map.Data.Types in 'Source\Data\Map.Data.Types.pas',
  Map.Data.Service in 'Source\Data\Map.Data.Service.pas',
  Map.Frame.InteractiveMap in 'Source\GUI\Frame\Map.Frame.InteractiveMap.pas' {InteractiveMap: TFrame},
  Map.Frame.Marker in 'Source\GUI\Frame\Map.Frame.Marker.pas' {MarkerDescript: TFrame},
  Map.CursorService in 'Source\GUI\Map.CursorService.pas',
  ME.GUI.PictureItem in 'Source\Common\ME.GUI.PictureItem.pas' {PictureItemItem: TFrame},
  ME.GUI.PictureList in 'Source\Common\ME.GUI.PictureList.pas' {PictureList: TFrame},
  ME.DB.Resource in 'Source\Entity\ME.DB.Resource.pas',
  ME.DB.QuestItem in 'Source\Entity\ME.DB.QuestItem.pas',
  ME.DAO.Resource in 'Source\DAO\ME.DAO.Resource.pas',
  ME.DAO.QuestItem in 'Source\DAO\ME.DAO.QuestItem.pas',
  ME.Service.Resource in 'Source\Service\ME.Service.Resource.pas',
  ME.Service.QuestItem in 'Source\Service\ME.Service.QuestItem.pas',
  ME.Profile in 'Source\Entity\ME.Profile.pas',
  ME.QuestTracker in 'Source\Entity\ME.QuestTracker.pas',
  ME.Options in 'Source\Entity\ME.Options.pas',
  ME.Filter.Profile in 'Source\GUI\Frame\ME.Filter.Profile.pas' {ProfileFilter: TFrame},
  ME.Presenter.Profile in 'Source\GUI\Presenter\ME.Presenter.Profile.pas',
  ME.Edit.Profile in 'Source\GUI\Edit\ME.Edit.Profile.pas' {edProfile},
  ME.Dialog.Message in 'Source\Common\ME.Dialog.Message.pas' {edMessage},
  TM.Frame.Options in 'Source\GUI\Frame\TM.Frame.Options.pas' {OptionsFrame: TFrame},
  App.Entity in 'Source\Common\App.Entity.pas',
  ME.Service.Profile in 'Source\Service\ME.Service.Profile.pas',
  ME.Service.QuestTracker in 'Source\Service\ME.Service.QuestTracker.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  Application.Initialize;

  // app initialization
  AppService := TAppService.Create(Application);

  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
