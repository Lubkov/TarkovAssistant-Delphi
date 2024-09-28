unit ME.Presenter.QuestTracker;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.QuestTracker;

type
  TEditQuestTrackerPresenter = class(TEditFormPresenter<TQuestTracker>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelQuestTrackerPresenter = class(TDelFormPresenter<TQuestTracker>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  App.Constants, ME.DB.Utils, ME.Service.QuestTracker;

{ TEditQuestTrackerPresenter }

procedure TEditQuestTrackerPresenter.InternalSave;
begin
  QuestTrackerService.Save(Instance);
end;

procedure TEditQuestTrackerPresenter.Cancel;
begin
  inherited;

end;

{ TDelQuestTrackerPresenter }

function TDelQuestTrackerPresenter.GetDelMessage: string;
begin
  Result := 'Удалить профиль?';
end;

procedure TDelQuestTrackerPresenter.InternalDelete;
begin
  QuestTrackerService.Remove(Instance);
end;

end.
