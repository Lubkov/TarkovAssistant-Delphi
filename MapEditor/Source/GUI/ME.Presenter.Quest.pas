unit ME.Presenter.Quest;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Quest;

type
  TEditQuestPresenter = class(TEditFormPresenter<TDBQuest>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelQuestPresenter = class(TDelFormPresenter<TDBQuest>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils, ME.Service.Quest;

{ TEditQuestPresenter }

procedure TEditQuestPresenter.InternalSave;
begin
  inherited;

  QuestService.Save(Instance);
end;

procedure TEditQuestPresenter.Cancel;
begin
  inherited;

end;

{ TDelQuestPresenter }

function TDelQuestPresenter.GetDelMessage: string;
begin
  Result := 'Удалить квест "' + Instance.Name + '"?';
end;

procedure TDelQuestPresenter.InternalDelete;
begin
  QuestService.Remove(Instance.ID);
end;

end.
