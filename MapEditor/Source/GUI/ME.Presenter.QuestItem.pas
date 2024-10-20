unit ME.Presenter.QuestItem;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Resource, ME.DB.QuestItem;

type
  TEditQuestItemPresenter = class(TEditFormPresenter<TDBQuestItem>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelQuestItemPresenter = class(TDelFormPresenter<TDBQuestItem>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils, ME.Service.QuestItem;

{ TEditQuestItemPresenter }

procedure TEditQuestItemPresenter.InternalSave;
begin
  if not IsNullID(Instance.MarkerID) then
    QuestItemService.Save(Instance);
end;

procedure TEditQuestItemPresenter.Cancel;
begin
  inherited;

end;

{ TDelQuestItemPresenter }

function TDelQuestItemPresenter.GetDelMessage: string;
begin
  Result := 'Удалить изображение предмета задания?';
end;

procedure TDelQuestItemPresenter.InternalDelete;
begin
  if not IsNullID(Instance.MarkerID) then
    QuestItemService.Remove(Instance);
end;

end.
