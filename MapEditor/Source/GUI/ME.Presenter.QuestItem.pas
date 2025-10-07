unit ME.Presenter.QuestItem;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Resource, ME.DB.QuestItem;

type
  TEditQuestItemPresenter = class(TEditFormPresenter<TDBQuestItem>)
  private
    FResourceID: Variant;
    FMarkerID: Variant;
  protected
    procedure SetInstance(const Value: TDBQuestItem); override;
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

procedure TEditQuestItemPresenter.SetInstance(const Value: TDBQuestItem);
begin
  inherited;

  if (Value <> nil) then begin
    FResourceID := Value.ResourceID;
    FMarkerID := Value.MarkerID;
  end
  else begin
    FResourceID := Null;
    FMarkerID := Null;
  end;
end;

procedure TEditQuestItemPresenter.InternalSave;
begin
  if not IsNullID(Instance.MarkerID) then begin
    QuestItemService.Save(Instance.ResourceID, Instance.MarkerID, Instance.Amount);

    if not IsNullID(FResourceID) and (FResourceID <> Instance.ResourceID) then
      QuestItemService.Remove(FResourceID, Instance.MarkerID);
  end;
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
    QuestItemService.Remove(Instance.ResourceID, Instance.MarkerID);
end;

end.
