unit ME.Presenter.QuestPart;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Marker, ME.DB.Resource;

type
  TEditQuestPartPresenter = class(TEditFormPresenter<TDBMarker>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelQuestPartPresenter = class(TDelFormPresenter<TDBMarker>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils, ME.Service.Marker, ME.Service.Resource;

{ TEditQuestPartPresenter }

procedure TEditQuestPartPresenter.InternalSave;
var
  IsNewInstance: Boolean;
  Resource: TDBResource;
begin
  inherited;

  IsNewInstance := Instance.IsNewInstance;
  MarkerService.Save(Instance);

  if IsNewInstance then
    for Resource in Instance.Images do begin
      Resource.MarkerID := Instance.ID;
      ResourceService.Save(Resource);
    end;
end;


procedure TEditQuestPartPresenter.Cancel;
begin
  inherited;

end;

{ TDelQuestPartPresenter }

function TDelQuestPartPresenter.GetDelMessage: string;
begin
  Result := 'Удалить подзадачу квеста?';
end;

procedure TDelQuestPartPresenter.InternalDelete;
begin
  MarkerService.Remove(Instance.ID);
end;

end.
