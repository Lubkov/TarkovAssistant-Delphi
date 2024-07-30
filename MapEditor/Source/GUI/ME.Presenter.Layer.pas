unit ME.Presenter.Layer;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Layer;

type
  TEditLayerPresenter = class(TEditFormPresenter<TDBLayer>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelLayerPresenter = class(TDelFormPresenter<TDBLayer>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.Service.Layer;

{ TEditLayerPresenter }

procedure TEditLayerPresenter.InternalSave;
begin
  inherited;

  LayerService.Save(Instance);
end;

procedure TEditLayerPresenter.Cancel;
begin
  inherited;

end;

{ TDelLayerPresenter }

function TDelLayerPresenter.GetDelMessage: string;
begin
  Result := 'Удалить уровень карты?';
end;

procedure TDelLayerPresenter.InternalDelete;
begin
  LayerService.Remove(Instance.ID);
end;

end.

