unit ME.Presenter.Layer;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Layer, ME.Service.Layer;

type
  TEditLayerPresenter = class(TEditFormPresenter<TLayer>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelLayerPresenter = class(TDelFormPresenter<TLayer>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils;

{ TEditLayerPresenter }

procedure TEditLayerPresenter.InternalSave;
begin
  inherited;

  if not IsNullID(Instance.MapID) then begin
    LayerService.Save(Instance);
    LayerService.SavePicture(Instance);
  end;
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
  LayerService.Remove(Instance);
end;

end.

