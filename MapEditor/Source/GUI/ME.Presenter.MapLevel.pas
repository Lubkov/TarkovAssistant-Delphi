unit ME.Presenter.MapLevel;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.MapLevel, ME.MapLevelService;

type
  TEditMapLevelPresenter = class(TEditFormPresenter<TMapLevel>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelMapLevelPresenter = class(TDelFormPresenter<TMapLevel>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils;

{ TEditMapLevelPresenter }

procedure TEditMapLevelPresenter.InternalSave;
begin
  inherited;

  if not IsNullID(Instance.MapID) then begin
    MapLevelService.Save(Instance);
    MapLevelService.SavePicture(Instance);
  end;
end;

procedure TEditMapLevelPresenter.Cancel;
begin
  inherited;

end;

{ TDelMapLevelPresenter }

function TDelMapLevelPresenter.GetDelMessage: string;
begin
  Result := 'Удалить уровень карты?';
end;

procedure TDelMapLevelPresenter.InternalDelete;
begin
  MapLevelService.Remove(Instance);
end;

end.

