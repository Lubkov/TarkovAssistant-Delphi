unit ME.Presenter.LocalMap;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.LocalMap, ME.MapLevel, ME.LocalMapService, ME.MapLevelService;

type
  TEditMapPresenter = class(TEditFormPresenter<TLocalMap>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelMapPresenter = class(TDelFormPresenter<TLocalMap>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils;

{ TEditMapPresenter }

procedure TEditMapPresenter.InternalSave;
var
  MapLevel: TMapLevel;
begin
  inherited;

  LocalMapService.Save(Instance);
  for MapLevel in Instance.Levels do
    if IsNullID(MapLevel.ID) then
      MapLevelService.Save(MapLevel);
end;

procedure TEditMapPresenter.Cancel;
begin
  inherited;

end;

{ TDelMapPresenter }

function TDelMapPresenter.GetDelMessage: string;
begin
  Result := 'Удалить карту "' + Instance.Name + '"?';
end;

procedure TDelMapPresenter.InternalDelete;
begin
  LocalMapService.Remove(Instance);
end;

end.

