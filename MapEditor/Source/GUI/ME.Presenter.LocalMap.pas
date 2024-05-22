unit ME.Presenter.LocalMap;

interface

uses
  System.SysUtils, System.Variants, System.Classes, Vcl.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.LocalMap, ME.LocalMapService;

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
begin
  inherited;

  LocalMapService.Save(Instance);
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

