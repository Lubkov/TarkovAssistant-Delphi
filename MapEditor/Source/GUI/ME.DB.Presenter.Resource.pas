unit ME.DB.Presenter.Resource;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Resource;

type
  TEditResourcePresenter = class(TEditFormPresenter<TDBResource>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelResourcePresenter = class(TDelFormPresenter<TDBResource>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  App.Constants, ME.Service.Resource;

{ TEditResourcePresenter }

procedure TEditResourcePresenter.InternalSave;
begin
  ResourceService.Save(Instance);
end;

procedure TEditResourcePresenter.Cancel;
begin
  inherited;

end;

{ TDelResourcePresenter }

function TDelResourcePresenter.GetDelMessage: string;
begin
  Result := 'Удалить скриншот маркера?';
end;

procedure TDelResourcePresenter.InternalDelete;
begin
  ResourceService.Remove(Instance);
end;

end.

