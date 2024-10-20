unit ME.DB.Presenter.Resource;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Resource;

type
  TEditResourcePresenterClass = class of TEditResourcePresenter;

  TEditResourcePresenter = class(TEditFormPresenter<TDBResource>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelResourcePresenterClass = class of TDelResourcePresenter;

  TDelResourcePresenter = class(TDelFormPresenter<TDBResource>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils, ME.Service.Resource;

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
  Result := 'Удалить ресурс?';
end;

procedure TDelResourcePresenter.InternalDelete;
begin
  ResourceService.Remove(Instance);
end;

end.

