unit ME.Presenter.Screenshot;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Presenter.Resource,
  ME.DB.Resource;

type
  TEditScreenshotPresenter = class(TEditResourcePresenter)
  private
  protected
    procedure InternalSave; override;
  public
  end;

  TDelScreenshotPresenter = class(TDelResourcePresenter)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  App.Constants, ME.DB.Utils, ME.Service.Resource;

{ TEditScreenshotPresenter }

procedure TEditScreenshotPresenter.InternalSave;
begin
  if not IsNullID(Instance.MarkerID) then
    ResourceService.Save(Instance);
end;

{ TDelScreenshotPresenter }

function TDelScreenshotPresenter.GetDelMessage: string;
begin
  Result := 'Удалить скриншот маркера?';
end;

procedure TDelScreenshotPresenter.InternalDelete;
begin
  if not IsNullID(Instance.MarkerID) then
    ResourceService.Remove(Instance);
end;

end.

