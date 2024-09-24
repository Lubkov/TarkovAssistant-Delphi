unit ME.Presenter.Options;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Options;

type
  TEditOptionsPresenter = class(TEditFormPresenter<TOptions>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

implementation

uses
  ME.DB.Utils, App.Service;

{ TEditOptionsPresenter }

procedure TEditOptionsPresenter.InternalSave;
begin
  inherited;

  AppService.SaveParams;
end;

procedure TEditOptionsPresenter.Cancel;
begin
  inherited;

end;

end.
