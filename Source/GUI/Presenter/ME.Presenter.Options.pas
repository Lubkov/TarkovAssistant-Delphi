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
  ME.DB.Utils, ME.Service.Options;

{ TEditOptionsPresenter }

procedure TEditOptionsPresenter.InternalSave;
begin
  inherited;

  OptionsService.Save(Instance);
end;

procedure TEditOptionsPresenter.Cancel;
begin
  inherited;

end;

end.
