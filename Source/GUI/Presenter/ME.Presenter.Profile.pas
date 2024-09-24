unit ME.Presenter.Profile;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Profile;

type
  TEditProfilePresenter = class(TEditFormPresenter<TProfile>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelProfilePresenter = class(TDelFormPresenter<TProfile>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  App.Constants, ME.DB.Utils, ME.Service.Profile;

{ TEditProfilePresenter }

procedure TEditProfilePresenter.InternalSave;
begin
  ProfileService.Save(Instance);
end;

procedure TEditProfilePresenter.Cancel;
begin
  inherited;

end;

{ TDelProfilePresenter }

function TDelProfilePresenter.GetDelMessage: string;
begin
  Result := 'Удалить профиль?';
end;

procedure TDelProfilePresenter.InternalDelete;
begin
  ProfileService.Remove(Instance);
end;

end.
