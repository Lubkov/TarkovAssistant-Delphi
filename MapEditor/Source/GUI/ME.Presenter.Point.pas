unit ME.Presenter.Point;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Point, ME.Service.Point;

type
  TEditPointPresenter = class(TEditFormPresenter<TPoint>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelPointPresenter = class(TDelFormPresenter<TPoint>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils;

{ TEditPointPresenter }

procedure TEditPointPresenter.InternalSave;
begin
  inherited;

  if not IsNullID(Instance.QuestID) then begin
    PointService.Save(Instance);
  end;
end;

procedure TEditPointPresenter.Cancel;
begin
  inherited;

end;

{ TDelPointPresenter }

function TDelPointPresenter.GetDelMessage: string;
begin
  Result := 'Удалить координату (' + IntToStr(Instance.X) + ', ' + IntToStr(Instance.Y) + ')"?';
end;

procedure TDelPointPresenter.InternalDelete;
begin
  PointService.Remove(Instance);
end;

end.
