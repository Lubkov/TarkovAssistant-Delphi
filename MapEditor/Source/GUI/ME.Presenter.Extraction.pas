unit ME.Presenter.Extraction;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.DB.Marker, ME.Service.Marker;

type
  TEditExtractionPresenter = class(TEditFormPresenter<TMarker>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelExtractionPresenter = class(TDelFormPresenter<TMarker>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils;

{ TEditExtractionPresenter }

procedure TEditExtractionPresenter.InternalSave;
begin
  inherited;

  if not IsNullID(Instance.MapID) then begin
    MarkerService.Save(Instance);
  end;
end;

procedure TEditExtractionPresenter.Cancel;
begin
  inherited;

end;

{ TDelExtractionPresenter }

function TDelExtractionPresenter.GetDelMessage: string;
begin
  Result := 'Удалить выход "' + Instance.Name + '"?';
end;

procedure TDelExtractionPresenter.InternalDelete;
begin
  MarkerService.Remove(Instance);
end;

end.
