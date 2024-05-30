unit ME.Presenter.Extraction;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter,
  ME.MapTag, ME.MapTagService;

type
  TEditExtractionPresenter = class(TEditFormPresenter<TMapTag>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelExtractionPresenter = class(TDelFormPresenter<TMapTag>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.DB.Utils, ME.PointService;

{ TEditExtractionPresenter }

procedure TEditExtractionPresenter.InternalSave;
begin
  inherited;

  if not IsNullID(Instance.MapID) then begin
    MapTagService.Save(Instance);
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
  MapTagService.Remove(Instance);
end;

end.
