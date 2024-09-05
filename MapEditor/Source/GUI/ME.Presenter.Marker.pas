unit ME.Presenter.Marker;

interface

uses
  System.SysUtils, System.Variants, System.Classes, FMX.Controls,
  ME.Edit.Form.Presenter, ME.Del.Form.Presenter, ME.DB.Marker, ME.DB.Resource;

type
  TEditMarkerPresenter = class(TEditFormPresenter<TDBMarker>)
  private
  protected
    procedure InternalSave; override;
    procedure Cancel; override;
  public
  end;

  TDelMarkerPresenter = class(TDelFormPresenter<TDBMarker>)
  protected
    function GetDelMessage: string; override;
    procedure InternalDelete; override;
  end;

implementation

uses
  ME.Service.Marker, ME.Service.Resource;

{ TEditMarkerPresenter }

procedure TEditMarkerPresenter.InternalSave;
//var
//  IsNewInstance: Boolean;
//  Resource: TDBResource;
begin
  inherited;

//  IsNewInstance := Instance.IsNewInstance;
  MarkerService.Save(Instance);

//  if IsNewInstance then
//    for Resource in Instance.Images do begin
//      Resource.MarkerID := Instance.ID;
//      ResourceService.Save(Resource);
//    end;
end;

procedure TEditMarkerPresenter.Cancel;
begin
  inherited;

end;

{ TDelMarkerPresenter }

function TDelMarkerPresenter.GetDelMessage: string;
begin
  Result := 'Удалить маркер "' + Instance.Caption + '"?';
end;

procedure TDelMarkerPresenter.InternalDelete;
begin
  MarkerService.Remove(Instance);
end;

end.
