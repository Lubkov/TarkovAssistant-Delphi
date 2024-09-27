unit ME.Grid.Screenshots;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Grid.Resources, Data.DB, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, MemDS, DBAccess, Uni,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, FMX.ScrollBox,
  FMX.Grid, FMX.Controls.Presentation, ME.DB.Resource, ME.DB.Marker,
  ME.DB.Presenter.Resource, FMX.Edit;

type
  TScreenshotsGrid = class(TResourcesDBGrid)
  private
  protected
    function GetResourceKind: TResourceKind; override;
    function GetCommandSQLText: string; override;
    function GetRefreshSQLText: string; override;
    function GetResourceID: Variant; override;
    function GetEditPresenterClass: TEditResourcePresenterClass; override;
    function GetDelPresenterClass: TDelResourcePresenterClass; override;

    function InternalEditRecord(const Resource: TDBResource): Boolean;
    function InternalDeleteRecord: Boolean;
  public
    procedure Init(const Marker: TDBMarker); override;
    procedure AddRecord; override;
    procedure EditRecord; override;
    procedure DeleteRecord; override;
  end;

implementation

{$R *.fmx}

uses
  App.Service, ME.DB.Utils, ME.Dialog.Message, ME.Service.Resource, ME.DB.Edit.Resource,
  ME.Presenter.Screenshot;

{ TScreenshotsGrid }

function TScreenshotsGrid.GetResourceKind: TResourceKind;
begin
  Result := TResourceKind.Screenshot;
end;

function TScreenshotsGrid.GetCommandSQLText: string;
begin
//  Result := inherited GetCommandSQLText;

  Result :=
    ' SELECT r.ID as ID, ' +
    '        r.Kind as Kind, ' +
    '        r.Description as Description ' +
    ' FROM Resource r ' +
    ' WHERE (r.Kind = :Kind) ';

  if Marker <> nil then
    Result := Result + ' AND (r.MarkerID = :MarkerID)';

  if Sorted then
    Result := Result + ' ORDER BY r.Description ';
end;

function TScreenshotsGrid.GetRefreshSQLText: string;
begin
  Result := '';
end;

function TScreenshotsGrid.GetResourceID: Variant;
begin
  Result := FID.Value;
end;

function TScreenshotsGrid.GetEditPresenterClass: TEditResourcePresenterClass;
begin
  Result := TEditScreenshotPresenter;
end;

function TScreenshotsGrid.GetDelPresenterClass: TDelResourcePresenterClass;
begin
  Result := TDelScreenshotPresenter;
end;

function TScreenshotsGrid.InternalEditRecord(const Resource: TDBResource): Boolean;
var
  Presenter: TEditResourcePresenter;
  Dialog: TedDBResource;
begin
  Dialog := TedDBResource.Create(Self);
  try
    Presenter := EditPresenterClass.Create(Dialog, Resource);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

function TScreenshotsGrid.InternalDeleteRecord: Boolean;
var
  Resource: TDBResource;
  Presenter: TDelResourcePresenter;
  Dialog: TedMessage;
begin
  Resource := TDBResource.Create;
  try
    Resource.ID := FID.Value;
    Resource.MarkerID := MarkerID;
    Resource.Kind := ResourceKind;
    Resource.Description := FDescription.AsString;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := DelPresenterClass.Create(Dialog, Resource);
      try
        Result := Presenter.Delete;
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
  finally
    Resource.Free;
  end;
end;

procedure TScreenshotsGrid.Init(const Marker: TDBMarker);
begin
  inherited;

  laTitle.Text := 'Список скриншотов маркера';
  PicturePanel.Resizing := False;
end;

procedure TScreenshotsGrid.AddRecord;
var
  Resource: TDBResource;
begin
  Resource := TDBResource.Create;
  try
    Resource.MarkerID := MarkerID;
    Resource.Kind := ResourceKind;
    if not InternalEditRecord(Resource) then
      Exit;

    F.DisableControls;
    try
      F.Refresh;
      F.Last;
    finally
      F.EnableControls;
    end;
  finally
    Resource.Free;
  end;
end;

procedure TScreenshotsGrid.EditRecord;
var
  Resource: TDBResource;
begin
  Resource := TDBResource.Create;
  try
    if not ResourceService.GetAt(FID.Value, Resource) then
      Exit;

    ResourceService.LoadPicture(Resource);
    if InternalEditRecord(Resource) then
      F.RefreshRecord;
  finally
    Resource.Free;
  end;
end;

procedure TScreenshotsGrid.DeleteRecord;
begin
  if IsNullID(FID.Value) or not InternalDeleteRecord then
    Exit;

  F.DisableControls;
  try
    F.Refresh;
  finally
    F.EnableControls;
  end;
end;

end.
