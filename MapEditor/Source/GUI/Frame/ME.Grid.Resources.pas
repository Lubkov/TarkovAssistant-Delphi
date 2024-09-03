unit ME.Grid.Resources;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, System.Actions, FMX.ActnList, System.ImageList,
  FMX.ImgList, FMX.Grid, FMX.ScrollBox, FMX.Controls.Presentation, Data.DB,
  MemDS, DBAccess, Uni, Fmx.Bind.Grid, System.Bindings.Outputs, FMX.ExtCtrls,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, ME.DB.Resource, ME.DB.Marker,
  ME.Frame.Picture;

type
  TDBResourcesGrid = class(TFrame)
    paTopPanel: TPanel;
    edAddResource: TSpeedButton;
    edEditResource: TSpeedButton;
    edDeleteResource: TSpeedButton;
    laTitle: TLabel;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAddResource: TAction;
    acEditResource: TAction;
    acDeleteResource: TAction;
    F: TUniQuery;
    FID: TIntegerField;
    FKind: TIntegerField;
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    GridBindings: TBindingsList;
    FDescription: TWideStringField;
    paPicture: TPanel;
    procedure acEditResourceExecute(Sender: TObject);
    procedure acAddResourceExecute(Sender: TObject);
    procedure acDeleteResourceExecute(Sender: TObject);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
  private
    FMarker: TDBMarker;
    FResourceKind: TResourceKind;
    FPicturePanel: TfrPicture;
  protected
    function GetCommandSQLText: string; virtual;
//    function InternalResourceAdd(const Resource: TDBResource): Boolean;
    function InternalEditRecord(const Resource: TDBResource): Boolean;
    function InternalDeleteRecord: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Marker: TDBMarker; const Kind: TResourceKind);
    procedure AddRecord; virtual;
    procedure EditRecord; virtual;
    procedure DeleteRecord; virtual;

    property Marker: TDBMarker read FMarker;
    property ResourceKind: TResourceKind read FResourceKind;
  end;

implementation

uses
  App.Service, ME.DB.Utils, ME.Service.Resource, ME.DB.Edit.Resource, ME.DB.Presenter.Resource,
  ME.Dialog.Message;

{$R *.fmx}

{ TDBResourcesGrid }

constructor TDBResourcesGrid.Create(AOwner: TComponent);
begin
  inherited;

  FMarker := nil;
  Grid.RowCount := 0;

  FPicturePanel := TfrPicture.Create(Self);
  FPicturePanel.Parent := paPicture;
  FPicturePanel.Align := TAlignLayout.Client;
  FPicturePanel.Readonly := True;
end;

destructor TDBResourcesGrid.Destroy;
begin

  inherited;
end;

function TDBResourcesGrid.GetCommandSQLText: string;
begin
  Result := 'SELECT r.ID as ID, r.Kind as Kind, r.Description as Description FROM Resource r ';
  if FMarker = nil then
    Result := Result + ' WHERE (1 = 1)'
  else
    case ResourceKind of
      TResourceKind.Screenshot:
        Result := Result + ' WHERE (r.MarkerID = :MarkerID)';
      TResourceKind.QuestItem:
        Result := Result +
          ' INNER JOIN QuestItem qi ON (qi.ResourceID = r.ID) AND (qi.MarkerID = :MarkerID)';
    end;
  Result := Result + ' AND (r.Kind = :Kind)';
end;

function TDBResourcesGrid.InternalEditRecord(const Resource: TDBResource): Boolean;
var
  Presenter: TEditResourcePresenter;
  Dialog: TedDBResource;
begin
  Dialog := TedDBResource.Create(Self);
  try
    Presenter := TEditResourcePresenter.Create(Dialog, Resource);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

function TDBResourcesGrid.InternalDeleteRecord: Boolean;
var
  Resource: TDBResource;
  Presenter: TDelResourcePresenter;
  Dialog: TedMessage;
begin
  Resource := TDBResource.Create;
  try
    Resource.ID := FID.Value;
    Resource.MarkerID := FMarker.ID;
    Resource.Kind := FResourceKind;
    Resource.Description := FDescription.AsString;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelResourcePresenter.Create(Dialog, Resource);
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

procedure TDBResourcesGrid.Init(const Marker: TDBMarker; const Kind: TResourceKind);
begin
  FMarker := Marker;
  FResourceKind := Kind;

  case ResourceKind of
    TResourceKind.Screenshot:
      laTitle.Text := 'Список скриншотов маркера';
    TResourceKind.QuestItem:
      laTitle.Text := 'Список квестовых предметов';
  end;
  FPicturePanel.Resize := Kind = TResourceKind.QuestItem;

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.CachedUpdates := (FMarker <> nil) and FMarker.IsNewInstance;
  F.SQL.Text := GetCommandSQLText;
  F.ParamByName('Kind').AsInteger := Ord(Kind);
  if F.FindParam('MarkerID') <> nil then
    F.ParamByName('MarkerID').Value := FMarker.ID;

  F.Open;
end;

procedure TDBResourcesGrid.AddRecord;
var
  Resource: TDBResource;
begin
  Resource := TDBResource.Create;
  try
    Resource.MarkerID := FMarker.ID;
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

procedure TDBResourcesGrid.EditRecord;
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

procedure TDBResourcesGrid.DeleteRecord;
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

procedure TDBResourcesGrid.acAddResourceExecute(Sender: TObject);
begin
  AddRecord;
end;

procedure TDBResourcesGrid.acEditResourceExecute(Sender: TObject);
begin
  EditRecord;
end;

procedure TDBResourcesGrid.acDeleteResourceExecute(Sender: TObject);
begin
  DeleteRecord;
end;

procedure TDBResourcesGrid.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  EditRecord;
end;

procedure TDBResourcesGrid.ActionList1Update(Action: TBasicAction;  var Handled: Boolean);
begin
  acAddResource.Enabled := True;
  acEditResource.Enabled := Grid.RowCount > 0;
  acDeleteResource.Enabled := Grid.RowCount > 0;
end;

procedure TDBResourcesGrid.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
begin
  ResourceService.LoadPicture(FID.Value, TResourceKind(FKind.AsInteger), FPicturePanel.Picture);
  FPicturePanel.ResizePicture;
end;

end.
