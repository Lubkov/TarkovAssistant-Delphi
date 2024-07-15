unit ME.Frame.ResourcesGrid;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, System.Actions, FMX.ActnList, System.ImageList,
  FMX.ImgList, FMX.Grid, FMX.ScrollBox, FMX.Controls.Presentation, Data.DB,
  MemDS, DBAccess, Uni, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, ME.DB.Resource;

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
    procedure acEditResourceExecute(Sender: TObject);
    procedure acAddResourceExecute(Sender: TObject);
    procedure acDeleteResourceExecute(Sender: TObject);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
  private
    function InternalResourceEdit(const Resource: TDBResource): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;
  end;

implementation

uses
  App.Service, ME.Service.Resource, ME.DB.Edit.Resource, ME.DB.Presenter.Resource,
  ME.Dialog.Message;

{$R *.fmx}

{ TDBResourcesGrid }

constructor TDBResourcesGrid.Create(AOwner: TComponent);
begin
  inherited;

  Grid.RowCount := 0;
end;

destructor TDBResourcesGrid.Destroy;
begin

  inherited;
end;

function TDBResourcesGrid.InternalResourceEdit(const Resource: TDBResource): Boolean;
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

procedure TDBResourcesGrid.Init;
begin
  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text := Format('SELECT ID, Kind, Description FROM Resource WHERE Kind = %d', [Ord(TResourceKind.QuestItem)]);
  F.Open;
end;

procedure TDBResourcesGrid.acAddResourceExecute(Sender: TObject);
var
  Resource: TDBResource;
begin
  Resource := TDBResource.Create;
  try
    Resource.Kind := TResourceKind.QuestItem;

    if InternalResourceEdit(Resource) then
      F.Refresh;
  finally
    Resource.Free;
  end;
end;

procedure TDBResourcesGrid.acEditResourceExecute(Sender: TObject);
var
  Resource: TDBResource;
begin
  Resource := TDBResource.Create;
  try
    if not ResourceService.GetAt(FID.Value, Resource) then
      Exit;

    if InternalResourceEdit(Resource) then
      F.RefreshRecord;
  finally
    Resource.Free;
  end;
end;

procedure TDBResourcesGrid.acDeleteResourceExecute(Sender: TObject);
var
  Resource: TDBResource;
  Presenter: TDelResourcePresenter;
  Dialog: TedMessage;
begin
  Resource := TDBResource.Create;
  try
    if not ResourceService.GetAt(FID.Value, Resource) then begin
      F.Refresh;
      Exit;
    end;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelResourcePresenter.Create(Dialog, Resource);
      try
        if Presenter.Delete then begin
          Grid.BeginUpdate;
          try
            F.Refresh;
          finally
            Grid.EndUpdate;
          end;
        end;
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

procedure TDBResourcesGrid.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  acEditResourceExecute(Grid);
end;

end.
