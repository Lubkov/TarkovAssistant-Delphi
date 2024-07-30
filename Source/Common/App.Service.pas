unit App.Service;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils,
  App.SQLite.Connection;

type
  TAppService = class(TComponent)
  private
    FDBConnection: TSQLiteConnection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadParams;
    procedure LoadDataFromJSON;
    procedure ConnectToDB;

    property DBConnection: TSQLiteConnection read FDBConnection;
  end;

var
  AppService: TAppService;

implementation

uses
  App.Constants, Map.Data.Service, ME.Service.Resource, ME.Service.Map,
  ME.Service.Layer, ME.Service.Quest, ME.Service.Marker, ME.Service.QuestItem;

{ TAppService }

constructor TAppService.Create(AOwner: TComponent);
begin
  inherited;

  DataService := TDataService.Create;

  // DB layer
  FDBConnection := TSQLiteConnection.Create(Self);
  ResourceService := TResourceService.Create(DBConnection.Connection);
  MapService := TMapService.Create(DBConnection.Connection);
  LayerService := TLayerService.Create(DBConnection.Connection);
  QuestService := TQuestService.Create(DBConnection.Connection);
  MarkerService := TMarkerService.Create(DBConnection.Connection);
  QuestItemService := TQuestItemService.Create(DBConnection.Connection);
end;

destructor TAppService.Destroy;
begin
  DataService.Free;

  ResourceService.Free;
  MapService.Free;
  FDBConnection.Free;
  LayerService.Free;
  QuestService.Free;
  MarkerService.Free;
  QuestItemService.Free;

  inherited;
end;

procedure TAppService.LoadParams;
begin
  AppParams.Load;
end;

procedure TAppService.LoadDataFromJSON;
var
  FileName: string;
begin
  FileName := TPath.Combine(AppParams.DataPath, 'data.json');
  DataService.Load(FileName);
end;

procedure TAppService.ConnectToDB;
const
  Database = 'data.db';
var
  FileName: string;
begin
  FileName := TPath.Combine(AppParams.DataPath, Database);
  FDBConnection.Database := FileName;
  FDBConnection.Connect;
end;

end.
