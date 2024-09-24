unit App.Service;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IOUtils,
  App.SQLite.Connection, ME.DB.Options;

type
  TAppService = class(TComponent)
  private
    FDBConnection: TSQLiteConnection;
    FOptions: TOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadParams;
    procedure LoadDataFromDB;
    procedure ConnectToDB;

    property DBConnection: TSQLiteConnection read FDBConnection;
    property Options: TOptions read FOptions;
  end;

var
  AppService: TAppService;

implementation

uses
  App.Constants, Map.Data.Service, ME.Service.Resource, ME.Service.Map,
  ME.Service.Layer, ME.Service.Quest, ME.Service.Marker, ME.Service.QuestItem,
  ME.Service.Profile, ME.Service.QuestTracker, ME.Service.Options;

{ TAppService }

constructor TAppService.Create(AOwner: TComponent);
begin
  inherited;

  DataService := TDataService.Create;
  FOptions := TOptions.Create;

  // DB layer
  FDBConnection := TSQLiteConnection.Create(Self);
  ResourceService := TResourceService.Create(DBConnection.Connection);
  MapService := TMapService.Create(DBConnection.Connection);
  LayerService := TLayerService.Create(DBConnection.Connection);
  QuestService := TQuestService.Create(DBConnection.Connection);
  MarkerService := TMarkerService.Create(DBConnection.Connection);
  QuestItemService := TQuestItemService.Create(DBConnection.Connection);
  ProfileService := TProfileService.Create(DBConnection.Connection);
  QuestTrackerService := TQuestTrackerService.Create(DBConnection.Connection);
  OptionsService := TOptionsService.Create(DBConnection.Connection);
end;

destructor TAppService.Destroy;
begin
  DataService.Free;
  FOptions.Free;

  ResourceService.Free;
  MapService.Free;
  FDBConnection.Free;
  LayerService.Free;
  QuestService.Free;
  MarkerService.Free;
  QuestItemService.Free;
  ProfileService.Free;
  QuestTrackerService.Free;
  OptionsService.Free;

  inherited;
end;

procedure TAppService.LoadParams;
begin
  AppParams.Load;
  ConnectToDB;
  OptionsService.Load(FOptions);
end;

procedure TAppService.LoadDataFromDB;
begin
  DataService.Load;
end;

procedure TAppService.ConnectToDB;
const
  Database = 'data.db';
var
  FileName: string;
begin
  FileName := TPath.Combine(AppParams.DataPath, Database);
  FDBConnection.Disconnect;
  FDBConnection.Database := FileName;
  FDBConnection.Connect;
end;

end.
