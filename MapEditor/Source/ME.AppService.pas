unit ME.AppService;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  ME.Connection;

type
  TMEService = class(TComponent)
  private
    FConnection: TMEConnection;

    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetDatabase: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

    property Connected: Boolean read GetConnected write SetConnected;
    property Connection: TMEConnection read FConnection;
    property Database: string read GetDatabase;
  end;

var
  AppService: TMEService;

implementation

uses
  ME.PointService, ME.Service.Map, ME.Service.Layer, ME.Service.Marker;

{ TMEService }

constructor TMEService.Create(AOwner: TComponent);
begin
  inherited;

  FConnection := TMEConnection.Create(Self);
  PointService := TPointService.Create(FConnection.Connection);
  MapService := TMapService.Create(FConnection.Connection);
  LayerService := TLayerService.Create(FConnection.Connection);
  MarkerService := TMarkerService.Create(FConnection.Connection);
end;

destructor TMEService.Destroy;
begin
  FreeAndNil(MapService);
  FreeAndNil(PointService);
  FreeAndNil(LayerService);
  FreeAndNil(MarkerService);

  FreeAndNil(FConnection);

  inherited;
end;

function TMEService.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TMEService.SetConnected(const Value: Boolean);
begin
  FConnection.Connected := Value;
end;

function TMEService.GetDatabase: string;
begin
  Result := FConnection.Connection.Database;
end;

procedure TMEService.Connect;
begin
  FConnection.Connect;
end;

procedure TMEService.Disconnect;
begin
  FConnection.Disconnect;
end;

end.
