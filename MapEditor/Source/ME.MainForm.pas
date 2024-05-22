unit ME.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  UniProvider, SQLiteUniProvider, Data.DB, MemDS, DBAccess, Uni,
  ME.AppService, ME.PointService, ME.Point, ME.LocalMap, ME.LocalMapService,
  ME.Frame.LocalMap, ME.Frame.MapLevel, SimpleLogger;

type
  TMainForm = class(TForm)
    UniConnection1: TUniConnection;
    UniQuery1: TUniQuery;
    SQLiteUniProvider1: TSQLiteUniProvider;
    Panel1: TPanel;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button8: TButton;
    Panel2: TPanel;
    Button6: TButton;
    Button7: TButton;
    Button1: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLocalMapPanel: TfrLocalMap;
    FMapLevelPanel: TfrMapLevel;

    procedure GetPoint(ID: Integer; const Point: TPoint);
    procedure GetLocalMap(ID: Integer; const LocalMap: TLocalMap);
    procedure OnLocalMapChange(const LocalMap: TLocalMap);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Logger.Memo := Memo1;

  try
    AppService.Connect;
  finally
    Logger.Lines.AddPair('Connected', AppService.Connected)
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  AppService.Disconnect;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FMapLevelPanel := TfrMapLevel.Create(Self);
  FMapLevelPanel.Parent := Panel4;
  FMapLevelPanel.Align := alClient;

  FLocalMapPanel := TfrLocalMap.Create(Self);
  FLocalMapPanel.Parent := Panel3;
  FLocalMapPanel.Align := alClient;
  FLocalMapPanel.OnLocalMapChange := OnLocalMapChange;
  FLocalMapPanel.Init(AppService.Connection.Connection);
end;

procedure TMainForm.GetPoint(ID: Integer; const Point: TPoint);
begin
  Logger.NewLine;
  if not PointService.GetAt(ID, Point) then
    Logger.Lines.AddPair('Record not found, ID', ID)
  else
    Logger.Lines.Add('#{0} Point = ({1}, {2})', [Point.ID, Point.X, Point.Y]);
end;

procedure TMainForm.GetLocalMap(ID: Integer; const LocalMap: TLocalMap);
begin
  Logger.NewLine;
  if not LocalMapService.GetAt(ID, LocalMap) then
    Logger.Lines.AddPair('Record not found, ID', ID)
  else begin
    Logger.Lines.Add('#{0} {1}', [LocalMap.ID, LocalMap.Name]);
    Logger.Lines.Add('  #{0} Left = ({1}, {2})', [LocalMap.Left.ID, LocalMap.Left.X, LocalMap.Left.Y]);
    Logger.Lines.Add('  #{0} Right = ({1}, {2})', [LocalMap.Right.ID, LocalMap.Right.X, LocalMap.Right.Y]);
  end;
end;

procedure TMainForm.OnLocalMapChange(const LocalMap: TLocalMap);
begin
  FMapLevelPanel.Init(LocalMap);
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  Point: TPoint;
  i: Integer;
begin
  Point := TPoint.Create;
  try
    for i := 0 to 2 do
      GetPoint(i, Point);
  finally
    Point.Free;
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  Point: TPoint;
begin
  Point := TPoint.Create;
  try
    Point.X := -100;
    Point.Y := 100;
    PointService.Insert(Point);

    Logger.NewLine;
    Logger.Lines.Add('#{0} Point = ({1}, {2})', [Point.ID, Point.X, Point.Y]);
  finally
    Point.Free;
  end;
end;

procedure TMainForm.Button5Click(Sender: TObject);
var
  Point: TPoint;
begin
  Point := TPoint.Create;
  try
    PointService.GetAt(8, Point);
    Point.X := 500;
    Point.Y := -500;
    PointService.Update(Point);

    PointService.GetAt(8, Point);
    Logger.NewLine;
    Logger.Lines.Add('#{0} Point = ({1}, {2})', [Point.ID, Point.X, Point.Y]);
  finally
    Point.Free;
  end;
end;

procedure TMainForm.Button8Click(Sender: TObject);
var
  Point: TPoint;
begin
  Point := TPoint.Create;
  try
    PointService.GetAt(8, Point);
    PointService.Remove(Point);

    Logger.NewLine;
    Logger.Lines.Add('#{0} Point deleted', [Point.ID]);
  finally
    Point.Free;
  end;
end;

procedure TMainForm.Button6Click(Sender: TObject);
var
  LocalMap: TLocalMap;
begin
  LocalMap := TLocalMap.Create;
  try
    GetLocalMap(1, LocalMap);
  finally
    LocalMap.Free;
  end;
end;

procedure TMainForm.Button7Click(Sender: TObject);
var
  LocalMap: TLocalMap;
begin
  LocalMap := TLocalMap.Create;
  try
    LocalMap.Name := 'Customs';
    LocalMap.Left.X := 800;
    LocalMap.Left.Y := -300;
    LocalMap.Right.X := -400;
    LocalMap.Right.Y := 300;
    LocalMapService.Insert(LocalMap);

    GetLocalMap(LocalMap.ID, LocalMap);
  finally
    LocalMap.Free;
  end;
end;

end.
