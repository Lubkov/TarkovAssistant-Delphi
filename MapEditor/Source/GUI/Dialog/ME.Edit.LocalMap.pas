unit ME.Edit.LocalMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox,
  FMX.NumberBox, ME.Dialog.Presenter, ME.Edit.Form.Presenter, ME.LocalMap;

type
  TedLocalMap = class(TForm, IEditDialog<TLocalMap>)
    Panel1: TPanel;
    buSuccess: TButton;
    buCancel: TButton;
    ActionList1: TActionList;
    acSuccess: TAction;
    acCancel: TAction;
    edMapName: TEdit;
    edLeft: TNumberBox;
    edTop: TNumberBox;
    edRight: TNumberBox;
    edBottom: TNumberBox;

    procedure acSuccessExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLocalMap: TLocalMap;

    function GetMapName: string;
    procedure SetMapName(const Value: string);
    function GetMapLeft: Integer;
    procedure SetMapLeft(const Value: Integer);
    function GetMapTop: Integer;
    procedure SetMapTop(const Value: Integer);
    function GetMapRight: Integer;
    procedure SetMapRight(const Value: Integer);
    function GetMapBottom: Integer;
    procedure SetMapBottom(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetModalResult: TModalResult;
    procedure SetModalResult(Value: TModalResult);

    procedure SetInstance(const Value: TLocalMap);
    procedure PostValues(const Value: TLocalMap);

    property MapName: string read GetMapName write SetMapName;
    property MapLeft: Integer read GetMapLeft write SetMapLeft;
    property MapTop: Integer read GetMapTop write SetMapTop;
    property MapRight: Integer read GetMapRight write SetMapRight;
    property MapBottom: Integer read GetMapBottom write SetMapBottom;
  end;

var
  edLocalMap: TedLocalMap;

implementation

{$R *.fmx}

constructor TedLocalMap.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TedLocalMap.Destroy;
begin

  inherited;
end;

procedure TedLocalMap.FormShow(Sender: TObject);
begin
//  FMapLevelPanel.Init();
end;

procedure TedLocalMap.acSuccessExecute(Sender: TObject);
begin
// For action enabled
end;

procedure TedLocalMap.acCancelExecute(Sender: TObject);
begin
  Close;
end;

function TedLocalMap.GetModalResult: TModalResult;
begin
  Result := ModalResult;
end;

procedure TedLocalMap.SetModalResult(Value: TModalResult);
begin
  ModalResult := Value;
end;

function TedLocalMap.GetMapName: string;
begin
  Result := edMapName.Text;
end;

procedure TedLocalMap.SetMapName(const Value: string);
begin
  edMapName.Text := Value;
end;

function TedLocalMap.GetMapLeft: Integer;
begin
  Result := Trunc(edLeft.Value);
end;

procedure TedLocalMap.SetMapLeft(const Value: Integer);
begin
  edLeft.Value := Value;
end;

function TedLocalMap.GetMapTop: Integer;
begin
  Result := Trunc(edTop.Value);
end;

procedure TedLocalMap.SetMapTop(const Value: Integer);
begin
  edTop.Value := Value;
end;

function TedLocalMap.GetMapRight: Integer;
begin
  Result := Trunc(edRight.Value);
end;

procedure TedLocalMap.SetMapRight(const Value: Integer);
begin
  edRight.Value := Value;
end;

function TedLocalMap.GetMapBottom: Integer;
begin
  Result := Trunc(edBottom.Value);
end;

procedure TedLocalMap.SetMapBottom(const Value: Integer);
begin
  edBottom.Value := Value;
end;

procedure TedLocalMap.SetInstance(const Value: TLocalMap);
begin
  FLocalMap := Value;

  if FLocalMap.IsNewInstance then
    Caption := 'Создание новой карты'
  else
    Caption := '#' + VarToStr(FLocalMap.ID) + '  Редактирование карты "' + FLocalMap.Name + '"';

  MapName := FLocalMap.Name;
  MapLeft := FLocalMap.Left.X;
  MapTop := FLocalMap.Left.Y;
  MapRight := FLocalMap.Right.X;
  MapBottom := FLocalMap.Right.Y;
end;

procedure TedLocalMap.PostValues(const Value: TLocalMap);
begin
  Value.Name := MapName;
  FLocalMap.Left.SetBounds(MapLeft, MapTop);
  FLocalMap.Right.SetBounds(MapRight, MapBottom);
end;

end.
