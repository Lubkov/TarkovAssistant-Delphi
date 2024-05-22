unit ME.Edit.LocalMap;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ME.Dialog.Presenter, ME.Edit.Form.Presenter, ME.LocalMap, System.Actions,
  Vcl.ActnList, Vcl.ExtCtrls;

type
  TedLocalMap = class(TForm, IEditDialog<TLocalMap>)
    laMapName: TLabel;
    edMapName: TEdit;
    edLeft: TEdit;
    edTop: TEdit;
    edRight: TEdit;
    edBottom: TEdit;
    Panel_Button: TPanel;
    btnCancel: TButton;
    btnNext: TButton;
    ActionList1: TActionList;
    acSuccess: TAction;
    acCancel: TAction;
    procedure acSuccessExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
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

{$R *.dfm}

{ TedLocalMap }

procedure TedLocalMap.acSuccessExecute(Sender: TObject);
begin
// For action enabled
end;

procedure TedLocalMap.acCancelExecute(Sender: TObject);
begin
  Close;
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
  if not TryStrToInt(edLeft.Text, Result) then
    Result := 0;
end;

procedure TedLocalMap.SetMapLeft(const Value: Integer);
begin
  edLeft.Text := IntToStr(Value);
end;

function TedLocalMap.GetMapTop: Integer;
begin
  if not TryStrToInt(edTop.Text, Result) then
    Result := 0;
end;

procedure TedLocalMap.SetMapTop(const Value: Integer);
begin
  edTop.Text := IntToStr(Value);
end;

function TedLocalMap.GetMapRight: Integer;
begin
  if not TryStrToInt(edRight.Text, Result) then
    Result := 0;
end;

procedure TedLocalMap.SetMapRight(const Value: Integer);
begin
  edRight.Text := IntToStr(Value);
end;

function TedLocalMap.GetMapBottom: Integer;
begin
  if not TryStrToInt(edBottom.Text, Result) then
    Result := 0;
end;

procedure TedLocalMap.SetMapBottom(const Value: Integer);
begin
  edBottom.Text := IntToStr(Value);
end;

function TedLocalMap.GetModalResult: TModalResult;
begin
  Result := ModalResult;
end;

procedure TedLocalMap.SetModalResult(Value: TModalResult);
begin
  ModalResult := Value;
end;

procedure TedLocalMap.SetInstance(const Value: TLocalMap);
begin
  FLocalMap := Value;

  if FLocalMap.IsNewInstance then
    Caption := 'Создание новой карты'
  else
    Caption := '#' + VarToStr(FLocalMap.ID) + ' ' + FLocalMap.Name;

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
