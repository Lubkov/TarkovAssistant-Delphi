unit ME.Edit.Point;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.Edit.Form, ME.Edit.Form.Presenter, ME.DB.Point, FMX.Edit, FMX.EditBox,
  FMX.NumberBox;

type
  TedPoint = class(TEditForm, IEditDialog<TPoint>)
    laTopPoint: TLabel;
    edPositionX: TNumberBox;
    edPositionY: TNumberBox;
  private
    FPoint: TPoint;

    function GetPositionX: Integer;
    procedure SetPositionX(const Value: Integer);
    function GetPositionY: Integer;
    procedure SetPositionY(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TPoint);
    procedure PostValues(const Value: TPoint);

    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
  end;

implementation

{$R *.fmx}

{ TedPoint }

constructor TedPoint.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TedPoint.Destroy;
begin

  inherited;
end;

function TedPoint.GetPositionX: Integer;
begin
  Result := Trunc(edPositionX.Value);
end;

procedure TedPoint.SetPositionX(const Value: Integer);
begin
  edPositionX.Value := Value;
end;

function TedPoint.GetPositionY: Integer;
begin
  Result := Trunc(edPositionY.Value);
end;

procedure TedPoint.SetPositionY(const Value: Integer);
begin
  edPositionY.Value := Value;
end;

procedure TedPoint.SetInstance(const Value: TPoint);
begin
  FPoint := Value;

  if FPoint.IsNewInstance then
    Caption := 'Добавление новой координаты'
  else
    Caption := '#' + VarToStr(FPoint.ID) + ' Редактирование координаты';

  PositionX := FPoint.X;
  PositionY := FPoint.Y;
end;

procedure TedPoint.PostValues(const Value: TPoint);
begin
  Value.X := PositionX;
  Value.Y := PositionY;
end;

end.
