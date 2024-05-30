unit ME.Edit.Extraction;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.DB.Marker, FMX.Edit, FMX.ListBox, FMX.EditBox, FMX.NumberBox,
  ME.Edit.Form.Presenter, ME.Point;

type
  TedExtraction = class(TEditForm, IEditDialog<TMarker>)
    edMarkerName: TEdit;
    edPositionX: TNumberBox;
    edPositionY: TNumberBox;
    laTopPoint: TLabel;
    edKindName: TComboBox;
  private
    FMarker: TMarker;

    function GetMarkerName: string;
    procedure SetMarkerName(const Value: string);
    function GetMarkerKind: TMarkerKind;
    procedure SetMarkerKind(const Value: TMarkerKind);
    function GetPositionX: Integer;
    procedure SetPositionX(const Value: Integer);
    function GetPositionY: Integer;
    procedure SetPositionY(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TMarker);
    procedure PostValues(const Value: TMarker);

    property MarkerName: string read GetMarkerName write SetMarkerName;
    property MarkerKind: TMarkerKind read GetMarkerKind write SetMarkerKind;
    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
  end;

implementation

{$R *.fmx}

{ TedExtraction }

constructor TedExtraction.Create(AOwner: TComponent);
var
  Kind: TMarkerKind;
begin
  inherited;

  edKindName.Clear;
  for Kind := tkPMCExtraction to tkCoopExtraction do
    edKindName.Items.Add(TMarker.KindToStr(Kind));
end;

destructor TedExtraction.Destroy;
begin

  inherited;
end;

function TedExtraction.GetMarkerName: string;
begin
  Result := edMarkerName.Text;
end;

procedure TedExtraction.SetMarkerName(const Value: string);
begin
  edMarkerName.Text := Value;
end;

function TedExtraction.GetMarkerKind: TMarkerKind;
begin
  Result := TMarkerKind(edKindName.ItemIndex);
end;

procedure TedExtraction.SetMarkerKind(const Value: TMarkerKind);
begin
  edKindName.ItemIndex := Ord(Value);
end;

function TedExtraction.GetPositionX: Integer;
begin
  Result := Trunc(edPositionX.Value);
end;

procedure TedExtraction.SetPositionX(const Value: Integer);
begin
  edPositionX.Value := Value
end;

function TedExtraction.GetPositionY: Integer;
begin
  Result := Trunc(edPositionY.Value);
end;

procedure TedExtraction.SetPositionY(const Value: Integer);
begin
  edPositionY.Value := Value
end;

procedure TedExtraction.SetInstance(const Value: TMarker);
begin
  FMarker := Value;

  if FMarker.IsNewInstance then
    Caption := 'Добавление нового выхода с карты'
  else
    Caption := '#' + VarToStr(FMarker.ID) + ' Редактирование выхода с карты';

  MarkerName := FMarker.Name;
  MarkerKind := FMarker.Kind;
  PositionX := FMarker.Left;
  PositionY := FMarker.Top;
end;

procedure TedExtraction.PostValues(const Value: TMarker);
begin
  Value.Name := MarkerName;
  Value.Kind := MarkerKind;
  Value.Left := PositionX;
  Value.Top := PositionY;
end;

end.
