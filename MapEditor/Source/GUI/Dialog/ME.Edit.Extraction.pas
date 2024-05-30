unit ME.Edit.Extraction;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.MapTag, FMX.Edit, FMX.ListBox, FMX.EditBox, FMX.NumberBox,
  ME.Edit.Form.Presenter, ME.Point;

type
  TedExtraction = class(TEditForm, IEditDialog<TMapTag>)
    edTagName: TEdit;
    edPositionX: TNumberBox;
    edPositionY: TNumberBox;
    laTopPoint: TLabel;
    edKindName: TComboBox;
  private
    FMapTag: TMapTag;

    function GetTagName: string;
    procedure SetTagName(const Value: string);
    function GetTagKind: TTagKind;
    procedure SetTagKind(const Value: TTagKind);
    function GetPositionX: Integer;
    procedure SetPositionX(const Value: Integer);
    function GetPositionY: Integer;
    procedure SetPositionY(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TMapTag);
    procedure PostValues(const Value: TMapTag);

    property TagName: string read GetTagName write SetTagName;
    property TagKind: TTagKind read GetTagKind write SetTagKind;
    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
  end;

implementation

{$R *.fmx}

{ TedMapTag }

constructor TedExtraction.Create(AOwner: TComponent);
var
  Kind: TTagKind;
begin
  inherited;

  edKindName.Clear;
  for Kind := tkPMCExtraction to tkCoopExtraction do
    edKindName.Items.Add(TMapTag.KindToStr(Kind));
end;

destructor TedExtraction.Destroy;
begin

  inherited;
end;

function TedExtraction.GetTagName: string;
begin
  Result := edTagName.Text;
end;

procedure TedExtraction.SetTagName(const Value: string);
begin
  edTagName.Text := Value;
end;

function TedExtraction.GetTagKind: TTagKind;
begin
  Result := TTagKind(edKindName.ItemIndex);
end;

procedure TedExtraction.SetTagKind(const Value: TTagKind);
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

procedure TedExtraction.SetInstance(const Value: TMapTag);
begin
  FMapTag := Value;

  if FMapTag.IsNewInstance then
    Caption := 'Добавление нового выхода с карты'
  else
    Caption := '#' + VarToStr(FMapTag.ID) + ' Редактирование выхода с карты';

  TagName := FMapTag.Name;
  TagKind := FMapTag.Kind;
  PositionX := FMapTag.Left;
  PositionY := FMapTag.Top;
end;

procedure TedExtraction.PostValues(const Value: TMapTag);
begin
  Value.Name := TagName;
  Value.Kind := TagKind;
  Value.Left := PositionX;
  Value.Top := PositionY;
end;

end.
