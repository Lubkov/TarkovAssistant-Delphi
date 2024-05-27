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
    FPosition: TPoint;

    function GetTagName: string;
    procedure SetTagName(const Value: string);
    function GetTagKind: TTagKind;
    procedure SetTagKind(const Value: TTagKind);
    function GetPosition: TPoint;
    procedure SetPosition(const Value: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TMapTag);
    procedure PostValues(const Value: TMapTag);

    property TagName: string read GetTagName write SetTagName;
    property TagKind: TTagKind read GetTagKind write SetTagKind;
    property Position: TPoint read GetPosition write SetPosition;
  end;

implementation

{$R *.fmx}

{ TedMapTag }

constructor TedExtraction.Create(AOwner: TComponent);
var
  Kind: TTagKind;
begin
  inherited;

  FPosition := TPoint.Create;
  edKindName.Clear;
  for Kind := tkPMCExtraction to tkCoopExtraction do
    edKindName.Items.Add(TMapTag.KindToStr(Kind));
end;

destructor TedExtraction.Destroy;
begin
  FPosition.Free;

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

function TedExtraction.GetPosition: TPoint;
begin
  Result := FPosition;
end;

procedure TedExtraction.SetPosition(const Value: TPoint);
begin
  FPosition.Assign(Value);
end;

procedure TedExtraction.SetInstance(const Value: TMapTag);
begin
  FMapTag := Value;

  if FMapTag.IsNewInstance then
    Caption := 'Добавление нового уровня карты'
  else
    Caption := '#' + VarToStr(FMapTag.ID) + ' Редактирование уровня карты';

  TagName := FMapTag.Name;
  TagKind := FMapTag.Kind;
  Position := FMapTag.Position;
end;

procedure TedExtraction.PostValues(const Value: TMapTag);
begin
  Value.Name := TagName;
  Value.Kind := TagKind;
  Value.Position := Position;
end;

end.
