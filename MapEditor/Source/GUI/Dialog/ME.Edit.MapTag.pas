unit ME.Edit.MapTag;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.MapTag, FMX.Edit, FMX.ListBox, FMX.EditBox, FMX.NumberBox,
  ME.Edit.Form.Presenter;

type
  TedMapTag = class(TEditForm, IEditDialog<TMapTag>)
    edTagName: TEdit;
    edPositionX: TNumberBox;
    edPositionY: TNumberBox;
    laTopPoint: TLabel;
    edKindName: TComboBox;
  private
    FMapTag: TMapTag;

    function GetTagName: string;
    procedure SetTagName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TMapTag);
    procedure PostValues(const Value: TMapTag);

    property TagName: string read GetTagName write SetTagName;
//    FKind: TTagKind;
//    FPosition: TPoint;
  end;

implementation

{$R *.fmx}

{ TedMapTag }

constructor TedMapTag.Create(AOwner: TComponent);
var
  Kind: TTagKind;
begin
  inherited;

  edKindName.Clear;
  for Kind := tkPMCExtraction to tkCoopExtraction do
    edKindName.Items.Add(TMapTag.KindToStr(Kind));
end;

function TedMapTag.GetTagName: string;
begin
  Result := edTagName.Text;
end;

procedure TedMapTag.SetTagName(const Value: string);
begin
  edTagName.Text := Value;
end;

procedure TedMapTag.SetInstance(const Value: TMapTag);
begin
  FMapTag := Value;

  if FMapTag.IsNewInstance then
    Caption := 'Добавление нового уровня карты'
  else
    Caption := '#' + VarToStr(FMapTag.ID) + ' Редактирование уровня карты';

  TagName := FMapTag.Name;
end;

procedure TedMapTag.PostValues(const Value: TMapTag);
begin
  Value.Name := TagName;
end;

end.
