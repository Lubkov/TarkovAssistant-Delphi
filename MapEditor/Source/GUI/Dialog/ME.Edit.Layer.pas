unit ME.Edit.Layer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.Objects,
  FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.ImgList, System.ImageList,
  ME.Edit.Form.Presenter, ME.Frame.Picture, ME.DB.Layer, FMX.Layouts;

type
  TedLayer = class(TEditForm, IEditDialog<TDBLayer>)
    paPicture: TPanel;
    Layout1: TLayout;
    edLevelName: TEdit;
    edLevel: TNumberBox;
    laLayerLevel: TLabel;
    laLayerName: TLabel;
  private
    FLayer: TDBLayer;
    FPicturePanel: TfrPicture;

    function GetLevel: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLevelName: string;
    procedure SetLevelName(const Value: string);
    function GetPicture: TBitmap;
    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TDBLayer);
    procedure PostValues(const Value: TDBLayer);

    property Level: Integer read GetLevel write SetLevel;
    property LevelName: string read GetLevelName write SetLevelName;
    property Picture: TBitmap read GetPicture write SetPicture;
  end;

implementation

{$R *.fmx}

constructor TedLayer.Create(AOwner: TComponent);
begin
  inherited;

  FPicturePanel := TfrPicture.Create(Self);
  FPicturePanel.Parent := paPicture;
  FPicturePanel.Align := TAlignLayout.Client;
end;

function TedLayer.GetLevel: Integer;
begin
  Result := Trunc(edLevel.Value);
end;

procedure TedLayer.SetLevel(const Value: Integer);
begin
  edLevel.Value := Value;
end;

function TedLayer.GetLevelName: string;
begin
  Result := edLevelName.Text;
end;

procedure TedLayer.SetLevelName(const Value: string);
begin
  edLevelName.Text := Value;
end;

function TedLayer.GetPicture: TBitmap;
begin
  Result := FPicturePanel.Picture;
end;

procedure TedLayer.SetPicture(const Value: TBitmap);
begin
  FPicturePanel.Picture := Value;
end;

procedure TedLayer.SetInstance(const Value: TDBLayer);
begin
  FLayer := Value;

  if FLayer.IsNewInstance then
    Caption := 'Добавление нового уровня карты'
  else
    Caption := 'Редактирование уровня карты';

  Level := FLayer.Level;
  LevelName := FLayer.Name;
  Picture := FLayer.Picture;
end;

procedure TedLayer.PostValues(const Value: TDBLayer);
begin
  Value.Level := Level;
  Value.Name := LevelName;

  if FPicturePanel.Changed then
    Value.Picture := Picture;
end;

end.
