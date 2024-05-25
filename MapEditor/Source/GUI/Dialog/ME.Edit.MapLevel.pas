unit ME.Edit.MapLevel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  FMX.Objects, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  ME.DB.Entity, ME.MapLevel, ME.Edit.Form.Presenter;

type
  TedMapLevel = class(TEditForm, IEditDialog<TMapLevel>)
    edLevel: TNumberBox;
    edLevelName: TEdit;
    paPicture: TPanel;
    edPicture: TImage;
  private
    FMapLevel: TMapLevel;

    function GetLevel: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLevelName: string;
    procedure SetLevelName(const Value: string);
    function GetPicture: TBitmap;
    procedure SetPicture(const Value: TBitmap);
  public
    procedure SetInstance(const Value: TMapLevel);
    procedure PostValues(const Value: TMapLevel);

    property Level: Integer read GetLevel write SetLevel;
    property LevelName: string read GetLevelName write SetLevelName;
    property Picture: TBitmap read GetPicture write SetPicture;
  end;

implementation

{$R *.fmx}

function TedMapLevel.GetLevel: Integer;
begin
  Result := Trunc(edLevel.Value);
end;

procedure TedMapLevel.SetLevel(const Value: Integer);
begin
  edLevel.Value := Value;
end;

function TedMapLevel.GetLevelName: string;
begin
  Result := edLevelName.Text;
end;

procedure TedMapLevel.SetLevelName(const Value: string);
begin
  edLevelName.Text := Value;
end;

function TedMapLevel.GetPicture: TBitmap;
begin
  Result := edPicture.Bitmap;
end;

procedure TedMapLevel.SetPicture(const Value: TBitmap);
begin
  edPicture.Bitmap.Assign(Value);
end;

procedure TedMapLevel.SetInstance(const Value: TMapLevel);
begin
  FMapLevel := Value;

  if FMapLevel.IsNewInstance then
    Caption := 'Добавление нового уровня карты'
  else
    Caption := '#' + VarToStr(FMapLevel.ID) + '  Редактирование уровня карты';

  Level := FMapLevel.Level;
  LevelName := FMapLevel.Name;
  Picture := FMapLevel.Picture;
end;

procedure TedMapLevel.PostValues(const Value: TMapLevel);
begin
  Value.Level := Level;
  Value.Name := LevelName;
  Value.Picture := Picture;
end;

end.
