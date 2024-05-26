unit ME.Edit.LocalMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, FMX.EditBox, FMX.NumberBox, FMX.Edit, System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, ME.LocalMap, ME.Dialog.Presenter, ME.Edit.Form.Presenter,
  ME.Frame.Picture, ME.Frame.MapLevel, FMX.TabControl;

type
  TedLocalMap = class(TEditForm, IEditDialog<TLocalMap>)
    edMapName: TEdit;
    edLeft: TNumberBox;
    edTop: TNumberBox;
    edBottom: TNumberBox;
    edRight: TNumberBox;
    paPicture: TPanel;
    laMapName: TLabel;
    laTopPoint: TLabel;
    laBottomPoint: TLabel;
    MainContainer: TTabControl;
    tabGeneral: TTabItem;
    tabMapLevel: TTabItem;
    tabExtractions: TTabItem;
  private
    FLocalMap: TLocalMap;
    FPicturePanel: TfrPicture;
    FMapLevelPanel: TfrMapLevel;

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
    function GetPicture: TBitmap;
    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TLocalMap);
    procedure PostValues(const Value: TLocalMap);

    property MapName: string read GetMapName write SetMapName;
    property MapLeft: Integer read GetMapLeft write SetMapLeft;
    property MapTop: Integer read GetMapTop write SetMapTop;
    property MapRight: Integer read GetMapRight write SetMapRight;
    property MapBottom: Integer read GetMapBottom write SetMapBottom;
    property Picture: TBitmap read GetPicture write SetPicture;
  end;

implementation

{$R *.fmx}

constructor TedLocalMap.Create(AOwner: TComponent);
begin
  inherited;

  MainContainer.TabIndex := tabGeneral.Index;

  FPicturePanel := TfrPicture.Create(Self);
  FPicturePanel.Parent := paPicture;
  FPicturePanel.Align := TAlignLayout.Client;
  FPicturePanel.Title := 'Изображени карты ' + #13#10 + '(для меню)';

  FMapLevelPanel := TfrMapLevel.Create(Self);
  FMapLevelPanel.Parent := tabMapLevel;
  FMapLevelPanel.Align := TAlignLayout.Client;
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

function TedLocalMap.GetPicture: TBitmap;
begin
  Result := FPicturePanel.Picture;
end;

procedure TedLocalMap.SetPicture(const Value: TBitmap);
begin
  FPicturePanel.Picture := Value;
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
  Picture := FLocalMap.Picture;

  FMapLevelPanel.Init(FLocalMap);
end;

procedure TedLocalMap.PostValues(const Value: TLocalMap);
begin
  Value.Name := MapName;
  FLocalMap.Left.SetBounds(MapLeft, MapTop);
  FLocalMap.Right.SetBounds(MapRight, MapBottom);
  Value.Picture := Picture;
end;

end.
