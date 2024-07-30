
unit ME.Edit.Marker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  FMX.Edit, FMX.ListBox, FMX.EditBox, FMX.NumberBox, ME.Edit.Form.Presenter,
  FMX.Layouts, System.ImageList, FMX.ImgList, FMX.Platform, ME.DB.Marker,
  FMX.TabControl, ME.DB.Resource, ME.Grid.Resources, ME.MemGrid.Resources;

type
  TedMarker = class(TEditForm, IEditDialog<TDBMarker>)
    edMarkerCaption: TEdit;
    laMarkerName: TLabel;
    ImageList24: TImageList;
    laScreenShotName: TLabel;
    MainContainer: TTabControl;
    tabGeneral: TTabItem;
    laKindName: TLabel;
    edKindName: TComboBox;
    Layout3: TLayout;
    Layout4: TLayout;
    VerticalLayout: TLayout;
    buTop: TSpeedButton;
    buBottom: TSpeedButton;
    HorizontalLayout: TLayout;
    edLeft: TSpeedButton;
    buRight: TSpeedButton;
    edIncrement: TComboBox;
    buGenerate: TButton;
    Layout1: TLayout;
    PositionLayout: TLayout;
    laTopPoint: TLabel;
    Layout2: TLayout;
    edPositionX: TNumberBox;
    edPositionY: TNumberBox;
    tabMarkerImages: TTabItem;
    procedure edLeftClick(Sender: TObject);
    procedure buRightClick(Sender: TObject);
    procedure buTopClick(Sender: TObject);
    procedure buBottomClick(Sender: TObject);
    procedure buGenerateClick(Sender: TObject);
  private
    FMarker: TDBMarker;
    FResourcesGrid: TDBResourcesGrid;

    function GetMarkerCaption: string;
    procedure SetMarkerCaption(const Value: string);
    function GetMarkerKind: TMarkerKind;
    procedure SetMarkerKind(const Value: TMarkerKind);
    function GetPositionX: Integer;
    procedure SetPositionX(const Value: Integer);
    function GetPositionY: Integer;
    procedure SetPositionY(const Value: Integer);
    function GetIncrement: Integer;
  protected
    function GetTitle(const Value: TDBMarker): string; virtual;
    procedure InternalSetInstance(const Value: TDBMarker); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TDBMarker);
    procedure PostValues(const Value: TDBMarker);

    property Marker: TDBMarker read FMarker;
    property MarkerCaption: string read GetMarkerCaption write SetMarkerCaption;
    property MarkerKind: TMarkerKind read GetMarkerKind write SetMarkerKind;
    property PositionX: Integer read GetPositionX write SetPositionX;
    property PositionY: Integer read GetPositionY write SetPositionY;
    property Increment: Integer read GetIncrement;
  end;

implementation

{$R *.fmx}

{ TedMarker }

constructor TedMarker.Create(AOwner: TComponent);
var
  Kind: TMarkerKind;
begin
  inherited;

  edKindName.Clear;
  for Kind := TMarkerKind.PMCExtraction to TMarkerKind.CoopExtraction do
    edKindName.Items.Add(TDBMarker.KindToStr(Kind));

  laScreenShotName.Visible := False;

  FResourcesGrid := nil;
//  FResourcesGrid := TDBResourcesGrid.Create(Self);
//  FResourcesGrid.Name := 'ImagesGrid';
//  FResourcesGrid.Parent := tabMarkerImages;
//  FResourcesGrid.Align := TAlignLayout.Client;

  MainContainer.TabIndex := tabGeneral.Index;
end;

destructor TedMarker.Destroy;
begin

  inherited;
end;

function TedMarker.GetMarkerCaption: string;
begin
  Result := edMarkerCaption.Text;
end;

procedure TedMarker.SetMarkerCaption(const Value: string);
begin
  edMarkerCaption.Text := Value;
end;

function TedMarker.GetMarkerKind: TMarkerKind;
begin
  Result := TMarkerKind(edKindName.ItemIndex);
end;

procedure TedMarker.SetMarkerKind(const Value: TMarkerKind);
begin
  edKindName.ItemIndex := Ord(Value);
end;

function TedMarker.GetPositionX: Integer;
begin
  Result := Trunc(edPositionX.Value);
end;

procedure TedMarker.SetPositionX(const Value: Integer);
begin
  edPositionX.Value := Value;
end;

function TedMarker.GetPositionY: Integer;
begin
  Result := Trunc(edPositionY.Value);
end;

procedure TedMarker.SetPositionY(const Value: Integer);
begin
  edPositionY.Value := Value;
end;

function TedMarker.GetTitle(const Value: TDBMarker): string;
begin
  if Value.IsNewInstance then
    Result := 'Добавление нового выхода с карты'
  else
    Result := 'Редактирование выхода с карты';
end;

procedure TedMarker.InternalSetInstance(const Value: TDBMarker);
begin

end;

function TedMarker.GetIncrement: Integer;
begin
  Result := StrToInt(edIncrement.Items[edIncrement.ItemIndex]);
end;

procedure TedMarker.SetInstance(const Value: TDBMarker);
begin
  FMarker := Value;

  if FMarker.IsNewInstance then
    FResourcesGrid := TResourcesMemGrid.Create(Self)
  else
    FResourcesGrid := TDBResourcesGrid.Create(Self);

  FResourcesGrid.Name := 'ImagesGrid';
  FResourcesGrid.Parent := tabMarkerImages;
  FResourcesGrid.Align := TAlignLayout.Client;

  Caption := GetTitle(Value);
  MarkerCaption := FMarker.Caption;
  MarkerKind := FMarker.Kind;
  PositionX := FMarker.Left;
  PositionY := FMarker.Top;

  FResourcesGrid.Init(FMarker, TResourceKind.Screenshot);

  InternalSetInstance(Value);
end;

procedure TedMarker.PostValues(const Value: TDBMarker);
begin
  FMarker.Caption := MarkerCaption;
  Value.Kind := MarkerKind;
  Value.Left := PositionX;
  Value.Top := PositionY;
end;

procedure TedMarker.edLeftClick(Sender: TObject);
begin
  PositionX := PositionX + Increment;
end;

procedure TedMarker.buRightClick(Sender: TObject);
begin
  PositionX := PositionX - Increment;
end;

procedure TedMarker.buTopClick(Sender: TObject);
begin
  PositionY := PositionY - Increment;
end;

procedure TedMarker.buBottomClick(Sender: TObject);
begin
  PositionY := PositionY + Increment;
end;

procedure TedMarker.buGenerateClick(Sender: TObject);
const
//  FileNameFmt = '2024-02-20[21-57]_X.x, 0.0, Y.y_0.0, 0.0, 0.0, 0.0_0.00 (0).png';
  FileNameFmt = '%s_%d.0, 0.0, %d.0_0.0, 0.0, 0.0, 0.0_0.00 (0).png';
var
  x, y: Integer;
  FileName: string;
  clp: IFMXClipboardService;
begin
  x := Trunc(edPositionX.Value);
  y := Trunc(edPositionY.Value);
  FileName := Format(FileNameFmt, [FormatDateTime('yyyy-mm-dd[hh-nn]', Now), x, y]);
  laScreenShotName.Text := FileName;
  laScreenShotName.Visible := True;

  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService) then begin
    clp := IFMXClipboardService(TPlatformServices.Current.GetPlatformService(IFMXClipboardService));
    clp.SetClipboard(FileName);
  end;
end;

end.
