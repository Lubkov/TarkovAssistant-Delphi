unit Map.Frame.Marker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, System.ImageList,
  FMX.ImgList, Map.Data.Types, FMX.ListBox, System.Rtti, ME.GUI.PictureList,
  ME.DB.Marker, ME.DB.Quest, ME.DB.Resource, ME.DB.QuestTracker;

type
  TMarkerDescript = class(TFrame)
    TitleLayout: TLayout;
    TraderImage: TImage;
    laQuestName: TLabel;
    laDescription: TLabel;
    MarkerImage: TImage;
    ItemsLayout: TLayout;
    Item1Image: TImage;
    MainLayout: TLayout;
    buClose: TSpeedButton;
    ButtonCloseLayout: TLayout;
    ImageList24: TImageList;
    TraderImageList: TImageList;
    MarkerStyleBook: TStyleBook;
    PreviewLayout: TLayout;
    Background: TRectangle;
    MaouseWheelImage: TImage;
    CompleteQuestLayout: TLayout;
    buCompleteQuest: TSpeedButton;

    procedure buCloseClick(Sender: TObject);
    procedure MarkerImageMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure buCompleteQuestClick(Sender: TObject);
  private
    [Weak] FMarker: TDBMarker;
    FMaxHeight: Single;
    FMaxWidth: Single;
    FOnClose: TNotifyEvent;
    FCurrentImageIndex: Integer;
    FItemIconList: TPictureList;
    FPictureIconList: TPictureList;

    procedure ShowResource(const Index: Integer);
    procedure ChangedPreviewIcon(ItemIndex: Integer);
    procedure LoadQuestItems(const Marker: TDBMarker);
    function InternalQuestTrackerEdit(const QuestTracker: TQuestTracker): Boolean;
    procedure SaveQuestTracker;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Marker: TDBMarker; const QuestName: string; Trader: TTrader);

    property MaxHeight: Single read FMaxHeight;
    property MaxWidth: Single read FMaxWidth;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  ME.Dialog.Message, Map.Data.Service, ME.Presenter.QuestTracker,
  ME.Edit.QuestTracker, ME.Service.QuestTracker;

{$R *.fmx}

// Завершить / Отменить выполнение

{ TMarkerDescript }

constructor TMarkerDescript.Create(AOwner: TComponent);
// #70A700 - focused color
// ##4E7500 - selected color
begin
  inherited;

  FMarker := nil;
  FOnClose := nil;
  Background.Fill.Color := $FF252525; // FFB97A57
  laQuestName.TextSettings.FontColor := $FFFFFFFF;
  laDescription.TextSettings.FontColor := $FFFFFFFF;

  FPictureIconList := TPictureList.Create(Self);
  FPictureIconList.Name := 'PictureIconList';
  FPictureIconList.Parent := PreviewLayout;
  FPictureIconList.Align := TAlignLayout.Client;
  FPictureIconList.ListDirection := TListDirection.Horizontal;
  FPictureIconList.ItemHeight := 52;
  FPictureIconList.ItemWidth := 85;
  FPictureIconList.OnChangeItem := ChangedPreviewIcon;

  FItemIconList := TPictureList.Create(Self);
  FItemIconList.Name := 'PictureList';
  FItemIconList.Parent := ItemsLayout;
  FItemIconList.Align := TAlignLayout.Client;
  FItemIconList.ListDirection := TListDirection.Vertical;
  FItemIconList.ItemWidth := 64;
  FItemIconList.Stretch := True;
  FItemIconList.HideFocus := True;
  FItemIconList.HideSelect := True;
end;

destructor TMarkerDescript.Destroy;
begin
  FOnClose := nil;
  FMarker := nil;

  inherited;
end;

procedure TMarkerDescript.ShowResource(const Index: Integer);
var
  Resource: TDBResource;
begin
  if FCurrentImageIndex = Index then
    Exit;

  FCurrentImageIndex := Index;

  if FMarker.Images.Count > Index then begin
    Resource := FMarker.Images[Index];

    DataService.LoadImage(Resource, MarkerImage.Bitmap);
    laDescription.Text := Resource.Description;
  end
  else begin
    MarkerImage.Bitmap.Assign(nil);
    laDescription.Text := '';
  end;
end;

procedure TMarkerDescript.ChangedPreviewIcon(ItemIndex: Integer);
begin
  ShowResource(ItemIndex);
end;

procedure TMarkerDescript.LoadQuestItems(const Marker: TDBMarker);
var
  QuestItem: TDBResource;
  Height: Single;
  Width: Single;
begin
  Height := 0;
  Width := 0;

  FItemIconList.Clear;
  for QuestItem in Marker.Items do begin
    if QuestItem.Picture.IsEmpty then
      DataService.LoadImage(QuestItem, QuestItem.Picture);

    Height := Height + QuestItem.Picture.Height;
    Width := Width + QuestItem.Picture.Width;
  end;

  if Height < Width then begin
    ItemsLayout.Align := TAlignLayout.Bottom;
    ItemsLayout.Height := 64;
    ItemsLayout.Margins.MarginRect(TRectF.Create(0, 5, 0, 0));
    FItemIconList.ListDirection := TListDirection.Horizontal;
  end
  else begin
    ItemsLayout.Align := TAlignLayout.Right;
    ItemsLayout.Width := 64;
    ItemsLayout.Margins.MarginRect(TRectF.Create(5, 0, 0, 0));
    FItemIconList.ListDirection := TListDirection.Vertical;
  end;

  for QuestItem in Marker.Items do
    FItemIconList.Add('' {QuestItem.Description}, QuestItem.Picture);
end;

function TMarkerDescript.InternalQuestTrackerEdit(const QuestTracker: TQuestTracker): Boolean;
var
  Presenter: TEditQuestTrackerPresenter;
  Dialog: TedQuestTracker;
begin
  Dialog := TedQuestTracker.Create(Self);
  try
    Presenter := TEditQuestTrackerPresenter.Create(Dialog, QuestTracker);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TMarkerDescript.SaveQuestTracker;
var
  QuestTracker: TQuestTracker;
begin
  QuestTracker := TQuestTracker.Create;
  try
    QuestTracker.MarkerID := FMarker.ID;
    QuestTracker.QuestID := FMarker.QuestID;
    QuestTracker.Status := TQuestStatus.qsNone;

    if not QuestTrackerService.GetMarkerState(FMarker.ID, QuestTracker) then
      QuestTracker.Status := TQuestStatus.qsFinished
    else
    if QuestTracker.Status = TQuestStatus.qsFinished then
      QuestTracker.Status := TQuestStatus.qsNew
    else
      QuestTracker.Status := TQuestStatus.qsFinished;

    QuestTrackerService.Save(QuestTracker);
  finally
    QuestTracker.Free;
  end;
end;

procedure TMarkerDescript.Init(const Marker: TDBMarker; const QuestName: string; Trader: TTrader);
const
  MarkerImageHeight = 360;
  MarkerImageWidth = 640;
var
  Bitmap: TBitmap;
  Resource: TDBResource;
  ImageHeight: Single;
begin
  FMarker := Marker;
  FCurrentImageIndex := -1;
  laQuestName.Text := 'Квест: "' + QuestName + '"';
  Bitmap := TraderImageList.Bitmap(TSizeF.Create(64, 64), Ord(Trader));
  TraderImage.Bitmap.Assign(Bitmap);
  ShowResource(0);

  if MarkerImage.Bitmap.IsEmpty then
    ImageHeight := MarkerImageHeight
  else
    ImageHeight := MarkerImageWidth * (MarkerImage.Bitmap.Height / MarkerImage.Bitmap.Width);

  FPictureIconList.Clear;
  if Marker.Images.Count > 1 then
    for Resource in Marker.Images do begin
      if Resource.Picture.IsEmpty then
        DataService.LoadImage(Resource, Resource.Picture);

      FPictureIconList.Add(Resource.Picture);
    end;

  if Marker.Images.Count > 1 then
    FPictureIconList.SelectedIndex := 0;

  PreviewLayout.Visible := Marker.Images.Count > 1;

  LoadQuestItems(Marker);
  ItemsLayout.Visible := Marker.Items.Count > 0;

  FMaxHeight := TitleLayout.Height + TitleLayout.Margins.Top;
  FMaxHeight := FMaxHeight + ImageHeight + MainLayout.Margins.Top + MainLayout.Margins.Bottom;
  FMaxHeight := FMaxHeight + laDescription.Height + laDescription.Margins.Top;
  FMaxWidth := MarkerImageWidth + MainLayout.Margins.Left + MainLayout.Margins.Right;

  if PreviewLayout.Visible then
    FMaxHeight := FMaxHeight + PreviewLayout.Height + PreviewLayout.Margins.Top + PreviewLayout.Margins.Bottom;

  if ItemsLayout.Visible then
    case FItemIconList.ListDirection of
      TListDirection.Vertical:
        FMaxWidth := FMaxWidth + ItemsLayout.Width + ItemsLayout.Margins.Left + ItemsLayout.Margins.Right;
      TListDirection.Horizontal:
        FMaxHeight := FMaxHeight + ItemsLayout.Height + ItemsLayout.Margins.Top + ItemsLayout.Margins.Bottom;
    end;

  MaouseWheelImage.Visible := FPictureIconList.Count > 1;
  MaouseWheelImage.Position.X := MarkerImage.Width - MaouseWheelImage.Width - 5;
  MaouseWheelImage.Position.Y := MarkerImage.Height - MaouseWheelImage.Height - 5;
end;

procedure TMarkerDescript.buCloseClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TMarkerDescript.MarkerImageMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  FPictureIconList.OnMouseWheel(WheelDelta);
end;

procedure TMarkerDescript.buCompleteQuestClick(Sender: TObject);
begin
  SaveQuestTracker;
end;

end.
