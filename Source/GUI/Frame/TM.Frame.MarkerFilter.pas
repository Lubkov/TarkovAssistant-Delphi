unit TM.Frame.MarkerFilter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, System.ImageList, FMX.ImgList,
  FMX.ListBox, Map.Data.Types, ME.MarkerFilter, ME.DB.Map, ME.DB.Marker, ME.DB.Quest;

type
  TMarkerFilterList = class(TFrame)
    ImageList24: TImageList;
    laExtractionGroup: TLabel;
    buPMCExtraction: TSpeedButton;
    buScavExtraction: TSpeedButton;
    buCoopExtraction: TSpeedButton;
    StyleBook: TStyleBook;
    paPMCExtraction: TLayout;
    paScavExtraction: TLayout;
    paCoopExtraction: TLayout;
    QuestGrid: TGridLayout;
    laQuestGroup: TLabel;
    QuestContainer: TVertScrollBox;
    buClose: TSpeedButton;
    procedure buCloseClick(Sender: TObject);
  private
    FMarkerFilter: TMarkerFilter;
    FMap: TDBMap;
    FQuests: TList<TSpeedButton>;
    FMaxHeight: Integer;
    FOnClose: TNotifyEvent;

    procedure OnMapChanged(Value: TDBMap);
    procedure OnExtractionButtonClick(Sender: TObject);
    procedure OnQuestButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const MarkerFilter: TMarkerFilter);

    property MaxHeight: Integer read FMaxHeight;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  App.Service;

{$R *.fmx}

constructor TMarkerFilterList.Create(AOwner: TComponent);
begin
  inherited;

  // PMCExtraction
  buPMCExtraction.Tag := Ord(TMarkerKind.PMCExtraction);
  buPMCExtraction.OnClick := OnExtractionButtonClick;
  // ScavExtraction
  buScavExtraction.Tag := Ord(TMarkerKind.ScavExtraction);
  buScavExtraction.OnClick := OnExtractionButtonClick;
  // CoopExtraction
  buCoopExtraction.Tag := Ord(TMarkerKind.CoopExtraction);
  buCoopExtraction.OnClick := OnExtractionButtonClick;

  FQuests := TObjectList<TSpeedButton>.Create;

  laExtractionGroup.FontColor := $FFFFFFFF;
  laQuestGroup.FontColor := $FFFFFFFF;

  FMaxHeight := 0;
  FOnClose := nil;
end;

destructor TMarkerFilterList.Destroy;
begin
  FOnClose := nil;
  FQuests.Free;

  inherited;
end;

procedure TMarkerFilterList.Init(const MarkerFilter: TMarkerFilter);
begin
  FMarkerFilter := MarkerFilter;
  FMarkerFilter.OnMapChanged := OnMapChanged;
end;

procedure TMarkerFilterList.OnMapChanged(Value: TDBMap);
var
  Idx: Integer;
  Quest: TDBQuest;
  Item: TSpeedButton;
  Marker: TDBMarker;
  PMCCount: Integer;
  ScavCount: Integer;
  CoopCount: Integer;
begin
  FMap := Value;
  if FMap = nil then begin
    FQuests.Clear;
    Exit;
  end;

  PMCCount := 0;
  ScavCount := 0;
  CoopCount := 0;
  for Marker in FMap.Markers do
    case Marker.Kind of
      TMarkerKind.PMCExtraction:
        Inc(PMCCount);
      TMarkerKind.ScavExtraction:
        Inc(ScavCount);
      TMarkerKind.CoopExtraction:
        Inc(CoopCount);
    end;

  buPMCExtraction.Text := TDBMarker.KindToStr(TMarkerKind.PMCExtraction) + ' (' + PMCCount.ToString + ')';
  buScavExtraction.Text := TDBMarker.KindToStr(TMarkerKind.ScavExtraction) + ' (' + ScavCount.ToString + ')';
  buCoopExtraction.Text := TDBMarker.KindToStr(TMarkerKind.CoopExtraction) + ' (' + CoopCount.ToString + ')';

  QuestGrid.BeginUpdate;
  try
    FQuests.Clear;
  finally
    QuestGrid.EndUpdate;
  end;

  QuestGrid.BeginUpdate;
  try
    FMarkerFilter.BeginUpdate;
    try
      for Idx := 0 to FMap.Quests.Count - 1 do begin
        Quest := FMap.Quests[Idx];

        Item := TSpeedButton.Create(Self);
        try
          Item.Tag := Idx;
          Item.Parent := QuestGrid;
          Item.Cursor := crHandPoint;
          Item.Text := Quest.Name + ' (' + IntToStr(Quest.Markers.Count) + ')';
          Item.StaysPressed := True;

          if AppService.IsQuestFinished(Quest) then
            Item.StyleLookup :=  'QuestFinishedStyle'
          else
            Item.StyleLookup :=  'FilterItemStyle';

          Item.IsPressed := AppService.GetQuestSelected(Quest);
          Item.TextSettings.HorzAlign := TTextAlign.Leading;
          Item.OnClick := OnQuestButtonClick;

          if Item.IsPressed then
            FMarkerFilter.EnableQuest(Idx)
          else
            FMarkerFilter.DisablQuest(Idx);
        finally
          FQuests.Add(Item);
        end;
      end;

      QuestGrid.Position.X := 0;
      QuestGrid.Position.Y := 0;
      QuestGrid.Width := QuestContainer.Width;
      QuestGrid.Height := QuestGrid.ItemHeight * FMap.Quests.Count;
    finally
      FMarkerFilter.EndUpdate;
    end;
  finally
    QuestGrid.EndUpdate;
  end;

  FMaxHeight := Trunc(QuestContainer.Position.Y + QuestGrid.Height) + 10;
  Self.Height := MaxHeight;
end;

procedure TMarkerFilterList.OnExtractionButtonClick(Sender: TObject);
var
  Item: TSpeedButton;
begin
  Item := TSpeedButton(Sender);

  if Item.IsPressed then
    FMarkerFilter.EnableGroup(TMarkerKind(Item.Tag))
  else
    FMarkerFilter.DisableGroup(TMarkerKind(Item.Tag));
end;

procedure TMarkerFilterList.OnQuestButtonClick(Sender: TObject);
var
  Item: TSpeedButton;
begin
  Item := TSpeedButton(Sender);

  if Item.IsPressed then
    FMarkerFilter.EnableQuest(Item.Tag)
  else
    FMarkerFilter.DisablQuest(Item.Tag);

  AppService.SetQuestSelected(FMap.Quests[Item.Tag], Item.IsPressed);
end;

procedure TMarkerFilterList.buCloseClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

end.



