unit fruTagFilter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, MapTagButton, QuestTagButton, LocalMap,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, dxSkinsCore,
  dxSkinOffice2013White, cxButtons, cxImageList, dxSkinOffice2019Colorful,
  dxSkinOffice2016Dark;

type
  TfrTagFilter = class(TFrame)
    ImageList: TImageList;
    laExtractions: TLabel;
    ImageList1: TImageList;
    cxImageList1: TcxImageList;
    buSelectAll: TcxButton;
    buDisableAll: TcxButton;
    cxButton1: TcxButton;
    ButtonImages: TImageList;
    laQuests: TLabel;

    procedure puCloseClick(Sender: TObject);
    procedure buSelectAllClick(Sender: TObject);
    procedure buDisableAllClick(Sender: TObject);
  private
    FMapTags: TList<TMapTagButton>;
    FQuestTags: TList<TQuestTagButton>;
    FOnClose: TNotifyEvent;
    FOnChange: TNotifyEvent;

    procedure OnMapTagButtonClick(Sender: TObject);
    procedure OnQuestTagButtonClick(Sender: TObject);
    procedure SelectAllTags(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadQuests(LocalMap: TLocalMap);
    function GetMaxHeight: Integer;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

constructor TfrTagFilter.Create(AOwner: TComponent);
var
  TagButton: TMapTagButton;
  MapTagType: TMapTagType;
  Index: Integer;
begin
  inherited;

  FOnChange := nil;

  FMapTags := TList<TMapTagButton>.Create;
  for MapTagType := tagPMCExtraction to tagSharedExtraction do begin
    Index := Ord(MapTagType);

    TagButton := TMapTagButton.Create(Self);
    FMapTags.Add(TagButton);
    TagButton.Parent := Self;
    TagButton.Top := 130 + 30 * Index;
    TagButton.Left := 30;
    TagButton.Width := 120;
    TagButton.Images := ImageList;
    TagButton.ImageIndex := Index * 2;
    TagButton.DisableImageIndex := Index * 2 + 1;
    TagButton.Font.Color := clWhite;
    TagButton.Caption := MapTypeTitle[MapTagType];
    TagButton.Enabled := True;
    TagButton.OnClick := OnMapTagButtonClick;
    TagButton.Tag := Index;
  end;

  laQuests.Top := 140 + 30 * Ord(tagQuest);
  FQuestTags := TList<TQuestTagButton>.Create;
end;

destructor TfrTagFilter.Destroy;
begin
  FMapTags.Free;
  FQuestTags.Free;

  inherited;
end;

procedure TfrTagFilter.puCloseClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TfrTagFilter.buSelectAllClick(Sender: TObject);
begin
  SelectAllTags(True);
end;

procedure TfrTagFilter.buDisableAllClick(Sender: TObject);
begin
  SelectAllTags(False);
end;

procedure TfrTagFilter.OnMapTagButtonClick(Sender: TObject);
var
  Control: TControl;
begin
  if not (Sender is TControl) then
    Exit;

  Control := TControl(Sender);
  Control.Enabled := not Control.Enabled;
  if Control.Enabled then
    MapFilter := MapFilter + [TMapTagType(Control.Tag)]
  else
    MapFilter := MapFilter - [TMapTagType(Control.Tag)];

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TfrTagFilter.OnQuestTagButtonClick(Sender: TObject);
var
  Control: TQuestTagButton;
begin
  if not (Sender is TQuestTagButton) then
    Exit;

  Control := TQuestTagButton(Sender);
  Control.Enabled := not Control.Enabled;
  QuestFilter[Control.Tag] := Control.Enabled;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TfrTagFilter.SelectAllTags(const Value: Boolean);
var
  TagButton: TMapTagButton;
begin
  if Value then
    MapFilter := [tagPMCExtraction, tagScavExtraction, tagSharedExtraction]
  else
    MapFilter := [];

  for TagButton in FMapTags do
    TagButton.Enabled := Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TfrTagFilter.LoadQuests(LocalMap: TLocalMap);
var
  QuestButton: TQuestTagButton;
  Quest: TQuestTag;
  Count: Integer;
  i: Integer;
begin
  for i := 0 to FQuestTags.Count - 1 do
    FQuestTags[i].Free;
  FQuestTags.Clear;

  Count := Length(LocalMap.Quests);
  for i := 0 to Count - 1 do begin
    Quest := LocalMap.Quests[i];

    QuestButton := TQuestTagButton.Create(Self);
    FQuestTags.Add(QuestButton);
    QuestButton.Parent := Self;
    QuestButton.Top := laQuests.Top + 25 * (i + 1);
    QuestButton.Left := 40;
    QuestButton.Width := ClientWidth - QuestButton.Left - 20;
    QuestButton.Font.Color := clWhite;
    QuestButton.Caption := Quest.Caption;
    QuestButton.Enabled := False;
    QuestButton.OnClick := OnQuestTagButtonClick;
    QuestButton.Tag := i;
  end;

  laQuests.Caption := ' весты (' + IntToStr(Count) + ')';
  laQuests.Visible := Count > 0;
end;

function TfrTagFilter.GetMaxHeight: Integer;
begin
  Result := 320 + FQuestTags.Count * 30;
end;

end.
