unit ME.Edit.QuestPart;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Marker, System.Actions, FMX.ActnList, FMX.ListBox, FMX.EditBox,
  FMX.NumberBox, FMX.Edit, FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.Layouts, ME.DB.Marker, FMX.TabControl, ME.Grid.Resources,
  ME.DB.Resource, ME.Grid.QuestItems, ME.MemGrid.QuestItems;

type
  TedQuestPart = class(TedMarker)
    tabQuestItems: TTabItem;
  private
    FQuestItemsGrid: TQuestItemsGrid;
  protected
    function GetMarkerKind: TMarkerKind; override;
    function GetTitle(const Value: TDBMarker): string; override;
    procedure InternalSetInstance(const Value: TDBMarker); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

{ TedQuestPart }

constructor TedQuestPart.Create(AOwner: TComponent);
begin
  inherited;

  FQuestItemsGrid := nil;

  edKindName.Visible := False;
  laKindName.Visible := False;
end;

function TedQuestPart.GetMarkerKind: TMarkerKind;
begin
  Result := TMarkerKind.Quest;
end;

function TedQuestPart.GetTitle(const Value: TDBMarker): string;
begin
  if Value.IsNewInstance then
    Result := 'Добавление новой подзадачи квеста'
  else
    Result := 'Редактирование подзадачи квеста';
end;

procedure TedQuestPart.InternalSetInstance(const Value: TDBMarker);
begin
  inherited;

  if Assigned(FQuestItemsGrid) then
    FQuestItemsGrid.Free;

  if Marker.IsNewInstance then
    FQuestItemsGrid := TQuestItemsMemGrid.Create(Self)
  else
    FQuestItemsGrid := TQuestItemsGrid.Create(Self);

  FQuestItemsGrid.Name := 'QuestItemsGrid';
  FQuestItemsGrid.Parent := tabQuestItems;
  FQuestItemsGrid.Align := TAlignLayout.Client;
  FQuestItemsGrid.Sorted := True;

  FQuestItemsGrid.Init(Marker);
end;

end.
