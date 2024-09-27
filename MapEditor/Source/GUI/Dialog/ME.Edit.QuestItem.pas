unit ME.Edit.QuestItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.DB.QuestItem, ME.Edit.Form.Presenter, ME.Grid.QuestResources;

type
  TedQuestItem = class(TEditForm, IEditDialog<TDBQuestItem>)
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
  private
    FQuestItem: TDBQuestItem;
    FResourcesGrid: TQuestResourcesGrid;
  protected
    function IsSuccessActionEnable: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TDBQuestItem);
    procedure PostValues(const Value: TDBQuestItem);
  end;

implementation

uses
  ME.DB.Utils;

{$R *.fmx}

{ TedQuestItem }

constructor TedQuestItem.Create(AOwner: TComponent);
begin
  inherited;

  FQuestItem := nil;

  FResourcesGrid := TQuestResourcesGrid.Create(Self);
  FResourcesGrid.Parent := Self;
  FResourcesGrid.Align := TAlignLayout.Client;
  FResourcesGrid.Sorted := True;
end;

procedure TedQuestItem.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  inherited;

  if (Key = vkF) and (TShiftStateItem.ssCtrl in Shift) then
    if FResourcesGrid.ShowFilter then
      FResourcesGrid.edFilterText.SetFocus;
end;

function TedQuestItem.IsSuccessActionEnable: Boolean;
begin
  Result := not IsNullID(FResourcesGrid.ResourceID);
end;

procedure TedQuestItem.SetInstance(const Value: TDBQuestItem);
begin
  FQuestItem := Value;

  FResourcesGrid.Init(nil);
  FResourcesGrid.SetPosition(FQuestItem.ResourceID);
end;

procedure TedQuestItem.PostValues(const Value: TDBQuestItem);
begin
  Value.ResourceID := FResourcesGrid.ResourceID;
end;

end.
