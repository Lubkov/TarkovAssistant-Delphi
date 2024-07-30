unit ME.Edit.Quest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.Actions, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.ActnList, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox,
  FMX.Layouts, ME.Edit.Form, ME.Edit.Form.Presenter, ME.DB.Quest;

type
  TedQuest = class(TEditForm, IEditDialog<TDBQuest>)
    MainLayout: TLayout;
    laQuestName: TLabel;
    edQuestName: TEdit;
    laTraderName: TLabel;
    edTraderName: TComboBox;
  private
    FQuest: TDBQuest;

    function GetQuestName: string;
    procedure SetQuestName(const Value: string);
    function GetTraderName: TTrader;
    procedure SetTraderName(const Value: TTrader);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TDBQuest);
    procedure PostValues(const Value: TDBQuest);

    property QuestName: string read GetQuestName write SetQuestName;
    property TraderName: TTrader read GetTraderName write SetTraderName;
  end;

implementation

{$R *.fmx}

{ TedQuest }

constructor TedQuest.Create(AOwner: TComponent);
var
  Trader: TTrader;
begin
  inherited;

  FQuest := nil;

  edTraderName.Clear;
  for Trader := TTrader.Prapor to TTrader.Lightkeeper do
    edTraderName.Items.Add(TDBQuest.TraderToStr(Trader));
end;

destructor TedQuest.Destroy;
begin
  FQuest := nil;

  inherited;
end;

function TedQuest.GetQuestName: string;
begin
  Result := edQuestName.Text;
end;

procedure TedQuest.SetQuestName(const Value: string);
begin
  edQuestName.Text := Value;
end;

function TedQuest.GetTraderName: TTrader;
begin
  if edTraderName.ItemIndex >= 0 then
    Result := TTrader(edTraderName.ItemIndex + 1)
  else
    Result := TTrader.None;
end;

procedure TedQuest.SetTraderName(const Value: TTrader);
begin
  edTraderName.ItemIndex := Ord(Value) - 1;
end;

procedure TedQuest.SetInstance(const Value: TDBQuest);
begin
  FQuest := Value;

  if FQuest.IsNewInstance then
    Caption := 'Добавление нового квеста'
  else
    Caption := 'Редактирование квеста';

  QuestName := FQuest.Name;
  TraderName := FQuest.Trader;
end;

procedure TedQuest.PostValues(const Value: TDBQuest);
begin
  Value.Name := QuestName;
  Value.Trader := TraderName;
end;

end.
