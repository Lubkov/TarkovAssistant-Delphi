unit ME.Edit.Quest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.Edit,
  ME.Edit.Form, ME.Edit.Form.Presenter, ME.DB.Quest;

type
  TedQuest = class(TEditForm, IEditDialog<TQuest>)
    edQuestName: TEdit;
  private
    FQuest: TQuest;

    function GetQuestName: string;
    procedure SetQuestName(const Value: string);
  public
    procedure SetInstance(const Value: TQuest);
    procedure PostValues(const Value: TQuest);

    property QuestName: string read GetQuestName write SetQuestName;
  end;

implementation

{$R *.fmx}

{ TedQuest }

function TedQuest.GetQuestName: string;
begin
  Result := edQuestName.Text;
end;

procedure TedQuest.SetQuestName(const Value: string);
begin
  edQuestName.Text := Value;
end;

procedure TedQuest.SetInstance(const Value: TQuest);
begin
  FQuest := Value;

  if FQuest.IsNewInstance then
    Caption := 'Добавление нового квеста'
  else
    Caption := '#' + VarToStr(FQuest.ID) + ' Редактирование квеста';

  QuestName := FQuest.Name;
end;

procedure TedQuest.PostValues(const Value: TQuest);
begin
  Value.Name := QuestName;
end;

end.
