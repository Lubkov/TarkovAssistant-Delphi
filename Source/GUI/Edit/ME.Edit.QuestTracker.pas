unit ME.Edit.QuestTracker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.Edit.Form.Presenter, ME.DB.QuestTracker;

type
  TedQuestTracker = class(TEditForm, IEditDialog<TQuestTracker>)
    laMessage: TLabel;
  private
    FQuestTracker: TQuestTracker;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TQuestTracker);
    procedure PostValues(const Value: TQuestTracker);
  end;

implementation

{$R *.fmx}

{ TedQuestTracker }

constructor TedQuestTracker.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TedQuestTracker.SetInstance(const Value: TQuestTracker);
begin
  FQuestTracker := Value;

  if Value.Status = TQuestStatus.qsFinished then
    laMessage.Text := 'Отменить выполнение подзадачи квеста?'
  else
    laMessage.Text := 'Отметить выполнение подзадачи квеста?';
end;

procedure TedQuestTracker.PostValues(const Value: TQuestTracker);
begin

end;

end.
