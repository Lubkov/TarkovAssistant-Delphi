unit ME.Form.Filter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  System.Actions, FMX.ActnList;

type
  TFormFilter = class(TFrame)
    ImageList1: TImageList;
    MainLayout: TLayout;
    ToolLayout: TLayout;
    ButtonLayout: TLayout;
    edClearFilter: TSpeedButton;
    ActionList1: TActionList;
    acClearFilter: TAction;
    procedure acClearFilterExecute(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FClearFilter: TAction;

    procedure ClearFilterUpdate(Sender: TObject);
  protected
    function GetKeyValue: Variant; virtual; abstract;
    procedure SetKeyValue(const Value: Variant); virtual; abstract;
    procedure DoChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init; virtual; abstract;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property ClearFilter: TAction read FClearFilter;
  end;

implementation

{$R *.fmx}

{ TFormFilter }

constructor TFormFilter.Create(AOwner: TComponent);
begin
  inherited;

  FOnChange := nil;

  FClearFilter := TAction.Create(Self);
  FClearFilter.Caption := '';
  FClearFilter.ImageIndex := 0;
  FClearFilter.OnExecute := acClearFilterExecute;
  FClearFilter.ActionList := ActionList1;
  FClearFilter.OnUpdate := ClearFilterUpdate;

  edClearFilter.Action := FClearFilter;
end;

destructor TFormFilter.Destroy;
begin
  FOnChange := nil;

  inherited;
end;

procedure TFormFilter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFormFilter.acClearFilterExecute(Sender: TObject);
begin
  KeyValue := Null;
end;

procedure TFormFilter.ClearFilterUpdate(Sender: TObject);
begin
  ToolLayout.Visible := TAction(Sender).Visible;
end;

end.
