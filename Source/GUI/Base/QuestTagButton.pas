unit QuestTagButton;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.UITypes, Vcl.ExtCtrls, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

type
  TQuestTagButton = class(TPanel)
  private
    FEnabled: Boolean;
  protected
    function GetEnabled: Boolean; reintroduce;
    procedure SetEnabled(Value: Boolean); reintroduce;
  public
    constructor Create(AOwner: TComponent); override;

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

{ TQuestTagButton }

constructor TQuestTagButton.Create(AOwner: TComponent);
begin
  inherited;

  FEnabled := True;
  Width := 150;
  Height := 24;
  Caption := '';
  BevelOuter := bvNone;
//  BevelKind := bkTile;
  Cursor := crHandPoint;
  Alignment := taLeftJustify;
//  AlignWithMargins := True;
//  Margins.SetControlBounds(5, 0, 5, 0);
end;

function TQuestTagButton.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TQuestTagButton.SetEnabled(Value: Boolean);
begin
  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  if FEnabled then begin
    Font.Style := Font.Style + [fsBold];
    Font.Color := clGreen;
  end
  else begin
    Font.Style := Font.Style - [fsBold];
    Font.Color := clWhite;
  end;
end;

end.
