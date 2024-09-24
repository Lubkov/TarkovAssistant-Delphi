unit ME.Edit.Profile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.Edit.Form.Presenter, ME.DB.Profile, FMX.Edit;

type
  TedProfile = class(TEditForm, IEditDialog<TProfile>)
    laProfileName: TLabel;
    edProfileName: TEdit;
  private
    FProfile: TProfile;

    function GetProfileName: string;
    procedure SetProfileName(const Value: string);
    function GetProfileKind: TPMCType;
    procedure SetProfileKind(const Value: TPMCType);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TProfile);
    procedure PostValues(const Value: TProfile);

    property ProfileName: string read GetProfileName write SetProfileName;
    property Kind: TPMCType read GetProfileKind write SetProfileKind;
  end;

implementation

{$R *.fmx}

{ TEditForm1 }

constructor TedProfile.Create(AOwner: TComponent);
begin
  inherited;

end;

function TedProfile.GetProfileName: string;
begin
  Result := edProfileName.Text;
end;

procedure TedProfile.SetProfileName(const Value: string);
begin
  edProfileName.Text := Value;
end;

function TedProfile.GetProfileKind: TPMCType;
begin

end;

procedure TedProfile.SetProfileKind(const Value: TPMCType);
begin

end;

procedure TedProfile.SetInstance(const Value: TProfile);
begin
  FProfile := Value;

  if FProfile.IsNewInstance then
    Caption := 'Добавление нового профиля пользователя'
  else
    Caption := 'Редактирование профиля пользователя';

  ProfileName := FProfile.Name;
end;

procedure TedProfile.PostValues(const Value: TProfile);
begin
  Value.Name := ProfileName;
end;

end.
