unit ME.Edit.Profile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.Edit.Form.Presenter, ME.Profile, FMX.Edit, FMX.ListBox;

type
  TedProfile = class(TEditForm, IEditDialog<TProfile>)
    laProfileName: TLabel;
    edProfileName: TEdit;
    edPMCKind: TComboBox;
  private
    FProfile: TProfile;

    function GetProfileName: string;
    procedure SetProfileName(const Value: string);
    function GetPMCType: TPMCType;
    procedure SetPMCType(const Value: TPMCType);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TProfile);
    procedure PostValues(const Value: TProfile);

    property ProfileName: string read GetProfileName write SetProfileName;
    property PMCType: TPMCType read GetPMCType write SetPMCType;
  end;

implementation

{$R *.fmx}

const
  BearPMCIndex = 0;
  UsecPMCIndex = 1;

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

function TedProfile.GetPMCType: TPMCType;
begin
  case edPMCKind.ItemIndex of
    UsecPMCIndex:
      Result := TPMCType.pmcUsec;
  else
    Result := TPMCType.pmcBear;
  end;
end;

procedure TedProfile.SetPMCType(const Value: TPMCType);
begin
  edPMCKind.ItemIndex := Ord(Value);
end;

procedure TedProfile.SetInstance(const Value: TProfile);
begin
  FProfile := Value;

  if FProfile.IsNewInstance then
    Caption := 'Добавление нового профиля пользователя'
  else
    Caption := 'Редактирование профиля пользователя';

  ProfileName := FProfile.Name;
  PMCType := FProfile.Kind;

  edProfileName.ReadOnly := not FProfile.IsNewInstance;
end;

procedure TedProfile.PostValues(const Value: TProfile);
begin
  Value.Name := ProfileName;
  FProfile.Kind := PMCType;
end;

end.
