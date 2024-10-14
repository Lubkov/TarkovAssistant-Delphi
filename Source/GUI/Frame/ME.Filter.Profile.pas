unit ME.Filter.Profile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.ImageList, System.Actions, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ListBox,
  FMX.Layouts, FMX.ImgList, FMX.ActnList, ME.Profile;

type
  TProfileFilter = class(TFrame)
    laProfileName: TLabel;
    edProfileName: TComboBox;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAddProfile: TAction;
    acEditProfile: TAction;
    acDeleteProfile: TAction;
    MainLayout: TLayout;
    ToolLayout: TLayout;
    edEditProfile: TSpeedButton;
    ProfileNameLayout: TLayout;
    ButtonLayout: TLayout;
    edAddProfile: TSpeedButton;
    edDelProfile: TSpeedButton;
//    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
    procedure acAddProfileExecute(Sender: TObject);
    procedure acDeleteProfileExecute(Sender: TObject);
    procedure acEditProfileExecute(Sender: TObject);
  private
//    FOnMapChanged: TDBMapChangedEvent;

    function GetProfileID: Variant;
    procedure SetProfileID(const Value: Variant);
    function GetProfileName: string;
    procedure SetProfileName(const Value: string);

    function InternalProfileEdit(const Profile: TProfile): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property ProfileID: Variant read GetProfileID write SetProfileID;
    property ProfileName: string read GetProfileName write SetProfileName;
//    property OnMapChanged: TDBMapChangedEvent read FOnMapChanged write FOnMapChanged;
  end;

implementation

uses
  App.Service, ME.DB.Utils, ME.Service.Profile, ME.Presenter.Profile, ME.Edit.Profile,
  ME.Dialog.Message;

{$R *.fmx}

{ TProfileFilter }

constructor TProfileFilter.Create(AOwner: TComponent);
begin
  inherited;

//  FOnMapChanged := nil;
end;

destructor TProfileFilter.Destroy;
begin
//  FOnMapChanged := nil;

  inherited;
end;

//procedure TProfileFilter.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
//begin
//  if Assigned(FOnMapChanged) then
//    FOnMapChanged(FID.Value);
//end;

function TProfileFilter.GetProfileID: Variant;
begin
//  if edProfileName.ItemIndex >= 0 then
//    Result := FID.Value
//  else
//    Result := Null;
end;

procedure TProfileFilter.SetProfileID(const Value: Variant);
begin
//  if F.Active then
//    F.Locate('ID', Value, []);
end;

function TProfileFilter.GetProfileName: string;
begin
  Result := edProfileName.Text;
end;

procedure TProfileFilter.SetProfileName(const Value: string);
begin
  edProfileName.ItemIndex := edProfileName.Items.IndexOf(Value);
end;

function TProfileFilter.InternalProfileEdit(const Profile: TProfile): Boolean;
var
  Presenter: TEditProfilePresenter;
  Dialog: TedProfile;
begin
  Dialog := TedProfile.Create(Self);
  try
    Presenter := TEditProfilePresenter.Create(Dialog, Profile);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TProfileFilter.Init;
begin
  ProfileService.GetAll(edProfileName.Items);
  edProfileName.ItemIndex := -1;
end;

procedure TProfileFilter.acAddProfileExecute(Sender: TObject);
var
  Profile: TProfile;
  Res: Boolean;
begin
  Profile := TProfile.Create;
  try
    Res := InternalProfileEdit(Profile);
    if not Res then
      Exit;

    ProfileService.Save(Profile);
    edProfileName.Items.Add(Profile.Name);
    edProfileName.ItemIndex := edProfileName.Count - 1;
  finally
    Profile.Free;
  end;
end;

procedure TProfileFilter.acEditProfileExecute(Sender: TObject);
//var
//  Profile: TProfile;
begin
//  Profile := TProfile.Create;
//  try
//    if not ProfileService.GetAt(FID.Value, Profile) then
//      Exit;
//
//    if InternalProfileEdit(Profile) then
//      F.RefreshRecord;
//  finally
//    Profile.Free;
//  end;
end;

procedure TProfileFilter.acDeleteProfileExecute(Sender: TObject);
//var
//  Profile: TProfile;
//  Presenter: TDelProfilePresenter;
//  Dialog: TedMessage;
begin
//  if IsNullID(FID.Value) then
//    Exit;
//
//  Profile := TProfile.Create;
//  try
//    Profile.ID := FID.Value;
//    Profile.Name := FProfileName.AsString;
//    Profile.Kind := TPMCType(FKind.AsInteger);
//
//    Dialog := TedMessage.Create(Self);
//    try
//      Presenter := TDelProfilePresenter.Create(Dialog, Profile);
//      try
//        if not Presenter.Delete then
//          Exit;
//
//        F.DisableControls;
//        try
//          F.Refresh;
//        finally
//          F.EnableControls;
//        end;
//
//        edProfileName.ItemIndex := -1;
//      finally
//        Presenter.Free;
//      end;
//    finally
//      Dialog.Free;
//    end;
//  finally
//    Profile.Free;
//  end;
end;

end.
