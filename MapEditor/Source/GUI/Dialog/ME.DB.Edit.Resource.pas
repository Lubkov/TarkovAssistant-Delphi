unit ME.DB.Edit.Resource;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  FMX.Edit, FMX.Layouts, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  ME.DB.Resource, ME.Frame.Picture, ME.Edit.Form.Presenter;

type
  TedDBResource = class(TEditForm, IEditDialog<TDBResource>)
    ImageDescription: TLayout;
    TopLayout: TLayout;
    edDescription: TMemo;
    laDescription: TLabel;
  private
    FResource: TDBResource;
    FPicturePanel: TfrPicture;

    function GetDescription: string;
    function GetPicture: TBitmap;
    procedure SetDescription(const Value: string);
    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInstance(const Value: TDBResource);
    procedure PostValues(const Value: TDBResource);

    property Description: string read GetDescription write SetDescription;
    property Picture: TBitmap read GetPicture write SetPicture;
  end;

implementation

{$R *.fmx}

{ TedMarkerImage }

constructor TedDBResource.Create(AOwner: TComponent);
begin
  inherited;

  FPicturePanel := TfrPicture.Create(Self);
  FPicturePanel.Parent := ImageDescription;
  FPicturePanel.Align := TAlignLayout.Client;
end;

function TedDBResource.GetDescription: string;
begin
  Result := edDescription.Text;
end;

procedure TedDBResource.SetDescription(const Value: string);
begin
  edDescription.Text := Value;
end;

function TedDBResource.GetPicture: TBitmap;
begin
  Result := FPicturePanel.Picture;
end;

procedure TedDBResource.SetPicture(const Value: TBitmap);
begin
  FPicturePanel.Picture := Value;
end;

procedure TedDBResource.SetInstance(const Value: TDBResource);
begin
  FResource := Value;

  if FResource.IsNewInstance then
    Caption := ''
  else
    Caption := '';

  Description := FResource.Description;
  Picture := FResource.Picture;
end;

procedure TedDBResource.PostValues(const Value: TDBResource);
begin
  Value.Description := Description;

  if FPicturePanel.Changed then
    Value.Picture := Picture;
end;

end.
