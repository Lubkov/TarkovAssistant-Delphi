unit ME.Grid.QuestResources;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Grid.Screenshots, Data.DB, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, MemDS, DBAccess, Uni,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, FMX.ScrollBox,
  FMX.Grid, FMX.Controls.Presentation, ME.DB.Resource, ME.DB.Marker,
  ME.DB.Presenter.Resource, FMX.Edit;

type
  TQuestResourcesGrid = class(TScreenshotsGrid)
  private
  protected
    function GetResourceKind: TResourceKind; override;
    function GetEditPresenterClass: TEditResourcePresenterClass; override;
    function GetDelPresenterClass: TDelResourcePresenterClass; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Init(const Marker: TDBMarker); override;
  end;

implementation

{$R *.fmx}

{ TQuestResourcesGrid }

constructor TQuestResourcesGrid.Create(AOwner: TComponent);
begin
  inherited;

  ShowFilter := True;
end;

function TQuestResourcesGrid.GetResourceKind: TResourceKind;
begin
  Result := TResourceKind.QuestItem;
end;

procedure TQuestResourcesGrid.Init(const Marker: TDBMarker);
begin
  inherited;

  laTitle.Text := 'Список квестовых предметов';
  PicturePanel.Resizing := True;
end;

function TQuestResourcesGrid.GetEditPresenterClass: TEditResourcePresenterClass;
begin
  Result := TEditResourcePresenter;
end;

function TQuestResourcesGrid.GetDelPresenterClass: TDelResourcePresenterClass;
begin
  Result := TDelResourcePresenter;
end;

end.
