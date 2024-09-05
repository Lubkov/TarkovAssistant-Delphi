unit ME.Grid.QuestItems;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Grid.Resources, Data.DB, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, MemDS, DBAccess, Uni,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, FMX.ScrollBox,
  FMX.Grid, FMX.Controls.Presentation, ME.DB.Resource, ME.DB.Marker;

type
  TQuestItemsGrid = class(TResourcesDBGrid)
  private
  protected
    function GetResourceKind: TResourceKind; override;
  public
    procedure Init(const Marker: TDBMarker); override;
  end;

implementation

{$R *.fmx}

{ TQuestItemsGrid }

function TQuestItemsGrid.GetResourceKind: TResourceKind;
begin
  Result := TResourceKind.QuestItem;
end;

procedure TQuestItemsGrid.Init(const Marker: TDBMarker);
begin
  inherited;

  laTitle.Text := 'Список квестовых предметов';
  PicturePanel.Resizing := True;
end;

end.
