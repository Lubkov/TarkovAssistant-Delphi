unit ME.MemGrid.QuestItems;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Grid.QuestItems, Data.DB, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, MemDS, DBAccess, Uni,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, FMX.ScrollBox,
  FMX.Grid, FMX.Controls.Presentation, ME.DB.Resource, ME.DB.QuestItem;

type
  TQuestItemsMemGrid = class(TQuestItemsGrid)
  private
  public
    procedure AddRecord; override;
    procedure EditRecord; override;
    procedure DeleteRecord; override;
  end;

implementation

uses
  ME.Service.Resource;

{$R *.fmx}

{ TQuestItemsMemGrid }

procedure TQuestItemsMemGrid.AddRecord;
var
  QuestItem: TDBQuestItem;
  Resource: TDBResource;
begin
  QuestItem := TDBQuestItem.Create;
  try
    if not InternalEditRecord(QuestItem) then
      Exit;

    Resource := TDBResource.Create;
    try
      ResourceService.GetAt(QuestItem.ResourceID, Resource);
      if Resource.IsNewInstance then
        Exit;
    finally
      if not Resource.IsNewInstance then
        Marker.Items.Add(Resource);
    end;

    F.DisableControls;
    try
      F.Append;
//      FID.Value := Null;
//      FMarkerID.Value := Null;
      FResourceID.Value := Resource.ID;
      FKind.AsInteger := Ord(Resource.Kind);
      FDescription.AsString := Resource.Description;
      F.Post;
    finally
      F.EnableControls;
    end;
  finally
    QuestItem.Free;
  end;
end;

procedure TQuestItemsMemGrid.EditRecord;
var
  QuestItem: TDBQuestItem;
  Resource: TDBResource;
begin
  Resource := Marker.Items[Grid.Selected];

  QuestItem := TDBQuestItem.Create;
  try
    QuestItem.ResourceID := Resource.ID;
    if not InternalEditRecord(QuestItem) then
      Exit;

    if not ResourceService.GetAt(QuestItem.ResourceID, Resource) then
      Exit;

    F.DisableControls;
    try
      F.Edit;
//      FID.Value := Null;
//      FMarkerID.Value := Null;
      FResourceID.Value := Resource.ID;
      FKind.AsInteger := Ord(Resource.Kind);
      FDescription.AsString := Resource.Description;
      F.Post;
    finally
      F.EnableControls;
    end;
  finally
    QuestItem.Free;
  end;
end;

procedure TQuestItemsMemGrid.DeleteRecord;
begin
  if (Grid.Row < 0) or not InternalDeleteRecord then
    Exit;
  Marker.Items.Delete(Grid.Selected);

  F.DisableControls;
  try
    F.Delete;
  finally
    F.EnableControls;
  end;
end;

end.
