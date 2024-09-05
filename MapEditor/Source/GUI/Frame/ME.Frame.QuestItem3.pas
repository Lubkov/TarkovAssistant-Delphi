unit ME.Frame.QuestItem3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Frame.Resource3, System.Rtti, FMX.Grid.Style, System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList, FMX.Grid, FMX.ScrollBox,
  FMX.Controls.Presentation, Map.Data.Types;

type
  TQuestItemsGrid2 = class(TResourcesGrid2)
  private
  protected
    function GetCount: Integer; override;
    function GetResource(Index: Integer): TResource; override;
    function GetResourceClass: TResourceClass; override;
    procedure InternalAddResource(const Resource: TResource); override;
    procedure InternalDeleteResource(const Index: Integer); override;
  public
  end;

implementation

{$R *.fmx}

{ TQuestItemsGrid }

function TQuestItemsGrid2.GetCount: Integer;
begin
  if Marker <> nil then
    Result := Marker.Items.Count
  else
    Result := 0;
end;

function TQuestItemsGrid2.GetResource(Index: Integer): TResource;
begin
  Result := Marker.Items[Index];
end;

function TQuestItemsGrid2.GetResourceClass: TResourceClass;
begin
  Result := TQuestItem;
end;

procedure TQuestItemsGrid2.InternalAddResource(const Resource: TResource);
begin
  Marker.Items.Add(TQuestItem(Resource));
end;

procedure TQuestItemsGrid2.InternalDeleteResource(const Index: Integer);
begin
  Marker.Items.Delete(Index);
end;

end.
