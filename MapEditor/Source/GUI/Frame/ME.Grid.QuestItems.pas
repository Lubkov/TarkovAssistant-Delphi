unit ME.Grid.QuestItems;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Grid.Resources, Data.DB, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, MemDS, DBAccess, Uni,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, FMX.ScrollBox,
  FMX.Grid, FMX.Controls.Presentation, ME.DB.Resource, ME.DB.Marker, ME.DB.QuestItem,
  ME.DB.Presenter.Resource;

type
  TQuestItemsGrid = class(TResourcesDBGrid)
    FMarkerID: TIntegerField;
    FResourceID: TIntegerField;
  private
  protected
    function GetResourceKind: TResourceKind; override;
    function GetCommandSQLText: string; override;
    function GetRefreshSQLText: string; override;
    function GetResourceID: Variant; override;
    function GetEditPresenterClass: TEditResourcePresenterClass; override;
    function GetDelPresenterClass: TDelResourcePresenterClass; override;

    function InternalEditRecord(const QuestItem: TDBQuestItem): Boolean;
    function InternalDeleteRecord: Boolean;
  public
    procedure Init(const Marker: TDBMarker); override;
    procedure AddRecord; override;
    procedure EditRecord; override;
    procedure DeleteRecord; override;
  end;

implementation

uses
  ME.Dialog.Message, ME.Presenter.QuestItem, ME.Edit.QuestItem, ME.Service.QuestItem;

{$R *.fmx}

{ TQuestItemsGrid }

function TQuestItemsGrid.GetResourceKind: TResourceKind;
begin
  Result := TResourceKind.QuestItem;
end;

function TQuestItemsGrid.GetCommandSQLText: string;
begin
  if Marker = nil then
    Result :=
      ' SELECT r.ID as ID, ' +
      '        0 as MarkerID, ' +
      '        r.ID as ResourceID, ' +
      '        r.Kind as Kind, ' +
      '        r.Description as Description ' +
      ' FROM Resource r ' +
      ' WHERE (r.Kind = :Kind) '
  else
    Result :=
      ' SELECT qi.ID as ID, ' +
      '        qi.MarkerID as MarkerID, ' +
      '        r.ID as ResourceID, ' +
      '        r.Kind as Kind, ' +
      '        r.Description as Description ' +
      ' FROM Resource r ' +
      '   INNER JOIN QuestItem qi ON (qi.ResourceID = r.ID) ' +
      '       AND (qi.MarkerID = :MarkerID) ' +
      '       AND (r.Kind = :Kind) ';


//SELECT qi.ID as ID,
//       qi.MarkerID as MarkerID,
//       r.ID as ResourceID,
//       r.Kind as Kind,
//       r.Description as Description
//FROM Resource r
//   INNER JOIN QuestItem qi ON (qi.ResourceID = r.ID)
//       AND (qi.ID = :ID)

//    Result :=
//      ' SELECT r.ID as ID, ' +
//      '        r.Kind as Kind, ' +
//      '        r.Description as Description, ' +
//      '        qi.ID as QuestItemID, ' +
//      '        qi.MarkerID as MarkerID ' +
//      ' FROM Resource r ' +
//      '   INNER JOIN QuestItem qi ON (qi.ResourceID = r.ID) ' +
//      '       AND (qi.MarkerID = :MarkerID) ' +
//      '       AND (r.Kind = :Kind) ';
end;

function TQuestItemsGrid.GetRefreshSQLText: string;
begin
  if Marker = nil then
    Result := ''
  else
    Result :=
      ' SELECT qi.ID as ID, ' +
      '        qi.MarkerID as MarkerID, ' +
      '        r.ID as ResourceID, ' +
      '        r.Kind as Kind, ' +
      '        r.Description as Description ' +
      ' FROM Resource r ' +
      '    INNER JOIN QuestItem qi ON (qi.ResourceID = r.ID) ' +
      '        AND (qi.ID = :ID) ';
end;

function TQuestItemsGrid.GetResourceID: Variant;
begin
  Result := FResourceID.Value;
end;

function TQuestItemsGrid.GetEditPresenterClass: TEditResourcePresenterClass;
begin
  Result := nil;
end;

function TQuestItemsGrid.GetDelPresenterClass: TDelResourcePresenterClass;
begin
  Result := nil;
end;

function TQuestItemsGrid.InternalEditRecord(const QuestItem: TDBQuestItem): Boolean;
var
  Presenter: TEditQuestItemPresenter;
  Dialog: TedQuestItem;
begin
  Dialog := TedQuestItem.Create(Self);
  try
    Presenter := TEditQuestItemPresenter.Create(Dialog, QuestItem);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

function TQuestItemsGrid.InternalDeleteRecord: Boolean;
var
  QuestItem: TDBQuestItem;
  Presenter: TDelQuestItemPresenter;
  Dialog: TedMessage;
begin
  QuestItem := TDBQuestItem.Create;
  try
//    QuestItem.ID := FID.Value;
    QuestItem.MarkerID := Marker.ID;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelQuestItemPresenter.Create(Dialog, QuestItem);
      try
        Result := Presenter.Delete;
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
  finally
    QuestItem.Free;
  end;
end;

procedure TQuestItemsGrid.Init(const Marker: TDBMarker);
begin
  inherited;

  laTitle.Text := 'Список квестовых предметов';
  PicturePanel.Resizing := True;
end;

procedure TQuestItemsGrid.AddRecord;
var
  QuestItem: TDBQuestItem;
begin
  QuestItem := TDBQuestItem.Create;
  try
    QuestItem.MarkerID := Marker.ID;
    if not InternalEditRecord(QuestItem) then
      Exit;

    F.DisableControls;
    try
      F.Refresh;
      F.Last;
    finally
      F.EnableControls;
    end;
  finally
    QuestItem.Free;
  end;
end;

procedure TQuestItemsGrid.EditRecord;
var
  QuestItem: TDBQuestItem;
begin
  QuestItem := TDBQuestItem.Create;
  try
    if not QuestItemService.GetAt(FID.Value, QuestItem) then
      Exit;

    if InternalEditRecord(QuestItem) then
      F.RefreshRecord;
  finally
    QuestItem.Free;
  end;
end;

procedure TQuestItemsGrid.DeleteRecord;
begin

end;

end.
