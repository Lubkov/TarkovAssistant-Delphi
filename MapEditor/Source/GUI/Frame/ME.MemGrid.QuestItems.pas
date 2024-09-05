unit ME.MemGrid.QuestItems;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Grid.QuestItems, Data.DB, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, MemDS, DBAccess, Uni,
  System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList, FMX.ScrollBox,
  FMX.Grid, FMX.Controls.Presentation;

type
  TQuestItemsMemGrid = class(TQuestItemsGrid)
  private
  public
  end;

implementation

{$R *.fmx}

end.
