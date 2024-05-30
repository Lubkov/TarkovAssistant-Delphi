unit ME.MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Objects, ME.Frame.Map, ME.Frame.MapLevel, ME.DB.Map;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMapPanel: TfrMap;

    procedure OnMapChanged(const Map: TMap);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  ME.AppService, ME.MapLevelService, ME.Service.Map;

{$R *.fmx}

// id, icon, name, выходы чвк(кол-во), выходы дикого(кол-во), выходы совместные(кол-во), квесты (кол-во)

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AppService.Connect;
  Self.Caption := '[Maps Editor] Database = "' + AppService.Database + '"';

  FMapPanel := TfrMap.Create(Self);
  FMapPanel.Parent := Self;
  FMapPanel.Align := TAlignLayout.Client;
  FMapPanel.OnChange := OnMapChanged;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FMapPanel.Init;
end;

procedure TMainForm.OnMapChanged(const Map: TMap);
begin

end;

end.
