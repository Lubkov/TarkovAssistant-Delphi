unit ME.MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Objects,
  ME.Frame.LocalMap, ME.Frame.MapLevel, ME.LocalMap;

type
  TMainForm = class(TForm)
    paLocalMap: TPanel;
    paMapLevel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLocalMapPanel: TfrLocalMap;
    FMapLevelPanel: TfrMapLevel;

    procedure OnMapChanged(const LocalMap: TLocalMap);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  ME.AppService, ME.MapLevelService, ME.LocalMapService;

{$R *.fmx}

// id, icon, name, выходы чвк(кол-во), выходы дикого(кол-во), выходы совместные(кол-во), квесты (кол-во)

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AppService.Connect;
  Self.Caption := '[Maps Editor] Database = "' + AppService.Database + '"';

  FLocalMapPanel := TfrLocalMap.Create(Self);
  FLocalMapPanel.Parent := paLocalMap;
  FLocalMapPanel.Align := TAlignLayout.Client;
  FLocalMapPanel.OnChange := OnMapChanged;

  FMapLevelPanel := TfrMapLevel.Create(Self);
  FMapLevelPanel.Parent := paMapLevel;
  FMapLevelPanel.Align := TAlignLayout.Client;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FLocalMapPanel.Init;
end;

procedure TMainForm.OnMapChanged(const LocalMap: TLocalMap);
begin
  if LocalMap <> nil then
    LocalMapService.LoadMapLevels(LocalMap, True);

  FMapLevelPanel.Init(LocalMap);
end;

end.
