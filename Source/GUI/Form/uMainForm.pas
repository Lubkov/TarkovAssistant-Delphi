unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.IniFiles, System.IOUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Imaging.jpeg, Vcl.Menus, Vcl.Buttons,
  System.ImageList, Vcl.ImgList, MapWrapper, FilesMonitor, LocalMap, SimpleLogger,
  App.Constants, FormWrapper, fruTagFilter, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxButtons, cxImageList, cxControls, cxContainer,
  dxGDIPlusClasses, cxImage, dxSkinOffice2016Dark, cxClasses, dxSkinsForm,
  fruLocation;

type
  TMainForm = class(TForm)
    paTest: TPanel;
    Button1: TButton;
    meLog: TMemo;
    MapTagImages: TImageList;
    MainMenuImages: TImageList;
    imLocalMap: TImage;
    Label1: TLabel;
    Label2: TLabel;
    MainContainer: TPanel;
    buFullScreen: TcxButton;
    ButtonImages: TcxImageList;
    buZoomIn: TcxButton;
    buZoomOut: TcxButton;
    buMapFilters: TcxButton;
    dxSkinController1: TdxSkinController;
    buCentreMap: TcxButton;
    paToolBar: TPanel;
    buLocation: TcxButton;
    ImageList32: TcxImageList;
    buSettings: TcxButton;
    buShowLog: TcxButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MainContainer2MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure MainContainer2MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure MapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure buLocationClick(Sender: TObject);
    procedure buSettingsClick(Sender: TObject);
  private
    FMapWrapper: TMapWrapper;
    FFormWrapper: TFormWrapper;
    FTestFileIndex: Integer;
    FMousePosition: TPoint;
    FTagFilter: TfrTagFilter;
    FLocationPanel: TfrLocation;

    procedure OnMapChange(Bitmap: TBitmap);
    procedure ShowMap(const Map: TLocalMap);
    procedure OnFullScreenButtonClick(Sender: TObject);
    procedure OnZoomInButtonClick(Sender: TObject);
    procedure OnZoomDecButtonClick(Sender: TObject);
    procedure OnMapFiltersButtonClick(Sender: TObject);
    procedure OnMapFiltersCloseClick(Sender: TObject);
    procedure OnCentreMapClick(Sender: TObject);
    procedure OnShowLogClick(Sender: TObject);
    procedure OnChoiceLocation(Value: TLocalMap);

    procedure OnMapFilterChanged(Sender: TObject);
    procedure SetFullScreenMode(const Value: Boolean);
    procedure SetMapControlVisible(const Value: Boolean);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  eduSettings;

{$R *.dfm}

//  Directory = 'c:\Users\Stellar\Documents\Escape from Tarkov\Screenshots\';
// $009C5200

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMousePosition.Empty := True;
  FTestFileIndex := 0;
  FFormWrapper := TFormWrapper.Create(Self);
  AppParams.Load;

  imLocalMap.Left := 0;
  imLocalMap.Top := 0;

  MainContainer.Color := clBlack;
  FMapWrapper := TMapWrapper.Create(AppParams.SreenshotPath);
  FMapWrapper.TrackLocation := AppParams.TrackLocation;
  FMapWrapper.Images := MapTagImages;
  FMapWrapper.OnMapChange := OnMapChange;
  FMapWrapper.TrackLocation := AppParams.TrackLocation;

  FLocationPanel := TfrLocation.Create(Self);
  FLocationPanel.Parent := Self;
  FLocationPanel.OnLocationChanged := OnChoiceLocation;
  FLocationPanel.Visible := False;

  FTagFilter := TfrTagFilter.Create(Self);
  FTagFilter.Parent := MainContainer;
  FTagFilter.Top := buFullScreen.Top;
  FTagFilter.OnClose := OnMapFiltersCloseClick;
  FTagFilter.OnChange := OnMapFilterChanged;
  FTagFilter.BringToFront;
  FTagFilter.Visible := False;

  buFullScreen.OnClick := OnFullScreenButtonClick;
  buZoomIn.OnClick := OnZoomInButtonClick;
  buZoomOut.OnClick := OnZoomDecButtonClick;
  buMapFilters.OnClick := OnMapFiltersButtonClick;
  buCentreMap.OnClick := OnCentreMapClick;
  buShowLog.OnClick := OnShowLogClick;
  SetMapControlVisible(False);

{$IFNDEF DEBUG}
  paTest.Visible := False;
{$ENDIF}
  Logger.Memo := meLog;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FMapWrapper.Free;
  FFormWrapper.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      SetFullScreenMode(False);
    VK_F11:
      SetFullScreenMode(True);
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  buFullScreen.Left := MainContainer.ClientWidth - buFullScreen.Width - 32;
  buZoomIn.Left := MainContainer.ClientWidth - buZoomIn.Width - 32;
  buZoomOut.Left := MainContainer.ClientWidth - buZoomOut.Width - 32;
  buMapFilters.Left := MainContainer.ClientWidth - buMapFilters.Width - 32;
  buCentreMap.Left := MainContainer.ClientWidth - buMapFilters.Width - 32;
  buShowLog.Left := MainContainer.ClientWidth - buMapFilters.Width - 32;

  FTagFilter.Left := buFullScreen.Left - FTagFilter.Width - 5;
end;

procedure TMainForm.OnFullScreenButtonClick(Sender: TObject);
begin
  SetFullScreenMode(not FFormWrapper.FullScreen);
end;

procedure TMainForm.OnZoomInButtonClick(Sender: TObject);
begin
  FMapWrapper.ZoomIn;
end;

procedure TMainForm.OnZoomDecButtonClick(Sender: TObject);
begin
  FMapWrapper.ZoomOut;
end;

procedure TMainForm.OnMapFiltersButtonClick(Sender: TObject);
begin
  FTagFilter.Visible := not FTagFilter.Visible;
//  if FTagFilter.Visible then
//    FTagFilter.LoadQuests(FMapWrapper.Map);

  FTagFilter.Left := buFullScreen.Left - FTagFilter.Width - 5;

  if MainContainer.ClientHeight - 10 < FTagFilter.GetMaxHeight then
    FTagFilter.Height := MainContainer.ClientHeight - 10
  else
    FTagFilter.Height := FTagFilter.GetMaxHeight;
end;

procedure TMainForm.OnMapFiltersCloseClick(Sender: TObject);
begin
  FTagFilter.Visible := False;
end;

procedure TMainForm.OnCentreMapClick(Sender: TObject);
begin
  imLocalMap.Left := (MainContainer.Width - imLocalMap.Width) div 2;
  imLocalMap.Top := (MainContainer.Height - imLocalMap.Height) div 2;
end;

procedure TMainForm.OnShowLogClick(Sender: TObject);
begin
  meLog.Parent := MainContainer;
  meLog.Visible := not meLog.Visible;
  if meLog.Visible then
    meLog.Align := alClient
  else
    meLog.Align := alNone;
end;

procedure TMainForm.OnChoiceLocation(Value: TLocalMap);
begin
  FLocationPanel.Visible := False;
  Application.ProcessMessages;

  if FMapWrapper.Map.Name <> Value.Name then begin
    ShowMap(Value);
    FTagFilter.LoadQuests(Value);
  end;
end;

procedure TMainForm.OnMapFilterChanged(Sender: TObject);
begin
  FMapWrapper.Refresh;
end;

procedure TMainForm.SetFullScreenMode(const Value: Boolean);
begin
  FFormWrapper.FullScreen := Value;
  paToolBar.Visible := not FFormWrapper.FullScreen;
  FormResize(Self);
end;

procedure TMainForm.SetMapControlVisible(const Value: Boolean);
begin
  buFullScreen.Visible := Value;
  buZoomIn.Visible := Value;
  buZoomOut.Visible := Value;
  buMapFilters.Visible := Value;
  buCentreMap.Visible := Value;
  buShowLog.Visible := Value;
end;

procedure TMainForm.OnMapChange(Bitmap: TBitmap);
begin
//{$IFNDEF DEBUG}
//  Logger.Lines.Add('OnMapChange');
//{$ENDIF}
  imLocalMap.Width := Bitmap.Width;
  imLocalMap.Height := Bitmap.Height;
  imLocalMap.Picture.Assign(Bitmap);
end;

procedure TMainForm.ShowMap(const Map: TLocalMap);
begin
  SetLength(QuestFilter, Length(Map.Quests));
  if Length(Map.Quests) > 0 then
    FillChar(QuestFilter[0], Length(Map.Quests) * SizeOf(Boolean), 0);

  FMapWrapper.LoadMap(Map);
  SetMapControlVisible(True);

  FMapWrapper.Start;
end;

procedure TMainForm.MapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMousePosition.X := X;
  FMousePosition.Y := Y;
end;

procedure TMainForm.MapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  Offset = 5;
var
  DeltaX, DeltaY: Integer;
begin
  if FMousePosition.Empty then
    Exit;

{$IFNDEF DEBUG}
  Label1.Caption := Sender.ClassName + '.Position: (' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
  Label2.Caption := 'Image: (' + IntToStr(imLocalMap.Left) + ', ' + IntToStr(imLocalMap.Top) + ')';
{$ENDIF}

  DeltaX := X - FMousePosition.X;
  DeltaY := Y - FMousePosition.Y;

  if (Abs(DeltaX) > Offset) or (Abs(DeltaY) > Offset) then begin
    imLocalMap.Left := imLocalMap.Left + DeltaX;
    imLocalMap.Top := imLocalMap.Top + DeltaY;

    FMousePosition.X := X - DeltaX;
    FMousePosition.Y := Y - DeltaY;

    buFullScreen.Repaint;
    buZoomIn.Repaint;
  end;
end;

procedure TMainForm.MapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMousePosition.Empty := True;
end;

procedure TMainForm.MainContainer2MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FMapWrapper.ZoomOut;
end;

procedure TMainForm.MainContainer2MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FMapWrapper.ZoomIn;
end;

procedure TMainForm.buLocationClick(Sender: TObject);
begin
  FLocationPanel.Left := paToolBar.Width + 5;
  FLocationPanel.Top := buLocation.Top;
  FLocationPanel.Visible := not FLocationPanel.Visible;
end;

procedure TMainForm.buSettingsClick(Sender: TObject);
var
  Dialog: TedSettings;
begin
  Dialog := TedSettings.Create(Self);
  try
    if Dialog.Open then begin

    end;
  finally
    Dialog.Free;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
// FileName = '2024-02-20[21-57]_X.x, 0.0, Y.y_0.0, 0.5, 0.0, -0.0_0.00 (0).png';
// 2024-04-20[12-34]_-917.5, -56.9, 254.4_0.0, 0.7, 0.0, -0.7_10.07 (0).png
const
  TestFolder = 'e:\Projects\Delphi\EscapeFromTarkov\Test\';
  MaxFilesCount = 4;
var
  FileName: string;
  i: Integer;
begin
  i := 0;
  for FileName in TDirectory.GetFiles(TestFolder, '*.png', TSearchOption.soTopDirectoryOnly) do begin
    if i = FTestFileIndex then begin
      TFile.Copy(FileName, TPath.Combine(AppParams.SreenshotPath, ExtractFileName(FileName)));
      Break;
    end;

    Inc(i);
  end;

  Inc(FTestFileIndex);
  if FTestFileIndex >= MaxFilesCount then
    FTestFileIndex := 0;
end;

end.
