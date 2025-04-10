unit ME.List.Filter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Form.Filter, System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox;

type
  TListFilter = class(TFormFilter)
    edFilterValues: TComboBox;
    laFilterName: TLabel;

    procedure edFilterValuesChange(Sender: TObject);
  private
  protected
    function GetDisplayValue: Variant; override;
    procedure SetDisplayValue(const Value: Variant); override;
    procedure Locate(const Value: Variant); override;
  public
  end;

implementation

{$R *.fmx}

{ TListFilter }

procedure TListFilter.edFilterValuesChange(Sender: TObject);
begin
  FIsGUIChanged := True;

  if edFilterValues.ItemIndex >= 0 then
    KeyValue := GetDisplayValue
  else
    KeyValue := Null;
end;

function TListFilter.GetDisplayValue: Variant;
begin
  Result := edFilterValues.ItemIndex;
end;

procedure TListFilter.SetDisplayValue(const Value: Variant);
begin
  edFilterValues.ItemIndex := Value;
end;

procedure TListFilter.Locate(const Value: Variant);
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    edFilterValues.ItemIndex := -1
  else
    SetDisplayValue(Value);
end;

end.
