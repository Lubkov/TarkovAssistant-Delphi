unit ME.Form.Filter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  IFormFilter<T> = interface
    ['{719BCA52-4B58-43BF-9A8D-4AC3CF756513}']

    function GetValue: T;
    procedure SetValue(const Value: T);

    property Value: T read GetValue write SetValue;
  end;

  TFormFilter = class(TFrame)
  private
  public
  end;

implementation

{$R *.fmx}

end.
