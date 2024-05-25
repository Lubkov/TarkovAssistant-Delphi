unit ME.Point;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TPoint = class(TEntity)
  private
    FLeft: Integer;
    FTop: Integer;
  public
    constructor Create; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    procedure SetBounds(X, Y: Integer);

    property X: Integer read FLeft write FLeft;
    property Y: Integer read FTop write FTop;
  end;

implementation

{ TPoint }

constructor TPoint.Create;
begin
  inherited Create;

  FLeft := 0;
  FTop := 0;
end;

procedure TPoint.Assign(const Source: TEntity);
begin
  inherited;

  FLeft := TPoint(Source).X;
  FTop := TPoint(Source).Y;
end;

procedure TPoint.Assign(const DataSet: TDataSet);
begin
  inherited;

  FLeft := DataSet.FieldByName('X').AsInteger;
  FTop := DataSet.FieldByName('Y').AsInteger;
end;

class function TPoint.EntityName: string;
begin
  Result := 'Point';
end;

class function TPoint.FieldList: string;
begin
  Result := 'ID, X, Y';
end;

procedure TPoint.SetBounds(X, Y: Integer);
begin
  FLeft := X;
  FTop := Y;
end;

end.
