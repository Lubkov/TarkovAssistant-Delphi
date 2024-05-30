unit ME.DB.Point;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TPoint = class(TEntity)
  private
    FQuestID: Variant;
    FLeft: Integer;
    FTop: Integer;
  public
    constructor Create; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    procedure SetBounds(X, Y: Integer);

    property QuestID: Variant read FQuestID write FQuestID;
    property X: Integer read FLeft write FLeft;
    property Y: Integer read FTop write FTop;
  end;

implementation

{ TPoint }

constructor TPoint.Create;
begin
  inherited Create;

  FQuestID := Null;
  FLeft := 0;
  FTop := 0;
end;

procedure TPoint.Assign(const Source: TEntity);
begin
  inherited;

  FQuestID := TPoint(Source).QuestID;
  FLeft := TPoint(Source).X;
  FTop := TPoint(Source).Y;
end;

procedure TPoint.Assign(const DataSet: TDataSet);
begin
  inherited;

  FQuestID := DataSet.FieldByName('QuestID').Value;
  FLeft := DataSet.FieldByName('X').AsInteger;
  FTop := DataSet.FieldByName('Y').AsInteger;
end;

class function TPoint.EntityName: string;
begin
  Result := 'Point';
end;

class function TPoint.FieldList: string;
begin
  Result := 'ID, QuestID, X, Y';
end;

procedure TPoint.SetBounds(X, Y: Integer);
begin
  FLeft := X;
  FTop := Y;
end;

end.
