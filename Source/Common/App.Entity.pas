unit App.Entity;

interface

uses
  System.Classes, System.SysUtils, System.Variants, System.SysConst, System.JSON,
  FMX.Graphics;

type
  TEntityClass = class of TEntity;

  TEntity = class(TObject)
  private
  protected
    function GetIsNewInstance: Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; virtual; abstract;
    procedure Assign(const Source: TJSONValue); overload; virtual; abstract;
    procedure AssignTo(const Dest: TJSONObject); virtual; abstract;

//    property RecordState: TRecordState read FRecordState write FRecordState;
    property IsNewInstance: Boolean read GetIsNewInstance;
  end;

implementation

{ TEntity }

constructor TEntity.Create;
begin
  inherited;

//  FRecordState := TRecordState.New;
end;

destructor TEntity.Destroy;
begin

  inherited;
end;

end.
