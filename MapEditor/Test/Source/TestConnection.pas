unit TestConnection;

interface

uses
  System.Classes, System.SysUtils, System.Variants, TestFramework, Data.DB,
  ME.Connection;

type
  TTestConnection = class(TTestCase)
  private
    FConnection: TMEConnection;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
    procedure TestDisconnect;
  end;

implementation

procedure TTestConnection.SetUp;
begin
  FConnection := TMEConnection.Create(nil);
end;

procedure TTestConnection.TearDown;
begin
  FreeAndNil(FConnection);
end;

procedure TTestConnection.TestConnect;
begin
  FConnection.Connect;
  CheckTrue(FConnection.Connected);
end;

procedure TTestConnection.TestDisconnect;
begin
  FConnection.Connect;
  CheckTrue(FConnection.Connected);
  FConnection.Disconnect;
  CheckFalse(FConnection.Connected);
end;

initialization
  RegisterTest(TTestConnection.Suite);

end.

