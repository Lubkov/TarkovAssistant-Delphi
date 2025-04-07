unit ME.Trader;

interface

uses
  System.SysUtils;

type
  TTrader = (None, Prapor, Therapist, Skier, Peacemaker, Mechanic, Ragman, Jaeger, Fence, Lightkeeper);

  function TraderToStr(Value: TTrader): string;

implementation

function TraderToStr(Value: TTrader): string;
begin
  case Value of
    TTrader.Prapor:
      Result := 'Прапор';
    TTrader.Therapist:
      Result := 'Терапевт';
    TTrader.Skier:
      Result := 'Лыжник';
    TTrader.Peacemaker:
      Result := 'Миротворец';
    TTrader.Mechanic:
      Result := 'Механик';
    TTrader.Ragman:
      Result := 'Барахольщик';
    TTrader.Jaeger:
      Result := 'Егерь';
    TTrader.Fence:
      Result := 'Скупщик';
    TTrader.Lightkeeper:
      Result := 'Смотритель';
  else
    Result := '';
  end;
end;

end.
