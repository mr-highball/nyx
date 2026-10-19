{ Both executable regions belong to the original include. Nested constructs
  exercise structural boundaries; the independently executed native child
  observes initialization and finalization without writing any disk file. }
initialization
  PhaseValue := 0;
  repeat
    Inc(PhaseValue);
  until PhaseValue = 2;
  case PhaseValue of
    2:
      begin
        try
          PhaseValue := 40;
        finally
          Inc(PhaseValue, 2);
        end;
      end;
  else
    PhaseValue := -1;
  end;

finalization

  if TracePhases then
  begin
    WriteLn('NYX_PHASE_FINAL=', PhaseValue);
  end;
