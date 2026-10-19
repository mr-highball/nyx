{ Each routine has an ordinary body. Setting the compiler's I/O status avoids
  any disk effect while making automatic caller-side checks independently
  observable. These implementation directives do not qualify interface/member
  eligibility or another compiler version/target. }
procedure BareIOCheck; iocheck;
begin
  System.InOutRes := 123;
end;

procedure BracketIOCheck; [IOCheck];
begin
  System.InOutRes := 124;
end;

procedure OrdinaryIOResult;
begin
  System.InOutRes := 125;
end;
