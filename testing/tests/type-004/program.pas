function f(s: String): String;
begin
    f := s + s;
end;

begin
    WriteLn(f('wow'), f('!'));
end.
