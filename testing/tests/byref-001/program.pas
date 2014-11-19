function f(var x: String): Integer;
begin
    x := 'Hello, world!';
    f := 1;
end;

var
    s: String;
begin
    f(s);
    WriteLn(s);
end.
