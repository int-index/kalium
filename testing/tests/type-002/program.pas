function f(x: Char): String;
begin
    f := x; { typecast: Char -> String }
end;

function g(x: String): String;
begin
    g := x;
end;

begin
    WriteLn(f('H'), g('e'), 'llo, world!');
end.
