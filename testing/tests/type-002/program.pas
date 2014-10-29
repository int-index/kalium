function f(x: Char): String;
begin
    f := x;
    { ^^ typecast: Char -> String }
end;

begin
    WriteLn(f('H'), f('e'), 'llo, world!');
           {  ^^^     ^^^
              typecast: Literal String -> Char }
end.
