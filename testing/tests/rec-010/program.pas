function fib_(x: Integer; var fib_2: Integer): Integer;
var
    a, b, c, d: Integer;
begin
    if x = 0 then
     begin
        fib_  := 0;
        fib_2 := 1;
     end
    else
     begin
        a := fib_(x div 2, b);
        c := a * (2 * b - a);
        d := b * b + a * a;
        if x mod 2 = 0
         then begin
                 fib_  := c;
                 fib_2 := d;
              end
         else begin
                 fib_  := d;
                 fib_2 := c + d;
              end;
     end;
end;

function fib(x: Integer): Integer;
var
    y: Integer;
begin
    fib := fib_(x, y);
end;

var
    x: LongInt;
begin
    ReadLn(x);
    x := fib(x);
    WriteLn(x);
end.
