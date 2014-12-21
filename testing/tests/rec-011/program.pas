(*
function M(n: Integer): Integer; forward;
*)

function F(n: Integer): Integer;
begin
   if n = 0 then
      F := 1
   else
      F := n - M(F(n-1));
end;

function M(n: Integer): Integer;
begin
   if n = 0 then
      M := 0
   else
      M := n - F(M(n-1));
end;

var
   i: Integer;

begin
   for i := 0 to 19 do
      write(F(i), ' ');
   writeln;
   for i := 0 to 19 do
      write(M(i), ' ');
   writeln;
end.
