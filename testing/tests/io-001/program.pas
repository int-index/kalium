function Greeting: String;
begin
    Greeting := 'Hello, world!';
end;

procedure Say(greet: String);
begin
    WriteLn(greet);
end;

begin
    Say(Greeting());
end.
