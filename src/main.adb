with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with Principal;
with ICD;
with ClosedLoop;

procedure Main is

begin
   --  Insert code here.
   ClosedLoop.Init;
   for I in Integer range 1..100 loop
      ClosedLoop.Tick;
   end loop;

end Main;
