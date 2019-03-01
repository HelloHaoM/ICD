with RandomNumber;
with Heart;
with Ada.Text_IO;use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ImpulseGenerator is
   
   procedure Init(Gen : out GeneratorType) is
   begin
      Gen.IsOn := False;
      Gen.Impulse := Measures.Joules'First;
   end Init;
   
   procedure On(Gen : in out GeneratorType) is
   begin
      Gen.IsOn := True;
      Put_Line("ImpulseGenerator is on");
   end On;
      
   procedure Off(Gen : in out GeneratorType) is
   begin
      Gen.IsOn := False;
      Put_Line("ImpulseGenerator is off");
   end Off;
   
   function IsOn(Gen : in GeneratorType) return Boolean is
   begin
      return Gen.IsOn;
   end IsOn;
   
   procedure SetImpulse(Gen : in out GeneratorType; J : in Measures.Joules) is
   begin
      -- Only set the impulse if the machine is on
      if Gen.IsOn then
         Gen.Impulse := J;
         Put("set Impulse to new value: ");
         Put(Item => J);
         New_Line;
      else
         Put_Line("set Impulse failed, Generator is off. ");
      end if;
   end SetImpulse;
   
   procedure Tick(Gen : in GeneratorType; Hrt : in out Heart.HeartType) is
      HrtVariable : Heart.HeartType;
   begin
      -- Administer the impulse if the generator is on
      if Gen.IsOn then
	 -- For an 'out' variable, we must create a new variable for
	 --  the call, and the copy the output value from
	 --  Heart.SetImpulse back to Ptnt
	 HrtVariable := Hrt;
	 Heart.SetImpulse(HrtVariable, Gen.Impulse);
	 Hrt := HrtVariable;
      end if;
   end Tick;
   
end ImpulseGenerator;
