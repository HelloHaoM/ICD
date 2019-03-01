with Heart;
with RandomNumber; 
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body HRM is
   
   Error : constant Float := 0.02;  -- The error margin of the pump readings
   
   procedure Init(Hrm : out HRMType) is
   begin
      Hrm.IsOn := False;
      Hrm.Rate := Measures.BPM'First;
   end Init;

   procedure On(Hrm : out HRMType; Hrt : in Heart.HeartType) is
   begin
     -- Get an initial reading for the heart
      Hrm.IsOn := True;
      Put_Line("HeartRateMonitor is on");
      Heart.GetRate(Hrt, Hrm.Rate);
   end On;
      
   procedure Off(Hrm : in out HRMType) is
   begin
      Hrm.IsOn := False;
      Put_Line("HeartRateMonitor is off");
   end Off;
   
   function IsOn(Hrm : in HRMType) return Boolean is
   begin
      return Hrm.IsOn;
   end IsOn;
   
   procedure GetRate(Hrm : in HRMType;
		     Rate : out Measures.BPM) is
   begin
      if Hrm.IsOn then
	 Rate := Hrm.Rate;
      else
	 Rate := Measures.BPM'First;
      end if;
   end GetRate;
   
   procedure Tick(Hrm : in out HRMType; Hrt : in Heart.HeartType) is
   begin
      if Hrm.IsOn then
	 -- read the heart rate from the heart
	 Heart.GetRate(Hrt, Hrm.Rate);
	 
	 -- Insert some random variation
	 Hrm.Rate := 
	   Measures.LimitBPM(RandomNumber.UniformIntegerWithError(Hrm.Rate, 
								  Error));
      else
	 -- If the monitor is not on, return 0 for both values
	 Hrm.Rate := Measures.BPM'First;
      end if; 
      Put("Measured heart rate  = ");
      Put(Item => Hrm.Rate);
      New_Line;
      
   end Tick;
end HRM;
