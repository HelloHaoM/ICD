with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with Principal;
with ICD;

package body ClosedLoop is

   Hrt : Heart.HeartType;                -- The simulated heart
   Monitor : HRM.HRMType;                -- The simulated heart rate monitor
   Generator : ImpulseGenerator.GeneratorType; -- The simulated generator
   HeartRate : BPM;
   Net : Network.Network;                -- The simulated network
   Card : Principal.PrincipalPtr := new Principal.Principal;  -- A cardiologist
   Clin : Principal.PrincipalPtr := new Principal.Principal;  -- A clinical assistant
   Patient : Principal.PrincipalPtr := new Principal.Principal; -- A patient
   Icd_tor : ICD.ICDType;                  --The ICD

   -- an array of known principals to use to initialise the network
   -- but note that the network can generate messages from other, unknown,
   -- principals too
   KnownPrincipals : access Network.PrincipalArray := new Network.PrincipalArray(0..2); 
   
   -- stores whether there was a message available on the network
   MsgAvailable : Boolean := True;
   
   -- stores the current message read from the network (if one was available)
   Msg : Network.NetworkMessage;
   
   -- stores some history information on measured heart rate
   History : Network.RateHistory;
   HistoryPos : Integer := History'First;
   CurrentTime : TickCount := 0;  -- current time as measured in ticks
   
   procedure Init is 
   begin
      
      -- set up the principals with the correct roles
   Principal.InitPrincipalForRole(Card.all,Principal.Cardiologist);
   Principal.InitPrincipalForRole(Clin.all,Principal.ClinicalAssistant);
   Principal.InitPrincipalForRole(Patient.all,Principal.Patient);
   KnownPrincipals(0) := Card;
   KnownPrincipals(1) := Clin;
   KnownPrincipals(2) := Patient;
   
   Put("Known Principals: "); New_Line;
   Principal.DebugPrintPrincipalPtr(Card); New_Line;
   Principal.DebugPrintPrincipalPtr(Clin); New_Line;
   Principal.DebugPrintPrincipalPtr(Patient); New_Line;
      
   -- Initialise the components and turn the machines on
   Heart.Init(Hrt);
   --HRM.Init(Monitor);
   --ImpulseGenerator.Init(Generator);
   Network.Init(Net,KnownPrincipals);
   ICD.Init( Icd_tor, Monitor, Generator);
   ICD.On(Icd_tor, Monitor, Generator, Hrt);
      
      
   end Init;
   
   
   procedure Tick is
   begin
      
      ICD.Tick(Icd_tor, Monitor, Generator, Hrt, CurrentTime, MsgAvailable, Net);
      
      Heart.Tick(Hrt);
      Network.Tick(Net);
      
      CurrentTime := CurrentTime + 1;
      delay 0.1;
      
   end Tick;
     
   
end ClosedLoop;
